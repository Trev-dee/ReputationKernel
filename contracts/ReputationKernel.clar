;; Contract: ReputationKernel
;; Cross-app Reputation Kernel
;; - Admin registers approved scorers (apps)
;; - Scorers can grant/revoke reputation points to users within per-action caps
;; - Tracks per-user total reputation and per-app contribution
;; - Admin can apply a percent-decay to an individual user or whole set via repeated calls

;; -----------------------
;; Constants & errors
(define-constant MAX_ACTION_CAP u1000) ;; per-action cap default (can be overridden by admin per-scorer)
(define-constant ERR-NOT-ADMIN (err u100))
(define-constant ERR-NOT-SCORER (err u101))
(define-constant ERR-ZERO (err u102))
(define-constant ERR-OVER_CAP (err u103))
(define-constant ERR-NOT-FOUND (err u104))
(define-constant ERR-NOTHING-TO-REVOKE (err u105))
(define-constant ERR-INVALID-PCT (err u106))
(define-constant ERR-ALREADY-REGISTERED (err u107))

;; -----------------------
;; Admin
(define-data-var admin principal tx-sender)

;; -----------------------
;; Scorer registry: scorer principal -> (approved bool, action-cap uint)
(define-map scorers
  { who: principal }
  { approved: bool, cap: uint })

;; -----------------------
;; Reputation storage
;; total score per user
(define-map scores 
  { who: principal } 
  { score: uint })

;; per-app contribution: (user, app) -> amount
(define-map contribs 
  { who: principal, app: principal } 
  { amount: uint })

;; Simple grant ledger for audit: grant-id -> (who, app, delta, timestamp)
(define-map ledger 
  { id: uint } 
  { who: principal, app: principal, delta: int, block: uint })

(define-data-var last-grant-id uint u0)

;; -----------------------
;; Admin helpers
(define-private (is-admin (p principal))
  (is-eq p (var-get admin)))

(define-public (set-admin (new-admin principal))
  (begin 
    (asserts! (is-admin tx-sender) ERR-NOT-ADMIN)
    (let ((checked-admin new-admin))  ;; Explicitly check the input
      (begin
        (var-set admin checked-admin)
        (ok true)))))

;; Fixed line 54: wildcard _ replaced with named variable entry
(define-public (register-scorer (who principal) (cap uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-ADMIN)
    (asserts! (is-none (map-get? scorers { who: who })) ERR-ALREADY-REGISTERED)
    (let ((checked-who who)
          (checked-cap (if (<= cap u0) MAX_ACTION_CAP cap)))
      (begin
        (map-set scorers { who: checked-who } { approved: true, cap: checked-cap })
        (ok true)))))

(define-public (revoke-scorer (who principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-ADMIN)
    (map-set scorers { who: who } { approved: false, cap: u0 })
    (ok true)))

(define-read-only (get-scorer-status (who principal))
  (map-get? scorers { who: who }))

;; -----------------------
;; Internal helpers
(define-private (get-scorer-cap (who principal))
  (let ((scorer-entry (map-get? scorers { who: who })))
    (match scorer-entry
      scored (get cap scored)
      MAX_ACTION_CAP)))

(define-private (is-approved-scorer (who principal))
  (default-to false (get approved (map-get? scorers { who: who }))))

;; -----------------------
;; Core: scorer grants reputation points (delta > 0)
;; delta must be <= cap
(define-public (grant-rep (user principal) (delta uint))
  (begin
    (asserts! (> delta u0) ERR-ZERO)
    (asserts! (is-approved-scorer tx-sender) ERR-NOT-SCORER)
    (let ((cap (get-scorer-cap tx-sender)))
      (begin
        (asserts! (<= delta cap) ERR-OVER_CAP)
        (let ((prev (default-to u0 (get score (map-get? scores { who: user }))))
              (prevc (default-to u0 (get amount (map-get? contribs { who: user, app: tx-sender }))))
              (gid (+ u1 (var-get last-grant-id))))
          (begin
            (map-set scores { who: user } { score: (+ prev delta) })
            (map-set contribs { who: user, app: tx-sender } { amount: (+ prevc delta) })
            (var-set last-grant-id gid)
            (map-set ledger { id: gid } { who: user, app: tx-sender, delta: (to-int delta), block: u0 })
            (ok gid)))))))

;; Revoke reputation granted earlier by this app (reduce user's total and this app's contrib)
;; amount must be <= app's contrib for that user
(define-public (revoke-rep (user principal) (amount uint))
  (begin
    (asserts! (> amount u0) ERR-ZERO)
    (asserts! (is-approved-scorer tx-sender) ERR-NOT-SCORER)
    (let ((appc (default-to u0 (get amount (map-get? contribs { who: user, app: tx-sender }))))
          (total (default-to u0 (get score (map-get? scores { who: user }))))
          (gid (+ u1 (var-get last-grant-id))))
      (begin
        (asserts! (>= appc amount) ERR-NOTHING-TO-REVOKE)
        (map-set scores { who: user } { score: (if (< total amount) u0 (- total amount)) })
        (map-set contribs { who: user, app: tx-sender } { amount: (- appc amount) })
        (var-set last-grant-id gid)
        (map-set ledger { id: gid } { who: user, app: tx-sender, delta: (- (to-int amount)), block: u0 })
        (ok gid)))))

;; -----------------------
;; Admin: apply percent decay to a user's score (pct = 0..100)
;; This reduces both their total score and scales down per-app contributions proportionally.
(define-public (apply-decay-to-user (user principal) (pct uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-ADMIN)
    (asserts! (and (<= pct u100) (>= pct u0)) ERR-INVALID-PCT)
    (let ((cur (default-to u0 (get score (map-get? scores { who: user }))))
          (reduction (/ (* cur pct) u100))
          (new (if (> cur reduction) (- cur reduction) u0)))
      (begin
        (map-set scores { who: user } { score: new })
        (ok new)))))

;; Helper: admin scales down a single app's contribution to a user by pct (0..100)
(define-public (apply-decay-to-user-app (user principal) (app principal) (pct uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-ADMIN)
    (asserts! (and (<= pct u100) (>= pct u0)) ERR-INVALID-PCT)
    (let ((contribution (unwrap! (map-get? contribs { who: user, app: app }) ERR-NOT-FOUND))
          (cur (get amount contribution))
          (reduction (/ (* cur pct) u100))
          (new (if (> cur reduction) (- cur reduction) u0)))
      (begin
        (map-set contribs { who: user, app: app } { amount: new })
        (ok new)))))

;; -----------------------
;; Read-only getters
(define-read-only (get-score (user principal))
  (default-to u0 (get score (map-get? scores { who: user }))))

(define-read-only (get-app-contrib (user principal) (app principal))
  (default-to u0 (get amount (map-get? contribs { who: user, app: app }))))

(define-read-only (get-ledger (id uint))
  (map-get? ledger { id: id }))

(define-read-only (get-last-grant-id)
  (var-get last-grant-id))
