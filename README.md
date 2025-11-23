# ReputationKernel

A decentralized cross-app reputation system built on Stacks (Clarity smart contract).

## Overview

ReputationKernel enables multiple applications to contribute to a unified reputation score for users. Apps (scorers) can grant or revoke reputation points within configurable caps, while admins manage the system and apply reputation decay.

## Features

- **Multi-app Reputation**: Apps register as scorers and grant/revoke reputation independently
- **Per-Action Caps**: Prevent any single app from inflating scores beyond limits
- **Reputation Tracking**: Total user score + per-app contribution breakdown
- **Admin Controls**: Register scorers, apply percentage-based decay to scores
- **Audit Ledger**: Immutable record of all reputation changes

## Quick Start

### Prerequisites
- [Clarinet](https://docs.hiro.so/clarinet) installed

### Setup
```bash
clarinet integrate
npm install
