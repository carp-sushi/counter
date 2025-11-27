# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A Haskell Servant web service that provides a counter API backed by Redis. The service exposes REST endpoints to increment, decrement, and query counters by key.

## Build System

This project uses Stack (LTS 23.28) with Make targets for common tasks.

### Essential Commands

```bash
# Build the project
stack build

# Run tests
stack test

# Run the server (port 9000)
stack run

# Format code with fourmolu
make format

# Lint code with hlint
make lint

# Watch mode for development
make watch

# Complete workflow (format, build, test)
make all

# Clean build artifacts
make clean
```

### Running Single Tests

Stack/Tasty doesn't have built-in single test filtering. Run all tests with `stack test`.

## Architecture

### Layer Structure

The codebase follows a clean architecture pattern with clear separation of concerns:

1. **Domain Layer** (`Counter.Domain`): Core types (`Counter`, `Key`, `Count`) and type classes (`Incrementer`, `Querier`)
2. **API Layer** (`Counter.Api`): Servant API type definition with routes at `/counters/api/v1/{key}`
3. **Handler Layer** (`Counter.Handler`): Request handlers that use domain type classes
4. **Application Layer** (`Counter.App`): Wires handlers to API routes using Servant's `ServerT`
5. **Environment Layer** (`Counter.Env`): `AppT` monad transformer stack with `Env` record for dependency injection
6. **Repository Layer** (`Counter.Repo`): `CounterRepo` record-of-functions pattern for testability
7. **Database Layer** (`Counter.Database`): Redis implementation of `CounterRepo`

### Key Patterns

**Repository Pattern**: The `CounterRepo` is a record containing function pointers, allowing easy substitution of implementations:
- Production: Redis-backed implementation in `Counter.Database`
- Testing: In-memory HashMap implementation in `test/State.hs`

**Type Class Constraints**: Handlers use `Incrementer` and `Querier` type classes rather than concrete types, enabling polymorphic implementations in `AppT`.

**Reader Monad**: The `AppT` transformer wraps `ReaderT Env`, providing access to:
- `envCounterRepo`: Repository implementation
- `envLogFn`: Logging function

**Dependency Injection**: The `Env` record is constructed at startup and threaded through the application via `AppT`, with type class instances (src/Counter/Env.hs:45-55) dispatching to repo functions.

### Parameter Order Convention

Top-level functions follow the pattern: data arguments first, environment/config last. This enables partial application:
```haskell
queryCounter :: Key -> Env -> IO Counter
incrementCounter :: Key -> Env -> IO Counter
```

## API Endpoints

- `GET /status` - Health check (returns "up")
- `POST /counters/api/v1/{key}` - Increment counter
- `DELETE /counters/api/v1/{key}` - Decrement counter
- `GET /counters/api/v1/{key}` - Query counter value

## Code Standards

### HLint Configuration

The project uses `.hlint.yaml` to prohibit dangerous Haskell functions and patterns:
- Partial functions (`head`, `tail`, `fromJust`, etc.) - use safe alternatives
- Unsafe IO operations (`unsafePerformIO`, etc.)
- `forkIO` - use the async library instead
- `return` - use `pure` instead (modern Haskell convention)
- String-based IO - use `Text` or `ByteString`
- Lazy folds - use strict versions (`foldl'`, `foldMap'`)

### Default Extensions

`OverloadedStrings` is enabled project-wide in `package.yaml`.

## Testing

Tests use Hspec with `hspec-wai` for HTTP endpoint testing. The test suite:
- Uses `fakeCounterRepo` backed by `MVar (HashMap Key Count)` for in-memory state
- Sets up fresh application state per test suite with `setupApp`
- Tests all API endpoints with real HTTP requests via `hspec-wai`

## External Dependencies

**Runtime**: Redis must be running on localhost:6379 (default connection in `Counter.Database.defaultConnection`)
