# Quick Test & Demo Reference

Fast reference for running tests and demos.

## Quick Commands

### Tests

```bash
# Run all tests
emacs --batch -l tests/run-all-tests.el

# Run specific suite
emacs --batch -l tests/test-core.el -f test-core-all
emacs --batch -l tests/test-llm.el -f test-llm-all
emacs --batch -l tests/test-federation.el -f test-federation-all
emacs --batch -l tests/e2e/test-workflow.el -f test-e2e-all
```

### Demos

```bash
# Interactive launcher
emacs -l demos/run-demos.el

# Run specific demo
emacs -l demos/01-personal-knowledge-base.el
emacs -l demos/02-research-assistant.el
emacs -l demos/03-code-analysis.el
emacs -l demos/04-team-collaboration.el
```

## Current Status

**LLM Tests:** ✅ 5/5 passing (100%)
**Core Tests:** ⚠️  2/5 passing (40% - module bugs)
**Federation Tests:** ⚠️  3/4 passing (75% - module bugs)
**E2E Tests:** ⚠️  Blocked by federation issues

## Known Issues

See [KNOWN_ISSUES.md](KNOWN_ISSUES.md) for:
- ECDSA signature bug (P0)
- Prolog query bug (P0)
- Datalog variable bug (P0)
- M-expression eval bug (P2)
- Federation blackboard bug (P3)

## Files

| File | Purpose |
|------|---------|
| `tests/run-all-tests.el` | Master test runner |
| `tests/test-core.el` | Core functionality tests |
| `tests/test-llm.el` | LLM integration tests |
| `tests/test-federation.el` | Federation tests |
| `tests/e2e/test-workflow.el` | End-to-end tests |
| `demos/run-demos.el` | Interactive demo launcher |
| `demos/01-*.el` | Personal knowledge base demo |
| `demos/02-*.el` | Research assistant demo |
| `demos/03-*.el` | Code analysis demo |
| `demos/04-*.el` | Team collaboration demo |

## Documentation

- [tests/README.md](tests/README.md) - Complete test documentation
- [demos/README.md](demos/README.md) - Complete demo documentation
- [TESTING-AND-DEMOS.md](TESTING-AND-DEMOS.md) - Master guide
- [KNOWN_ISSUES.md](KNOWN_ISSUES.md) - Bug tracking
- [TEST-SUITE-SUMMARY.md](TEST-SUITE-SUMMARY.md) - Implementation summary
