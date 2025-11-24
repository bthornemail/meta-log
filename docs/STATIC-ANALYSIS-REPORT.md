# Static Analysis Report

**Generated**: 2025-11-24 01:44:34

## Summary

- **Total Functions Defined**: 1042
- **Functions Called**: 709
- **Potentially Unused Functions**: 274

## Potentially Unused Functions

| Function | File | Line | Type | Notes |
|----------|------|------|------|-------|
| `org-babel-execute` | `modules/meta-log-babel.el` | 23 | internal | Internal function |
| `org-babel-execute` | `modules/meta-log-babel.el` | 29 | internal | Internal function |
| `org-babel-execute` | `modules/meta-log-babel.el` | 40 | internal | Internal function |
| `org-babel-execute` | `modules/meta-log-babel.el` | 51 | internal | Internal function |
| `org-babel-execute` | `modules/meta-log-babel.el` | 57 | internal | Internal function |
| `org-babel-execute` | `modules/meta-log-babel.el` | 64 | internal | Internal function |
| `generate-template` | `modules/meta-log-metacircular.el` | 152 | internal | Internal function |
| `make-integrated-system-state` | `scheme/autonomy/integrated-system.scm` | 26 | internal | Internal function |
| `save-learning-state` | `scheme/autonomy/learning.scm` | 93 | internal | Internal function |
| `run-complexity-benchmark` | `scheme/consciousness/complexity.scm` | 165 | internal | Internal function |
| `current-time-millis` | `scheme/consciousness/complexity.scm` | 185 | internal | Internal function |
| `random-noise` | `scheme/consciousness/dynamics.scm` | 182 | internal | Internal function |
| `reflection-depth` | `scheme/consciousness/metrics.scm` | 187 | internal | Internal function |
| `self-model-accuracy` | `scheme/consciousness/metrics.scm` | 204 | internal | Internal function |
| `get-current-consciousness-state` | `scheme/consciousness/self-recognition.scm` | 57 | internal | Internal function |
| `list-sensors` | `scheme/sensors/manager.scm` | 87 | internal | Internal function |
| `json-` | `scheme/substrate/canvasl.scm` | 50 | internal | Internal function |
| `sexp-` | `scheme/substrate/canvasl.scm` | 65 | internal | Internal function |
| `bytevector-` | `scheme/substrate/cdmp.scm` | 23 | internal | Internal function |
| `uuid-generate` | `scheme/substrate/runtime.scm` | 38 | internal | Internal function |
| `current-timestamp` | `scheme/substrate/runtime.scm` | 68 | internal | Internal function |
| `execute-next-task` | `scheme/substrate/runtime.scm` | 204 | internal | Internal function |
| `static-analysis-run` | `scripts/static-analysis.el` | 110 | internal | Internal function |

## Analysis Notes

- Functions marked as 'internal' (containing `--`) are excluded
- Test functions are excluded
- Functions only called in their definition file may be legitimate
- **Manual review required** before removing any functions
- Public API functions should be reviewed especially carefully
- Scheme functions with `?` suffix are now properly detected
- Dynamic dispatch (apply/funcall) is now detected
- Note: Some dynamic dispatch patterns may still be missed

## Recommendations

1. **Review Public API Functions**: Check if unused public API functions should be:
   - Documented as optional/advanced features
   - Deprecated if no longer needed
   - Kept for backward compatibility

2. **Internal Functions**: Review if these are:
   - Helper functions for future use
   - Part of incomplete implementations
   - Truly unused and safe to remove

