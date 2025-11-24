# Static Analysis Report

**Generated**: 2025-11-24 02:08:08

## Summary

- **Total Functions Defined**: 1043
- **Functions Called**: 710
- **Potentially Unused Functions**: 162

## Potentially Unused Functions

| Function | File | Line | Type | Notes |
|----------|------|------|------|-------|
| `make-integrated-system-state` | `scheme/autonomy/integrated-system.scm` | 26 | incomplete | Part of incomplete module - keep for future implementation |
| `save-learning-state` | `scheme/autonomy/learning.scm` | 93 | incomplete | Part of incomplete module - keep for future implementation |
| `run-complexity-benchmark` | `scheme/consciousness/complexity.scm` | 165 | incomplete | Part of incomplete module - keep for future implementation |
| `current-time-millis` | `scheme/consciousness/complexity.scm` | 185 | incomplete | Part of incomplete module - keep for future implementation |
| `random-noise` | `scheme/consciousness/dynamics.scm` | 182 | incomplete | Part of incomplete module - keep for future implementation |
| `reflection-depth` | `scheme/consciousness/metrics.scm` | 187 | incomplete | Part of incomplete module - keep for future implementation |
| `self-model-accuracy` | `scheme/consciousness/metrics.scm` | 204 | incomplete | Part of incomplete module - keep for future implementation |
| `get-current-consciousness-state` | `scheme/consciousness/self-recognition.scm` | 57 | incomplete | Part of incomplete module - keep for future implementation |
| `list-sensors` | `scheme/sensors/manager.scm` | 87 | incomplete | Part of incomplete module - keep for future implementation |
| `org-babel-execute` | `modules/meta-log-babel.el` | 23 | internal | Internal function |
| `org-babel-execute` | `modules/meta-log-babel.el` | 29 | internal | Internal function |
| `org-babel-execute` | `modules/meta-log-babel.el` | 40 | internal | Internal function |
| `org-babel-execute` | `modules/meta-log-babel.el` | 51 | internal | Internal function |
| `org-babel-execute` | `modules/meta-log-babel.el` | 57 | internal | Internal function |
| `org-babel-execute` | `modules/meta-log-babel.el` | 64 | internal | Internal function |
| `generate-template` | `modules/meta-log-metacircular.el` | 152 | internal | Internal function |
| `json-` | `scheme/substrate/canvasl.scm` | 50 | internal | Internal function |
| `sexp-` | `scheme/substrate/canvasl.scm` | 65 | internal | Internal function |
| `bytevector-` | `scheme/substrate/cdmp.scm` | 23 | internal | Internal function |
| `uuid-generate` | `scheme/substrate/runtime.scm` | 38 | internal | Internal function |
| `current-timestamp` | `scheme/substrate/runtime.scm` | 68 | internal | Internal function |
| `execute-next-task` | `scheme/substrate/runtime.scm` | 204 | internal | Internal function |
| `meta-log-find-evolutions-package` | `modules/meta-log-automata.el` | 19 | public_api_documented | Documented public API - likely called by users, not internally |
| `meta-log-datalog-add-rule` | `modules/meta-log-datalog.el` | 44 | public_api_documented | Documented public API - likely called by users, not internally |
| `meta-log-p-adic-point-in-hp` | `modules/meta-log-p-adic.el` | 66 | public_api_documented | Documented public API - likely called by users, not internally |
| `meta-log-p-adic-modular-form` | `modules/meta-log-p-adic.el` | 78 | public_api_documented | Documented public API - likely called by users, not internally |
| `meta-log-partition-recover` | `modules/meta-log-partition.el` | 118 | public_api_documented | Documented public API - likely called by users, not internally |
| `meta-log-partition-padic-height-e8` | `modules/meta-log-partition.el` | 134 | public_api_documented | Documented public API - likely called by users, not internally |
| `meta-log-partition-detect-e8-ramification` | `modules/meta-log-partition.el` | 143 | public_api_documented | Documented public API - likely called by users, not internally |
| `meta-log-prolog-add-rule` | `modules/meta-log-prolog.el` | 83 | public_api_documented | Documented public API - likely called by users, not internally |
| `meta-log-qqf-e8-theta-link` | `modules/meta-log-quadratic-forms.el` | 234 | public_api_documented | Documented public API - likely called by users, not internally |
| `meta-log-quaternion-norm-form` | `modules/meta-log-quaternion.el` | 66 | public_api_documented | Documented public API - likely called by users, not internally |
| `meta-log-3d-project-canvasl` | `modules/meta-log-3d-projection.el` | 22 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-3d-project-to-r5rs` | `modules/meta-log-3d-projection.el` | 101 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-automata-classify-geometric-type` | `modules/meta-log-automata.el` | 105 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-automata-extract-geometric-consensus` | `modules/meta-log-automata.el` | 137 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-benchmark-get-memory` | `modules/meta-log-benchmark.el` | 46 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-canvas-api-generate-code` | `modules/meta-log-canvas-api.el` | 56 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-canvas-api-build-template` | `modules/meta-log-canvas-api.el` | 70 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-chat-protect-history` | `modules/meta-log-chat.el` | 236 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-crypto-generate-mnemonic` | `modules/meta-log-crypto.el` | 310 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-crypto-derive-key-quaternion` | `modules/meta-log-crypto.el` | 386 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-crypto-bip32-to-e8` | `modules/meta-log-crypto.el` | 395 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-crypto-verify` | `modules/meta-log-crypto.el` | 480 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-crypto-get-local-identity` | `modules/meta-log-crypto.el` | 819 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-crypto-get-local-private-key` | `modules/meta-log-crypto.el` | 825 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-dashboard-refresh` | `modules/meta-log-dashboard.el` | 31 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-datalog-create-db` | `modules/meta-log-datalog.el` | 24 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-datalog-evaluate-program` | `modules/meta-log-datalog.el` | 71 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-datalog-stratify-program` | `modules/meta-log-datalog.el` | 86 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-epicycloid-point` | `modules/meta-log-drinfeld.el` | 109 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-drinfeld-shimura-uniformization` | `modules/meta-log-drinfeld.el` | 147 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-drinfeld-deltoid-cusps` | `modules/meta-log-drinfeld.el` | 187 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-drinfeld-astroid-symmetry` | `modules/meta-log-drinfeld.el` | 199 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-e8-theta-modular-transformation` | `modules/meta-log-e8-theta.el` | 152 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-e8-theta-eisenstein-series-check` | `modules/meta-log-e8-theta.el` | 338 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-e8-initialize` | `modules/meta-log-e8.el` | 183 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-get-local-peer-identity` | `modules/meta-log-federation.el` | 80 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-announce-peer` | `modules/meta-log-federation.el` | 107 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-discover-peers` | `modules/meta-log-federation.el` | 123 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-sync-blackboard` | `modules/meta-log-federation.el` | 170 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-get-blackboard-state` | `modules/meta-log-federation.el` | 186 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-merge-state` | `modules/meta-log-federation.el` | 193 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-setup-peer-discovery` | `modules/meta-log-federation.el` | 225 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-load-blackboard` | `modules/meta-log-federation.el` | 252 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-save-node-to-blackboard` | `modules/meta-log-federation.el` | 270 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-detect-partition` | `modules/meta-log-federation.el` | 295 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-handle-partition` | `modules/meta-log-federation.el` | 311 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-federation-swarm-orbit` | `modules/meta-log-federation.el` | 327 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-deltoid-3-cusp-points` | `modules/meta-log-geometric-alignments.el` | 35 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-astroid-4-cusp-points` | `modules/meta-log-geometric-alignments.el` | 56 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-astroid-quaternion-symmetry` | `modules/meta-log-geometric-alignments.el` | 65 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-p-adic-rosette` | `modules/meta-log-geometric-alignments.el` | 152 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-p-adic-deltoid` | `modules/meta-log-geometric-alignments.el` | 167 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-p-adic-epicycloid` | `modules/meta-log-geometric-alignments.el` | 180 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-geometric-alignments-render` | `modules/meta-log-geometric-alignments.el` | 199 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-geometric-select-type` | `modules/meta-log-geometric-consensus.el` | 58 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-geometric-must-local` | `modules/meta-log-geometric-consensus.el` | 133 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-geometric-should-local` | `modules/meta-log-geometric-consensus.el` | 139 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-geometric-may-local` | `modules/meta-log-geometric-consensus.el` | 145 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-geometric-must-federation` | `modules/meta-log-geometric-consensus.el` | 151 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-geometric-should-federation` | `modules/meta-log-geometric-consensus.el` | 157 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-geometric-must-global` | `modules/meta-log-geometric-consensus.el` | 163 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-geometric-generate-proof-certificate` | `modules/meta-log-geometric-consensus.el` | 169 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-identity-get-current-peer` | `modules/meta-log-identity.el` | 148 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-identity-set-current-peer` | `modules/meta-log-identity.el` | 153 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-identity-sign-message-current` | `modules/meta-log-identity.el` | 158 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-identity-verify-peer-current` | `modules/meta-log-identity.el` | 166 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-identity-verify-frbac-permission` | `modules/meta-log-identity.el` | 202 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-ingest-watch-folder` | `modules/meta-log-ingest.el` | 204 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-ingest-file-changed` | `modules/meta-log-ingest.el` | 216 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-inode-read-file` | `modules/meta-log-inode.el` | 141 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-inode-find-hard-links` | `modules/meta-log-inode.el` | 152 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-inode-find-duplicates` | `modules/meta-log-inode.el` | 168 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-inode-track-moves` | `modules/meta-log-inode.el` | 200 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-llm-cache-find-similar` | `modules/meta-log-llm-cache.el` | 306 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-llm-learning-load` | `modules/meta-log-llm-learning.el` | 265 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-llm-learning-export-patterns` | `modules/meta-log-llm-learning.el` | 292 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-llm-learning-import-patterns` | `modules/meta-log-llm-learning.el` | 307 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-llm-learning-p-adic-features` | `modules/meta-log-llm-learning.el` | 332 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-llm-apply-vocabulary` | `modules/meta-log-llm.el` | 252 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-logger-warn` | `modules/meta-log-logger.el` | 163 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-logger-error` | `modules/meta-log-logger.el` | 170 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-logger-fatal` | `modules/meta-log-logger.el` | 177 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-logger-enable-category` | `modules/meta-log-logger.el` | 200 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-logger-disable-category` | `modules/meta-log-logger.el` | 206 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-self-describe` | `modules/meta-log-metacircular.el` | 99 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-generate-template-template` | `modules/meta-log-metacircular.el` | 122 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-mqtt-process-filter` | `modules/meta-log-mqtt.el` | 87 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-mqtt-send-peer-message` | `modules/meta-log-mqtt.el` | 218 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-mqtt-load-config-from-org` | `modules/meta-log-mqtt.el` | 243 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-org-find-template-files` | `modules/meta-log-org.el` | 113 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-org-search-templates` | `modules/meta-log-org.el` | 102 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-bridge-prolog-query` | `modules/meta-log-prolog-bridge.el` | 29 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-bridge-prolog-add-fact` | `modules/meta-log-prolog-bridge.el` | 38 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-bridge-prolog-add-rule` | `modules/meta-log-prolog-bridge.el` | 51 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-bridge-datalog-query` | `modules/meta-log-prolog-bridge.el` | 61 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-bridge-datalog-add-fact` | `modules/meta-log-prolog-bridge.el` | 70 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-bridge-datalog-add-rule` | `modules/meta-log-prolog-bridge.el` | 83 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-bridge-call-vision-api` | `modules/meta-log-prolog-bridge.el` | 108 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-prolog-create-db` | `modules/meta-log-prolog.el` | 21 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-protocol-handle-canvasl` | `modules/meta-log-protocol.el` | 48 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-protocol-handle-webrtc` | `modules/meta-log-protocol.el` | 56 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-protocol-handle-mqtt` | `modules/meta-log-protocol.el` | 63 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-provenance-record-transformation` | `modules/meta-log-provenance.el` | 32 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-server-register-client` | `modules/meta-log-server.el` | 36 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-server-list-clients` | `modules/meta-log-server.el` | 161 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-server-remove-client` | `modules/meta-log-server.el` | 166 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-setup-welcome-screen` | `modules/meta-log-setup.el` | 37 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-setup-step-1-llm` | `modules/meta-log-setup.el` | 72 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-setup-step-2-notes` | `modules/meta-log-setup.el` | 190 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-setup-step-3-finish` | `modules/meta-log-setup.el` | 254 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-setup-save-config` | `modules/meta-log-setup.el` | 314 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-shimura-rigid-analytic-space` | `modules/meta-log-shimura-padic.el` | 73 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-shimura-bip32-uniformization` | `modules/meta-log-shimura-padic.el` | 100 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-shimura-p-adic-modular-form` | `modules/meta-log-shimura-padic.el` | 115 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-shimura-local-global-consensus` | `modules/meta-log-shimura-padic.el` | 131 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-org-find-template-files` | `modules/meta-log-template-discovery.el` | 125 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-template-federation-share-template` | `modules/meta-log-template-federation.el` | 25 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-template-federation-handle-shared-template` | `modules/meta-log-template-federation.el` | 66 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-template-federation-setup` | `modules/meta-log-template-federation.el` | 103 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-template-federation-query-templates` | `modules/meta-log-template-federation.el` | 114 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-unix-to-canvasl` | `modules/meta-log-unix-types.el` | 153 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-unix-kg-register` | `modules/meta-log-unix-types.el` | 229 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-utct-create-zero` | `modules/meta-log-utct.el` | 38 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-utct-create-from-basis` | `modules/meta-log-utct.el` | 42 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-utct-apply-transformation` | `modules/meta-log-utct.el` | 70 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-utct-compute-delta` | `modules/meta-log-utct.el` | 77 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-utct-branch-cut` | `modules/meta-log-utct.el` | 81 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-utct-harmony-verify` | `modules/meta-log-utct.el` | 108 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-utct-scale` | `modules/meta-log-utct.el` | 140 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-utct-norm` | `modules/meta-log-utct.el` | 151 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-utct-inner-product` | `modules/meta-log-utct.el` | 155 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-verifiable-computation-execute` | `modules/meta-log-verifiable-computation.el` | 41 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-verifiable-computation-execute-function` | `modules/meta-log-verifiable-computation.el` | 91 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-verifiable-computation-query` | `modules/meta-log-verifiable-computation.el` | 161 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-webrtc-process-filter` | `modules/meta-log-webrtc.el` | 76 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-webrtc-process-sentinel` | `modules/meta-log-webrtc.el` | 95 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-webrtc-handle-signal` | `modules/meta-log-webrtc.el` | 124 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-webrtc-setup-signaling` | `modules/meta-log-webrtc.el` | 224 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-webrtc-send-signal` | `modules/meta-log-webrtc.el` | 232 | public_api_undocumented | ⚠️ Review before removal |
| `meta-log-webrtc-load-config-from-org` | `modules/meta-log-webrtc.el` | 248 | public_api_undocumented | ⚠️ Review before removal |

## Analysis Notes

- Functions marked as 'internal' (containing `--`) are excluded
- Test functions are excluded
- Interactive functions (marked with `(interactive)`) are excluded
- Functions in incomplete modules are marked as 'incomplete', not 'unused'
- Functions only called in their definition file may be legitimate
- **Manual review required** before removing any functions
- Public API functions should be reviewed especially carefully
- Scheme functions with `?` suffix are now properly detected
- Dynamic dispatch (apply/funcall/intern) is now detected
- String-based dispatch (cond/case/pcase with string=) is now detected
- Scheme `load` statements are tracked
- Note: Some dynamic dispatch patterns may still be missed


## Interactive Functions Detected (240)

These functions are marked with `(interactive)` and are called by users, not internally:

- `demo-code-documentation`
- `demo-code-full`
- `demo-code-refactoring-insights`
- `demo-code-search`
- `demo-code-setup`
- `demo-code-structure-analysis`
- `demo-collab-code-review`
- `demo-collab-discovery`
- `demo-collab-federated-learning`
- `demo-collab-full`
- `demo-collab-knowledge-sharing`
- `demo-collab-setup`
- `demo-collab-team-metrics`
- `demo-kg-math-full`
- `demo-kg-math-integration`
- `demo-kg-math-queries`
- `demo-kg-math-relationships`
- `demo-kg-math-setup`
- `demo-launcher-menu`
- `demo-launcher-run-all`
- `demo-math-drinfeld`
- `demo-math-e8`
- `demo-math-e8-theta`
- `demo-math-full`
- `demo-math-geometric`
- `demo-math-integration`
- `demo-math-p-adic`
- `demo-math-quadratic-forms`
- `demo-math-quaternion`
- `demo-math-shimura`
- `demo-personal-kb-full`
- `demo-personal-kb-insights`
- `demo-personal-kb-query`
- `demo-personal-kb-setup`
- `demo-quick-start`
- `demo-research-chat`
- `demo-research-full`
- `demo-research-learning`
- `demo-research-queries`
- `demo-research-setup`
- `demo-research-synthesis`
- `e8-funding-demo--simulate-llm-prediction`
- `e8-funding-demo-comparison`
- `e8-funding-demo-full`
- `e8-funding-demo-roi-summary`
- `e8-funding-demo-show-e8-solution`
- `e8-funding-demo-show-llm-problems`
- `e8-funding-demo-use-case-crypto`
- `e8-funding-demo-use-case-partition`
- `e8-funding-demo-use-case-voter`
- `meta-log-`
- `meta-log-ask`
- `meta-log-auto-enhance-check-and-run`
- `meta-log-auto-enhance-check-implementation`
- `meta-log-auto-enhance-continuous-learning`
- `meta-log-auto-enhance-gap-analysis`
- `meta-log-auto-enhance-generate-modules`
- `meta-log-auto-enhance-predict-needs`
- `meta-log-auto-enhance-run`
- `meta-log-auto-enhance-save-wordnet-mappings`
- `meta-log-auto-enhance-setup`
- `meta-log-auto-enhance-track-context`
- `meta-log-auto-enhance-wordnet`
- `meta-log-benchmark-clear`
- `meta-log-benchmark-report`
- `meta-log-benchmark-report-all`
- `meta-log-benchmark-run`
- `meta-log-binary-concat`
- `meta-log-binary-create-cbs`
- `meta-log-binary-rotate`
- `meta-log-binary-slice`
- `meta-log-binary-transform`
- `meta-log-binary-xor`
- `meta-log-bootstrap-self`
- `meta-log-chat`
- `meta-log-chat-clear`
- `meta-log-chat-fallback-response`
- `meta-log-chat-insert-welcome`
- `meta-log-chat-save`
- `meta-log-chat-send`
- `meta-log-collective-intelligence-query`
- `meta-log-core-initialize`
- `meta-log-crypto-decrypt`
- `meta-log-crypto-derive-bitcoin-key`
- `meta-log-crypto-derive-ethereum-key`
- `meta-log-crypto-derive-meta-log-key`
- `meta-log-crypto-encrypt`
- `meta-log-crypto-load-keys-from-org`
- `meta-log-crypto-load-mnemonic-from-org`
- `meta-log-crypto-parse-key`
- `meta-log-crypto-save-keys-to-org`
- `meta-log-crypto-save-mnemonic-to-org`
- `meta-log-crypto-set-local-identity`
- `meta-log-dashboard`
- `meta-log-dashboard-get-recent-queries`
- `meta-log-dashboard-help`
- `meta-log-dashboard-show-graph`
- `meta-log-dashboard-update-stats`
- `meta-log-datalog-extract-facts`
- `meta-log-datalog-immediate-query`
- `meta-log-datalog-merge-bindings`
- `meta-log-datalog-query-interactive`
- `meta-log-datalog-subst`
- `meta-log-discover-template`
- `meta-log-extract-facts-from-jsonl`
- `meta-log-federation-init`
- `meta-log-get-db`
- `meta-log-identity-create-peer`
- `meta-log-identity-generate-peer-id`
- `meta-log-identity-load-peer`
- `meta-log-identity-save-peer`
- `meta-log-ingest-folder`
- `meta-log-initialize`
- `meta-log-inode-clean-stale`
- `meta-log-inode-kg-query-by-inode`
- `meta-log-inode-kg-register-document`
- `meta-log-inode-registry-stats`
- `meta-log-kg-add-to-datalog`
- `meta-log-kg-add-to-prolog`
- `meta-log-kg-discover-missing-modules`
- `meta-log-kg-discover-patterns`
- `meta-log-kg-enhance-template-discovery`
- `meta-log-kg-export-graph`
- `meta-log-kg-extract-headings`
- `meta-log-kg-generate-improvement-report`
- `meta-log-kg-generate-module`
- `meta-log-kg-ingest-directory`
- `meta-log-kg-learn-wordnet-mappings`
- `meta-log-kg-query`
- `meta-log-kg-query-enhanced`
- `meta-log-kg-query-mathematical`
- `meta-log-kg-query-prolog`
- `meta-log-kg-stats`
- `meta-log-llm--display-result`
- `meta-log-llm--load-vocabulary`
- `meta-log-llm--prolog-to-datalog`
- `meta-log-llm-add-vocabulary`
- `meta-log-llm-anthropic--parse-expansion`
- `meta-log-llm-anthropic-set-key`
- `meta-log-llm-anthropic-test`
- `meta-log-llm-anthropic-with-context`
- `meta-log-llm-backend-available-p`
- `meta-log-llm-build-expansion-prompt`
- `meta-log-llm-cache--normalize-query`
- `meta-log-llm-cache-clear`
- `meta-log-llm-cache-evict-lru`
- `meta-log-llm-cache-load`
- `meta-log-llm-cache-remove`
- `meta-log-llm-cache-save`
- `meta-log-llm-cache-show-stats`
- `meta-log-llm-cache-show-top-queries`
- `meta-log-llm-cache-stats`
- `meta-log-llm-cache-top-queries`
- `meta-log-llm-classify-dimension`
- `meta-log-llm-expand-concept`
- `meta-log-llm-initialize`
- `meta-log-llm-learn-from-query`
- `meta-log-llm-learning--add-association`
- `meta-log-llm-learning--extract-pattern`
- `meta-log-llm-learning--learn-associations`
- `meta-log-llm-learning-correct-translation`
- `meta-log-llm-learning-find-related-concepts`
- `meta-log-llm-learning-learn-vocabulary`
- `meta-log-llm-learning-save`
- `meta-log-llm-learning-stats`
- `meta-log-llm-learning-suggest-translation`
- `meta-log-llm-openai--parse-expansion`
- `meta-log-llm-openai--parse-text-response`
- `meta-log-llm-openai-test`
- `meta-log-llm-openai-use-groq`
- `meta-log-llm-openai-use-ollama`
- `meta-log-llm-openai-use-openai`
- `meta-log-llm-openai-use-together`
- `meta-log-llm-query`
- `meta-log-llm-register-backend`
- `meta-log-llm-save-vocabulary`
- `meta-log-llm-stats`
- `meta-log-llm-tflite--get-kg-concepts`
- `meta-log-llm-tflite-expand`
- `meta-log-llm-tflite-setup`
- `meta-log-llm-tflite-test`
- `meta-log-llm-to-datalog`
- `meta-log-llm-to-prolog`
- `meta-log-llm-translate-local`
- `meta-log-llm-translate-remote`
- `meta-log-load-all-automata`
- `meta-log-load-automata`
- `meta-log-logger-clear`
- `meta-log-logger-get-entries`
- `meta-log-logger-print-entries`
- `meta-log-m-expr-eval-church`
- `meta-log-m-expr-eval-datalog`
- `meta-log-m-expr-eval-eval`
- `meta-log-m-expr-eval-interactive`
- `meta-log-m-expr-eval-prolog`
- `meta-log-m-expr-eval-query`
- `meta-log-m-expr-str-to-number`
- `meta-log-m-to-s`
- `meta-log-mode`
- `meta-log-modify-self`
- `meta-log-mqtt-connect`
- `meta-log-org-heading-extract-facts`
- `meta-log-org-load-blackboard`
- `meta-log-org-map-to-canvas`
- `meta-log-org-property-to-metadata`
- `meta-log-org-save-blackboard`
- `meta-log-parse-canvasl`
- `meta-log-prolog-query-interactive`
- `meta-log-prolog-resolve`
- `meta-log-prolog-resolve-body`
- `meta-log-protocol-execute-source-block`
- `meta-log-protocol-handle-org-source-block`
- `meta-log-protocol-init`
- `meta-log-protocol-rpc-call`
- `meta-log-provenance-get`
- `meta-log-provenance-verify`
- `meta-log-query-self`
- `meta-log-r5rs-call`
- `meta-log-r5rs-eval`
- `meta-log-r5rs-eval-interactive`
- `meta-log-r5rs-load-engine`
- `meta-log-s-to-m`
- `meta-log-search`
- `meta-log-server-get-blackboard-content`
- `meta-log-server-get-blackboard-version`
- `meta-log-server-init`
- `meta-log-server-set-blackboard-file`
- `meta-log-server-sync-blackboard`
- `meta-log-setup`
- `meta-log-substrate-create-memory`
- `meta-log-substrate-get-memory`
- `meta-log-substrate-initialize`
- `meta-log-substrate-resolve-uri`
- `meta-log-template-about-self`
- `meta-log-template-discovery-build-canvasl`
- `meta-log-template-discovery-save-template`
- `meta-log-unix-map-tree`
- `meta-log-unix-topology-stats`
- `static-analysis--analyze-modules`
- `static-analysis-run`

## Recommendations

1. **Review Public API Functions**: Check if unused public API functions should be:
   - Documented as optional/advanced features
   - Deprecated if no longer needed
   - Kept for backward compatibility

2. **Internal Functions**: Review if these are:
   - Helper functions for future use
   - Part of incomplete implementations
   - Truly unused and safe to remove

