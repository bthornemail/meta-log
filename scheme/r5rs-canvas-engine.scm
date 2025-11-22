;;; r5rs-canvas-engine.scm --- Main R5RS Canvas Engine
;;; Meta-Log Substrate System - R5RS Scheme Implementation
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; Main entry point for R5RS Canvas Engine. Loads all substrate modules
;;; and provides unified API.

;;; Code:

;; Load substrate modules
;; All paths relative to scheme/ directory
(load "substrate/runtime.scm")
(load "substrate/binary.scm")
(load "substrate/provenance.scm")
(load "substrate/content-address.scm")
(load "substrate/canvasl.scm")
(load "substrate/prolog-interface.scm")
(load "substrate/waveform.scm")
(load "substrate/wdl.scm")
(load "substrate/cdmp.scm")

;; Load Q* modules
(load "qstar/core.scm")
(load "qstar/scoring.scm")
(load "qstar/a-star.scm")

;; Load consciousness modules
(load "consciousness/state.scm")

;; Load physics modules
(load "physics/quantum.scm")
(load "physics/gr.scm")

;; Main API functions are now available from loaded modules
;; All functions defined in substrate modules are accessible

