;;; meta-log-http-client.el --- HTTP Client for Scheme Bridge
;;; Copyright (C) 2025 Meta-Log Research Group

;;; Commentary:
;;; HTTP client functions that can be called from Scheme via bridge.

;;; Code:

(require 'url)
(require 'json nil t)

(defvar meta-log-http--base-urls
  '((vision-api . "http://localhost:8002")
    (substrate-api . "http://localhost:8001")
    (quantum-sim . "http://localhost:8003"))
  "Base URLs for FastAPI services.")

(defun meta-log-http-call (service endpoint method data)
  "Make HTTP call to FastAPI service.
SERVICE: symbol (vision-api, substrate-api, etc.)
ENDPOINT: string endpoint path (e.g., \"/vision/extract-sift\")
METHOD: 'get or 'post
DATA: alist of request data
Returns response data as alist."
  (let ((base-url (cdr (assoc service meta-log-http--base-urls)))
        (url-string (concat base-url endpoint))
        (url-request-method (upcase (symbol-name method)))
        (url-request-data (when (and (eq method 'post) (fboundp 'json-encode))
                            (json-encode data))))
    (condition-case err
        (let ((buffer (url-retrieve-synchronously url-string)))
          (if buffer
              (progn
                (with-current-buffer buffer
                  (goto-char (point-min))
                  ;; Skip HTTP headers
                  (when (re-search-forward "^$" nil t)
                    (forward-char))
                  (let ((json-object-type 'alist)
                        (json-array-type 'list)
                        (response (if (fboundp 'json-read)
                                      (json-read)
                                    ;; Fallback: return raw buffer content
                                    (buffer-substring (point) (point-max)))))
                    (kill-buffer buffer)
                    response))
            (error "Failed to retrieve URL")))
      (error
       (message "HTTP call failed: %S" err)
       nil))))

(defun meta-log-http-call-vision-api (endpoint data)
  "Call vision API endpoint.
ENDPOINT: string endpoint path
DATA: alist of request data
Returns response data."
  (meta-log-http-call 'vision-api endpoint 'post data))

(provide 'meta-log-http-client)

;;; meta-log-http-client.el ends here

