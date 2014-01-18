;; Non-blocking TLS

;; tls-success is not correct by default in Emacs 24.3.1 with openssl
;; 1.0.1e 11 Feb 2013, so it should be customized to "- Handshake was
;; completed\|Server public key is"

(require 'tls)
(require 'cl)


(defcustom tls-failed "*** Fatal error\\|New, (NONE), Cipher is (NONE)\\|Cannot connect to\\|connect:errno"
  "Regular expression indicating failed TLS connections.
The default is what GnuTLS's \"gnutls-cli\" or OpenSSL's
\"openssl s_client\" outputs."
  :version "24.3"
  :type 'regexp
  :group 'tls)


(defun circe-make-tls-stream (&rest properties)
  "Open a TLS connection, tries to mimic make-network-process.
Mostly wraps `circe-open-tls-stream'.
Required properties: host, service, success-func
Other properties: name, fail-thunk, buffer, coding, filter, sentinel"
  (lexical-let* ((host (plist-get properties :host))
                 (name (or (plist-get properties :name) host))
                 (service (plist-get properties :service))
                 (success-func (plist-get properties :success-func))
                 (fail-thunk (plist-get properties :fail-thunk))
                 (buffer (or (plist-get properties :buffer) (current-buffer)))
                 (coding (plist-get properties :coding))
                 (filter (plist-get properties :filter))
                 (sentinel (plist-get properties :sentinel))
                 (query-on-exit-flag (plist-get properties :query-on-exit-flag)))
    (if (and host service success-func)
        (circe-open-tls-stream name
                               host
                               service
                               #'(lambda (process)
                                   (with-current-buffer buffer
                                     (when filter
                                       (set-process-filter process filter))
                                     (when sentinel
                                       (set-process-sentinel process sentinel))
                                     (when coding
                                       (set-process-coding-system process coding coding))
                                     (when query-on-exit-flag
                                       (set-process-query-on-exit-flag process query-on-exit-flag))
                                     (when buffer
                                       (set-process-buffer process buffer))
                                     (funcall success-func process)))
                               #'(lambda ()
                                   (with-current-buffer buffer
                                     (when fail-thunk
                                       (funcall fail-thunk)))))
      (message "circe-make-tls-stream: host, service and success-func are required"))))



(defun circe-open-tls-stream (name host port success-func fail-thunk)
  "Tweaked `open-tls-stream', this one is non-blocking."
  (let ((cmds tls-program)
	(name name))
    (message "Opening TLS connection to `%s'..." host)
    (circe-tls-try-cmds cmds host port success-func fail-thunk)))


(defun circe-tls-try-cmds (cmds host port success-func fail-thunk)
  (if cmds
      (circe-tls-try-cmd (car cmds)
                         host
                         port
                         success-func
                         `(lambda (proc) (circe-tls-try-cmds ',(cdr cmds)
                                                             ,host
                                                             ,port
                                                             ,success-func
                                                             ,fail-thunk)))
    (funcall fail-thunk)))


(defun circe-tls-try-cmd (cmd host port success-func fail-thunk)
  (let ((process-connection-type tls-process-connection-type)
        (formatted-cmd
         (format-spec
          cmd
          (format-spec-make
           ?h host
           ?p (if (integerp port)
                  (int-to-string port)
                port)))))
    (message "Opening TLS connection with `%s'..." formatted-cmd)
    (setq process (start-process
                   name nil shell-file-name shell-command-switch
                   formatted-cmd))
    (if process
        (set-process-filter process (circe-make-tls-filter host
                                                           formatted-cmd
                                                           success-func
                                                           fail-thunk
                                                           ""))
      (funcall fail-thunk))))


(defun circe-make-tls-filter (host formatted-cmd success-func fail-thunk acc)
  (lexical-let ((host host)
                (formatted-cmd formatted-cmd)
                (success-func success-func)
                (fail-thunk fail-thunk)
                (acc acc))
    (lambda (process output)
      (if (and process
               (memq (process-status process) '(open run)))
          ;; process is alive
          (let ((out (concat acc output)))
            (if (string-match tls-success out)
                ;; connected
                (if (string-match tls-end-of-info out)
                    ;; and it's end-of-info already
                    (circe-tls-final-check host 
                                           formatted-cmd 
                                           process 
                                           success-func 
                                           fail-thunk 
                                           out)
                  ;; read until end-of-info
                  (message "Opening TLS connection with `%s'... almost done" formatted-cmd)
                  (set-process-filter process (circe-make-skip-info-filter host
                                                                           formatted-cmd
                                                                           success-func
                                                                           fail-thunk
                                                                           out)))
              ;; no success (yet)
              (if (or (string-match tls-end-of-info out)
                      (string-match tls-failed out))
                  ;; failed -> failed; end of info, but not success -> failed
                  (progn
                    (message "Opening TLS connection with `%s'... failed" formatted-cmd)
                    (delete-process process)
                    (funcall fail-thunkd))
                ;; wait more
                (set-process-filter process (circe-make-tls-filter host
                                                                   formatted-cmd
                                                                   success-func
                                                                   fail-thunk
                                                                   out)))))
        ;; process is dead
        (delete-process process)
        (message "Opening TLS connection with `%s'... failed" formatted-cmd)
        (funcall fail-thunk)))))

(defun circe-make-skip-info-filter (host formatted-cmd success-func fail-thunk acc)
  (lexical-let ((host host)
                (formatted-cmd formatted-cmd)
                (success-func success-func)
                (fail-thunk fail-thunk)
                (acc acc))
    (lambda (process output)
      (if (and process
               (memq (process-status process) '(open run)))
          (let ((out (concat acc output)))
            (if (string-match tls-end-of-info out)
                (circe-tls-final-check host
                                       formatted-cmd 
                                       process 
                                       success-func
                                       fail-thunk 
                                       out)
              (set-process-filter process (circe-make-skip-info-filter host
                                                                       formatted-cmd
                                                                       success-func
                                                                       fail-thunk
                                                                       out))))
        (delete-process process)
        (message "Opening TLS connection with `%s'... failed" formatted-cmd)
        (funcall fail-thunk)))))


(defun circe-tls-final-check (host formatted-cmd process success-func fail-thunk output)
  (if (or
       (and tls-checktrust
            (string-match tls-untrusted output)
            (or
             (and (not (eq tls-checktrust 'ask))
                  (message "The certificate presented by `%s' is NOT trusted." host))
             (not (yes-or-no-p
                   (format "The certificate presented by `%s' is NOT trusted. Accept anyway? " host)))))
       (and tls-hostmismatch
            (string-match tls-hostmismatch output)
            (not (yes-or-no-p
                  (format "Host name in certificate doesn't match `%s'. Connect anyway? " host)))))
      ;; something is wrong
      (progn
        (message "Opening TLS connection with `%s'... failed" formatted-cmd)
        (set-process-filter process nil)
        (delete-process process)
        (funcall fail-thunk))
    (message "Opening TLS connection with `%s'... done" formatted-cmd)
    (set-process-filter process nil)
    (funcall success-func process)))

(provide 'circe-tls)
