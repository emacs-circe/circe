;; see #405
(require 'circe)
(setq gnutls-verify-error t)

(irc-connect :host "expired.badssl.com" :service 443 :tls t)
