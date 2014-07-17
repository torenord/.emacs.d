(require 'dired)

(when (require 'dired-details nil 'noerror)
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))

(provide 'setup-dired)
