(require 'dired)

(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

(provide 'setup-dired)
