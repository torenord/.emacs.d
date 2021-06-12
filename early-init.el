;; Turn off garbage collection during startup
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold ,gc-cons-threshold)
             (garbage-collect)))
(setq gc-cons-threshold most-positive-fixnum)
