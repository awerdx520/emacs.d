;;; init-eval.el --- Programming development -*- lexical-binding: t -*-
;; Quickrun codes, including cpp. awesome!
(use-package quickrun
  :general
  (thomas-leader-define
    "e" '(:ignore t :wk "eval")
    "ea" 'quickrun-with-arg
    "eq" 'quickrun
    "er" 'quickrun-region
    "es" 'quickrun-shell
    "ep" 'quickrun-replace-region)
  :config
  (setq quickrun-focus-p nil
        quickrun-input-file-extension ".qr"))

(provide 'init-eval)
