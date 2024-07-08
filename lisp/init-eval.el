;;; init-eval.el --- Programming development -*- lexical-binding: t -*-
;; Quickrun codes, including cpp. awesome!
(use-package quickrun
  :general
  (thomas-leader-define
    "er" 'quickrun)
  :config
  (setq quickrun-focus-p nil
        quickrun-input-file-extension ".qr"))

(provide 'init-eval)
