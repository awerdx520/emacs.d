;;; init-rust.el --- Rust -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook ((rust-mode rust-ts-mode) . rainbow-delimiters-mode)
  :config
  (with-no-warnings
    (with-eval-after-load 'lsp-mode
      (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-extern-crate"])))
  :custom
  (rust-indent-where-clause t)
  (rust-format-on-save t)
  (rust-format-show-buffer nil))

;; Cargo integration
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(provide 'init-rust)
;;; init-rust.el ends here
