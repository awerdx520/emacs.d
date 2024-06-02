;;; init-lsp.el --- Programming development -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:


(use-package lsp-bridge
  :straight '(lsp-bridge  :fetcher github :repo "manateelazycat/lsp-bridge"
              :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
              :build (:not compile))
  :hook (prog-mode . lsp-bridge-semantic-tokens-mode)
  :general
  (:states '(normal visual)
           "gd" 'lsp-bridge-find-def
           "gD" 'lsp-bridge-find-references
           "gi" 'lsp-bridge-find-impl
           "gf" 'project-find-file
           "gt" 'lsp-bridge-find-type-def
           "]e" 'lsp-bridge-diagnostic-jump-next
           "[e" 'lsp-bridge-diagnostic-jump-prev)

  (:states 'normal :keymaps 'lsp-bridge-ref-mode-map
           "RET" 'lsp-bridge-ref-open-file-and-stay
           "SPC" 'lsp-bridge-ref-open-file)
  :init
  ;; (setq lsp-bridge-deferred-debug-on-signal t
  ;;       lsp-bridge-enable-log nil )

  (global-lsp-bridge-mode)
  (defvar +lsp-bridge-data-dir
    (expand-file-name "lsp-bridge" thomas-data-dir)
    "Lsp-Bridge 相关配置目录.")

  :config
  ;; 关闭补全直接显示文档
  (setq acm-enable-doc nil)

  ;; 定期(以秒为单位)给远程服务器发送心跳包
  (setq lsp-bridge-remote-heartbeat-interval 10
        lsp-bridge-org-babel-lang-list '("c" "c++" "python" "java" "go" "rust" "scala"))

  ;; 跳转定义和引用的 fallback function
  (setq lsp-bridge-find-def-fallback-function #'xref-find-definitions
        lsp-bridge-find-ref-fallback-function #'xref-find-references)

  ;; 设置 semantic mode
  (defface lsp-bridge-semantic-tokens-variable-face
    '((t (:inherit font-lock-variable-name-face)))
    "Face used for variable name."
    :group 'lsp-bridge-semantic-tokens)

  (defface lsp-bridge-semantic-tokens-global-scope-face
    '((t :weight extra-bold))
    "Face used for globalScope token."
    :group 'lsp-bridge-semantic-tokens)

  (setq-default lsp-bridge-semantic-tokens-type-faces
                [("variable" . lsp-bridge-semantic-tokens-variable-face)])

  (setq-default lsp-bridge-semantic-tokens-type-modifier-faces
                [("globalScope" . lsp-bridge-semantic-tokens-global-scope-face)])

  (setq-default lsp-bridge-semantic-tokens-ignore-modifier-limit-types [])

  ;;
  (with-eval-after-load 'evil
    (setq evil-goto-definition-functions
          '(lsp-bridge-find-def evil-goto-definition-xref)))
  ;; 在 Org Babel 里使用 LSP 补全
  (setq lsp-bridge-enable-org-babel t))

(provide 'init-lsp)
;;; init-lsp.el ends here
