;;; init-lsp.el --- Programming development -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; TODO bash 存在重复格式化问题
(use-package lsp-bridge
  :straight '(lsp-bridge  :fetcher github :repo "manateelazycat/lsp-bridge"
                          :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                          :build (:not compile))
  :hook (prog-mode . lsp-bridge-mode)
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
  :config
  (setq c-basic-offset 4)

  ;; 关闭补全直接显示文档
  (setq acm-enable-doc nil)

  ;; 定期(以秒为单位)给远程服务器发送心跳包
  (setq lsp-bridge-remote-heartbeat-interval 10)

  ;; 设 lsp-bridge.py 启动环境
  (setq lsp-bridge-python-command "/home/thomas/.conda/envs/lsp-bridge/bin/python"
        lsp-bridge-enable-auto-format-code t)

  (setq lsp-bridge-org-babel-lang-list '("c" "c++" "python" "java" "go" "rust" "scala")
        ;; 开启 org-bable 补全
        lsp-bridge-enable-org-babel t)

  ;; 跳转定义和引用的 fallback function
  (setq lsp-bridge-find-def-fallback-function #'xref-find-definitions
        lsp-bridge-find-ref-fallback-function #'xref-find-references)

  ;; 添加 lsp-bringe-find-def 到 evil 跳转函数中
  (with-eval-after-load 'evil
    (setq evil-goto-definition-functions
          '(lsp-bridge-find-def evil-goto-definition-xref))))

;; 使用 topsy 提醒你正在编辑远程文件。
(use-package topsy
  :after lsp-bridge
  :config
  (setcdr (assoc nil topsy-mode-functions)
          (lambda ()
            (when (lsp-bridge-is-remote-file) "[LBR] REMOTE FILE")))

  ;; do not activate when the current major mode is org-mode
  (add-hook 'lsp-bridge-mode-hook (lambda ()
                                    (unless (derived-mode-p 'org-mode)
                                      (topsy-mode 1)))))
(provide 'init-lsp)
;;; init-lsp.el ends here
