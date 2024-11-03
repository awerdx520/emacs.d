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
  (thomas-leader-define
    ;; code
    "ca" 'lsp-bridge-code-action ; 弹出代码修复菜单
    "cj" 'lsp-bridge-find-def ; 跳转到定义位置
    "cJ" 'lsp-bridge-find-def-other-window ; 在其他窗口跳转到定义位置
    "cr" 'lsp-bridge-rename ; 重命名
    "ci" 'lsp-bridge-find-impl ; 跳转到接口实现位置
    "cI" 'lsp-bridge-find-impl-other-window ; 在其他窗口跳转到接口实现位置
    "ct" 'lsp-bridge-find-type-def ; 跳转到类型定义位置
    "cT" 'lsp-bridge-find-type-def-other-window ; 在其他窗口跳转到类型定义位置
    "cD" 'lsp-bridge-find-references ; 查看代码引用
    "ck" 'lsp-bridge-show-documentation
    "cx" 'lsp-bridge-diagnostic-list ); 列出所有诊断信息

  ;;  evil
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
  ;; 关闭补全直接显示文档
  (setq acm-enable-doc nil
        acm-enable-quick-access t
        lsp-bridge-enable-hover-diagnostic t
        ;; 开启 org-bable 补全
        lsp-bridge-enable-org-babel t
        lsp-bridge-org-babel-lang-list '("c" "c++" "python" "java" "go" "rust" "scala"))

  ;; TODO 未生效，弹出窗口显示签名
  ;;  (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)

  ;; 定期(以秒为单位)给远程服务器发送心跳包
  (setq lsp-bridge-remote-heartbeat-interval 10
        ;; 设 lsp-bridge.py 启动环境
        lsp-bridge-python-command "/home/thomas/.conda/envs/lsp-bridge/bin/python")

  ;; 跳转定义和引用的 fallback function 未生效
  (setq lsp-bridge-find-def-fallback-function #'xref-find-definitions
        lsp-bridge-find-ref-fallback-function #'xref-find-references
        lsp-bridge-find-def-select-in-open-windows t)

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
