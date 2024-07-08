;;; core-package.el 包管理工具 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: 五月 07, 2024
;; Modified: 五月 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/core-package
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; Use straight as package manager
(setq straight-base-dir thomas-local-dir
      straight-repository-branch "develop"
      ;; Since byte-code is rarely compatible across different versions of
      ;; Emacs, it's best we build them in separate directories, per emacs
      ;; version.
      straight-build-dir (format "build-%s" emacs-version)
      straight-cache-autoloads t
      straight--process-log nil
      ;; Doom doesn't encourage you to modify packages in place. Disabling this
      ;; makes 'doom sync' instant (once everything set up), which is much nicer
      ;; UX than the several seconds modification checks.
      straight-check-for-modifications nil
      straight-enable-package-integration nil
      ;; Before switching to straight, `doom-local-dir' would average out at
      ;; around 100mb with half Doom's modules at ~230 packages. Afterwards, at
      ;; around 1gb. With shallow cloning, that is reduced to ~400mb. This has
      ;; no affect on packages that are pinned, however (run 'doom purge' to
      ;; compact those after-the-fact). Some packages break when shallow cloned
      ;; (like magit and org), but we'll deal with that elsewhere.
      straight-vc-git-default-clone-depth '(1 single-branch)
      straight-use-package-by-default t)

(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        straight-base-dir))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; config use-usepackage
(setq use-package-always-ensure nil
      use-package-enable-imenu-support t
      use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-expand-minimally t
      use-package-compute-statistics nil)
(setq byte-compile-warnings '(cl-functions))
(straight-use-package 'use-package)

;;
;;; 安装一些核心包
;;;
;; 用于阻止一些包在 modeline 上的添加信息已经与 use-package 集成，
;; 可用 :diminish xxx-mode 配置当前包禁止显示在 modeline 上
(use-package diminish)

;; 添加性能测试
(use-package benchmark-init
  :demand t
  :config
  (require 'benchmark-init-modes)
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; Posframe can pop up a frame at point, this posframe is a child-frame connected to its root window’s buffer.
(use-package posframe)

;; Workaround with minified source files
(use-package so-long
  :straight (:type built-in)
  :hook (thomas-first-file . global-so-long-mode)
  :config
  (setq so-long-threshold 10240)
  ;; 保持 save-place 不在大型/长文件中运行
  (appendq! so-long-variable-overrides '((font-lock-maximum-decoration . 1)
                                         (save-place-alist . nil)))
  ;; Text files could possibly be too long too
  (add-to-list 'so-long-target-modes 'text-mode)
  ;; 禁用某些对于大缓冲区来说可能不必要/昂贵的模式
  (appendq! so-long-minor-modes '(rainbow-mode flycheck-mode eldoc-mode
                                               ws-butler-mode highlight-numbers-mode
                                               rainbow-delimiters-mode
                                               highlight-indent-guides-mode)))

(use-package general
  :config
  ;; leader key define
  (general-create-definer thomas-leader-define
    :states '(normal visual motion evilified)
    :prefix thomas-leader-key
    :keymaps 'override)

  ;; localleader key define
  (general-create-definer thomas-localleader-define
    :states '(normal visual motion evilified)
    :prefix thomas-localleader-key
    :keymaps 'override))

(use-package better-jumper
  :hook (thomas-first-input . better-jumper-mode)
  :commands thomas-set-jump-a thomas-set-jump-maybe-a thomas-set-jump-h
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (with-eval-after-load 'evil
    (general-def
      [remap evil-jump-forward]  'better-jumper-jump-forward
      [remap evil-jump-backward] 'better-jumper-jump-backward
      [remap xref-pop-marker-stack] 'better-jumper-jump-backward))
  (general-def
    [remap xref-go-back] 'better-jumper-jump-backward
    [remap xref-go-forward] 'better-jumper-jump-forward)
  :config
  (defun thomas-set-jump-a (fn &rest args)
    "Set a jump point and ensure fn doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (defun thomas-set-jump-maybe-a (fn &rest args)
    "Set a jump point if fn actually moves the point."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply fn args)))
          (dest (point-marker)))
      (unless (equal origin dest)
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      (set-marker origin nil)
      (set-marker dest nil)
      result))

  (defun thomas-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil)

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  ;;
  ;; I'm not advising `kill-buffer' because I only want this to affect
  ;; interactively killed buffers.
  (advice-add #'kill-current-buffer :around #'thomas-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'thomas-set-jump-a))

(use-package gcmh
  :straight (:host github :repo "emacsmirror/gcmh")
  :hook (thomas-first-input . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

(use-package which-key
  :diminish which-key-mode "Ⓚ"
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)

  (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)
  (defun +which-key-set-line-spaceing-fn ()
    "Set `which-key' line spacing"
    (setq line-spacing 3))
  (add-hook 'which-key-init-buffer-hook #'+which-key-set-line-spaceing-fn)
  ;;
  (which-key-add-key-based-replacements thomas-leader-key "<leader>")
  (which-key-add-key-based-replacements thomas-localleader-key "<localleader>"))

(use-package avy
  :config
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        ;; the unpredictability of this (when enabled) makes it a poor default
        avy-single-candidate-jump nil))

;; colorful help mode
(use-package helpful
  :general
  (:keymaps 'override
            [remap describe-function] #'helpful-callable
            [remap describe-command]  #'helpful-command
            [remap describe-variable] #'helpful-variable
            [remap describe-key]      #'helpful-key
            [remap describe-symbol]   #'helpful-symbol)
  (:states 'normal :keymaps '(help-mode-map helpful-mode-map)
           "q" 'quit-window
           [escape] 'quit-window
           "Q" 'kill-current-buffer)
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  (with-eval-after-load 'apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))

    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol))))))

  ;; Quick editing in `describe-variable'
  (with-eval-after-load 'help-fns
    (put 'help-fns-edit-variable 'disabled nil)))

;; TODO 优化 shell 启动时间
(use-package exec-path-from-shell
  :config
  ;; 设成nil 则不从 .zshrc 读 只从 .zshenv读（可以加快速度，但是需要你将环境变量相关的都放到 .zshenv 中，而非 .zshrc 中)
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("LOCATION" "PATH" "MANPATH" "GOROOT" "GOPATH" "EDITOR" "PYTHONPATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(provide 'core-package)
;;; core-package.el ends here
