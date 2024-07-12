;;; core-help.el --- Bring vim back -*- lexical-binding: t -*-

(use-package help
  :straight (:type built-in)
  :general
  (thomas-leader-define
    ;; Help
    "h" '(:ignore t :wk "help")
    "hbf" 'which-key-show-full-keymap
    "hbi" 'which-key-show-minor-mode-keymap
    "hbk" 'which-key-show-keymap
    "hbm" 'which-key-show-major-mode
    "hbt" 'which-key-show-top-level
    "h RET" 'info-emacs-manual
    "h'" 'describe-char
    "h." 'display-local-help
    "h?" 'help-for-help
    "ha" 'apropos
    "hA" 'apropos-documentation
    "hbb" 'describe-bindings
    "hc" 'describe-key-briefly
    "hC" 'describe-coding-system
    "he" 'view-echo-area-messages
    "hf" 'describe-function
    "hF" 'describe-face
    "hg" 'describe-gnu-project
    "hi" 'info
    "hI" 'describe-input-method
    "hk" 'describe-key
    "hK" 'Info-goto-emacs-key-command-node
    "hl" 'view-lossage
    "hL" 'describe-language-environment
    "hm" 'describe-mode
    "hM" 'describe-keymap
    "ho" 'describe-symbol
    "hP" 'find-library
    "hq" 'help-quit
    "hR" 'info-display-manual
    "hs" 'describe-syntax
    "hS" 'info-lookup-symbol
    "hv" 'describe-variable
    "hw" 'where-is
    "hW" 'woman
    "hR" 'info-display-manual
    "hx" 'describe-command
    ;;
    "h C-d" 'view-emacs-debugging
    "h C-f" 'view-emacs-FAQ
    "h C-n" 'view-emacs-news
    "h C-p" 'view-emacs-problems
    "h C-t" 'view-emacs-todo
    "h C-w" 'describe-no-warranty))


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

(provide 'core-help)
