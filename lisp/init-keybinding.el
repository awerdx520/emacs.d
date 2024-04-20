;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  :hook (after-init . evil-mode)
  ;; Don't quit Emacs on `:q'.
  ;;
  ;; Rebind `f'/`s' to mimic `evil-snipe'.
  :bind (([remap evil-quit] . kill-this-buffer)
         :map evil-motion-state-map
         ("f" . evil-avy-goto-char-in-line)

         :map evil-normal-state-map
         ("s" . evil-avy-goto-char-timer))
  :custom
  ;; Switch to the new window after splitting
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  ;; when `visual-line-mode' enabled, exchange j/k with gj/gk
  (evil-respect-visual-line-mode t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-fine-undo t)
  (evil-want-C-g-bindings t)
  (evil-want-C-u-scroll t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-symbol-word-search t)
  :general
  (thomas-leader-window
   "+" 'evil-window-increase-height
   "-" 'evil-window-decrease-height
   ":" 'evil-ex
   "<" 'evil-window-decrease-width
   ">" 'evil-window-decrease-width
   "_" 'evil-window-set-height
   "b" 'evil-window-bottom-right
   "c" 'evil-window-delete
   "d" 'evil-window-delete
   "h" 'evil-window-left
   "j" 'evil-window-down
   "k" 'evil-window-up
   "l" 'evil-window-right
                                        ;"m" 'maxiz
   "n" 'evil-window-new
   "p" 'evil-window-mru
   "q" 'evil-quit
   "r" 'evil-window-rotate-downwards
   "R" 'evil-window-rotate-upwards
   "s" 'evil-window-split
   "t" 'evil-window-top-left
   "v" 'evil-window-vsplit
   "w" 'evil-window-next
   "W" 'evil-window-prev
   "x" 'evil-window-exchange
   "|" 'evil-window-set-width))


(use-package evil-collection
  :after evil
  :hook (after-init . evil-collection-init)
  :bind (([remap evil-show-marks] . evil-collection-consult-mark)
         ([remap evil-show-jumps] . evil-collection-consult-jump-list))
  :custom
  (evil-collection-setup-debugger-keys nil)
  (evil-collection-calendar-want-org-bindings t)
  (evil-collection-unimpaired-want-repeat-mode-integration t))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode))


(with-eval-after-load 'view
  (general-def :keymaps 'view-mode-map
    :states 'normal
    [escape] 'View-quit-all))

(with-eval-after-load 'man
  (general-def :keymaps 'Man-mode-map
    :states 'normal
    "q" 'kill-current-buffer))

(provide 'init-keybinding)
;;; init-evil.el ends here
