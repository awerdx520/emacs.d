;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'core-basic)

(thomas-leader-define
  ;; Top
  ":" 'execute-extended-command
  ";" 'pp-eval-expression
  "`" 'switch-to-buffer

  ;; Buffer
  "b" '(:ignore t :wk "buffer")
  ;;    "b-" 'thomas/toggle-narrow-buffer
  "b[" 'previous-buffer
  "b]" 'next-buffer
  "bc" 'clone-indirect-buffer
  "bC" 'clone-indirect-buffer-other-window
  "bd" 'kill-current-buffer
  "bi" 'ibuffer
  "bk" 'kill-current-buffer
  "bl" 'evil-switch-to-windows-last-buffer
  "bm" 'bookmark-set
  "bM" 'bookmark-delete
  "bn" 'next-buffer
  "bN" 'evil-buffer-new
  "bp" 'previous-buffer
  "br" 'revert-buffer
  "bs" 'save-buffer
  "bS" 'evil-write-all
  "by" '+default/yank-buffer-contents
  "bz" 'bury-buffer

  ;; Code
  "c" '(:ignore t :wk "code")
  "cc" 'compile
  "cC" 'recompile
  "cf" 'apheleia-format-buffer
  ;; File
  "f" '(:ignore t :wk "file")
  "ff" 'find-file
  "fe" '+default/find-file-in-emacs

  ;; Git
  "g" '(:ignore t :wk "git")
  ;; Insert
  "i" '(:ignore t :wk "insert")
  "ie" 'emoji-search

  ;; Open
  "o" '(:ignore t :wk "open")
  "o-" 'dired-jump
  "oA" 'org-agenda
  "of" 'make-frame
  "oF" 'select-frame-by-name
  "ot" 'term

  ;; Project
  "p" '(:ignore t :wk "project")
  "pl" 'devdocs-lookup

  ;; Quit
  "q" '(:ignore t :wk "quit/session")
  "qf" 'delete-frame
  "qF" 'delete-other-frames
  "qK" 'save-buffers-kill-emacs
  "qq" 'save-buffers-kill-terminal

  ;; Remote
  "r" '(:ignore t :wk "remote")

  ;; Search
  "s" '(:ignore t :wk "search")
  "se" '+default/search-emacs
  "sb" '+default/search-buffer
  "ss" '+default/search-buffer
  "sB" '+default/search-all-buffer

  ;; Window
  "w" '(:ignore t :wk "window")
  "w=" 'balance-windows
  "wf" 'ffap-other-window
  "wT" 'tear-off-window
  "w C-o" 'delete-other-windows)

(general-def :states 'normal
  ;;    "] t" '+evil/next-frame
  ;;   "[ t" '+evil/previous-frame
  "] f" '+evil/next-file
  "[ f" '+evil/previous-file)

(provide 'init-keybinding)
;;; init-keybinding.el ends here
