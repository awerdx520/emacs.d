;;; init-snippents.el Emacs 代码片段支持 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-snippents
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :init
  ;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
  ;; is just right (only shows errors).
  (setq yas-verbosity 2)
  :config
  ;; HACK In case `+snippets-dir' and `doom-snippets-dir' are the same, or
  ;;      duplicates exist in `yas-snippet-dirs'.
  (advice-add #'yas-snippet-dirs :filter-return #'delete-dups))

;; 添加常用 snippets 片段
(use-package yasnippet-snippets
  :after yasnippt)

;; 创建代码片段
(use-package auto-yasnippet
  :after yasnippet)

;; yassnippet
(use-package consult-yasnippet
  :after (yasnippet consult)
  :defer t)


;;
;;; file-template mock from doom-emacs

(defvar +file-templates-dir
  (expand-file-name "templates/" thomas-emacs-dir)
  "The path to a directory of yasnippet folders to use for file templates.")

(defvar +file-templates-default-trigger "__"
  "The default yasnippet trigger key (a string) for file template rules that
don't have a :trigger property in `+file-templates-alist'.")

(defvar +file-templates-inhibit nil
  "If non-nil, inhibit file template expansion.")

(defvar +file-templates-alist
  '(;; General
    (gitignore-mode)
    (dockerfile-mode)
    ("/docker-compose\\.yml$" :mode yaml-mode)
    ("/Makefile$"             :mode makefile-gmake-mode)
    ;; elisp
    ("/\\.dir-locals\\.el$")
    ("-test\\.el$" :mode emacs-ert-mode)
    (emacs-lisp-mode :trigger "__package")
    (snippet-mode)
    ;; C/C++
    ("/main\\.c\\(?:c\\|pp\\)$"   :trigger "__main.cpp"    :mode c++-mode)
    ("/win32_\\.c\\(?:c\\|pp\\)$" :trigger "__winmain.cpp" :mode c++-mode)
    ("\\.c\\(?:c\\|pp\\)$"        :trigger "__cpp" :mode c++-mode)
    ("\\.h\\(?:h\\|pp\\|xx\\)$"   :trigger "__hpp" :mode c++-mode)
    ("\\.h$" :trigger "__h" :mode c-mode)
    (c-mode  :trigger "__c")
    ;; direnv
    ("/\\.envrc$" :trigger "__envrc" :mode direnv-envrc-mode)
    ;; go
    ("/main\\.go$" :trigger "__main.go" :mode go-mode :project t)
    (go-mode :trigger "__.go")
    ;; web-mode
    ("/normalize\\.scss$" :trigger "__normalize.scss" :mode scss-mode)
    ("/master\\.scss$" :trigger "__master.scss" :mode scss-mode)
    ("\\.html$" :trigger "__.html" :mode web-mode)
    (scss-mode)
    ;; java
    ("/main\\.java$" :trigger "__main" :mode java-mode)
    ("/build\\.gradle$" :trigger "__build.gradle" :mode android-mode)
    ("/src/.+\\.java$" :mode java-mode)
    ;; javascript
    ("/package\\.json$"        :trigger "__package.json" :mode json-mode)
    ("/bower\\.json$"          :trigger "__bower.json" :mode json-mode)
    ("/gulpfile\\.js$"         :trigger "__gulpfile.js" :mode js-mode)
    ("/webpack\\.config\\.js$" :trigger "__webpack.config.js" :mode js-mode)
    ;; Lua
    ("/main\\.lua$" :trigger "__main.lua" :mode love-mode)
    ("/conf\\.lua$" :trigger "__conf.lua" :mode love-mode)
    ;; Markdown
    (markdown-mode)
    ;; Markdown
    (nxml-mode)
    ;; Nix
    ("/shell\\.nix$" :trigger "__shell.nix")
    (nix-mode)
    ;; Org
    (org-journal-mode :ignore t)
    (org-mode)
    ;; PHP
    ("\\.class\\.php$" :trigger "__.class.php" :mode php-mode)
    (php-mode)
    ;; Python
    ;; TODO ("tests?/test_.+\\.py$" :trigger "__" :mode nose-mode)
    ;; TODO ("/setup\\.py$" :trigger "__setup.py" :mode python-mode)
    (python-mode)
    ;; Ruby
    ("/lib/.+\\.rb$"      :trigger "__module"   :mode ruby-mode :project t)
    ("/spec_helper\\.rb$" :trigger "__helper"   :mode rspec-mode :project t)
    ("_spec\\.rb$"                              :mode rspec-mode :project t)
    ("/\\.rspec$"         :trigger "__.rspec"   :mode rspec-mode :project t)
    ("\\.gemspec$"        :trigger "__.gemspec" :mode ruby-mode :project t)
    ("/Gemfile$"          :trigger "__Gemfile"  :mode ruby-mode :project t)
    ("/Rakefile$"         :trigger "__Rakefile" :mode ruby-mode :project t)
    (ruby-mode)
    ;; Rust
    ("/Cargo\\.toml$" :trigger "__Cargo.toml" :mode rust-mode)
    ("/main\\.rs$" :trigger "__main.rs" :mode rust-mode)
    ;; Slim
    ("/\\(?:index\\|main\\)\\.slim$" :mode slim-mode)
    ;; Shell scripts
    ("\\.zunit$" :trigger "__zunit" :mode sh-mode)
    (fish-mode)
    (sh-mode)
    ;; Solidity
    (solidity-mode :trigger "__sol"))
  "An alist of file template rules. The CAR of each rule is either a major mode
symbol or regexp string. The CDR is a plist. See `set-file-template!' for more
information.")


;;;###autoload
(defun +thomas/project-p (dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (when-let ((project (project-current nil dir)))
    (project-root project)))

;;
;;; Libary

;;;###autoload
(cl-defun +file-templates--expand (pred &key project mode trigger ignore _when)
  "Auto insert a yasnippet snippet into current file and enter insert mode (if
evil is loaded and enabled)."
  (when (and pred (not ignore))
    (when (if project (+thomas/project-p) t)
      (unless mode
        (setq mode
              (if (and (symbolp pred) (not (booleanp pred)))
                  pred
                major-mode)))
      (unless mode
        (user-error "Couldn't determine mode for %s file template" pred))
      (unless trigger
        (setq trigger +file-templates-default-trigger))
      (if (functionp trigger)
          (funcall trigger)
        (require 'yasnippet)
        (unless yas-minor-mode
          (yas-minor-mode-on))
        (when (and yas-minor-mode
                   (when-let
                       (template (cl-find trigger (yas--all-templates (yas--get-snippet-tables mode))
                                          :key #'yas--template-key :test #'equal))
                     (yas-expand-snippet (yas--template-content template)))
                   (and (featurep 'evil) evil-local-mode)
                   (and yas--active-field-overlay
                        (overlay-buffer yas--active-field-overlay)
                        (overlay-get yas--active-field-overlay 'yas--field)))
          (evil-change-state 'insert))))))

(defun +file-template-p (rule)
  "Return t if RULE applies to the current buffer."
  (let ((pred (car rule))
        (plist (cdr rule)))
    (and (or (and (symbolp pred)
                  (eq major-mode pred))
             (and (stringp pred)
                  (stringp buffer-file-name)
                  (string-match-p pred buffer-file-name)))
         (or (not (plist-member plist :when))
             (funcall (plist-get plist :when)
                      buffer-file-name))
         rule)))

(defun +file-templates-check-h ()
  "Check if the current buffer is a candidate for file template expansion. It
must be non-read-only, empty, and there must be a rule in
`+file-templates-alist' that applies to it."
  (and (not +file-templates-inhibit)
       buffer-file-name        ; this buffer represents a file and
       (not buffer-read-only)  ; ...isn't read-only
       (bobp) (eobp)           ; ...is empty
       (not (member (substring (buffer-name) 0 1) '("*" " ")))  ; ...isn't a "special" buffer
       (not (bound-and-true-p org-capture-current-plist))  ; ...isn't an org-capture buffer
       (not (file-exists-p buffer-file-name))  ; ...is a new file
       (not (buffer-modified-p))    ; ...hasn't been modified
       (null (buffer-base-buffer))  ; ...isn't an indirect clone
       (when-let (rule (cl-find-if #'+file-template-p +file-templates-alist))
         (apply #'+file-templates--expand rule))))

;;
;;; Commands
;;;###autoload
(defun +file-templates/insert-license ()
  "Insert a license file template into the current file."
  (interactive)
  (require 'yasnippet)
  (unless (gethash 'text-mode yas--tables)
    (yas-reload-all t))
  (let ((templates
         (let (yas-choose-tables-first ; avoid prompts
               yas-choose-keys-first)
           (cl-loop for tpl in (yas--all-templates (yas--get-snippet-tables 'text-mode))
                    for uuid = (yas--template-uuid tpl)
                    if (string-prefix-p "__license-" uuid)
                    collect (cons (string-remove-prefix "__license-" uuid) tpl)))))
    (when-let (uuid (yas-choose-value (mapcar #'car templates)))
      (yas-expand-snippet (cdr (assoc uuid templates))))))

;;;###autoload
(defun +file-templates/debug ()
  "Tests the current buffer and outputs the file template rule most appropriate
for it. This is used for testing."
  (interactive)
  (cl-destructuring-bind (pred &rest plist &key trigger mode &allow-other-keys)
      (or (cl-find-if #'+file-template-p +file-templates-alist)
          (user-error "Found no file template for this file"))
    (if (or (functionp trigger)
            (cl-find trigger
                     (yas--all-templates
                      (yas--get-snippet-tables
                       mode))
                     :key #'yas--template-key :test #'equal))
        (message "Found %s" (cons pred plist))
      (message "Found rule, but can't find associated snippet: %s" (cons pred plist)))))

;;
;;; Bootstrap
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs '+file-templates-dir 'append #'eq))

;;
(add-hook 'find-file-hook #'+file-templates-check-h)

(provide 'init-snippets)
;;; init-snippets.el ends here
