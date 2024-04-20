;;; init-package.el Emacs 包管理配置  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 22, 2024
;; Modified: April 22, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-package
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; Ensure that, if we do need package.el, it is configured correctly. You really
;; shouldn't be using it, but it may be convenient for quickly testing packages.
(setq package-user-dir (concat thomas-local-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir))

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

;; config use-usepackage
(setq use-package-always-ensure nil
      use-package-enable-imenu-support t
      use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-expand-minimally t
      use-package-compute-statistics nil)

;;; Standard package repositories
(with-eval-after-load 'package
  ;; Standard package repositories
  (setq package-archives '(("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                           ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))

;; Load straight bootstrap file
(defvar bootstrap-version)
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

(setq byte-compile-warnings '(cl-functions))
(straight-use-package 'use-package)

(provide 'init-package)
;;; init-package.el ends here
