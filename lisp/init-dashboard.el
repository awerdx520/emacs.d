;;; init-dashboard.el Emacs 仪表盘 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-dashboard
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package dashboard
  :init
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons `(((,(if (fboundp 'nerd-icons-octicon)
                                             (nerd-icons-octicon "nf-oct-mark_github")
                                           "¡ï")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))
                                       (,(if (fboundp 'nerd-icons-octicon)
                                             (nerd-icons-octicon "nf-oct-download")
                                           "?")
                                        "Upgrade" "Upgrade packages synchronously" (lambda (&rest _) (package-upgrade-all nil)) success))))
  :config
  (setq dashboard-startup-banner 'logo
        dashboard-projects-backend 'project-el
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-init-info t
        dashboard-set-navigator t
        dashboard-items '((recents   . 10)
                          (bookmarks . 5)
                          (projects  . 7)
                          (agenda . 5)
                          (registers . 5)))
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
