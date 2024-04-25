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

;; 在仪表盘上展示 hacker news
(use-package dashboard-hackernews)

;;
(use-package dashboard
  :after dashboard-hackernews
  :custom-face
  (dashboard-items-face ((t (:weight normal))))
  (dashboard-heading-face ((t (:weight bold))))
  :init
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons `(((,(if (fboundp 'nerd-icons-octicon)
                                             (nerd-icons-octicon "nf-oct-mark_github")
                                           "¡ï")
                                        "GitHub" "Browse" (lambda (&rest _) (browse-url homepage-url)))

                                       (,(if (fboundp 'nerd-icons-octicon)
                                             (nerd-icons-octicon "nf-oct-download")
                                           "?")
                                        "Upgrade" "Upgrade packages synchronously"
                                        (lambda (&rest _) (package-upgrade-all nil)) success))))
  :config
  ;; 在 Server 模式下，创建 frame 时显示仪表盘
  (setq initial-buffer-choice (lambda ()
                                (get-buffer-create "*dashboard*")))
  ;;
  (setq dashboard-startup-banner '2
        dashboard-projects-backend 'project-el)
  ;;
  (setq dashboard-set-init-info t
        dashboard-set-navigator t)
  (setq dashboard-items '((recents   . 5)
                          (projects  . 7)
                          (agenda . 5)
                          (bookmarks . 5)
                          (hackernews . 8)))
  ;; TODO 设置 heading icon 存在找不到 nero-icons 字体的问题
  (setq dashboard-set-heading-icons t
        dashboard-center-content t
        dashboard-set-file-icons t
        dashboard-icon-type 'nerd-icons
        dashboard-heading-icons '((hackernews . "nf-oct-paperclip")
                                  (agenda . "nf-oct-calendar")
                                  (recents . "nf-oct-file")
                                  (projects . "nf-oct-project")
                                  (bookmarks . "nf-oct-bookmark")))
  ;; 启动 dashboard 设置
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
