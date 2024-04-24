;;; custom.el -*- lexical-binding: t; -*-
;;;
;; Startup Maximum
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Font size
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun thomas-setup-font ()
  "Setup Emacs Font."
  (when (display-graphic-p)
    (cl-loop for font in '("Source Code Pro" "Fira Code" "Monaco"
                           "Dejavu Sans Mono" "Menlo" "Cascadia Code" "SF Mono"
                           "Lucida Console" "Consolas" "SAS Monospace")
             when (font-installed-p font)
             return (set-face-attribute
                     'default nil
                     :font (font-spec :family font
                                      :weight 'normal
                                      :slant 'normal
                                      :size (cond ((eq system-type 'gnu/linux) 13.0)
                                                  ((eq system-type 'windows-nt) 12.5)))))
    (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
                           "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode
                                      (font-spec :family font
                                                 :size (cond ((eq system-type 'gnu/linux) 13.0)
                                                             ((eq system-type 'windows-nt) 15.0)))
                                      nil 'prepend))
    (cl-loop for font in '("Sarasa Term SC Nerd" "思源黑体 CN" "思源宋体 CN" "微软雅黑 CN"
                           "Source Han Sans CN" "Source Han Serif CN"
                           "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                           "Microsoft Yahei UI" "Microsoft Yahei")
             when (font-installed-p font)
             return (set-fontset-font t '(#x4e00 . #x9fff)
                                      (font-spec :name font
                                                 :weight 'normal
                                                 :slant 'normal
                                                 :size (cond ((eq system-type 'gnu/linux)12.5)
                                                             ((eq system-type 'windows-nt) 15.0)))))
    (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
             when (font-installed-p font)
             return (set-fontset-font t '(#x20000 . #x2A6DF)
                                      (font-spec :name font
                                                 :weight 'normal
                                                 :slant 'normal
                                                 :size (cond ((eq system-type 'gnu/linux)13.0)
                                                             ((eq system-type 'windows-nt) 15.0)))))))

(thomas-setup-font)
(add-hook 'window-setup-hook #'thomas-setup-font)
(add-hook 'server-after-make-frame-hook #'thomas-setup-font)

;;;



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(package-selected-packages '(valign markdown-mode))
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "cd /home/thomas/Workspace/Work/DataClean/normalize/dacnspark && python setup.py bdist_egg"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
