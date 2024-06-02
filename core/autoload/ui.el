;;; ui.el --- autoload functions used in buffer. -*- lexical-binding: t -*-;
;;
;; Copyright (C) 2017-2022 awerdx520@gmail.com
;;
;; Author: Kevin Leung <awerdx520@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This ui is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;; Code:

;; Font size
;;;###autoload
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;;;###autoload
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
                                      :weight 'regular
                                      :slant 'normal
                                      :size (cond ((eq system-type 'gnu/linux) 14.0)
                                                  ((eq system-type 'windows-nt) 12.5)))))
    (cl-loop for font in '("OpenSansEmoji" "Noto Color Emoji" "Segoe UI Emoji"
                           "EmojiOne Color" "Apple Color Emoji" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode
                                      (font-spec :family font
                                                 :size (cond ((eq system-type 'gnu/linux) 14.0)
                                                             ((eq system-type 'windows-nt) 15.0)))
                                      nil 'prepend))
    (cl-loop for font in '("Sarasa Term SC Nerd" "思源黑体 CN" "思源宋体 CN" "微软雅黑 CN"
                           "Source Han Sans CN" "Source Han Serif CN"
                           "WenQuanYi Micro Hei" "文泉驿等宽微米黑"
                           "Microsoft Yahei UI" "Microsoft Yahei")
             when (font-installed-p font)
             return (set-fontset-font t '(#x4e00 . #x9fff)
                                      (font-spec :name font
                                                 :weight 'regular
                                                 :slant 'normal
                                                 :size (cond ((eq system-type 'gnu/linux)14.0)
                                                             ((eq system-type 'windows-nt) 15.0)))))
    (cl-loop for font in '("HanaMinB" "SimSun-ExtB")
             when (font-installed-p font)
             return (set-fontset-font t '(#x20000 . #x2A6DF)
                                      (font-spec :name font
                                                 :weight 'regular
                                                 :slant 'normal
                                                 :size (cond ((eq system-type 'gnu/linux)14.0)
                                                             ((eq system-type 'windows-nt) 15.0)))))))

