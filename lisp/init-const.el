;; init-const.el --- Define constants.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2024 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Define constants.
;;

;;; Code:
(require 'cl-lib)
(eval-when-compile
  (require 'subr-x))

(defconst thomas-leader-key "SPC"
  "Set default leader key.")

(defconst thomas-leader-alt-key "M-SPC"
  "Set default leader alt key.")

(defconst thomas-localleader-key "SPC m"
  "Set default local leader key.")

(defconst homepage-url "https://github.com/awerdx520/"
  "Github Homepage url.")

(defconst IS-WSL
  (and (eq system-type 'gnu/linux)
       (string-match "-[Mm]icrosoft" operating-system-release))
  "Are we running on a GNU/Linux system?")

(defvar thomas-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defvar thomas-local-dir (concat thomas-emacs-dir ".local/")
  "Root directory for local storage.

Use this as a storage location for this system's installation of thomas Emacs.")

(defvar thomas-data-dir (concat thomas-local-dir "etc/")
  "Where thomas stores its global data files.

Data files contain shared and long-lived data that thomas, Emacs, and their
packages require to function correctly or at all.")

(defvar thomas-cache-dir (concat thomas-local-dir "cache/")
  "Where thomas stores its global cache files.

Cache files represent unessential data that shouldn't be problematic when
deleted (besides, perhaps, a one-time performance hit), lack portability (and so
shouldn't be copied to other systems/configs), and are regenerated when needed,
without user input (e.g. a `thomas sync`).")

(defvar thomas-state-dir (concat thomas-local-dir "state/")
  "Where Doom stores its global state files.
State files contain unessential, unportable, but persistent data which, if lost
won't cause breakage, but may be inconvenient as they cannot be automatically
regenerated or restored. For example, a recently-opened file list is not
essential, but losing it means losing this record, and restoring it requires
revisiting all those files.")


(provide 'init-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
