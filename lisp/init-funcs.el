
;; UI
;; search
(defun +thomas/search-buffer ()
  "Conduct a text search on the current buffer.

If a selection is active and multi-line, perform a search restricted to that
region.

If a selection is active and not multi-line, use the selection as the initial
input and search the whole buffer for it."
  (interactive)
  (let (start end multiline-p)
    (save-restriction
      (when (region-active-p)
        (setq start (region-beginning)
              end   (region-end)
              multiline-p (/= (line-number-at-pos start)
                              (line-number-at-pos end)))
        (deactivate-mark)
        (when multiline-p
          (narrow-to-region start end)))

      (if (and start end (not multiline-p))
          (consult-line
           (replace-regexp-in-string
            " " "\\\\ "
            (rxt-quote-pcre
             (buffer-substring-no-properties start end))))
        (call-interactively #'consult-line)))))
;;;
(defun +thomas/search-emacsd (regexp)
  "Conduct a text search in files under `thomas-emacs-dir'"
  (interactive (list (project--read-regexp)))
  (let ((default-directory thomas-emacs-dir)
        (vc-directory-exclusion-list '(".git" ".local")))
    (project-find-regexp regexp)))

;; file
(defun +thomas/glob (&rest segments)
  "Return file list matching the glob created by joining SEGMENTS.

The returned file paths will be relative to `default-directory', unless SEGMENTS
concatenate into an absolute path.

Returns nil if no matches exist.
Ignores `nil' elements in SEGMENTS.
If the glob ends in a slash, only returns matching directories."
  (declare (side-effect-free t))
  (let* (case-fold-search
         file-name-handler-alist
         (path (apply #'file-name-concat segments)))
    (if (string-suffix-p "/" path)
        (cl-delete-if-not #'file-directory-p (file-expand-wildcards (substring path 0 -1)))
      (file-expand-wildcards path))))

;;; ]f / [f
(defun +thomas/find--current-buffer-n-pos-file (n)
  (unless buffer-file-name
    (user-error "Must be called from a file-visiting buffer"))
  (let* ((directory (file-name-directory buffer-file-name))
         (filename (file-name-nondirectory buffer-file-name))
         (files (cl-remove-if-not #'file-regular-p
                                  (+thomas/glob (file-name-directory buffer-file-name) "[!.]*")))
         (index (cl-position filename files :test #'file-equal-p)))

    (when (null index)
      (user-error "Couldn't find this file in current directory"))

    (let ((index (+ index n)))
      (cond ((>= index (length files))
             (user-error "No files after this one"))
            ((< index 0)
             (user-error "No files before this one"))
            ((expand-file-name (nth index files) directory))))))

(defun +thomas/find-next-file (count)
  "Open file following this one, alphabetically, in the same directory."
  (interactive "p")
  (find-file (+thomas/find--current-buffer-n-pos-file count)))

(defun +thomas/find-previous-file (count)
  "Open file preceding this one, alphabetically, in the same directory."
  (interactive "p")
  (find-file (+thomas/find--current-buffer-n-pos-file (- count))))

(defun +thomas/find-file-in-emacsd (&optional include-all)
  "Find user emacs dir file"
  (interactive "P")
  (let  ((default-directory user-emacs-directory)
         (vc-directory-exclusion-list '(".git" ".local")))
    (project-find-file include-all)))


(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))



;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; Fonts
(defun +thomas/setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height 130))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW Neo Xihei" "WenQuanYi Micro Hei Mono" "LXGW WenKai Screen"
                           "LXGW WenKai Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.3)))
                      (set-fontset-font t 'han (font-spec :family font))))))




(provide 'init-funcs)
