;;; core-autoload.el -*- lexical-binding: t; -*-

(defconst thomas-core-autoload-dir (expand-file-name "core/autoload/" thomas-emacs-dir)
  "Core autoload directory.")

(defvar generated-autoload-file nil
  "This is neccessary, otherwise raise error.
`Defining as dynamic an already lexical var`.")

(defvar thomas-autoload-file
  (expand-file-name "autoload.el" thomas-local-dir)
  "Autoload file.")

(defun thomas-autoload/generate-define (loaddef &rest DIRS)
  "LOADDEF DIRS."
  (let ((generated-autoload-file loaddef))
    (when (or (not (file-exists-p generated-autoload-file))
              noninteractive)
      (loaddefs-generate DIRS thomas-autoload-file))))

(defun thomas-autoload/reload ()
  "Generate autoload file from `core/autoload'."
  (interactive)
  (when (file-exists-p thomas-autoload-file)
    (delete-file thomas-autoload-file t)
    (message "delete old autoload file: %s" thomas-autoload-file))

  (thomas-autoload/generate-define thomas-autoload-file thomas-core-autoload-dir)
  (load thomas-autoload-file nil 'nomessage)
  (message "generate autoload file: %s done." thomas-autoload-file))

(unless (file-exists-p thomas-autoload-file)
  (thomas-autoload/reload))

(load thomas-autoload-file nil 'nomessage)

(provide 'core-autoload)
