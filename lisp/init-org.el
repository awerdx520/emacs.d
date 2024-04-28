(use-package org-download
  :config
  (when IS-WSL
    (setenv "XDG_SESSION_TYPE" "wayland")
    (eval-after-load 'org-download
      ;;; 修复 WSL 下粘贴剪贴板中的图片错误
      (defun org-download-clipboard (&optional basename)
        "Capture the image from the clipboard and insert the resulting file."
        (interactive)
        (let ((org-download-screenshot-method
               (if (executable-find "wl-paste")
                   "wl-paste -t image/bmp | convert bmp:- %s"
                 (user-error
                  "Please install the \"wl-paste\" program included in wl-clipboard"))))
          (org-download-screenshot basename))))))

