;;; init-python.el --- Python -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defun thomas/lsp-bridge-get-single-lang-server-by-project (project-path filepath)
  "Lsp-bridge virtualenv support.

Since not every virtualenv has installed epc, you may need to configure lsp-bridge-python-command
as the environment to run epc."
  (let* ((json-object-type 'plist)
         (custom-config (concat thomas-cache-dir
                                "lsp-bridge/pyright/pyright.json"))
         (default-config (json-read-file (expand-file-name
                                          "straight/repo/lsp-bridge/langserver/pyright.json" thomas-local-dir)))
         (settings (plist-get default-config :settings)))
    (plist-put settings :pythonPath
               (executable-find "python"))
    (make-directory (file-name-directory custom-config) t)
    (with-temp-file custom-config
      (insert (json-encode default-config)))

    custom-config))


(use-package python
  :straight (:type built-in)
  :hook ((python-mode python-ts-mode) . rainbow-delimiters-mode)
  :mode ("\\.py\\'" . python-mode)
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  :config
  ;; Stop the spam!
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems link the unversioned one to Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; 添加重新获取 lsp-bridge
  (add-hook 'python-mode-hook (lambda ()
                                (setq-local lsp-bridge-get-single-lang-server-by-project
                                            'thomas/lsp-bridge-get-single-lang-server-by-project))))


;; python 项目管理软件
(use-package poetry
  :after python
  :init
  (setq poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

;; python 默认虚拟环境管理器
(use-package pyvenv
  :commands pyvenv-deactivate pyvenv-activate
  :config
  (defun pyrightconfig-write ()
    "Write a `pyrightconfig.json' file at the root of a project with
`venvPath` and `venv`."
    (let* ((vp (string-trim-right pyvenv-virtual-env "/"))
           (root (file-name-directory vp))
           (venv (file-name-base vp))
           (out-file (expand-file-name "pyrightconfig.json" root)))
      (with-temp-file out-file
        (insert (json-encode (list :venvPath root
                                   :venv venv))))
      (message "Configured `%s` to use environment `%s`" out-file pyvenv-virtual-env)))
  (add-hook 'pyvenv-post-activate-hooks #'pyrightconfig-write)

  ;; pyvenv 激活环境重启 lsp-bridge
  (add-hook 'pyvenv-post-activate-hooks (lambda () (lsp-bridge-restart-process))))

(use-package cython-mode
  :mode "\\.p\\(yx\\|x[di]\\)\\'"
  :config
  (setq cython-default-compile-format "cython -a %s"))

(use-package flycheck-cython
  :after cython-mode)

(provide 'init-python)
;;; init-python.el ends here
