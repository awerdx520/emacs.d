;; -*- lexical-binding: t -*-

(use-package rime
  :init
  (setq default-input-method "rime")
  :config
  (setq rime-user-data-dir (concat thomas-cache-dir "rime/")))

(use-package pangu-spacing
  :hook ((text-mode . pangu-spacing-mode)
         ;; Always insert `real' space in org-mode.
         (org-mode . (lambda ()
                       (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))))

(use-package ace-pinyin
  :after avy
  :init (setq ace-pinyin-use-avy t)
  :custom-face (aw-leading-char-face ((t (:foreground "white" :background "#2a6041")
                                         :weight bold :height 2.5 :box (:line-width 10 :color "#2a6041"))))
  :config
  (ace-pinyin-global-mode t))


(use-package evil-pinyin
  :after evil
  :config
  (setq-default evil-pinyin-with-search-rule 'always)
  (global-evil-pinyin-mode 1))

(provide 'init-chinese)


