;;; init-text.el --- Writing -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Built in Plugins
;; help
;; Type text
(use-package text-mode
  :straight (:type built-in)
  :hook visual-line-mode
  :config
  ;; better word wrapping for CJK characters
  (setq word-wrap-by-category t
        ;; paragraphs
        sentence-end-double-space nil))

;; Pixel alignment for org/markdown tables
(use-package valign
  :hook ((markdown-mode org-mode) . valign-mode))

;; The markdown mode is awesome! unbeatable
(use-package markdown-mode
  :init
  (advice-add #'markdown--command-map-prompt :override #'ignore)
  (advice-add #'markdown--style-map-prompt   :override #'ignore)
  :mode ("README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :bind (:map markdown-mode-style-map
              ("r" . markdown-insert-ruby-tag)
              ("d" . markdown-insert-details))
  :config
  (defun markdown-insert-ruby-tag (text ruby)
    "Insert ruby tag with `TEXT' and `RUBY' quickly."
    (interactive "sText: \nsRuby: \n")
    (insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" text ruby)))

  (defun markdown-insert-details (title)
    "Insert details tag (collapsible) quickly."
    (interactive "sTitle: ")
    (insert (format "<details><summary>%s</summary>\n\n</details>" title)))
  :config
  (setq markdown-header-scaling t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t))

(provide 'init-text)
;;; init-text.el ends here
