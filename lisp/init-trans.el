;;; init-trans.el utils -*- lexical-binding: t; -*-
;; Translate Setting
(use-package go-translate
  :commands gt-do-translate
  :general
  (thomas-leader-define
    "d" '(:ignore t :wk "do")
    "d." 'gt-do-translate)
  :config
  (setq gt-langs '(en zh)
        gt-buffer-render-follow-p t
        gt-buffer-prompt-window-config
        '(display-buffer-reuse-window (inhibit-same-window . nil)))

  (setq gt-default-translator
        (gt-translator
         :taker (gt-taker :langs '(en zh) :text 'word)
         :engines (list  (gt-youdao-dict-engine) (gt-bing-engine))
         :render (gt-posframe-pop-render))))

(provide 'init-trans)
