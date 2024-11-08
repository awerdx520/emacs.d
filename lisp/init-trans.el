;;; init-trans.el utils -*- lexical-binding: t; -*-
;; Translate Setting
(use-package go-translate
  :config
  (setq gt-langs '(en zh)
        gt-buffer-render-follow-p t
        gt-buffer-prompt-window-config
        '(display-buffer-reuse-window (inhibit-same-window . nil)))

  (setq gt-default-translator
        (gt-translator
         :taker (gt-taker :langs '(en zh) :text 'word)
         :engines (list (gt-google-engine :if '(and not-word parts))
                        (gt-youdao-dict-engine :if '(or src:zh tgt:zh))
                        (gt-bing-engine :if 'word)
                        (gt-youdao-suggest-engine :if '(and word src:en)))
         :render (gt-posframe-pop-render)))

  (setq gt-sentence-translator
        (gt-translator
         :taker (gt-taker :langs '(en zh) :text 'sentence)
         :engines (gt-google-engine)
         :render (gt-posframe-pop-render)))

  (defun +thomas/translate-sentence (sentence)
    "docstring"
    (interactive "P")
    (gt-start gt-sentence-translator))

  ;; TODO gt-buffer-render 在 evil 模式下键绑定有问题
  (setq gt-input-translator
        (gt-translator
         :taker (gt-taker :langs '(en zh) :text 'word :prompt t)
         :engines (list (gt-bing-engine :if 'word)
                        (gt-youdao-suggest-engine :if '(and word src:en)))
         :render (gt-buffer-render)))

  (defun +thomas/translate-input (word)
    "Translate with interactive in minibuffer, default select word at point."
    (interactive "P")
    (gt-start gt-input-translator))

  (cl-defmethod gt-thing-at-point ((_ (eql 'brackets)) (_ t))
    (form-at-point 'string))

  (setq gt-brackets-translator
        (gt-translator
         :taker (gt-taker :text 'brackets)
         :engines (gt-google-engine)
         :render (gt-posframe-pop-render)))

  (defun +thomas/translate-brackets (args)
    "docstring"
    (interactive "P")
    (gt-start gt-brackets-translator))


  (thomas-leader-define
    "d." 'gt-do-translate
    "ds" '+thomas/translate-sentence
    "di" '+thomas/translate-input
    "db" '+thomas/translate-brackets))


(provide 'init-trans)
