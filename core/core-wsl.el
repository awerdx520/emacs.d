;;; init-wsl.el --- Bring vim back -*- lexical-binding: t -*-

;; 解决复制中文出现乱码
(if IS-WSL
    (set-selection-coding-system 'gbk)
  (set-selection-coding-system 'utf-8))

(provide 'core-wsl)
