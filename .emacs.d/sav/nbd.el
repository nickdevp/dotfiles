(setq vc-follow-symlinks nil)

; parens
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; c 
(setq c-default-style "linux"
      c-basic-offset 4)

(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
