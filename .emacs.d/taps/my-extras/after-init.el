;; global keys
(global-set-key (kbd "C-c C-c") 'comment-region)

;; c/c++ only keys
(define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)
(define-key c-mode-base-map (kbd "C-c w") 'bde-wrap)
(define-key c-mode-base-map (kbd "C-c f") 'bde-format)
(define-key c-mode-base-map (kbd "C-c r") 'bde-right-align)

;;
;;(add-hook 'c++-mode-hook 'eglot-ensure)

;; Use company mode for c/c++
;;(use-package company :ensure t)
