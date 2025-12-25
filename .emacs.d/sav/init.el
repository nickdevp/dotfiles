;; Basic environment settings
(setq
 default-truncate-lines          t
 inhibit-startup-message         t
 initial-scratch-message         nil
 make-backup-files               nil
 line-number-display-limit-width 2000
 message-log-max                 1000
 visible-bell                    t
 x-alt-keysym                   'meta ; Make ALT key send Meta
 x-cut-buffer-max               65536 ; default=20000
)

;
;;      Buffer-local variables
;;
;;      If a SYMBOL is not buffer-local for the current buffer, and is
;;      not marked automatically buffer-local, `setq-default' has the
;;      same effect as `setq'.  If SYMBOL is buffer-local for the
;;      current buffer, then this changes the value that other buffers
;;      will see (as long as they don't have a buffer-local value),
;;      but not the value that the current buffer sees.
;
(setq-default
 fill-column                    79 
 indent-tabs-mode               nil     ; Always spaces, never tabs
 indicate-empty-lines           t
 ;; For a new non-file buffer set its major mode based on the buffer name.
 ;; http://thread.gmane.org/gmane.emacs.devel/115520/focus=115794
 major-mode                     (lambda ()
                                  (if buffer-file-name
                                      (fundamental-mode)
                                    (let ((buffer-file-name (buffer-name)))
                                      (set-auto-mode))))
)


;
;;      Load path
;(add-to-list 'load-path "/usr/share/emacs/site-lisp")
;(add-to-list 'load-path cel/emacs-lisp-dir) ; ~/share/emacs/site-lisp


;
;;      Extensions to ignore
;
(dolist (extension '(",v" ".lastlink" ".swp" ".tsk"))
  (add-to-list 'completion-ignored-extensions extension))

;
;;      Modes I like
;
(blink-cursor-mode              0)
(column-number-mode             1)
(minibuffer-electric-default-mode 1)


;
;;      Don't enter search and replace args if buffer is read-only
;
(defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))



;; Make fold markers ("...")  more visible (from Tassilo Horn <tassilo@member.fsf.org>)
(defface selective-display
  '((default :bold t)
    (((supports :underline "red")) :underline "red"))
  "Face used for fold markers like ...")
(unless standard-display-table
  (setq standard-display-table (make-display-table)))
(set-display-table-slot standard-display-table 'selective-display
                        (vconcat (mapcar
                                  (lambda (c)
                                    (make-glyph-code c 'selective-display))
                                  "...")))
(put 'downcase-region 'disabled nil)


;; load other files

;; bde
(defconst bde-file (locate-user-emacs-file "bde.el")
  "BDE emacs functions")
(load-file bde-file)
;(load "~/blp-emacs/bde")


(defconst nbd-stuff (locate-user-emacs-file "nbd.el")
  "custom stuff")
(load-file nbd-stuff)

