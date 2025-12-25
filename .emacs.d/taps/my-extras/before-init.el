;;-----------------------------------------------------------------------------
;; BDE Formatting Utils
;;-----------------------------------------------------------------------------
(defun bde-wrap (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "bdewrap"
                           (current-buffer) t))

(defun bde-format (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "bde-format"
                           (current-buffer) t))

(defun bde-right-align ()
  "Right align current line up to the fill column."
  (interactive)
  (save-excursion
    (let ((numColumns 0) numNew bp el cn)
      (if mark-active
         (progn
           (setq bp (region-beginning))
           (goto-char (region-end))
           (setq el (line-number-at-pos (region-end)))
           (when (= (point) (point-at-bol))
             (setq el (1- el))))
        (progn
          (setq bp (point))
          (setq el (line-number-at-pos bp))))
      (goto-char bp)
      (setq cn (current-column))
      (while (<= (line-number-at-pos (point)) el)
        (setq numColumns (max (- (point-at-eol) (point-at-bol)) numColumns))
        (forward-line))
      (setq numNew (- fill-column numColumns))
      (goto-char bp)
      (while (<= (line-number-at-pos (point)) el)
        (if (= (line-number-at-pos (point)) (line-number-at-pos bp))
            (goto-char bp)
          (goto-char (point-at-bol)))
        (re-search-forward "^//" (point-at-eol) t)
        (if (< 0 numNew)
          (insert-char ?\s numNew)
          (delete-char (abs numNew)))
       (forward-line))
     (deactivate-mark))))
