;; Provides preference overrides in exordium
;; See modules/init-prefs.el for availble preferences


(setq exordium-theme 'tomorrow-night)

;; don't override Ctrl-z behavior
(setq exordium-keyboard-ctrl-z-undo nil)

;; Requires UTF-8 locale (i.e., set LANG=en_US.UTF8 in .bashrc)
(setq exordium-enable-powerline t)

;; don't automatically close parens
(setq exordium-enable-electric-pair-mode nil)

;; ediff split windows left / right
(custom-set-variables
 '(ediff-split-window-function 'split-window-horizontally))
