;; * Speed startup
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)

;; Convince emacs that alacrity is fine
(add-to-list 'term-file-aliases '("alacritty" . "xterm"))

;; Disable GUI stuff as early as possible
(when (display-graphic-p)
  ;; Disable ctrl Z, but only in GUI mode.  This is still useful
  ;; in a terminal
  (global-unset-key "\^z"))

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
