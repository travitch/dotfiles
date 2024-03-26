;; * Speed startup
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 100000000)))

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Convince emacs that alacrity is fine
(add-to-list 'term-file-aliases '("alacritty" . "xterm"))

;; Disable GUI stuff as early as possible
(when (display-graphic-p)
  ;; Disable ctrl Z, but only in GUI mode.  This is still useful
  ;; in a terminal
  (global-unset-key "\^z"))

(setq package-enable-at-startup nil)

;; Work around an issue with updating built-in packages (especially eldoc)
;;
;; https://github.com/progfolio/elpaca/issues/236
(setq elpaca-menu-functions '(elpaca-menu-gnu-devel-elpa))

(provide 'early-init)
;;; early-init.el ends here
