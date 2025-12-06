;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; This configuration is managed primarily with the straight.el package manager.

;;; Code:

(defconst +tr/custom-file "~/.emacs.d/custom.el")
(load +tr/custom-file 'noerror)

;; Disable some unneeded UI elements (as early as possible)
(when (display-graphic-p)
  (tooltip-mode -1))

;; * Package system setup

(setq straight-check-for-modifications '(check-on-save))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Hide some minor mode indicators in the modeline (and, since this can introduce a keyword to
;; use-package, we load it early).
(use-package diminish)

(use-package eldoc
  :straight (:type built-in)
  :diminish)

;; Configuration for builtins
(use-package emacs
  :straight (:type built-in)
  :init
  ;; ** Customization requiring function calls

  ;; Have delete and backspace delete the entire marked region, if any
  (delete-selection-mode t)
  ;; Automatically decompress some file types
  (auto-compression-mode t)
  ;; Show matching parens
  (show-paren-mode t)
  ;; Enable syntax highlighting
  (global-font-lock-mode 1)
  ;; Indicate file size in the modeline
  (size-indication-mode 1)

  ;; ** Filling parameters
  ;;
  ;; Set up some bits to try to make paragraph filling  behave a bit better.

  ;; Try to make auto-fill work a bit better in the context of bulleted lists.
  (setq paragraph-start "\f\\|>*[ \t]*$\\|>*[ \t]*[-+*] \\|>*[ \t]*[0-9#]+\\. ")
  (setq paragraph-separate "[ ]*\\(//+\\|\\**\\)\\([ ]*\\| <.*>\\)$\\|^\f")
  (setq-default fill-column 100)

  ;; ** Core Behavior

  ;; Suppress annoying backup files
  (setq make-backup-files nil)

  ;; ** GUI Control

  ;; Maximize font locking
  (setq font-lock-maximum-decoration t)
  ;; Flash the screen instead of generating an audible bell
  (setq visible-bell t)
  ;; Show column numbers next to line numbers
  (setq column-number-mode t)
  ;; Highlight marked regions (default is invisible)
  (setq transient-mark-mode t)
  (setq mark-even-if-inactive nil)
  ;; Don't let the cursor hit the bottom line
  (setq scroll-margin 3)


  ;; ** Window control

  ;; Only split a window vertically if it has at least this many lines; prevents a
  ;; preponderance of tiny windows
  (setq split-height-threshold 50)
  ;; Keep the compilation window small
  (setq compilation-window-height 8)
  ;; Make font locking quiet
  (setq font-lock-verbose nil)


  ;; ** Copy and paste
  ;;
  ;; Attempt to tame the various clipboards and make interaction with the system
  ;; clipboard a bit smoother.
  (setq select-enable-clipboard t)
  (setq select-enable-primary t)
  (setq save-interprogram-paste-before-kill t)

  ;; ** Interaction
  ;;
  ;; Make interactive prompts a bit less annoying

  ;; Let me use just y or n to answer prompts
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; Do not enter the interactive debugger when encountering elisp errors.  Set to
  ;; t when debugging
  (setq debug-on-error nil)

  ;; ** Buffer-local variables
  ;;
  ;; Set up defaults for values that are buffer local (using setq-default instead
  ;; of just setq)

  ;;; Set some buffer-local variables to sensible defaults
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default truncate-lines t)
  (setq-default require-final-newline t)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (defun find-file-noselect--advice-around-with-line-number (orig-fun filename &rest args)
    "Advice wrapper to open files from the command line with a line number specified."
    (save-match-data
      (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
             (line-number (and matched
                               (match-string 2 filename)
                               (string-to-number (match-string 2 filename))))
             (filename (if matched (match-string 1 filename) filename))

             ;; Call the underlying function
             (buf (apply orig-fun filename args)))

        (when line-number
          (with-current-buffer buf
            ;; goto-line is for interactive use
            (goto-char (point-min))
            (forward-line (1- line-number))))

        ;; Always need to return the buffer for other things to use
        buf)))
  ;; By advising find-file-noselect instead of find-file we also get this
  ;; behavior in lots of other places, e.g. opening files from emacsclient.
  (advice-add #'find-file-noselect :around #'find-file-noselect--advice-around-with-line-number)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package project
  :bind-keymap
  (("C-c p" . project-prefix-map)))

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (add-hook 'org-mode-hook #'toc-org-enable)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)))
  :bind
  (:map org-mode-map
        ("C-c M-l" . org-store-link))
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELED(c@)")))
  (setq org-log-into-drawer t))

;; Visual improvements for org mode
(use-package org-modern
  :commands (org-modern-mode org-modern-agenda)
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; * Package selection and initialization

;; ** Built-in modes
;;
;; This section contains customizations of built-in modes that don't actually
;; need to be installed.  It is set up through use-package so that
;; customizations are grouped by the relevant modes.

(use-package comp
  :straight (:type built-in)
  :config
  ;; Be a bit less noisy with async native compilation warnings
  (setq native-comp-async-report-warnings-errors nil))

(defun tr/show-trailing-whitespace ()
  "A trivial wrapper function around setting a value that can be used in a hook."
  (setq show-trailing-whitespace t))

(use-package text-mode
  :straight (:type built-in)
  :preface (provide 'text-mode)
  :init
  (add-hook 'text-mode-hook #'tr/show-trailing-whitespace))

(use-package prog-mode
  :straight (:type built-in)
  :preface (provide 'prog-mode)
  :init
  (add-hook 'prog-mode-hook #'tr/show-trailing-whitespace))

(use-package conf-mode
  :straight (:type built-in)
  :preface (provide 'conf-mode)
  :init
  (add-hook 'conf-mode-hook #'tr/show-trailing-whitespace))

;; Set up some handling of mail mode (used through mutt).  mail-mode is provided
;; by the sendmail library built-in to emacs, but we want to add some hooks here.
(use-package sendmail
  :straight (:type built-in)
  :mode ("/tmp/mutt.*" . mail-mode)
  :init
  (add-hook 'mail-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'mail-mode-hook #'mail-text))

(use-package abbrev
  :diminish abbrev-mode
  :straight (:type built-in)
  :config
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)))

;; Give unique tags to buffers that have the same name to distinguish them.
(use-package uniquify
  :straight (:type built-in)
  :config
  (setq uniquify-buffer-name-style #'post-forward-angle-brackets))

;; ** Themes

;; Use solarized light, and turn down some of the more aggressive font choices
;; that (especially a few that are proportional).
;; (use-package solarized-theme
;;   :hook (after-init . (lambda () (load-theme 'solarized-light t)))
;;   :config
;;   (setq solarized-use-variable-pitch nil)
;;   (setq solarized-scale-org-headlines nil))

(use-package modus-themes
;;  :hook (after-init . (lambda () (load-theme 'modus-operandi)))
  )

(use-package ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :hook (after-init . (lambda () (modus-themes-load-theme 'ef-light))))

;; This is a modeline replacement that is a bit cleaner while still being lightweight (compared to e.g., spaceline)
;; (use-package simple-modeline
;;   :hook (after-init . simple-modeline-mode))

(use-package lambda-line
  :straight (:type git :host github :repo "lambda-emacs/lambda-line")
  :custom
  (lambda-line-position 'bottom) ;; Set position of status-line
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  (lambda-line-gui-mod-symbol " ⬤")
  (lambda-line-gui-rw-symbol  " ◯")
  (lambda-line-space-top +.50)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  :hook (after-init . lambda-line-mode)
  :config
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

;; This mode provides a function that enables a server running from emacs that
;; can edit text boxes in browsers using an appropriate extension.
;;
;; See Ghost Text: https://github.com/fregante/GhostText
(use-package atomic-chrome
  :commands (atomic-chrome-start-server)
  :defines atomic-chrome-default-major-mode
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode))

;; ** Programming modes

;; Set up some extra alignment rules for haskell mode to allow for easy
;; alignment (with M-]).
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (bind-key "C-c h p" #'hydra-haskell-pragma/body)
  (bind-key "C-c h i i"  #'haskell-interactive-import-begin)
  :bind
  ("C-c h i g" . haskell-navigate-imports)
  ("C-c h i r" . haskell-navigate-imports-return)
  ("C-c h i f" . haskell-mode-format-imports)
  :defines (haskell-font-lock-symbols haskell-indentation-starter-offset)
  :config
  (setq haskell-font-lock-symbols nil)
  (setq haskell-indentation-starter-offset 2)
  (add-hook 'align-load-hook
            (lambda ()
              (add-to-list 'align-rules-list
                           '(haskell-types
                             (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                             (modes quote (haskell-mode literate-haskell-mode))))
              (add-to-list 'align-rules-list
                           '(haskell-assignment
                             (regexp . "\\(\\s-+\\)=\\s-+")
                             (modes quote (haskell-mode literate-haskell-mode))))
              (add-to-list 'align-rules-list
                           '(haskell-arrows
                             (regexp . "\\(\\s-+\\)\\(->\\|=>\\|::\\|→\\)\\s-+")
                             (modes quote (haskell-mode literate-haskell-mode))))
              (add-to-list 'align-rules-list
                           '(haskell-left-arrows
                             (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                             (modes quote (haskell-mode literate-haskell-mode)))))))

(use-package cmake-ts-mode
  :straight (:type built-in)
  :mode ("CMakeLists.txt$" . cmake-ts-mode))

(use-package csharp-mode
  :mode ("\\.cs$" . csharp-mode))

(use-package fsharp-mode
  :mode ("\\.fs$" . fsharp-mode))

(use-package qml-mode
  :mode ("\\.qml$" . qml-mode))

;; A mode for mixed HTML + JS + CSS
(use-package web-mode
  :mode ("\\.html$" . web-mode))

(use-package rjsx-mode
  :mode ("\\.jsx$" . rjsx-mode))

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.ts$" . typescript-ts-mode)
         ("\\.js$" . typescript-ts-mode)))

(use-package clojure-mode
  :mode ("\\.clj$" . clojure-mode))

(use-package go-ts-mode
  :straight (:type built-in)
  :mode ("\\.go$" . go-ts-mode))

(use-package scala-mode
  :mode ("\\.sc$\\|\\.scala$" . scala-mode))

(use-package julia-mode
  :mode ("\\.jl$" . julia-mode))

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

(use-package kotlin-mode
  :mode ("\\.kt$" . kotlin-mode))

(use-package neocaml
  :straight (:host github :repo "bbatsov/neocaml"))

(use-package vimrc-mode
  :mode ("vimrc" . vimrc-mode))

(use-package nix-mode
  :mode ("\\.nix$" . nix-mode))

(use-package idris-mode
  :mode ("\\.idr$" . idris-mode))

(use-package rust-ts-mode
  :straight (:type built-in)
  :mode ("\\.rs$" . rust-ts-mode))

(use-package forth-mode
  :mode ("\\.fth$" . forth-mode))

(use-package python-ts-mode
  :straight (:type built-in)
  :config
  (setq python-shell-interpreter "ipython3")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  :mode (("\\.py$" . python-ts-mode)
         ("SConstruct$" . python-ts-mode)
         ("SConscript$" . python-ts-mode)))

(use-package code-cells
  :mode ("\\.ipynb$" . code-cells-mode))

(use-package fish-mode
  :mode ("\\.fish$" . fish-mode))

(use-package smithy-mode
  :mode ("\\.smithy$" . smithy-mode))

(use-package lean4-mode
  :straight (lean4-mode
       :type git
	     :host github
	     :repo "leanprover/lean4-mode"
	     :files ("*.el" "data"))
  :mode ("\\.lean$" . lean4-mode))

(use-package z3-mode
  :mode ("\\.smt$" . z3-mode))

(use-package matlab-mode
  :mode ("\\.m$" . matlab-mode))

;; This is a JSON mode that is optimized to handle larger files well
(use-package jsonian
  :mode ("\\.json$" . jsonian-mode))

(use-package csv-mode
  :mode ("\\.csv$" . csv-mode))

(use-package protobuf-mode
  :mode ("\\.proto$" . protobuf-mode))

(use-package groovy-mode
  :mode (("\\.gradle$" . groovy-mode)
         ("\\.groovy$" . groovy-mode)))

(use-package ninja-mode
  :mode ("\\.ninja$" . ninja-mode))

(defconst +tr/mason-lsps '(basedpyright
                           bash-language-server
                           smithy-language-server
                           jdtls
                           java-debug-adapter
                           typescript-language-server
                           pyrefly))

;; Tools for installing LSP servers
(use-package mason
  :init
  (defun tr/install-lsps ()
    "Install required LSPs."
    (mason-ensure
     (lambda ()
       (dolist (lsp-name +tr/mason-lsps)
         (ignore-errors (mason-install (symbol-name lsp-name)))))))
  :functions (mason-ensure mason-install)
  :hook
  (after-init-hook . tr/install-lsps))

;; JDTLS replies with non-standard file URLs in some cases.  This code handles them.
;;
;; Taken from https://gitlab.com/skybert/my-little-friends/-/blob/master/emacs/.emacs.d/tkj-java-eglot.el
(defun tr/jdt-file-name-handler (operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir "/tmp/.eglot")
         (source-file
          (expand-file-name
           (file-name-concat
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(add-to-list 'file-name-handler-alist '("\\`jdt://" . tr/jdt-file-name-handler))

(defconst +tr/jdtls-path "jdtls"
  "The absolute path to the JDTLS language server binary.")

(defconst +tr/java-debug-plugin-path (expand-file-name "~/.emacs.d/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin_0.53.2.jar")
  "The absolute path to the compiled Java debug plugin (for dape).")

(defconst +tr/lombok-jar-path (expand-file-name "~/.emacs.d/lombok-1.18.38.jar")
  "The absolute path to the lombok jar.")

;; Set up eglot (which now ships with emacs)
(use-package eglot
  :straight (:type built-in)
  :init
  ;; Prevent the server from auto-updating buffer contents while typing
  (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  ;; Hide inlays, but in a way that they can be re-enabled more easily than ignoring the server
  ;; capability
  (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  :config
  ;; This is a suggestion floating around that ignores some JSON-RPC messages to improve efficiency
  (fset #'jsonrpc--log-event #'ignore)
  ;; Add a custom handler for Dafny programs to start up Dafny's built-in LSP server
  (add-to-list 'eglot-server-programs '(dafny-mode . ("dafny" "server")))

  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyrefly" "lsp")))

  (add-to-list 'eglot-server-programs '(smithy-mode . ("smithy-language-server")))

  (add-to-list 'eglot-server-programs '((nocaml-mode :language-id "ocaml") . ("ocamllsp")))

  (let ((lombok-arg (format "--jvm-arg=-javaagent:%s" +tr/lombok-jar-path))
        (zgc-arg "--jvm-arg=-XX:+UseZGC")
        (generational "--jvm-arg=-XX:+ZGenerational")
        (jdtls-bin +tr/jdtls-path)
        (init-opts `(:settings
                     (:java
                       (:contentProvider (:preferred "fernflower")))
                     :extendedClientCapabilities (:classFileContentsSupport t)
                     :bundles [,+tr/java-debug-plugin-path
                               ])))
    (add-to-list 'eglot-server-programs `((java-mode java-ts-mode) . (,jdtls-bin ,zgc-arg ,generational ,lombok-arg :initializationOptions ,init-opts))))

  ;; jdtls has a non-standard reply for workspace edit requests
  ;;
  ;; See: https://github.com/joaotavora/eglot/discussions/888
  (cl-defmethod eglot-execute-command
    (_server (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments))

  :commands (eglot eglot-inlay-hints-mode eglot-code-actions eglot-rename)
  :bind (("C-c e a" . eglot-code-actions)
         ("C-c e r" . eglot-rename)))

(use-package consult-eglot
  :bind ("C-c e s" . consult-eglot-symbols)
  :commands (consult-eglot-symbols))

(use-package consult-jq
  :straight (:host github :repo "bigbuger/consult-jq")
  :commands (consult-jq))

;; dape is a debug adapter interface that is compatible with eglot.
(use-package dape
  :commands (dape))

(use-package corfu
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :commands (global-corfu-mode)
  :hook (after-init . global-corfu-mode))

(use-package cape
  :commands (cape-dabbrev cape-file)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu-terminal
  :straight (:repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :commands (corfu-terminal-mode)
  :init
  (defun tr/enable-corfu-terminal ()
    "Enable corfu-terminal if running outside the GUI context."
    (unless (display-graphic-p)
      (corfu-terminal-mode +1)))
  :hook (after-init . tr/enable-corfu-terminal))

;; ** Markup modes

(use-package plantuml-mode
  :mode ("\\.plantuml$" . plantuml-mode))

(use-package rst
  :straight (:type built-in)
  :mode ("\\.rst$" . rst-mode)
  :init
  (add-hook 'rst-mode-hook #'visual-line-mode))

(use-package kdl-mode
  :straight (:host github :repo "bobuk/kdl-mode")
  :mode ("\\.kdl$" . kdl-mode))

(use-package markdown-mode
  :mode ("\\.markdown$\\|\\.md$" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  :defines (markdown-command markdown-fontify-code-blocks-natively)
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "comrak"))

(use-package yaml-mode
  :mode (("\\.ya?ml$" . yaml-mode)))

(use-package adoc-mode
  :mode ("\\.adoc$" . adoc-mode))

(use-package toc-org
  :after org
  :commands (toc-org-enable))

;; This is for the D2 declarative diagramming language
;;
;; See https://d2lang.com/
(use-package d2-mode
  :mode ("\\.d2$" . d2-mode))

(use-package graphql-mode
  :mode
  ("\\.graphql$" . graphql-mode)
  ("\\.gql$" . graphql-mode))

(use-package font-latex
  :straight (:type built-in)
  :defer t
  :config
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning 'color))

(use-package tex
  :straight (:type built-in)
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-source-correlate-start-server t))

;; (use-package auctex
;;   :ensure  (auctex :pre-build (("./autogen.sh")
;;                                ("./configure"
;;                                 "--without-texmf-dir"
;;                                 "--with-lispdir=.")
;;                                ("make")))
;;   :init
;;   (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
;;   (add-hook 'LaTeX-mode-hook #'visual-line-mode)
;;   (add-hook 'LaTeX-mode-hook
;;             #'(lambda ()
;;                 (custom-set-faces '(font-latex-slide-title-face ((t (:inherit font-lock-type-face)))))
;;                 (font-latex-update-sectioning-faces))))

;; ** Configuration modes

(use-package git-modes
  :mode (("^\\.gitignore$" . gitignore-mode)
         ("gitconfig$" . gitconfig-mode)))

(use-package ini-mode
  :mode ("\\.ini$" . ini-mode))

(use-package dockerfile-mode
  :mode ("Dockerfile$" . dockerfile-mode))

;; ** Major tools

;; Show the next possible keys in a key chord, after a key chord has been
;; started and after a delay.  Useful for learning new keybindings and figuring
;; out what is bound under a given prefix.
(use-package which-key
  :diminish
  :commands (which-key-mode)
  :hook (after-init . which-key-mode))

(use-package compile
  :straight (:type built-in)
  :defer
  :config
  ;; Scroll the compilation window as output is generated
  (setq compilation-scroll-output t))

(use-package cargo
  :commands (cargo-minor-mode)
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'toml-mode-hook #'cargo-minor-mode))

(use-package browse-url
  :straight (:type built-in)
  :commands (browse-url-generic)
  :if window-system
  :config
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "firefox"))

(use-package literate-calc-mode
  :commands (literate-calc-minor-mode literate-calc-eval-line literate-calc-eval-region literate-calc-eval-buffer))

(use-package ediff-wind
  :straight (:type built-in)
  :defines ediff-window-setup-function
  :functions ediff-setup-windows-plain
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package dired
  :straight (:type built-in)
  :commands
  (dired dired-jump)
  :init
  (with-eval-after-load 'dired (require 'dired-x))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t))

(use-package with-editor)

;; The ultimate git interface
(use-package magit
  :defines (magit-auto-revert-mode magit-auto-revert-immediately magit-diff-refine-hunk git-commit-major-mode git-commit-summary-max-length)
  :init
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-setup-hook #'visual-line-mode)
  :config
  ;; Edit it commit messages as if they were markdown
  (setq git-commit-major-mode 'markdown-mode)
  (setq git-commit-summary-max-length 500)
  (setq magit-auto-revert-mode nil)
  (setq magit-auto-revert-immediately nil)
  (setq magit-diff-refine-hunk 'all)
  :mode ("COMMIT_EDITMSG$" . git-commit-mode)
  :bind ("C-x g" . magit-status))

;; This package (and keybinding) generates a link to the current point in the
;; buffer (or selected range) on github
(use-package git-link
  :bind ("C-c g l" . git-link))

;; Allow loading very large files in an efficient way (i.e., on demand and
;; incrementally)
(use-package vlf
  :commands (vlf))

;; Local (and searchable) dev documentation
(use-package devdocs
  :commands (devdocs-lookup devdocs-peruse))

;; Improve clipboard interaction, especially in terminals.
;;
;; This uses the OSC 52 escape sequence to tell the terminal to sync the clipboard if running in a terminal
(use-package clipetty
  :diminish
  :hook (after-init . global-clipetty-mode))

;; Pop out the contents of comments to edit them as separate markdown documents.  This is most
;; useful for long-form design documentation comments.
(use-package separedit
  :bind ("C-c '" . separedit)
  :commands (separedit)
  :defines separedit-default-mode
  :config
  (setq separedit-default-mode 'markdown-mode))

(use-package tempel
  :bind ("M-=" . tempel-insert))

(use-package ligature
  :init
  (add-hook 'prog-mode-hook #'ligature-mode)
  :commands (ligature-mode)
  :functions ligature-set-ligatures
  :config
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<==>" "<!--"
                                       ":::" "::=" "=:" "===" "==>" "=!=" "=>>" "=/=" "!=="
                                       ">=>" ">=" ">->" "->>" "-->"
                                       "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "</>"
                                       "www"
                                       "/*" "*/"
                                       "::" ":=" "==" "=>" "!="
                                       ">=" "->"
                                       "<=" "<-" "</"
                                       ".=" ".-"
                                       ";;" "/>" "//" "__")))

(use-package ispell
  :straight (:type built-in)
  :defines (ispell-program-name ispell-dictionary)
  :config
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-dictionary "en_US"))

;; On-the-fly spell checking in various modes.  The prog-mode version spell
;; checks text appearing in comments and string literals.
(use-package jit-spell
  :diminish
  :commands (jit-spell-correct-word jit-spell-mode)
  :init
  (add-hook 'text-mode-hook #'jit-spell-mode)
  (add-hook 'rst-mode-hook #'jit-spell-mode)
  (add-hook 'prog-mode-hook #'jit-spell-mode)
  (add-hook 'mail-mode-hook #'jit-spell-mode))

;; Do some on-the-fly linting using flycheck and various checkers.
(use-package flycheck
  :commands (flycheck-mode)
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'python-mode-hook #'flycheck-mode)
  (add-hook 'python-ts-mode-hook #'flycheck-mode)
  (add-hook 'rst-mode-hook #'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
  (add-hook 'sql-mode-hook #'flycheck-mode)
  (add-hook 'LaTeX-mode-hook #'flycheck-mode)
  (add-hook 'yaml-mode-hook #'flycheck-mode)
  (add-hook 'json-mode-hook #'flycheck-mode)
  (add-hook 'bash-ts-mode-hook #'flycheck-mode)
  (add-hook 'sh-mode-hook #'flycheck-mode))

;; Visual undo (requires emacs-28)
(use-package vundo
  :commands (vundo)
  :bind (("C-/" . vundo)))

;; Convert a buffer into HTML, preserving syntax highlighting
(use-package htmlize
  :commands (htmlize))

;; Undo (and redo) hard line wraps
(use-package unfill
  :commands (unfill-toggle))

;; A mode for profiling emacs startup times
(use-package esup
  :commands (esup)
  :defines esup-depth
  :config (setq esup-depth 0))

;; ** Helpful minor modes

;; Highlight 'TODO' and 'FIXME' notes in buffers (as a minor mode)
(use-package hl-todo
  :commands (hl-todo-mode)
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'LaTeX-mode-hook #'hl-todo-mode))

;; Fix pasting into terminal emacs (i.e., paste text as an atomic unit instead
;; of a character at a time)
(use-package bracketed-paste
  :commands (bracketed-paste-enable)
  :hook (after-init . bracketed-paste-enable))


;; Add support for fancy quotes and a few other typographical niceties in plain
;; text modes
(use-package typo
  :diminish typo-mode
  :commands (typo-mode)
  :init
  (add-hook 'mail-mode-hook #'typo-mode))

;; ** Navigation

(use-package bufler
  :diminish
  :commands (bufler bufler-mode bufler-switch-buffer)
  :hook (after-init . bufler-mode))

;; Enable vertico (vertical completion)
(use-package vertico
  :commands (vertico-mode)
  :hook (after-init . vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight (:type built-in)
  :commands (savehist-mode)
  :hook (after-init . savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(substring orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))



(use-package consult
  ;; Replace bindings
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("C-r" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :commands (consult-annotate-mode consult-annotate-command consult-show-xrefs consult-ripgrep consult-xref consult-multi-occur)

  :defines consult-project-function

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur))

(use-package xref
  :straight (:type built-in)
  :config
  (setq xref-show-definitions-function #'consult-xref)
  (setq xref-show-xrefs-function #'consult-xref))

(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :commands (marginalia-mode)
  :hook (after-init . marginalia-mode))

(use-package helpful
  :commands (helpful-function helpful-variable helpful-at-point helpful-key))

;; A mode for jumping to definitions that works far better than it has any right
;; to.  Works on most languages worth mentioning, and then some.
(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :defines dumb-jump-force-searcher
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-force-searcher 'rg))

(use-package resize-window
  :bind (("C-c ;" . resize-window)))

;; An interesting package for building modal commands
(use-package hydra
  :config
  (defhydra hydra-zoom ()
    "zoom"
    ("+" text-scale-increase "in")
    ("-" text-scale-decrease "out")
    ("0" (text-scale-adjust 0) "reset")
    ("q" nil "quit" :color red))

  (defhydra hydra-move ()
    "move"
    ("n" next-line "next-line")
    ("p" previous-line "prev-line")
    ("f" forward-char "next-char")
    ("b" backward-char "prev-char")
    ("s" forward-word "next-word")
    ("r" backward-word "prev-word")
    ("a" beginning-of-line "home")
    ("e" move-end-of-line "end")
    ("m" tr/forward-midpoint "next-mid")
    ("M" tr/backward-midpoint "prev-mid")
    ("v" scroll-up-command "up")
    ;; Converting M-v to V here by analogy.
    ("V" scroll-down-command "down")
    ("w" copy-region-as-kill "copy")
    ("W" kill-region "kill")
    ;; ("l" recenter-top-bottom)
    ("q" nil "quit" :color red))

  (defun hydra-mark ()
    "A wrapper around hydra-move that sets the mark"
    (interactive)
    (set-mark (point))
    (hydra-move/body))

  (global-set-key (kbd "C--") #'hydra-zoom/body)
  (global-set-key (kbd "C-+") #'hydra-zoom/body)
  (global-set-key (kbd "C-<SPC>") #'hydra-mark)
  (global-set-key (kbd "C-c C-m") #'hydra-move/body)
  (global-set-key (kbd "C-c m") #'hydra-move/body))

;; Very fast and precise navigation based on short substrings.
;;
;; Press the key stroke to start the mode, type a few characters, then type the
;; value displayed on the hint you want the cursor to jump to.
(use-package avy
  :defines (avy-dispatch-alist avy-all-windows)
  :commands (avy-goto-char-timer avy-process)
  :config
  (setf (alist-get ?g avy-dispatch-alist) 'avy-action-goto)
  (setq avy-all-windows nil))

;; A more visual interface to querying and replacing
(use-package anzu
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)))

;; Visual window switching instead of cycling through windows
(use-package ace-window
  :bind
  (("C-x o" . ace-window)))

(use-package ialign
  :bind
  (("C-x l" . ialign)))

;; Only clean up whitespace of changed lines
(use-package ws-butler
  :diminish
  :commands (ws-butler-mode)
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;; * Tree sitter

(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (java "https://github.com/tree-sitter/tree-sitter-java" "master" "src")
    (javadoc "https://github.com/rmuir/tree-sitter-javadoc" "main" "src")
    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun tr/install-treesitter-grammars ()
  "Install all of the defined tree-sitter grammars."
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang)))))

;; Install tree-sitter modes if needed
(use-package treesit
  :straight (:type built-in)
  :hook (after-init . tr/install-treesitter-grammars)
  :config
  ;; This special setting method is required for the font lock level
  (customize-set-variable 'treesit-font-lock-level 4)
  ;; Use the remap functionality for a few modes that are difficult to directly configure. The
  ;; script-centric ones are especially tricky due to the interpreter mode hooks.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode))))

(defun tr/java-indent-style ()
  "Override the built-in Java indentation rules."
  `((java
     ;; Avoid double-indenting the body of a lambda relative to the previous
     ;; line. The second rule cleans up the closing brace.
     ((match "block$" "lambda_expression") standalone-parent 0)
     ((node-is "}") standalone-parent 0)
     ((parent-is "block$") standalone-parent 4)

    ;; Place the base rules after the overrides
   ,@(alist-get 'java java-ts-mode--indent-rules))))


(defun tr/add-javadoc-in-java-ts-mode ()
  "Add javadoc syntax highlighting to java-ts-mode."
  (when (treesit-ready-p 'java)
    (when (treesit-ready-p 'javadoc)
        (setq-local treesit-range-settings
                    (treesit-range-rules
                      :embed 'javadoc
                      :host 'java
                      :local t
                      `(((block_comment) @capture (:match ,(rx bos "/**") @capture)))))
        (setq c-ts-common--comment-regexp (rx (or "description" "line_comment" "block_comment")))
        (add-to-list 'jit-spell-ignored-faces 'font-lock-delimiter-face 'append)
        (defvar tr/treesit-font-lock-settings-javadoc
          (treesit-font-lock-rules
           :language 'javadoc
           :override t
           :feature 'document
           '((document) @font-lock-doc-face)

           :language 'javadoc
           :override t
           :feature 'keyword
           '((tag_name) @font-lock-escape-face)

           :language 'javadoc
           :override t
           :feature 'definition
           '((param_tag parameter_name: (identifier) @font-lock-punctuation-face))

           :language 'javadoc
           :override t
           :feature 'definition
           '((identifier) @font-lock-variable-face)

           ;; Assign some special faces to Javadoc elements that should not be spell checked.  This
           ;; works in conjunction with the jit-spell-ignored-faces change above, which marks this
           ;; face as exempt from spell checking.
           ;;
           ;; These are elements that refer to method references, which are rarely words.
           :language 'javadoc
           :override t
           :feature 'document
           '((link_tag reference: (reference) @font-lock-delimiter-face))

           :language 'javadoc
           :override t
           :feature 'document
           '((code_tag text: (code) @font-lock-delimiter-face))

           :language 'javadoc
           :override t
           :feature 'document
           '((see_tag reference: (reference) @font-lock-delimiter-face))))
        (setq-local treesit-font-lock-settings (append treesit-font-lock-settings tr/treesit-font-lock-settings-javadoc)))))

(defun tr/init-java-ts-mode ()
  "This hook just sets up the java indent style."
  (setq-local treesit-simple-indent-rules (tr/java-indent-style)))

(use-package c-ts-mode
  :straight (:type built-in)
  :mode ("\\.c$" . c-ts-mode))

;; Configure Java treesitter mode; this declaration tells straight to not install
;; the mode, as tree-sitter is built-in.
(use-package java-ts-mode
  :straight (:type built-in)
  :mode ("\\.java$" . java-ts-mode)
  :init
  (add-hook 'java-ts-mode-hook #'tr/init-java-ts-mode)
  (add-hook 'java-ts-mode-hook #'tr/add-javadoc-in-java-ts-mode)
  (add-hook 'java-ts-mode-hook #'(lambda () (setq paragraph-separate "[ ]*\\(//+\\|\\**\\)\\([ ]*\\| <.*>\\)$\\|^\f"))))

(use-package bash-ts-mode
  :straight (:type built-in)
  :mode (("\\.bash$" . bash-ts-mode)
         ("\\.sh$" . bash-ts-mode)))

(defun selective-text-flyspell ()
  (interactive)
  (let ((no-flyspell-modes '(help-mode magit-status-mode)))
    (cond ((member major-mode no-flyspell-modes) (flyspell-mode 0))
          (t (flyspell-mode 1)))))

(defun llvmize (&optional start end)
  "Convert the current buffer or region (from START to END) containing C code to LLVM assembly via clang and opt."
  (interactive)
  (let ((start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max)))
        (major-mode 'llvm-mode)
        (buf (generate-new-buffer "*llvm-asm*")))
    (set-buffer-major-mode buf)
    (shell-command-on-region start end "clang -emit-llvm -x c -c -o - - | opt -S -mem2reg -basicaa -gvn" buf)
    (set-buffer buf)
    (setq buffer-read-only t)
    (switch-to-buffer-other-window buf)))

(defun tr/backward-midpoint ( )
  "Move backward to midpoint between current position and beginning of line."
  (interactive)
  (backward-char (/ (- (point) (line-beginning-position)) 2)))

(defun tr/forward-midpoint ( )
  "Move forward to midpoint between current position and end of line."
  (interactive)
  (forward-char (/ (- (line-end-position) (point)) 2)))

(defun tr/close-compilation-window ()
  "Destroy the compilation window, if there is one."
  (interactive)
  (declare (interactive-only))
  (delete-windows-on "*compilation*"))

(defun tr/avy-prompt (s)
  "Prompt for an avy target and activate avy with it."
  (interactive "MAvy: ")
  (avy-process (avy--regex-candidates s)))

;;; Smart narrow/widen
(defun tr/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(defun tr/comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line."
  (interactive)
  (if (use-region-p)
      (progn
        (comment-dwim nil)
        (flyspell-delete-region-overlays (region-beginning) (region-end)))
    (if (eq (point) (line-end-position))
        (progn
          (comment-dwim nil)
          (flyspell-delete-region-overlays (line-beginning-position) (line-end-position)))
      (if (fboundp 'comment-line)
          (comment-line 1)
        (progn
          (comment-or-uncomment-region (line-beginning-position) (line-end-position))
          (flyspell-delete-region-overlays (line-beginning-position) (line-end-position))
          (forward-line ))))))

(use-package haskell-pragma
  :straight (:host github :repo "travitch/haskell-pragma.el")
  :commands (hydra-haskell-pragma/body))

(use-package haskell-interactive-import
  :straight (:host github :repo "travitch/haskell-interactive-import.el")
  :commands (haskell-interactive-import-begin))

;; * Keybindings

;; ** Custom keybindings

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c C-c") 'compile)
;; (global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x r i")  'string-insert-rectangle)
;; Don't prompt for the buffer to kill - I always mean the current one
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "M-*") 'pop-tag-mark)

(global-set-key (kbd "M-;") 'tr/comment-dwim)
(global-set-key [remap fill-paragraph] #'unfill-toggle)
(global-set-key [remap org-fill-paragraph] #'unfill-toggle)
(global-set-key (kbd "C-x n") 'tr/narrow-or-widen-dwim)
(global-set-key (kbd "C-c k") 'tr/close-compilation-window)
(global-set-key (kbd "C-c C-k") 'tr/close-compilation-window)
(global-set-key (kbd "M-`") 'tr/avy-prompt)

;; ** Disable annoying keys

;; This key is "suspend-frame", which is close to "save-file" and
;; just blocks all input. arrgggg
(global-unset-key "\C-x\C-z")

(global-unset-key (kbd "C-h n"))
(global-unset-key (kbd "C-h h"))
(global-unset-key (kbd "C-h C-a"))
(global-unset-key (kbd "C-h C-c"))
(global-unset-key (kbd "C-h C-f"))
(global-unset-key (kbd "C-h RET"))
(global-unset-key (kbd "C-h C-a"))
(global-unset-key (kbd "C-h C-o"))
(global-unset-key (kbd "C-h C-n"))
(global-unset-key (kbd "C-h C-h"))
(global-unset-key (kbd "C-h C-p"))
(global-unset-key (kbd "C-h C-t"))
(global-unset-key (kbd "C-h C-w"))
(global-unset-key (kbd "C-h ?"))
(global-unset-key (kbd "C-h t"))
(global-unset-key (kbd "C-x c"))
;; Deletes to the end of the paragraph, which is never useful
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-<"))
(global-unset-key (kbd "M->"))
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-="))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-z"))
(global-unset-key (kbd "M-{"))
(global-unset-key (kbd "M-}"))

;; Unset the arrow keys because I accidentally hit them on the kinesis keyboard
;; sometimes, and it is very annoying.
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

(defun tr/compute-font-size (frame)
  "Inspired by https://emacs.stackexchange.com/a/44930/17066. FRAME is ignored.
If I let Windows handle DPI everything looks blurry."
  ;; Using display names is unreliable...switched to checking the resolution
  (let* ((attrs (frame-monitor-attributes)) ;; gets attribs for current frame
         (width-mm (nth 1 (nth 3 attrs)))
         (width-px (nth 3 (nth 2 attrs)))
         (size nil)) ;; default size
    (cond ((eq width-px 1920) (setq size 10))
          ((eq width-px 2560) (setq size 14))
          (t (setq size 14)))
    (set-frame-font (font-spec :family "MonoLisa" :size size ))))
    ;; (set-frame-font (format "MonoLisa CF %s" size))))

(when (display-graphic-p)
  (add-hook 'window-size-change-functions #'tr/compute-font-size))

(load "~/.emacs.d/local" 'noerror)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0592f1b03ba5d7d8cfade2ce3a23db4c0d5f9926c9ae918dd740739dd95b697c"
     "c3076fdee603e9768817cfe8dbf6253d5b3cf3bf4602cb32fa2f1df62fe70b1c"
     "5e39e95c703e17a743fb05a132d727aa1d69d9d2c9cde9353f5350e545c793d4"
     "6819104c5f7d70485b32c10323aa396806d282fcee5b707e462bf3d156f44c39"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "c038d994d271ebf2d50fa76db7ed0f288f17b9ad01b425efec09519fa873af53"
     "fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c"
     "712dda0818312c175a60d94ba676b404fc815f8c7e6c080c9b4061596c60a1db"
     "99d1e29934b9e712651d29735dd8dcd431a651dfbe039df158aa973461af003e"
     "c1638a7061fb86be5b4347c11ccf274354c5998d52e6d8386e997b862773d1d2" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:
