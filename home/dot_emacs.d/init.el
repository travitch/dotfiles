;; * Configuration
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Disable some unneeded UI elements
(when (display-graphic-p)
  (tooltip-mode -1))

;; ** Core Behavior

;; Suppress annoying backup files
(setq make-backup-files nil)

;; Set up authentication sources (used for forge)
(setq auth-sources '((:source "~/.emacs.d/authinfo.gpg")))
(setq auth-source-debug t)


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

;; ** Filling parameters
;;
;; Set up some bits to try to make paragraph filling  behave a bit better.

;; Try to make auto-fill work a bit better in the context of bulleted lists.
(setq paragraph-start "\f\\|>*[ \t]*$\\|>*[ \t]*[-+*] \\|>*[ \t]*[0-9#]+\\. ")
(setq paragraph-separate "[ ]*\\(//+\\|\\**\\)\\([ ]*\\| <.*>\\)$\\|^\f")

(setq-default fill-column 100)


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

;; ** Buffer-local variables
;;
;; Set up defaults for values that are buffer local (using setq-default instead
;; of just setq)

;;; Set some buffer-local variables to sensible defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines t)
(setq-default require-final-newline t)

;; * Package system setup
;;
;; Currently using elpaca

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq use-package-always-ensure t))

;; Block until current queue processed.
(elpaca-wait)

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (add-hook 'org-mode-hook #'toc-org-enable)
  (add-hook 'org-mode-hook #'visual-line-mode)
  :config
  (setq org-log-done t))

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
  :ensure nil
  :config
  ;; Be a bit less noisy with async native compilation warnings
  (setq native-comp-async-report-warnings-errors nil))

(defun tr/show-trailing-whitespace ()
  "This is a trivial wrapper function around setting a value that can be used in a hook."
  (setq show-trailing-whitespace t))

(use-package text-mode
  :ensure nil
  :preface (provide 'text-mode)
  :init
  (add-hook 'text-mode-hook #'tr/show-trailing-whitespace))

(use-package prog-mode
  :ensure nil
  :preface (provide 'prog-mode)
  :init
  (add-hook 'prog-mode-hook #'tr/show-trailing-whitespace))

(use-package conf-mode
  :ensure nil
  :preface (provide 'conf-mode)
  :init
  (add-hook 'conf-mode-hook #'tr/show-trailing-whitespace))

;; Set up some handling of mail mode (used through mutt).  mail-mode is provided
;; by the sendmail library built-in to emacs, but we want to add some hooks here.
(use-package sendmail
  :ensure nil
  :mode ("/tmp/mutt.*" . mail-mode)
  :init
  (add-hook 'mail-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'mail-mode-hook #'mail-text))

(use-package abbrev
  :diminish abbrev-mode
  :ensure nil
  :config
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)))

;; Give unique tags to buffers that have the same name to distinguish them.
(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style #'post-forward-angle-brackets))

;; ** Themes

;; Use solarized light, and turn down some of the more aggressive font choices
;; that (especially a few that are proportional).
;; (use-package solarized-theme
;;   :hook (elpaca-after-init . (lambda () (load-theme 'solarized-light t)))
;;   :config
;;   (setq solarized-use-variable-pitch nil)
;;   (setq solarized-scale-org-headlines nil))

(use-package modus-themes
  :hook (elpaca-after-init . (lambda () (load-theme 'modus-operandi-tinted))))

;; This is a modeline replacement that is a bit cleaner while still being lightweight (compared to e.g., spaceline)
(use-package simple-modeline
  :hook (elpaca-after-init . simple-modeline-mode))

;; This mode provides a function that enables a server running from emacs that
;; can edit text boxes in browsers using an appropriate extension (see Ghost Text)
(use-package atomic-chrome
  :commands (atomic-chrome-start-server))

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
  :ensure nil
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
  :ensure nil
  :mode (("\\.ts$" . typescript-ts-mode)
         ("\\.js$" . typescript-ts-mode)))

(use-package erlang
  :mode ("\\.erl$" . erlang-mode))

(use-package clojure-mode
  :mode ("\\.clj$" . clojure-mode))

(use-package go-ts-mode
  :ensure nil
  :mode ("\\.go$" . go-ts-mode))

(use-package scala-mode
  :mode ("\\.sc$\\|\\.scala$" . scala-mode))

(use-package julia-mode
  :mode ("\\.jl$" . julia-mode))

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

(use-package kotlin-mode
  :mode ("\\.kt$" . kotlin-mode))

;; The best ocaml mode
(use-package tuareg
  :mode ("\\.ml$\\|\\.mli$" . tuareg-mode))

(use-package vimrc-mode
  :mode ("vimrc" . vimrc-mode))

(use-package nix-mode
  :mode ("\\.nix$" . nix-mode))

(use-package idris-mode
  :mode ("\\.idr$" . idris-mode))

(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs$" . rust-ts-mode))

(use-package forth-mode
  :mode ("\\.fth$" . forth-mode))

(use-package python-ts-mode
  :ensure nil
  :config
  (setq python-shell-interpreter "ipython3")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  :mode (("\\.py$" . python-ts-mode)
         ("SConstruct$" . python-ts-mode)
         ("SConscript$" . python-ts-mode)))

(use-package fish-mode
  :mode ("\\.fish$" . fish-mode))

(use-package lean-mode
  :mode ("\\.lean$" . lean-mode))

(use-package boogie-friends
  :ensure (boogie-friends :host github :repo "travitch/boogie-friends" :branch "tr/new-lsp-versions")
  :config
  (setq lsp-dafny-preferred-version "4.3.0")
  :mode (("\\.bpl$" . boogie-mode)
         ("\\.dfy$" . dafny-mode)))

(use-package z3-mode
  :mode ("\\.smt$" . z3-mode))

(use-package matlab-mode
  :mode ("\\.m$" . matlab-mode))

(use-package jsonian
  :mode ("\\.json$" . jsonian-mode))

(use-package csv-mode
  :mode ("\\.csv$" . csv-mode))

(use-package protobuf-mode
  :mode ("\\.proto$" . protobuf-mode))

;; A newer jsonrpc is required for dape
(use-package jsonrpc)

;; Use the built-in eglot (upgrading is kind of challenging)
(use-package eglot
  :ensure nil
  :commands (eglot))

(use-package eglot-java
  :config
  (setq eglot-java-eclipse-jdt-args (cons (format "-javaagent:%s" (expand-file-name "~/.emacs.d/lombok-1.18.30.jar")) eglot-java-eclipse-jdt-args))
  :commands (eglot-java-mode))

(use-package dape
  :commands (dape))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq read-process-output-max (* 1024 1024))
  ;; Hack around some builds of emacs not having all image formats
  (when (boundp 'image-types)
    (add-to-list 'image-types 'gif)
    (add-to-list 'image-types 'svg))
  :config


  ;; Override the default version of this to display in the echo area instead of an annoying view
  ;; buffer
  (defun lsp-describe-thing-at-point ()
    "Display the full documentation of the thing at point."
    (interactive)
    (let ((contents (->> (lsp--text-document-position-params)
                         (lsp--make-request "textDocument/hover")
                         (lsp--send-request)
                         (gethash "contents"))))
      (eldoc-minibuffer-message "%s" (lsp--render-on-hover-content contents t))))

  (setq lsp-log-io nil)
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-eldoc-render-all t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-enable-file-watchers t)
  (add-to-list 'lsp-file-watch-ignored-directories "build$")
  (setq lsp-inlay-hint-enable nil)
  (setq lsp-signature-render-documentation t)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-idle-delay 2)
  ;; Disable the completion; it is too aggressive
  (setq lsp-completion-provider :none)


  :commands (lsp))

(use-package dap-mode
  :after lsp-mode
  :commands (dap-hydra dap-auto-configure-mode)
  :init
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (dap-auto-configure-mode))

(use-package lsp-java
  :commands (dap-java-debug lsp-java-organize-inputs)
  :config
  (setq lsp-java-vmargs (append lsp-java-vmargs
                                `(,(format "-javaagent:%s" (expand-file-name "~/.emacs.d/lombok-1.18.30.jar")))))
  (setq lsp-java-signature-help-enabled t))

(use-package groovy-mode
  :mode (("\\.gradle$" . groovy-mode)
         ("\\.groovy$" . groovy-mode)))

(use-package ninja-mode
  :mode ("\\.ninja$" . ninja-mode))

;; ** Markup modes

(use-package plantuml-mode
  :mode ("\\.plantuml$" . plantuml-mode))

(use-package rst
  :ensure nil
  :mode ("\\.rst$" . rst-mode)
  :init
  (add-hook 'rst-mode-hook #'visual-line-mode))

(use-package markdown-mode
  :mode ("\\.markdown$\\|\\.md$" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  :config
  (setq markdown-command "pandoc"))

(use-package yaml-mode
  :mode (("\\.ya?ml$" . yaml-mode)))

(use-package adoc-mode
  :mode ("\\.adoc$" . adoc-mode))

(use-package toc-org
  :after org
  :commands (toc-org-enable))

(use-package d2-mode
  :mode ("\\.d2$" . d2-mode))

(use-package graphql-mode
  :mode
  ("\\.graphql$" . graphql-mode)
  ("\\.gql$" . graphql-mode))

(use-package font-latex
  :ensure nil
  :defer t
  :config
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning 'color))

(use-package tex
  :ensure nil
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


;; Highlighting for git commit messages
(use-package git-commit
  :commands (git-commit-turn-on-flyspell)
  :mode ("COMMIT_EDITMSG$" . git-commit-mode))

;; ** Configuration modes

(use-package git-modes
  :mode (("^\\.gitignore$" . gitignore-mode)
         ("gitconfig$" . gitconfig-mode)))

(use-package ini-mode
  :mode ("\\.ini$" . ini-mode))

(use-package dockerfile-mode
  :mode ("Dockerfile$" . dockerfile-mode))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :commands (turn-on-eldoc-mode)
  :hook (elpaca-after-init . turn-on-eldoc-mode))

;; ** Major tools

;; Show the next possible keys in a key chord, after a key chord has been
;; started and after a delay.  Useful for learning new keybindings and figuring
;; out what is bound under a given prefix.
(use-package which-key
  :diminish
  :commands (which-key-mode)
  :hook (elpaca-after-init . which-key-mode))

(use-package compile
  :ensure nil
  :defer
  :config
  ;; Scroll the compilation window as output is generated
  (setq compilation-scroll-output t))

(use-package cargo
  :commands (cargo-minor-mode)
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'toml-mode-hook #'cargo-minor-mode))

(use-package flycheck-rust
  :commands (flycheck-rust-setup)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package browse-url
  :ensure nil
  :commands (browse-url-generic)
  :if window-system
  :config
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "xdg-open"))

;; The ultimate git interface
(use-package magit
  :config
  ;; Disable vc-mode (using magit)
  (setq vc-handled-backends nil)
  (setq magit-auto-revert-mode nil)
  (setq magit-auto-revert-immediately nil)
  (setq magit-diff-refine-hunk t)
  ;; Edit it commit messages as if they were markdown
  (setq git-commit-major-mode 'markdown-mode)
  (setq git-commit-summary-max-length 500)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  :init
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-mode-hook #'visual-line-mode)
  :bind ("C-x g" . magit-status))


;; This package (and keybinding) generates a link to the current point in the
;; buffer (or selected range) on github
(use-package git-link
  :bind ("C-c g l" . git-link))

;; Allow loading very large files in an efficient way (i.e., on demand and
;; incrementally)
(use-package vlf
  :commands (vlf))

;; Improve clipboard interaction, especially in terminals.
;;
;; This uses the OSC 52 escape sequence to tell the terminal to sync the clipboard if running in a terminal
(use-package clipetty
  :ensure t
  :hook (elpaca-after-init . global-clipetty-mode))

;; Hide some minor mode indicators in the modeline
(use-package diminish)

(use-package ligature
  :init
  (add-hook 'prog-mode-hook #'ligature-mode)
  :commands (ligature-mode)
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

;; On-the-fly spell checking in various modes.  The prog-mode version spell
;; checks text appearing in comments and string literals.
(use-package jit-spell
  :commands (jit-spell-correct-word jit-spell-mode)
  :config
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_US")
  :init
  (add-hook 'text-mode-hook #'jit-spell-mode)
  (add-hook 'rst-mode-hook #'jit-spell-mode)
  (add-hook 'prog-mode-hook #'jit-spell-mode)
  (add-hook 'mail-mode-hook #'jit-spell-mode))

;; Do some on-the-fly linting using flycheck and various checkers.
(use-package flycheck
  :commands (flycheck-mode)
  :init
  (add-hook 'python-mode-hook #'flycheck-mode)
  (add-hook 'python-ts-mode-hook #'flycheck-mode)
  (add-hook 'rst-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-ts-mode-hook #'flycheck-mode)
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

(use-package unfill
  :commands (unfill-toggle))

;; A mode for profiling emacs startup times
(use-package esup
  :commands (esup)
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
  :hook (elpaca-after-init . bracketed-paste-enable))


;; Add support for fancy quotes and a few other typographical niceties in plain
;; text modes
(use-package typo
  :diminish typo-mode
  :commands (typo-mode)
  :init
  (add-hook 'mail-mode-hook #'typo-mode))

;; ** Navigation

(use-package bufler
  :commands (bufler bufler-mode bufler-switch-buffer)
  :hook (elpaca-after-init . bufler-mode))

;; Enable vertico
(use-package vertico
  :commands (vertico-mode)
  :hook (elpaca-after-init . vertico-mode)

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
  :ensure nil
  :commands (savehist-mode)
  :hook (elpaca-after-init . savehist-mode))

(use-package emacs
  :ensure nil
  :init
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

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

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

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)
  (setq xref-show-definitions-function #'consult-xref)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq consult-project-function #'projectile-project-root))

(use-package marginalia
  ;; The :init configuration is always executed (Not lazy!)
  :commands (marginalia-mode)
  :hook (elpaca-after-init . marginalia-mode))

(use-package affe
  :after orderless
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package helpful
  :commands (helpful-function helpful-variable helpful-at-point helpful-key))

(use-package poporg
  :bind
  ("C-c \"" . poporg-dwim)
  :commands (poporg-dwim))

;; A mode for jumping to definitions that works far better than it has any right
;; to.  Works on most languages worth mentioning, and then some.
(use-package dumb-jump
  :commands (dumb-jump-xref-activate)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-force-searcher 'rg))

(use-package resize-window
  :bind (("C-c ;" . resize-window)))

(use-package dogears
  :commands (dogears-list dogears-mode)
  :hook (elpaca-after-init . dogears-mode)
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)))

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

;; Project management
(use-package projectile
  :diminish projectile-mode
  :bind (("C-c p p" . projectile-switch-project))
  :commands (projectile-mode projectile-project-root)
  :hook (elpaca-after-init . projectile-mode)
  :config
  (setq projectile-keymap-prefix  (kbd "C-c p"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p s r") 'consult-ripgrep)
  (add-to-list 'projectile-globally-ignored-directories ".stack-work")
  (add-to-list 'projectile-globally-ignored-directories ".cabal-sandbox")
  (add-to-list 'projectile-globally-ignored-directories "dist-newstyle")
  (setq projectile-git-command "git ls-files -zc --exclude-standard --recurse-submodules")
  (setq projectile-completion-system 'default)
  (setq projectile-enable-caching t)
  (setq projectile-tags-backend 'etags)
  (setq projectile-globally-ignored-file-suffixes '("o" "hi" "a" "so" "p_o"
                                                    "jpg" "png" "JPG" "PANG" "PEG" "Peg")))

(use-package eacl
  :commands (eacl-complete-line)
  :config
  ;; Use projectile to get the root, if possible.  This has many more heuristics
  ;; and can do a better job
  (setq eacl-project-root-callback #'(lambda () (if (boundp 'projectile-project-root)
                                                    (projectile-project-root)
                                                  (eacl-get-project-root))))
  ;; Override this eacl function to force it to never use git-grep (which doesn't grep
  ;; in submodules)
  (defun eacl-git-p () nil))

;; Very fast and precise navigation based on short substrings.
;;
;; Press the key stroke to start the mode, type a few characters, then type the
;; value displayed on the hint you want the cursor to jump to.
(use-package avy
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

;; ** Completion

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :commands (global-corfu-mode)
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :hook (elpaca-after-init . global-corfu-mode))

(use-package cape
  :commands (cape-dabbrev cape-file)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(defun tr/enable-corfu-terminal ()
  "Enable corfu-terminal if running outside the GUI context."
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package corfu-terminal
  :ensure (corfu-terminal :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :commands (corfu-terminal-mode)
  :hook (elpaca-after-init . tr/enable-corfu-terminal))

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
  :ensure nil
  :hook (elpaca-after-init . tr/install-treesitter-grammars)
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

(defun tr/init-java-ts-mode ()
  "This hook just sets up the java indent style."
  (setq-local treesit-simple-indent-rules (tr/java-indent-style)))

(use-package c-ts-mode
  :ensure nil
  :mode ("\\.c$" . c-ts-mode))

;; Configure Java treesitter mode; this declaration tells elpaca to not install
;; the mode, as tree-sitter is built-in.
(use-package java-ts-mode
  :ensure nil
  :mode ("\\.java$" . java-ts-mode)
  :bind (:map java-ts-mode-map
              ("C-c i" . java-imports-add-import-dwim))
  :init
  (add-hook 'java-ts-mode-hook #'tr/init-java-ts-mode)
  (add-hook 'java-ts-mode-hook #'(lambda () (setq paragraph-separate "[ ]*\\(//+\\|\\**\\)\\([ ]*\\| <.*>\\)$\\|^\f")))
  (add-hook 'java-ts-mode-hook #'java-imports-scan-file))

;; A utility for adding and sorting Java imports, with a local cache
;; of class names to packages
(use-package java-imports
  :commands (java-imports-add-import-dwim java-imports-scan-file)
  :config
  (setq java-imports-save-buffer-after-import-added nil)
  (setq java-imports-find-block-function #'java-imports-find-place-sorted-block))

(use-package bash-ts-mode
  :ensure nil
  :mode (("\\.bash$" . bash-ts-mode)
         ("\\.sh$" . bash-ts-mode)))

(defun selective-text-flyspell ()
  (interactive)
  (let ((no-flyspell-modes '(help-mode magit-status-mode)))
    (cond ((member major-mode no-flyspell-modes) (flyspell-mode 0))
          (t (flyspell-mode 1)))))

(defun llvmize (&optional start end)
  "Convert the current buffer or region (containing C code) to LLVM assembly via clang and opt."
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
  :ensure (haskell-pragma :host github :repo "travitch/haskell-pragma.el")
  :commands (hydra-haskell-pragma/body))

(use-package haskell-interactive-import
  :ensure (haskell-interactive-import :host github :repo "travitch/haskell-interactive-import.el")
  :commands (haskell-interactive-import-begin))

;; * Keybindings

;; ** Custom keybindings

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c C-c") 'compile)
;; (global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x r i")  'string-insert-rectangle)
;; Don't prompt for the buffer to kill - I always mean the current one
(global-set-key (kbd "C-x k") 'kill-this-buffer)
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

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:
