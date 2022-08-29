;; * Configuration
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Disable some unneeded UI elements
(when (display-graphic-p)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(menu-bar-mode -1)

;; Be a bit less noisy with async native compilation warnings
(setq comp-async-report-warnings-errors nil)

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
(setq paragraph-separate "$")
(setq-default fill-column 80)


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
;; Currently using straight.el (with use-package compatibility)

(setq straight-check-for-modifications '(check-on-save))
(setq straight-cache-autoloads t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set up use-package so that we can use that to specify package setup below
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Next, we need a special hack to install org-mode because it has a complicated
;; setup that straight.el can't handle yet.
(require 'subr-x)

(straight-use-package 'git)

(use-package org
  :mode ("\\.org$" . org-mode)
  :init
  (setq org-log-done t)
  (add-hook 'org-mode-hook 'toc-org-enable)
  (add-hook 'org-mode-hook 'visual-line-mode))

;; * Package selection and initialization

;; ** Built-in modes
;;
;; This section contains customizations of built-in modes that don't actually
;; need to be installed.  It is set up through use-package so that
;; customizations are grouped by the relevant modes.

(defun tr/show-trailing-whitespace ()
  "This is a trivial wrapper function around setting a value that can be used in a hook."
  (setq show-trailing-whitespace t))

(use-package text-mode
  :straight nil
  :preface (provide 'text-mode)
  :init
  (add-hook 'text-mode-hook 'tr/show-trailing-whitespace))

(use-package prog-mode
  :straight nil
  :preface (provide 'prog-mode)
  :init
  (add-hook 'prog-mode-hook 'tr/show-trailing-whitespace))

(use-package conf-mode
  :straight nil
  :preface (provide 'conf-mode)
  :init
  (add-hook 'conf-mode-hook 'tr/show-trailing-whitespace))

;; Set up some handling of mail mode (used through mutt).  mail-mode is provided
;; by the sendmail library built-in to emacs, but we want to add some hooks here.
(use-package sendmail
  :straight nil
  :mode ("/tmp/mutt.*" . mail-mode)
  :config
  (add-hook 'mail-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'mail-mode-hook 'mail-text))

(use-package abbrev
  :diminish abbrev-mode
  :straight nil
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;; Give unique tags to buffers that have the same name to distinguish them.
(use-package uniquify
  :straight nil
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; ** Themes

(use-package zenburn-theme
  :disabled)

;; Use solarized light, and turn down some of the more aggressive font choices
;; that (especially a few that are proportional).
(use-package solarized-theme
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (load-theme 'solarized-light t))

;; This is a modeline replacement that is a bit cleaner while still being lightweight (compared to e.g., spaceline)
(use-package simple-modeline
  :hook (after-init . simple-modeline-mode))

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

;; Use the Superior Lisp Interation ModE for Common Lisp
(use-package slime
  :commands (slime)
  :init
  (setq slime-contribs '(slime-fancy)))

(use-package cmake-mode
  :mode ("CMakeLists.txt$" . cmake-mode))

(use-package csharp-mode
  :mode ("\\.cs$" . csharp-mode))

(use-package fsharp-mode
  :mode ("\\.fs$" . fsharp-mode))

(use-package qml-mode
  :mode ("\\.qml$" . qml-mode))

;; A mode for mixed HTML + JS + CSS
(use-package web-mode
  :mode ("\\.html$" . web-mode))

;; The most reasonable mode for javascript
(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :init
  (setq-default js2-basic-offset 2))

(use-package typescript-mode
  :mode ("\\.ts$" . typescript-mode))

(use-package clojure-mode
  :mode ("\\.clj$" . clojure-mode))

(use-package go-mode
  :mode ("\\.go$" . go-mode))

(use-package scala-mode
  :mode ("\\.sc$\\|\\.scala$" . scala-mode))

(use-package julia-mode
  :mode ("\\.jl$" . julia-mode))

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode))

;; The best ocaml mode
(use-package tuareg
  :mode ("\\.ml$\\|\\.mli$" . tuareg-mode))

(use-package vimrc-mode
  :mode ("vimrc" . vimrc-mode))

(use-package nix-mode
  :mode ("\\.nix$" . nix-mode))

(use-package idris-mode
  :mode ("\\.idr$" . idris-mode))

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode))

(use-package fstar-mode
  :mode ("\\.fsi$" . fstar-mode))

(use-package forth-mode
  :mode ("\\.fth$" . forth-mode))

(use-package python
  :init
  (setq python-shell-interpreter "ipython3")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  :mode (("\\.py$" . python-mode)
         ("SConstruct$" . python-mode)
         ("SConscript$" . python-mode)))

(use-package lean-mode
  :mode ("\\.lean$" . lean-mode))

(use-package boogie-friends
  :mode (("\\.bpl$" . boogie-mode)
         ("\\.dfy$" . dafny-mode)))

(use-package z3-mode
  :mode ("\\.smt$" . z3-mode))

(use-package matlab-mode
  :mode ("\\.m$" . matlab-mode))

(use-package json-mode
  :mode ("\\.json$" . json-mode))

(use-package csv-mode
  :mode ("\\.csv$" . csv-mode))

(use-package riscv-mode
  :mode ("\\.riscv$" . riscv-mode))

;; *** C++
(use-package modern-cpp-font-lock
  :commands (modern-c++-font-lock-mode))

;; Horrible C++ mode setup
(use-package cc-mode
  :mode ("\\.c$" . c-mode)
  :mode ("\\.cpp$\\|\\.cc$\\|\\.C$\\|\\.cxx$\\|\\.hpp$\\|\\.h$" . c++-mode)
  :config
  (defconst tristan-c-style
    '((c-tab-always-indent . t)
      (c-echo-syntactic-information-p . t)
      (tab-width . 2)
      (c-basic-offset . 2)
      (indent-tabs-mode . nil))
    "My C Style")

  (defun my-c-mode-font-lock-if0 (limit)
    (cpp-highlight-buffer t)
    nil)

  (c-add-style "PERSONAL" tristan-c-style)

  (defun c-doc-hook ()
    "A hook to set up javadoc-style documentation highlighting in C and C++ modes"
    (setq c-doc-comment-style
          '((java-mode . javadoc)
            (c-mode . javadoc)
            (c++-mode . javadoc)))
    (set-face-foreground 'font-lock-doc-face (face-foreground font-lock-comment-face)))
  (add-hook 'c-initialization-hook 'c-doc-hook)

  (defun my-c-mode-hook ()
    (c-set-style "PERSONAL")
    (local-set-key "\C-m" 'newline-and-indent)
    (c-set-offset 'inline-open 0)
    (c-set-offset 'member-init-intro 0)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'arglist-cont-nonempty '++)
    (font-lock-add-keywords
     nil
     '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend)))
     'add-to-end))

  (add-hook 'c-mode-common-hook 'my-c-mode-hook)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

;; CPP is the sub-mode used to highlight C preprocessor directives.  It is
;; actually separate from cc-mode, so we configure it here.
(use-package cpp
  :straight nil
  :defer t
  :config
  (setq cpp-known-face 'default)
  (setq cpp-unknown-face 'default)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list '(("0" font-lock-comment-face default both)
                        ("1" default font-lock-comment-face both))))

(use-package ninja-mode
  :mode ("\\.ninja$" . ninja-mode))

;; ** Markup modes


(use-package rst
  :mode ("\\.rst$" . rst-mode)
  :init
  (add-hook 'rst-mode-hook 'visual-line-mode))

(use-package markdown-mode
  :mode ("\\.markdown$\\|\\.md$" . markdown-mode)
  :init
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  :config
  (setq markdown-command "pandoc"))

(use-package yaml-mode
  :mode ("\\.yaml$" . yaml-mode))

(use-package adoc-mode
  :mode ("\\.adoc$" . adoc-mode))

(use-package toc-org
  :after org)

(use-package font-latex
  :straight nil
  :defer t
  :config
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning 'color))

(use-package tex
  :straight nil
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-source-correlate-start-server t))

(use-package auctex
  :mode ("\\.tex$" . latex-mode)
  :init
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook
            #'(lambda ()
                (custom-set-faces '(font-latex-slide-title-face ((t (:inherit font-lock-type-face)))))
                (font-latex-update-sectioning-faces))))

;; Highlighting for git commit messages
(use-package git-commit
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
  :straight nil
  :diminish eldoc-mode)

;; ** Major tools

;; Show the next possible keys in a key chord, after a key chord has been
;; started and after a delay.  Useful for learning new keybindings and figuring
;; out what is bound under a given prefix.
(use-package which-key
  :diminish
  :commands (which-key-mode)
  :config (which-key-mode))

(use-package compile
  :straight nil
  :defer
  :init
  ;; Scroll the compilation window as output is generated
  (setq compilation-scroll-output t))

(use-package cargo
  :commands (cargo-minor-mode)
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'toml-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :commands (flycheck-rust-setup)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package browse-url
  :straight nil
  :commands (browse-url-generic)
  :if window-system
  :init
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "xdg-open"))

;; Extends tramp with an access method for editing files inside of running
;; Docker containers:
;;
;; > C-x C-f /docker:user@container:/path/to/file
(use-package docker-tramp)

;; The ultimate git interface
(use-package magit
  :init
  ;; Disable vc-mode (using magit)
  (setq vc-handled-backends nil)
  (setq magit-auto-revert-mode nil)
  (setq magit-auto-revert-immediately nil)
  (setq magit-diff-refine-hunk t)
  ;; Edit it commit messages as if they were markdown
  (setq git-commit-major-mode 'markdown-mode)
  (setq git-commit-summary-max-length 500)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
  (add-hook 'git-commit-mode-hook 'visual-line-mode)
  :bind ("C-x g" . magit-status))

;; Interaction with github (or other forge) repositories (e.g., for issues and PRs)
(use-package forge
  :after magit
  :commands (forge-pull forge-list-issues forge-list-pullreqs))

;; An emacs interface for doing github code review
(use-package code-review
  :commands (code-review-start code-review-forge-pr-at-point))

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
  :hook (after-init . global-clipetty-mode))

;; Hide some minor mode indicators in the modeline
(use-package diminish)

(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :init
  (add-hook 'prog-mode-hook 'ligature-mode)
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
(use-package flyspell
  :diminish flyspell-mode
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_US")
  (add-hook 'rst-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'mail-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'selective-text-flyspell))

(use-package flyspell-correct
  :commands (flyspell-correct-dummy)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-dummy))

;; Do some on-the-fly linting using flycheck and various checkers.
(use-package flycheck
  :commands (flycheck-mode)
  :init
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'rst-mode-hook 'flycheck-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'sql-mode-hook 'flycheck-mode)
  (add-hook 'LaTeX-mode-hook 'flycheck-mode)
  (add-hook 'yaml-mode-hook 'flycheck-mode)
  (add-hook 'json-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode))

(use-package undo-fu
  :commands (undo-fu-only-undo undo-fu-only-redo)
  :bind (("C-/" . undo-fu-only-undo) ("C-?" . undo-fu-only-redo)))

;; Convert a buffer into HTML, preserving syntax highlighting
(use-package htmlize
  :commands (htmlize))

(use-package unfill
  :commands (unfill-toggle))

;; A mode for profiling emacs startup times
(use-package esup
  :commands (esup)
  :init (setq esup-depth 0))

;; ** Helpful minor modes

;; Highlight 'TODO' and 'FIXME' notes in buffers (as a minor mode)
(use-package hl-todo
  :commands
  (hl-todo-mode)
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode)
  (add-hook 'LaTeX-mode-hook 'hl-todo-mode))

;; Fix pasting into terminal emacs (i.e., paste text as an atomic unit instead
;; of a character at a time)
(use-package bracketed-paste
  :init
  (bracketed-paste-enable))

;; Add support for fancy quotes and a few other typographical niceties in plain
;; text modes
(use-package typo
  :diminish typo-mode
  :init
  (add-hook 'mail-mode-hook 'typo-mode))

;; ** Navigation

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

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
  :init
  (savehist-mode))

(use-package emacs
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

(use-package crdt
  :straight (crdt :type git :host nil :repo "https://code.librehq.com/qhong/crdt.el.git")
  :commands (crdt-share-buffer crdt-connect))

(use-package consult
  :straight (consult :type git :host github :repo "minad/consult" :branch "main")
  ;; Replace bindings
  :bind (("C-c o" . consult-outline)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r h" . consult-register-store) ; mnemonic: register "here"
         ("C-x r b" . consult-bookmark)
         ("M-g o" . consult-outline) ;; "M-s o" is a good alternative
         ("M-g m" . consult-mark)    ;; "M-s m" is a good alternative
         ("C-s" . consult-line)
         ("C-r" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g e" . consult-compile-error)
         ("<help> a" . consult-apropos))
  :commands (consult-annotate-mode consult-annotate-command consult-show-xrefs consult-ripgrep)
  :init
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)
  (autoload 'projectile-project-root "projectile")
  (setq xref-show-definitions-function 'consult-xref)
  (setq xref-show-xrefs-function 'consult-xref)
  (setq consult-project-root-function #'projectile-project-root))

(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia" :branch "main")
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

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

;; Project management
(use-package projectile
  :diminish projectile-mode
  :bind (("C-c p p" . projectile-switch-project))
  :config
  (projectile-mode 1)
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

;; Generic completion framework.  This setup doesn't use any of the fancy
;; backends and relies on basic abbrev.
(use-package company
  :diminish company-mode
  :commands (global-company-mode)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-show-numbers t)
  (setq company-backends '(company-shell
                           company-shell-env
                           company-cabal
                           company-dabbrev-code
                           company-dabbrev)))

(use-package company-cabal
  :commands (company-cabal))

(use-package company-shell
  :commands (company-shell company-shell-env))

(use-package shackle
  :init
  (setq-default shackle-rules '((compilation-mode :other t)))
  (shackle-mode 1))

(use-package ws-butler
  :diminish
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;; * Extra customization

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
  :straight (haskell-pragma :type git :host github :repo "travitch/haskell-pragma.el")
  :commands (hydra-haskell-pragma/body))

(use-package haskell-interactive-import
  :straight (haskell-interactive-import :type git :host github :repo "travitch/haskell-interactive-import.el")
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
          ((eq width-px 2560) (setq size 12))
          (t (setq size 14)))
    (set-frame-font (font-spec :family "MonoLisa Custom" :size size ))))
    ;; (set-frame-font (format "MonoLisa CF %s" size))))

(when (display-graphic-p)
  (add-hook 'window-size-change-functions #'tr/compute-font-size))
;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:
