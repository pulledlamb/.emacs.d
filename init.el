;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               Editor is just a tool.
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; This file contains customized configuration codes, which
;; have been divided into multiple sections by ^L character.
;; One can use Ctrl-TAB to have a look of outline

;;; EXTERNAL DEPENDENCIES:
;; LSP servers:
;;       pylsp, clangd, fortls, texlab/digestif
;; Spell checker:
;;       languagetool, grammarly, aspell/huspell
;; Lint checker:
;;       pyflakes, shell checker (brew)
;; Fonts:
;;       all-the-icons, Roboto Mono, Iosevka, SF Mono
;; Others:
;;       ripgrep, fzf, libvterm, PDF tools, multimarkdown (brew),
;;       npm package `livedown` and `math-preview`

;;; Code:
;;; FUNDEMENTAL

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; Package system setup
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
(setq user-emacs-directory "~/.emacs.d/")

(use-package no-littering)
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


;; window split
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; commenting
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; Customize when to check package modification (much much faster)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; Cause straight.el to cache the autoloads of all used packages in a single
;; file on disk thus reduce IO operations
(setq straight-cache-autoloads t)

;; Initialise PACKAGE MANAGER: straight.el
;; if the boostrap doesn't work, manually run
;; git clone https://github.com/raxod502/straight.el.git ~/.emacs.d/straight/repos/straight.el
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

;; Intergration with use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Emacs Native Compilation Feature support
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

;; Update user load path
;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("extra-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

;; Custom file
(setq custom-file (concat user-emacs-directory "/extra-lisp/custom.el"))
(load custom-file :noerror)

(when (eq system-type 'darwin)
  (defvar brew-parent-dir "/opt/homebrew/")
  (defvar brew-bin-dir (expand-file-name "bin/" brew-parent-dir))
  (defvar emacs-path "/opt/homebrew/Cellar/emacs-plus@28/28.0.50"))

;; Avoid matching file name with regrex list during startup
(let ((file-name-handler-alist nil)) "~/.emacs.d/init.el")

;; Benchmark init time
(use-package esup
  :config
  (setq esup-depth 0))

;;; EDITOR

;;Coding system
;; In some old machines, you might need specify these in .bashrc
;; export LAGNUAGE=en_US.UTF-8
;; export LANG=en_US.UTF-8
;; export LC_ALL=en_US.UTF-8
;; export LC_CTYPE=en_US.UTF-8

(defvar default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Improved global minor mode for scrolling in Emacs 29
(if (> emacs-major-version 28)
    (pixel-scroll-precision-mode))

;;We are lazy human :)
(if (> emacs-major-version 27)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;;No more backup files~
(setq-default make-backup-files nil)

;;No more strange ring bell
(setq ring-bell-function 'ignore)

(setq confirm-kill-processes t)

;; smooth mouse wheel
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Delete selection
(delete-selection-mode t)


(defun my-scratch-buffer ()
"Create a new scratch buffer -- \*hello-world\*"
(interactive)
  (let ((n 0)
        bufname buffer)
    (catch 'done
      (while t
        (setq bufname (concat "*hello-world"
          (if (= n 0) "" (int-to-string n))
            "*"))
        (setq n (1+ n))
        (when (not (get-buffer bufname))
          (setq buffer (get-buffer-create bufname))
          (with-current-buffer buffer
            (org-mode))
          ;; When called non-interactively, the `t` targets the other window (if it exists).
          (throw 'done (display-buffer buffer t))) ))))
;; (my-scratch-buffer)
;; (setq initial-major-mode 'org-mode)
(setq initial-scratch-message "ಠ_ಠ")

;; Chinese input method within Emacs, rely on dynamic modules, gcc, make, and librime (powered by the RIME team)
;; Doc: https://github.com/DogLooksGood/emacs-rime/blob/master/INSTALLATION.org
;; ------------------------------------------------------------
;; Customisation:
;; * Emacs-rime has seperate config from the system rime's *
;; To customise any configuration, e.g., switch traditional/simplified Chinese,
;; place your default.custom.yaml file to "~/.emacs.d/rime/", and M-x rime-deploy
;; There's also a convinient out-of-box config on github: https://github.com/maomiui/rime.git
;; ------------------------------------------------------------
(use-package rime
  :defer t
  :straight (rime :type git
		  :host github
		  :repo "DogLooksGood/emacs-rime"
		  :files ("*.el" "Makefile" "lib.c"))
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist")
  :config
  (setq rime-emacs-module-header-root (expand-file-name "include/" emacs-path))
  (setq rime-show-candidate 'posframe)
  (setq rime-posframe-properties
	(list :font "Sarasa Mono SC Nerd"
	      :internal-border-width 10))
  (setq rime-translate-keybindings
	'("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>"
	  "<up>" "<down>" "<prior>" "<next>" "<delete>"))
  (setq rime-posframe-style 'horizontal)
  :bind
  ("C-\\" . toggle-input-method))

;; wrap up line (use visual-line-mode and visual-fill-column-mode instead)
;; (setq truncate-lines nil)
;; (setq-default word-wrap nil)
(use-package visual-fill-column
  :ensure t
  :hook
  (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text t)
  ;; wrap long line
  (visual-fill-column-width 140))

;; overrides certain minor modes and variables to
;; improve the perforamce when open files with long lines
(use-package so-long
  :straight (:type built-in)
  :hook
  (after-init . global-so-long-mode)
  :custom
  (so-long-action 'so-long-minor-mode))

;; auto revert buffer
(use-package autorevert
  :straight (:type built-in)
  :hook
  (after-init . global-auto-revert-mode))
;; ;; enable for image mode too
;; (add-hook 'image-mode-hook 'auto-revert-mode)
;; (add-hook 'pdf-view-mode-hook
;;           (lambda ()
;;             (setq global-auto-revert-ignore-buffer t)))

;;;; Enable mouse operation in terminal emacs
(unless (display-graphic-p)
  ;; specifies the mode where <BS> or <BACKSPACE> is <DEL>
  (normal-erase-is-backspace-mode 0)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Hack clipboard for macOS in TUI mode
(defun my/kill-ring-save (orig-fun beg end &optional region)
  (unless (display-graphic-p)
    (let ((inhibit-message t))
      (shell-command-on-region beg end "pbcopy")))
  (funcall orig-fun beg end region))
(advice-add 'kill-ring-save :around #'my/kill-ring-save)


;;use undo-tree-visualize to show history
(use-package undo-tree
  :hook
  (after-init . global-undo-tree-mode)
  :bind
  ("C-c u" . undo-tree-visualize))

;; undo and redo changes in the *window configuration*
(use-package winner
  :straight (:type built-in)
  :hook
  (after-init . winner-mode)
  :bind
  (:map winner-mode-map
	("C-M-b" . winner-undo)
	("C-M-f" . winner-redo)))

;;; Auto-save
(use-package super-save
  :hook
  (after-init . super-save-mode)
  :config
  ;; (super-save-mode 1)
  ;; turn off the buil-in auto-save
  (setq auto-save-default nil)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (setq super-save-remote-files nil)
  (setq super-save-exclude '(".gpg")))

;; A replacement to buil-tin M-w, now you can
;; save word/sexp/list/defun/file by M-w w/s/l/d/f
(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;; Smartly clean whitespace
;; (use-package whitespace-cleanup-mode
;;   :hook
;;   (prog-mode . whitespace-mode))

;; locally remove trailing whitespace for programming mode
(add-hook 'prog-mode-hook
          (lambda () (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)))

;; An alternative way to cleanup whitespace
(use-package ws-butler
  :defer t
  :straight (:type git :host github
		   :repo "lewang/ws-butler")
  :hook
  (prog-mode . ws-butler-mode))

;; Automatically add spacing around operators
(use-package electric-operator
  :hook
  ;; (python-mode . electric-operator-mode)
  ;; (emacs-lisp-mode . electric-operator-mode)
  (ess-r-mode . electric-operator-mode))

(use-package smart-newline
  :straight (:type git :host github
		   :repo "ainame/smart-newline.el")
  :config
  (smart-newline-mode 1)
  :bind
  ("C-j" . smart-newline))

;; smartly select region, press until it selects what you want
;; C-M-SPC also does same on Mac
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; assign every marked line a cursor
(use-package multiple-cursors
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  ("C-M-<down>" . mc/mark-next-like-this)
  ("C-S-<up>" . mc/mark-previous-like-this)
  ("M-<mouse-1>" . mc/add-cursor-on-click))


;;; TERMINAL, COMPLETION, LINT/SPELL CHECKER, SNIPPET

;; Shells in Emacs (more of an interface)
;; Tips: you can use M-r to search in shell history
;; History references like '!' (reference), ‘!!’ (last cmd) and ‘^’ (substituion, e.g., ^a^b) are supported
;; If you don't know the history reference, use C-c C-l to list all (will work for most comint buffers)
(use-package shell
  :straight (:type built-in)
  :config
  (defun no-echo-input-in-shell ()
    "Do not echo my input command"
    (setq comint-process-echoes t))
  (add-hook 'shell-mode-hook 'no-echo-input-in-shell)
  ;; can't delete output text
  (setq comint-prompt-read-only t)
  (add-hook 'comint-preoutput-filter-functions
	    (lambda (text)
	      (propertize text 'read-only t)))
  :bind
  ("C-x s" . shell)
  (:map shell-mode-map
	("<up>" . comint-previous-input)
	("C-p" . comint-previous-input)
	("<down>" . comint-next-input)
	("C-n" . comint-next-input)
	("C-l" . comint-clear-buffer)
	("SPC" . comint-magic-space)))     ;magically expand history reference

;; -path as well
(use-package vterm
  :bind
  ("C-x t" . vterm))

;; get better rendering experience for term/ansi-term
(use-package eterm-256color
  :hook
  (term-mode . eterm-256color-mode))

;; a comint extension, e.g., :view *.jpg to view a plot in shell
;; other useful cmd: :e (edit), :ssh,
(use-package shx
  :hook
  (after-init . shx-global-mode))

(use-package python-mode
  :ensure t
  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args ""))

;;auto-completion system
(use-package company
  :config
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; (setq company-backends '((company-capf company-dabbrev company-files)))

  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (setq company-global-modes '(not inferior-python-mode))
  (setq company-idle-delay 0.3)
  (setq company-show-numbers t)
  :hook
  (after-init . global-company-mode)
;;   :bind
;;   (:map company-active-map
;; 	("C-n" . company-select-next)
;; 	("C-p" . company-select-previous)
;; ;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; 	;;               NEW
;; ;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; 	("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  )
(defun add-pcomplete-to-capf ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))


(defun text-mode-hook-setup ()
  ;; make `company-backends' local is critcal
  ;; or else, you will have completion in every major mode, that's very annoying!
  (make-local-variable 'company-backends)

  ;; company-ispell is the plugin to complete words
  (add-to-list 'company-backends 'company-ispell)
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-dabbrev)

  ;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
  ;;  but I prefer hard code the dictionary path. That's more portable.
  (setq company-ispell-dictionary (file-truename "~/.emacs.d/misc/english-words.txt")))

(add-hook 'text-mode-hook 'text-mode-hook-setup)

;; (add-hook 'org-mode-hook #'add-pcomplete-to-capf)


;; hide `company-ispell' echo message "Starting 'look' process".
(use-package shut-up
  :ensure t
   :init
   (advice-add 'ispell-lookup-words :around
	       (lambda (orig &rest args)
		 (shut-up (apply orig args)))))

;; company for shell script
(use-package company-shell
  :config
  (add-to-list 'company-backends 'company-shell-env))

;;A machine-learning based backend for company
;;May conflict with company-flx-mode/ESS mode
;; (use-package company-tabnine
;;   :defer 1
;;   :after company
;;   :config
;;   (eval-after-load 'company-tabnine
;;     (if (not (file-directory-p "~/.TabNine/"))
;; 	(company-tabnine-install-binary)))
;;   (add-to-list 'company-backends #'company-tabnine)
;;   )


;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


;; (use-package company-org-block
;;   :defer t
;;   :custom
;;   (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
;;   :hook ((org-mode . (lambda ()
;; 		       (setq-local company-backends '(company-org-block))
;; 		       (company-mode 1)))))
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


;;A fuzzy matching of company
(use-package company-flx
  :hook
  (company-mode . company-flx-mode))

;;simple and fast sorting and filtering framework for comppany
(use-package company-prescient
  :hook
  (company-mode . company-prescient-mode)
  :config
  (setq prescient-filter-method '(literal regexp initialism)))

(use-package company-posframe
  :hook
  (company-mode . company-posframe-mode)
  :custom
  (company-posframe-quickhelp-delay 0.15)
  (company-posframe-quickhelp-show-header nil)
  :config
  (setq posframe-arghandler #'my-posframe-arghandler)
  (defun my-posframe-arghandler (buffer-or-name arg-name value)
    (let ((info '(:internal-border-width 0)))
      (or (plist-get info arg-name) value))))

;; Alternative to company-posframe, there's also company-quickhelp
;; (use-package company-box
;;   :hook
;;   (company-mode . company-box-mode))

;;To specify new version of git on remote machine so I can run magit locally
;;add ~/.ssh/config and ~/.ssh/known_hosts first
;;then ssh-keygen -t rsa => ssh-copy-id name@host_name
;; .inputrc file may trigger bug of "timeout reached"
(use-package tramp
  :defer 1
  :straight (:type built-in)
  :if (memq system-type '(gnu/linux darwin))
  :config
  ;; use for debug
  ;; (setq tramp-verbose 6)
  (add-to-list 'tramp-remote-path "/usr/local/bin/git")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;tramp mode to cache password
  (setq password-cache-expiry nil))

(use-package counsel-tramp
  :after (counsel tramp)
  :config
  (setq counsel-tramp-custom-connections '("/ssh:mogu@almond.ggy.bris.ac.uk:/home/mogu/cgenie.muffin/"
					   "/ssh:mogu@sprout.ggy.bris.ac.uk:/home/mogu/cgenie.muffin/"))
  (setq tramp-default-method "ssh")
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  :bind
  ("C-c s" . counsel-tramp)
  )

;; visit https://github.com/jacktasia/dumb-jump to see more alternative ways
;; like TAGS system
;;======================================================================
;;depends on external programs: The-Silver-Searcher/ripgrep and emacs package ag/rg
;;======================================================================
(use-package dumb-jump
  :defer t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-default-project "~/cgenie.muffin")
  (add-to-list 'auto-mode-alist '("\\.config\\'" . shell-script-mode)))

(use-package ag :defer t)
(use-package rg :defer t)
(use-package ripgrep :defer t)

(use-package which-key
  :hook
  (after-init . which-key-mode)
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2))
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; on-the-fly syntax checker,  use C-c ! as prefix, e.g., C-c ! v to verify the checkers
;; use M-g n/p to navigate error, or use C-c e (counsel-flycheck)

;; python flycheck depdencies
;; pip install pyflakes -> fast and don't check code style

(use-package flycheck-pyflakes
  :after python)

;; Alternative
;; pip install prospector -> don't check code style but relatively slower
;; (use-package flycheck-prospector
;;   :config
;;   (flycheck-prospector-setup)
;;   )

;; shell -> shell-checker
;; python -> pyflakes
;; R -> disabled
;; Elisp -> default
(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(sh-posix-dash
					     sh-posix-bash
					     python-flake8
					     python-pylint
					     python-mypy
					     python-pyright
					     python-pycompile
					     emacs-lisp-checkdoc)))

;; (use-package flycheck-grammarly
;;   :config
;;   (setq flycheck-grammarly-check-time 0.8))

(use-package flycheck-inline
  :hook
  (flycheck-mode . flycheck-inline-mode))

;; https://github.com/languagetool-org/languagetool
;; alternative: https://github.com/emacs-languagetool/flycheck-languagetool
(use-package languagetool
  :config
  (setq languagetool-language-tool-jar
	(concat (getenv "HOME") "/Library" "/LanguageTool-5.5-stable/languagetool-commandline.jar"))
  (setq languagetool-language-tool-server-jar
	(concat (getenv "HOME") "/Library" "/LanguageTool-5.5-stable/languagetool-server.jar"))
  (setq languagetool-server-user-arguments '("-p" "8082"))
  (setq languagetool-default-language "en-US")
  (setq languagetool-java-bin "/usr/bin/java")
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"))
  :bind
  ("C-c C-; c" . languagetool-check)
  ("C-c C-; d" . languagetool-clear-buffer)
  ("C-c C-; i" . languagetool-correct-at-point)
  ("C-c C-; b" . languagetool-buffer)
  ("C-c C-; l" . languagetool-set-language))

;; Doc: https://github.com/egh/zotxt-emacs
;; (use-package zotxt-emacs)

;; A dictionary inside Emacs, by abo-abo!
(use-package define-word
  :bind
  ("C-c d" . define-word-at-point)
  :config
  (setq define-word-default-service 'webster))

;;flyspell setting
(use-package flyspell
  :straight (:type built-in)
  :hook
  (text-mode . flyspell-mode)
  (org-mode . flyspell-mode)
  (LaTeX-mode . flyspell-mode)
  :config
  (setq-default ispell-program-name "aspell") ;;depends on aspell in the path
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  (setq-default ispell-local-dictionary "en_US")
  (setq ispell-extra-args '("--sug-mode=fast" "--lang=en_US"
			    "--camel-case" "--run-together")))

;; Quickly switch dictionaries
;; Adapted from DiogoRamos' snippet on https://www.emacswiki.org/emacs/FlySpell#h5o-5

(let ((langs '("francais" "english")))
  (defvar lang-ring (make-ring (length langs))
    "List of Ispell dictionaries you can switch to using ‘cycle-ispell-languages’.")
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  "Switch to the next Ispell dictionary in ‘lang-ring’."
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))
(global-set-key [f10] #'cycle-ispell-languages) ; replaces ‘menu-bar-open’.

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; Flyspell interface
;; Use M-o to do words action (e.g., save)
(use-package flyspell-correct-ivy
  :after flyspell-correct)

(use-package yasnippet
  :straight yasnippet-snippets ;; Collection of snippets
  :hook (after-init . yas-global-mode))

;;manually choose a snippet
(use-package ivy-yasnippet
  :after (ivy yasnippet)
  :bind
  (("C-c i" . ivy-yasnippet))
  :config
  (setq ivy-yasnippet-expand-keys 'smart))

;;Git + Emacs = boom!
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x c" . magit-checkout))

;;a magit prefix help page
(use-package transient
  :defer t)

;;This package reads proper environment variable in MacOS GUI version
;;To speed up this package, (1) separate configuration into
;;non-interactive (.zshenv) and interactive (.zshrc) part;
;;(2) set explicit path in .zshenv (which is what we will use, you should
;;put your PATH variable like /usr/local/bin/python3.9 in this file)
;;Find out more in https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  ;; :if (memq window-system '(mac ns x))
  :config
  ;; (setq exec-path-from-shell-arguments nil) ;;read non-interactive shell config
  (exec-path-from-shell-initialize))


;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; setting environment variables
(exec-path-from-shell-copy-env "PATH")

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(use-package use-package-ensure-system-package
  :after exec-path-from-shell) ;;extend use-package, put after exec-path-from-shell

(use-package popwin
  :hook
  (after-init . popwin-mode))

(use-package ivy
  :hook
  (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-wrap t)
  (setq ivy-height 9)
  ;;  (setq ivy-format-function 'ivy-format-function-line)
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ;; Marking candidates
  (defun scimax-ivy-toggle-mark ()
    "Toggle the mark"
    (interactive)
    (if (ivy--marked-p)
	(ivy-unmark)
      (ivy-mark))
    (ivy-previous-line))

  (define-key ivy-minibuffer-map (kbd "C-<tab>")
    #'scimax-ivy-toggle-mark)

  :bind
  (("\C-s" . swiper)
   ("C-c v" . ivy-push-view)
   ("C-c V" . ivy-pop-view)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   :map ivy-switch-buffer-map
   ("C-k" . ivy-previous-line)
   ("C-l" . ivy-done)
   ("C-d" . ivy-switch-buffer-kill)
   :map ivy-reverse-i-search-map
   ("C-k" . ivy-previous-line)
   ("C-d" . ivy-reverse-i-search-kill)))
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(use-package counsel
  :after ivy
  :bind
  (;; ("C-x C-b" . counsel-ibuffer)
   ;; ("C-x b" . counsel-switch-buffer)
   ("C-c b" . counsel-imenu) ;; imenus provides a list of definition
   ("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop) ;;something like a clipboard
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-c t" . counsel-load-theme)
   ("C-c j" . counsel-git-grep)
   ("C-c g" . counsel-git) ;;find file in current git directory
   ("C-c l" . counsel-git-log)
   ("C-c r" . counsel-rg)  ;;rg find tex
   ("C-c f" . counsel-fzf) ;;fzf find file
   ("C-c e" . counsel-flycheck)
   ("C-c C-r" . counsel-recentf)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)
   ))

(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
(setq recentf-exclude '(".*-autoloads\\.el\\'"
                        "[/\\]\\elpa/"
                        "bookmark"
                        ))
;; (use-package recentf
;;   :straight (:type built-in)
;;   :config
;;   (setq-default
;;    recentf-max-saved-items 30
;;    recentf-exclude `("/tmp/",
;; 		     (concat "~/.emacs.d/straight/build" "/.*-autoloads\\.el\\'")))
;;   (global-set-key (kbd "<f3>") #'recentf-open-files)
;;   :hook
;;   (after-init . recentf-mode))


;;sorting and filtering framework for ivy
(use-package ivy-prescient
  :after counsel
  :hook
  (ivy-mode . ivy-prescient-mode)
  :config
  (setq ivy-prescient-sort-commands t
	ivy-prescient-enable-sorting nil
	ivy-prescient-retain-classic-highlighting t))

;; Project management tool
(use-package projectile
  :after ivy
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :hook
  (after-init . counsel-projectile-mode))

;; Built-in project manager, support git repos only
;; (use-package project
;;   :straight (:type built-in)
;;   :config
;;   (defun my/project-files-in-directory (dir)
;;     "Use `fd' to list files in DIR."
;;     (let* ((default-directory dir)
;; 	   (localdir (file-local-name (expand-file-name dir)))
;; 	   (command (format "fd -H -t f -0 . %s" localdir)))
;;       (project--remote-file-names
;;        (sort (split-string (shell-command-to-string command) "\0" t)
;; 	     #'string<))))

;;   (cl-defmethod project-files ((project (head local)) &optional dirs)
;;     "Override `project-files' to use `fd' in local projects."
;;     (mapcan #'my/project-files-in-directory
;; 	    (or dirs (list (project-root project))))))

;; yet another robust find file in project
;; but don't rely on fzf
(use-package find-file-in-project
  :bind
  ("C-x f" . find-file-in-project-at-point))

;;Faster cursor movement - go to anywhere
(use-package avy
  :bind
  ("C-\"" . avy-goto-char)		;input: one character
  ("C-'" . avy-goto-char-2)		;input: two characters
  ("M-g w" . avy-goto-word-1)
  ("M-g l" . avy-goto-line))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(use-package ace-window
  ;; :bind
  ;; ("M-o" . ace-window)
  ;; ("C-x o" . ace-swap-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2)))))
  )
(global-set-key (kbd "M-o") 'ace-window)
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; workspace manager and just show buffers in this space
;; C-x x s to switch/create a new perspective
(use-package perspective
  :bind
  (("C-x k" . persp-kill-buffer*)
   ("C-x C-b" . persp-ibuffer)
   ("C-x b" . persp-counsel-switch-buffer))
  :config
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (persp-ibuffer-set-filter-groups)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic))))
  (persp-mode)
  :custom
  (persp-sort 'access)
  (persp-mode-prefix-key (kbd "C-x w")))


;;; KEYBINDING

;; Set meta command for Mac OS
;; If you are using a external Windows keyboard, remeber to choose
;; USB keyboard in Preference -> Keyboard -> modify keyboard -> select keyboard
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(global-set-key (kbd "C-x k") 'kill-buffer)
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(global-set-key (kbd "C-M-;") 'comment-box)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x ,") 'beginning-of-buffer)
(global-set-key (kbd "C-x .") 'end-of-buffer)
;; globally go to previous position; "C-u C-SPC" to do same locally
(global-set-key (kbd "C-c C-SPC") 'pop-global-mark)
(define-key key-translation-map (kbd "C-d") (kbd "<deletechar>"))
(global-set-key (kbd "\C-d") 'delete-forward-char)
;; evil insert mode
(evil-define-key 'insert global-map (kbd "C-J") 'evil-next-line)
(evil-define-key 'insert global-map (kbd "C-K") 'evil-previous-line)
;; repeat command
(global-set-key (kbd "<f4>") #'repeat)


;; M-up/down to move text
(use-package move-text
  :config
  (move-text-default-bindings))

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (move-beginning-of-line 1)
  (push-mark nil nil t)
  ;;(forward-line 1)
  (end-of-line 1))
(global-set-key (kbd "C-l") 'select-current-line)

(defun open-init-file()
  "Open my init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)

;; Mark set
;; C-x C-x -> set mark and move back to previous position
;; C-x h to select all



(use-package general
  :ensure t
  :config
  (general-auto-unbind-keys)
  (defconst leader "\\")
  (general-create-definer my/leader-def
    :prefix leader)

  (defun quote-backslash()
    (interactive)
    (insert "\\"))

  ;; ** Global Keybindings
  (my/leader-def
    "\\" 'quote-backslash

    "g l" '(avy-goto-line :which-key "goto-line")
    "g g" '(goto-line :which-key "goto-line-number")
    "g m" '(exchange-point-and-mark :which-key "go-back-and-mark")
    "g b" '(pop-global-mark :which-key "go-back")
    "g f" '(counsel-file-jump :which-key "goto-file")

    "n f" 'org-roam-node-find
    "n i" 'org-roam-node-insert
    "n b" 'org-roam-buffer-toggle
    "n v" 'org-roam-ui-mode
    "n k" 'org-id-get-create
    "n c" 'org-roam-capture
    "n s" 'org-roam-db-autosync-mode

    "j o" 'ein:run
    "j s" 'ein:stop

    "x s" 'persp-switch

    "h a" '(mark-whole-buffer :which-key "select-all")

    "." 'mc/mark-next-like-this
    "," 'mc/mark-previous-like-this

    "f" 'counsel-find-file
    "q" 'save-buffers-kill-terminal
    "r" 'counsel-recentf

    "1" 'ranger

    "<left>" '(centaur-tabs-backward :which-key "last-tab")
    "<right>" '(centaur-tabs-forward :which-key "next-tab"))

  (my/leader-def 'insert
    "\\" 'quote-backslash)

  ;; ** Mode Keybindings
  (my/leader-def prog-mode-map
    "b" 'counsel-imenu
    "s" 'shell
    "t" 'vterm
    "c" 'counsel-flycheck
    "%" 'query-replace)

  (my/leader-def text-mode-map
    "d" 'define-word-at-point
    "c" 'languagetool-check
    "i" 'languagetool-correct-at-point)

  (my/leader-def projectile-mode-map
    "p f" 'projectile-find-file
    "p s" 'projectile-ripgrep
    "p p" 'projectile-switch-project
    "p r" 'projectile-replace)

  (my/leader-def markdown-mode-map
    "l" 'livedown-preview
    "k" 'livedown-kill)

  (my/leader-def python-mode-map
    "r" 'python-shell-send-buffer
    "h" 'eldoc)

  (my/leader-def ess-r-mode-map
    "r" 'ess-eval-buffer-and-go
    "h" 'eldoc)

  (my/leader-def dired-mode-map
    "e" 'dired-toggle-read-only)

  (general-auto-unbind-keys :off))

;; a human-friendly keymap of built-in code-folding package
;; alternatives: vimish-fold, Origami
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "<f5>") 'hs-toggle-hiding)

;; adjust font size
(setq-default text-scale-mode-step 1.1)

;; one "BODY", multiple "HEADs", try "C-c 5 =" to know the magic
(use-package hydra
  :defer t
  :config
  (defhydra hydra-zoom (global-map "C-c z")
    "zoom in/out windows, or increase/decrease text"
    ("=" text-scale-increase "in")
    ("-" text-scale-decrease "out"))

  (defhydra hydra-vi (:pre (set-cursor-color "#40e0d0")
			   :post (progn
				   (set-cursor-color "#ffffff")
				   (message
				    "Thank you, come again.")))
    "vi-style movement in Emacs"
    ("l" forward-char)
    ("h" backward-char)
    ("j" next-line)
    ("k" previous-line)
    ("q" nil "quit"))
  :bind
  ("C-c v" . hydra-vi/body))

;; a center-floating posframe for hydra
(use-package hydra-posframe
  :after hydra
  :straight
  (:type git :host github
	 :repo "Ladicle/hydra-posframe")
  :hook
  (after-init . hydra-posframe-mode))

;; define a hydra-leader-key for major mode, something like C-c
(use-package major-mode-hydra
  :after all-the-icons
  :bind
  ("S-SPC" . major-mode-hydra)
  :config
  (defvar hydra-r-title (s-concat
			 (s-repeat 28 " ")
			 (all-the-icons-icon-for-file "f.R")
			 " "
			 "ESS R commands"))
  (defvar hydra-py-title (s-concat
			  (s-repeat 23 " ")
			  (all-the-icons-icon-for-file "f.py")
			  " "
			  "Python mode commands"))
  (defvar hydra-el-title (s-concat
			  (s-repeat 18 " ")
			  (all-the-icons-icon-for-file "f.el")
			  " "
			  "Emacs-lisp commands"))
  (defvar hydra-tex-title (s-concat
			   (s-repeat 23 " ")
			   (all-the-icons-icon-for-file "f.tex")
			   " "
			   "LaTeX commands"))
  (major-mode-hydra-define emacs-lisp-mode
    (:foreign-keys warn :quit-key "q"
		   :title hydra-el-title :separator "-")
    ("Eval"
     (("b" eval-buffer "buffer")
      ("e" eval-defun "defun")
      ("r" eval-region "region"))
     "REPL"
     (("I" ielm "ielm"))
     "Doc"
     (("d" describe-foo-at-point "thing-at-pt")
      ("f" counsel-describe-function "function")
      ("v" counsel-describe-variable "variable")
      ("i" info-lookup-symbol "info lookup"))
     "Project"
     (("p D" project-dired "dired")
      ("p f" project-find-file "find")
      ("p d" project-find-dir "directory")
      ("p s" project-find-regexp "search")
      ("p p" project-switch-project "switch")
      ("p r" project-query-replace-regexp "replace"))))
  (major-mode-hydra-define python-mode
    (:foreign-keys warn :quit-key "q"
		   :title hydra-py-title :separator "-")
    ("Execute"
     (("b" python-shell-send-buffer "buffer")
      ("r" python-shell-send-region "region")
      ("f" python-shell-send-file "file")
      ("l" python-shell-send-statement "line")
      ("j" ein:run "jupyter"))
     "REPL"
     (("c" run-python "console")
      ("s" shell "shell")
      ("u" undo-tree-mode "undo-tree")
      ("y" ivy-yasnippet "yasnippet")
      ("g" magit-status "git"))
     "Find"
     (("i" counsel-imenu "function")
      ("R" counsel-rg "ripgrep")
      ("F" counsel-fzf "find"))
     "Check"
     (("e" flycheck-list-errors "errors")
      ("c" flycheck-clear "clear")
      ("v" flycheck-verify-setup "setup"))
     "Project"
     (("p D" project-dired "dired")
      ("p f" project-find-file "find")
      ("p d" project-find-dir "directory")
      ("p s" project-find-regexp "search")
      ("p p" project-switch-project "switch")
      ("p r" project-query-replace-regexp "replace"))))
  (major-mode-hydra-define ess-r-mode
    (:foreign-keys warn :quit-key "q"
		   :separator "-"
		   :title hydra-r-title)
    ("Run"
     (("b" ess-eval-buffer-and-go "buffer")
      ("r" ess-eval-region-and-go "region")
      ("f" ess-eval-function-and-go "function"))
     "Power"
     (("c" run-ess-r "console")
      ("s" shell "terminal")
      ("u" undo-tree-mode "undo-tree")
      ("y" ivy-yasnippet "yasnippet")
      ("g" magit-status "git"))
     "Find"
     (("i" counsel-imenu "function")
      ("R" counsel-rg "ripgrep")
      ("F" counsel-fzf "find"))
     "Check"
     (("l" flycheck-list-errors "all")
      ("c" flycheck-clear "clear")
      ("v" flycheck-verify-setup "setup"))
     "Project"
     (("p D" project-dired "dired")
      ("p f" project-find-file "find")
      ("p d" project-find-dir "directory")
      ("p s" project-find-regexp "search")
      ("p p" project-switch-project "switch")
      ("p r" project-query-replace-regexp "replace"))))
  (major-mode-hydra-define latex-mode
    (:foreign-keys warn :quit-key "q"
		   :separator "-"
		   :title hydra-tex-title)
    ("Compile"
     (("m" TeX-command-master "master-cmd")
      ("v" TeX-view "view"))
     "Preview"
     (("p p" math-preview-at-point "preview-at-pt")
      ("p a" math-preview-all "preview-all")
      ("p r" math-preview-region "preview-region")
      ("p c p" math-preview-clear-at-point "clear-at-pt")
      ("p c a" math-preview-clear-all "clear-all")
      ("p c r" math-preview-clear-region "clear-region"))
     "Insert"
     (("e" LaTeX-environment "env")
      ("]" LaTeX-close-environment "close env")
      ("f" TeX-font "font")
      ("s" LaTeX-section "section")
      ("i" LaTeX-insert-item "item"))
     "Languagetool"
     (("c" languagetool-check "check")
      ("d" languagetool-clear-buffer "clear")
      ("i" languagetool-correct-at-point "at-pt")
      ("b" languagetool-buffer "buffer")
      ("S" languagetool-set-language "setting")))))

;; beautify hydra
(use-package pretty-hydra
  :after all-the-icons
  :config
  (defvar hydra-ui-title (s-concat (s-repeat 20 " ")
				   (all-the-icons-faicon "windows")
				   " Apperance"))
  (pretty-hydra-define hydra-ui
    (:foreign-keys warn :title hydra-ui-title :quit-key "q")
    ("Theme"
     (("d" night-theme "dark-theme")
      ("l" light-theme "light-theme")
      ("t" counsel-load-theme "choose"))
     "Window"
     (("b" split-window-right "split horizontally")
      ("v" split-window-below "split vertically")
      ("f" toggle-frame-fullscreen "fullscreen")
      ("m" ace-delete-other-windows "maximize")
      ("o" ace-window "others"))
     "Page"
     (("n" forward-page "next")
      ("p" backward-page "previous"))))
  (global-set-key (kbd "C-c w") 'hydra-ui/body))

;; (defconst EMACS29+   (> emacs-major-version 28))
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :init
  (if (> emacs-major-version 28)
  ;; (when EMACS29+
    ;; REVIEW See Wilfred/elisp-refs#35. Remove once fixed upstream.
    (defvar read-symbol-positions-list nil))
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-h f" . helpful-callable)
  ("C-h V" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h d" . helpful-at-point)
  ("C-h C" . helpful-command)
  ("C-h F" . helpful-function))

;; find and replace
(global-set-key (kbd "C-c %") 'query-replace)
(global-set-key (kbd "C-c R") 'query-replace-regexp)

;; select one and edit all (https://github.com/victorhge/iedit)
;; iedit is also dependency of lispy, use M-i to toggle
(use-package iedit
  :bind
  ("M-i" . iedit-mode))

;;; WINDOW, UI & APPEARANCE

;;If you are running Emacs in MacOS, then I recommend you using
;;Emacs-mac <--with-no-title-bars> which improves GUI performance a lot
;;Also: for the Emacs-mac you can swipe between buffer by using two fingers (cool!)
(when (eq system-type 'darwin)
  (progn
    (setq dired-use-ls-dired nil) ;;to avoid error "ls does not support --dired" in MacOS
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ;;same title bar color
    (add-to-list 'default-frame-alist '(ns-appearance . light))))

;;Auto-max the frame at startup
(defun auto-max-frame ()
  "Maxize/full screen the frame according to the OS type."
  (interactive)
  (if (eq system-type 'darwin)
      (toggle-frame-maximized)
    (toggle-frame-fullscreen)))

(if (display-graphic-p)
    (add-hook 'after-init-hook #'auto-max-frame))

;;no more startup message/screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;;Display time in the mode line
(add-hook 'after-init-hook 'display-time-mode)
(setq display-time-format "%B %d %H:%M %p")
(setq system-time-locale nil)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 15)))
;;If use doom-mode-line, then display battery
(add-hook 'doom-modeline-mode-hook 'display-battery-mode)

(use-package keycast
  :ensure t
  :commands keycast-mode
  :config
  (setq keycast-separator-width 1)
  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing!")))

  (defun store-action-key+cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd)
    cmd)

  (defun store-action-key-no-cmd (cmd)
    (setq keycast--this-command-keys (this-single-command-keys)
          keycast--this-command cmd))

  (defun keycast-capture-avy-dispatch (char)
    (if-let ((cmd (assoc char avy-dispatch-alist)))
        (setq keycast--this-command-keys (make-vector 1 char)
              keycast--this-command (cdr cmd))))

  (advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)
  ;; (advice-add 'avy-goto-char-timer :filter-return #'store-action-key+cmd)
  (advice-add 'avy-handler-default :before #'keycast-capture-avy-dispatch)

  (defun force-keycast-update (&rest _)
    (force-mode-line-update t))

  (dolist (cmd '(embark-act embark-become))
    (advice-add cmd :before #'force-keycast-update))
  (add-hook 'pre-command-hook 'keycast--update t)
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  )

;;Don't display load average percentage
(setq display-time-default-load-average nil)

;;Cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

;;Highlight current line
(add-hook 'after-init-hook 'global-hl-line-mode)

;; (use-package yascroll
;;   :hook
;;   (after-init . global-yascroll-bar-mode))

(use-package ivy-posframe
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist
	'((complete-symbol . ivy-posframe-display-at-point)
	  (swiper . ivy-posframe-display-at-window-center)
	  (counsel-M-x . ivy-posframe-display-at-window-center)
	  (t . ivy-posframe-display)))
  (setq ivy-posframe-parameters
	'((left-fringe . 8)
	  (right-fringe . 8)))
  (setq ivy-posframe-width 200
	ivy-posframe-border-width 0)
  (setq ivy-posframe-height-alist '((swiper . 10)
				    (t . 10)))
  ;; fix the width
  (defun ivy-posframe-get-size ()
    "Set the ivy-posframe size according to the current frame."
    (let ((height (or ivy-posframe-height (or ivy-height 10)))
	  (width (min (or ivy-posframe-width 200) (round (* 0.75 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))
  (setq ivy-posframe-size-function 'ivy-posframe-get-size)
  :hook
  (ivy-mode . ivy-posframe-mode))

;; similar to ivy-frame
;; (use-package mini-frame
;;   :hook
;;   (after-init . mini-frame-mode)
;;   :config
;;   (setq resize-mini-frames t)
;;   ;; for gnome shell
;;   ;; (setq x-gtk-resize-child-frames 'resize-mode)
;;   :custom
;;   (mini-frame-show-parameters
;;    '((top . 0.25)
;;      (width . 0.7)
;;      (left . 0.5)))
;;   (mini-frame-ignore-commands
;;    '(eval-expression "edebug-eval-expression"
;; 		     debugger-eval-expression swiper))
;;   (mini-frame-create-lazy nil))

;; highlight cursor when scroll window
(use-package beacon
  :straight (:type git :host github
		   :repo "Malabarba/beacon")
  :hook
  (after-init . beacon-mode))

;; type-writer sound effect
;; (use-package selectric-mode
;;   :hook
;;   (after-init . selectric-mode))

;; highlight a little bit "important" buffers
;; depends on what theme you're using
(use-package solaire-mode
  :hook
  (after-init . solaire-global-mode))

;; minimal columns for Emacs to split window horizontally
(setq split-width-threshold 130)

(use-package smart-cursor-color
  :hook
  (after-init . smart-cursor-color-mode))

(setq x-underline-at-descent-line t)

;;Display line number
(global-display-line-numbers-mode t) ;;the linum-mode has been obsolete
(setq display-line-numbers-width 0)

;;Disable line number for certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		LaTeX-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;turn off electric-indent-mode but use aggressive-indent-mode
(electric-indent-mode 1)

;; (use-package aggressive-indent
;;   :hook
;;   (prog-mode . aggressive-indent-mode))

(setq frame-inhibit-implied-resize nil)

;;-----------Dired replacement-------------

(setq dired-listing-switches "-alFhv")
(setq counsel-dired-listing-switches "-alFhv")

(use-package ranger
  ;; use `zP` to switch deer mode and ranger
  :custom
  (ranger-preview-file t)
  (ranger-width-preview 0.5)
  (ranger-max-preview-size 10)
  (ranger-dont-show-binary t)
  :bind
  (:map ranger-mode-map
	("g" . ranger-refresh)))

;;--------------------------------------------------
;; Matching parenthesis
;;--------------------------------------------------

;; Showing matching parentheses (built-in)
;; (show-paren-mode t)
;; (setq show-paren-delay 0)
;; (setq show-paren-style 'parenthesis) ;;Options: parenthesis/expression/mixed

(use-package highlight-parentheses
  :hook
  (after-init . global-highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-highlight-adjacent t)
  ;;  (setq highlight-parentheses-colors '("BlueViolet" "DarkOrchid" "orchid" "Plum"))
  )

;;--> Option 1 (built-in)
(electric-pair-mode t)
(setq electric-pair-pairs '((?\" . ?\")
			    (?\` . ?\`)
			    (?\( . ?\))
			    (?\{ . ?\})))

;; use lispy-mode (a vi-like editing) for lisp parentheses
;; remove electric-pair-mode first
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (electric-pair-local-mode -1)))

;; >>> Basic lispy usage:
;; jkhl to move, f/b to foward/backward level
;; c to copy, m to mark, e to evaluate, d to swith parenthesis side (C-d to delete)
;; >/< to slurp/barf: push out/pull in
;; w/s to move marked regions up/down
;; M-j to split, + to join
;; To insert a single parenthsis, use a C-q prefix
;; M-x check-parens to check unbalanced parens

(use-package lispy
  :hook
  (emacs-lisp-mode . lispy-mode)
  :bind
  ("C-M-l" . lispy-mode)
  (:map lispy-mode-map
	("M-o" . ace-window))
  :config
  ;; allow delete single parenthesis
  (define-key lispy-mode-map-lispy (kbd "C-d") nil))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package minions
  :config
  (minions-mode 1)
  :bind
  ([S-down-mouse-3] . minions-minor-modes-menu))

(use-package mood-line
  :hook
  (after-init . mood-line-mode))

;; show emoji
;; (use-package emojify
;;   :hook
;;   (after-init . global-emojify-mode)
;;   :custom
;;   (emojify-display-style 'unicode)
;;   (emojify-download-emojis-p t)
;;   (emojify-emoji-styles '(ascii github unicode)))

(use-package nyan-mode
  :hook
  (doom-modeline-mode . nyan-mode))

;; defer if it's slow
(use-package dashboard
  :if (and (< (length command-line-args) 2)
	   (fboundp 'native-comp-available-p))
  :config
  (setq dashboard-set-init-info nil)
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "I CAN DO ALL THINGS?!	 ಠ_ಠ")
  ;;    (setq dashboard-startup-banner 3)
  (setq dashboard-startup-banner "~/.emacs.d/fancy-splash/world.png")

  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	                  ;; (bookmarks . 5)
			  ;; (projects . 5)
			  ;; (agenda . 5)
			  ;; (registers . 5)
                          )) ;;add org-agenda could slow start-up speed
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; show Dashboard in frames created with emacsclient -c
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (define-key dashboard-mode-map (kbd "n") 'next-line)
  (define-key dashboard-mode-map (kbd "p") 'previous-line))

(use-package highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

(use-package highlight-symbol
  ;; An alternative package is highlight-thing
  :bind
  ("C-<f9>" . highlight-symbol)
  ("<f9>" . highlight-symbol-next)
  ("S-<f9>" . highlight-symbol-prev)
  ("M-<f9>" . highlight-symbol-query-replace))

(use-package minimap
  :custom
  (minimap-window-location 'right)
  (minimap-width-fraction 0.05)
  (minimap-minimum-width 15)
  :bind
  ("<f6>" . minimap-mode))

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq inhibit-compacting-font-caches t))

(use-package treemacs
  :bind
  ("<f8>" . treemacs))

(use-package treemacs-all-the-icons
  :defer t
  :requires
  (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package all-the-icons-dired
  :if window-system
  ;;need to run all-the-icons-install-fonts first to avoid grabled icon
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package ivy-rich
  :after ivy
  :hook
  (ivy-mode . ivy-rich-mode)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-path-style 'absolute)
  :custom
  (ivy-rich-modify-columns
   'ivy-switch-buffer
   '((ivy-rich-switch-buffer-size (:align right)))
   'counsel-M-x
   '((counsel-M-x-transformer (:width 40)))
   ))

(use-package all-the-icons-ivy-rich
  :if window-system
  :after ivy-rich
  :config
  (setq all-the-icons-ivy-rich-icon-size 1.0)
  (setq inhibit-compacting-font-caches t)
  (all-the-icons-ivy-rich-mode t))

;; Enable icons in the ibuffer
(use-package all-the-icons-ibuffer
  :if window-system
  :config
  (setq inhibit-compacting-font-caches t)
  :hook
  (ibuffer-mode . all-the-icons-ibuffer-mode))

(global-tab-line-mode t)
;; (use-package centaur-tabs
;;   :config
;;   (centaur-tabs-headline-match)
;;   (setq centaur-tabs-set-bar 'over)
;;   (setq centaur-tabs-gray-out-icons 'buffer)
;;   ;;(setq centaur-tabs-set-icons t)
;;   (setq centaur-tabs-plain-icons t)
;;   (setq centaur-tabs-close-button "x")
;;   (setq centaur-tabs-set-modified-marker t)
;;   (setq centaur-tabs-modified-marker "*")
;;   (setq centaur-tabs-height 20)
;;   ;;(setq centaur-tabs-label-fixed-length 10) ;;fixed length
;;   ;; (centaur-tabs-change-fonts "Roboto Mono" 130)
;;   (setq centaur-tabs-show-navigation-buttons nil)
;;   :bind
;;   ("C-x <left>" . centaur-tabs-backward)
;;   ("C-x <right>" . centaur-tabs-forward)
;;   :hook
;;   (dired-mode . centaur-tabs-local-mode)
;;   (after-init . centaur-tabs-mode))

;;press your keyboard fast and hard !!!
(use-package power-mode
  :defer t
  :straight (power-mode :type git :host github
			:repo "elizagamedev/power-mode.el"))

;; to display ^L page break
(use-package form-feed
  :hook
  (emacs-lisp-mode . form-feed-mode))

;; (use-package focus
;;   :defer t
;;   :hook
;;   (text-mode . focus-mode)
;;   :config
;;   (add-to-list 'focus-mode-to-thing '((text-mode . sentence)
;; 				      (prog-mode . defun)
;; 				      (latex-mode . paragraph))))

;; (use-package smooth-scroll
;;   :load-path "extra-lisp/"
;;   :hook
;;   (after-init . smooth-scroll-mode)
;;   )


;;; FONT, THEME & COLOR SCHEME

;;English font: Iosevka/Inconsolata/Juliamono/Jetbrains Mono/Roboto Mono/Monaco/Fira Code/SF Mono/Operator Mono
;;Chinese font: Wenquanyi Micro Hei Mono/Sarasa UI SC Mono/Sarasa Mono SC Nerd/Noto Sans CJK SC Mono (work perfectly with Iosevka/Inconsolata)
;;Variable-pitch font, ETBembo/New York
;;Unicode: Symbola

(defun init-font ()
  "Set English and CJK font for Emacs."
  (interactive)
  ;; English font
  (if (display-graphic-p)
      (progn
	;; English font
	(set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "SF Mono" 15))
	;; CJK font
	(dolist (charset '(kana han symbol cjk-misc bopomofo))
	  (set-fontset-font (frame-parameter nil 'font)
			    charset
			    (font-spec :family "Noto Serif SC"))))))

;; Use emacs daemon, put following lines to shell config file
;; alias emacs=/path_2_miniconda3/bin/emacs
;; alias emacsclient=/path_2_miniconda/bin/emacsclient
;; alias ed="emacs --daemon"
;; alias ec="emacsclient -c"
;; alias eq="emacsclient -e '(save-buffers-kill-emacs)'"
;; If you want more fuzzy cmd:
;; alias emcas=emacs
;; alias emasc=emacs
;; alias enacs=emacs

;; Set font and auto-fullscreen in daemon-mode, put after init-ui.el
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (init-font)
		  (auto-max-frame))))
  (add-hook 'after-init-hook 'init-font))

;;A bunch of themes
(use-package solarized-theme :defer t)
(use-package base16-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t )
(use-package gruvbox-theme :defer t )
(use-package tao-theme :defer t )
(use-package humanoid-themes :defer t )
(use-package twilight-bright-theme :defer t )
(use-package ample-theme :defer t )
(use-package eziam-theme :defer t ) ;;almost perfect light theme
(use-package spacemacs-common :defer t :straight spacemacs-theme)
(use-package doom-themes
  :defer t
  :config
  ;;treemacs setting
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (setq doom-themes-treemacs-theme "doom-color")
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package apropospriate-theme
  :defer t
  :custom
  (apropospriate-mode-line-height 1.0))

;; loading default theme
(setq custom-safe-themes t)
(setq-default custom-enabled-themes '(humanoid-light))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes ',custom-enabled-themes)))

(add-hook 'after-init-hook 'reapply-themes)

;; Toggle between light and dark
(defun light-theme ()
  "Activate a light color theme.
  Recommendation:
     solarized-light, leuven, spacemacs-light, eziam, twilight-bright, modus-operandi, doom-homage-white, doom-tomorrow-day."
  (interactive)
  (setq custom-enabled-themes '(twilight-bright))
  (reapply-themes))

(defun night-theme ()
  "Activate a dark color theme.
  Recommendation: humanoid-dark, doom-city-light, doom-xcode
  doom-one/vibrant, doom-dark+, sanityinc-tomorrow-night, doom-wilmersdorf,
  doom-badge, doom-laserwave, doom-shades-of-purple"
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-night))
  (reapply-themes))

;;Transprancy setting
(set-frame-parameter (selected-frame) 'alpha '(97 100))
(add-to-list 'default-frame-alist '(alpha 97 100))

;;Font Setting
(setq inhibit-compacting-font-caches t)

;;Varialble/fixed pictch font setting, essential for org-mode
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Noto Serif SC" :height 160))))
 '(fixed-pitch ((t ( :family "Roboto Mono" :height 150)))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange" :height 1.2))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-link ((t (:foreground "light blue" :underline t))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8)))))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.2 :weight bold :background "#F2F3F4" :overline "#A7A6AA" :foreground "#455F39"))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

(custom-set-faces
 '(org-block-begin-line
   ((t (:foreground "#787787" :background "#D6DCD9" :underline "#A7A6AA" :extend nil))))
 '(org-block
   ((t (:background "#A6B4A1"))))
 '(org-block-end-line
   ((t (:foreground "#008ED1" :background "#EAEAFF" :overline "#A7A6AA" :extend t :underline nil))))
 '(org-meta-line
   ((t (:background "#E7E7E7"))))
 )

;;Unicode font setting
(when (member "Symbola" (font-family-list))
  (set-fontset-font "fontset-default" nil
		    (font-spec :size 16 :name "Symbola")))

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))


;;; PROGRAMMING LANGUAGES & LSP

;;==============================
;;            Eglot           ;;
;;==============================
;;eglot can work with tramp-mode, but you should install
;;your server-programs on remote, not local
(use-package eglot
  :config
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1))) ;;Decouple flymake and eglot
  ;;============================================
  ;; make sure every command works separately in shell environment
  (set 'ad-redefinition-action 'accept)
  ;; install clangd first
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) ("clangd")))
  ;; pip3 install fortran-language-server
  (add-to-list 'eglot-server-programs '(f90-mode . ("fortls")))
  ;; use tex-lab or digestif as TeX server
  (add-to-list 'eglot-server-programs '((LaTeX-mode tex-mode context-mode texinfo-mode bibtex-mode)
					. ("texlab")))
  ;; pip3 install python-lsp-server
  ;; jupterlab has some experimental lsp server, install and change it above: pip3 install git+https://github.com/krassowski/python-language-server.git@main
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  ;; install.packages("languageserver")
  ;; Note R can be tricky in zsh due to the built-in command "r"
  ;; more R lsp server setting can be done in .Rprofile or .lintr file
  (add-to-list 'eglot-server-programs '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()")))
  ;;============================================
  :hook
  (python-mode . eglot-ensure)
  (f90-mode . eglot-ensure)
  (ess-r-mode . eglot-ensure)
  (LaTeX-mode . eglot-ensure)
  ;;============================================
  ;;local keybindings
  :bind
  (:map eglot-mode-map
	("C-c r" . eglot-rename)
	("C-c h" . eldoc))
  ;;or add follwing lines to :config section
  ;;(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  ;;(define-key eglot-mode-map (kbd "C-c h") 'eldoc)
  )

;; A lsp for grammarly, still in early development
;; (use-package eglot-grammarly
;;   :straight (:type git :host github
;; 		   :repo "emacs-grammarly/eglot-grammarly")
;;   :hook (text-mode . (lambda ()
;;                        (require 'eglot-grammarly)
;;                        (call-interactively #'eglot))))

;;==============================
;;           Python           ;;
;;==============================

;; Interpreter choice
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
;;(setq python-shell-interpreter "python3.9")

;;python-style indent
(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset-verbose nil) ;;don't complain about the indent anymore
(setq python-indent-guess-indent nil)
(setq indent-tabs-mode t) ;; whether tabs are used for indentation

;;debug setting
(setq python-shell-completion-native-enable nil) ;;or pip3 install pyreadline to avoid warning
(setq python-shell-prompt-detect-failure-warning nil)
(setq python-shell-enable-font-lock nil) ;;make printing fast

;;A dirty solution of showing inferior-python input codes
;;from https://github.com/jorgenschaefer/elpy/issues/924
(defun python-shell-append-to-output (string)
  (let ((buffer (current-buffer)))
    (set-buffer (process-buffer (python-shell-get-process)))
    (let ((oldpoint (point)))
      (goto-char (process-mark (python-shell-get-process)))
      (insert string)
      (set-marker (process-mark (python-shell-get-process)) (point))
      (goto-char oldpoint))
    (set-buffer buffer)))

(defadvice python-shell-send-string
    (around advice-python-shell-send-string activate)
  (interactive)
  (let* ((append-string1
          (if (string-match "import codecs, os;__pyfile = codecs.open.*$" string)
              (replace-match "" nil nil string)
            string))
         (append-string2
          (if (string-match "^# -\\*- coding: utf-8 -\\*-\n*$" append-string1)
              (replace-match "" nil nil append-string1)
            append-string1))
         (append-string
          (if (string-match "^\n*$" append-string2)
              (replace-match "" nil nil append-string2)
            append-string2)))
    (python-shell-append-to-output
     (concat (string-trim-right append-string) "\n")))
  (if (called-interactively-p 'any)
      (call-interactively (ad-get-orig-definition 'python-shell-send-string))
    ad-do-it))

;;python-mode local keybinding
(with-eval-after-load 'python
  (defun python-run-current-line ()
    "a wrapper of python-shell-send-statement"
    (interactive)
    (python-shell-send-statement)
    (forward-line))
  (define-key python-mode-map (kbd "C-<return>") 'python-run-current-line)
  (define-key inferior-python-mode-map (kbd "C-l") 'comint-clear-buffer)
  ;; (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inferior-python-mode-map (kbd "C-p") 'comint-previous-input)
  ;; (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-input)
  (define-key inferior-python-mode-map (kbd "C-n") 'comint-next-input))

(use-package conda
  :config
  (conda-env-initialize-interactive-shells)
  (cond ((file-directory-p "~/miniconda3")
	 (setq conda-anaconda-home (expand-file-name "~/miniconda3")
	       conda-env-home-directory (expand-file-name "~/miniconda3")))
	((file-directory-p "~/miniforge3")
	 (setq conda-anaconda-home (expand-file-name "~/miniforge3")
	       conda-env-home-directory (expand-file-name "~/miniforge3")))
	(((eq system-type 'darwin))
	 (setq conda-anaconda-home "/opt/homebrew/Caskroom/miniforge"
	       conda-env-home-directory "/opt/homebrew/Caskroom/miniforge")))

  ;; when in conda-project-env-name or has environmental.yml auto activate
  (conda-env-autoactivate-mode t)
  :bind
  ("C-c c a" . conda-env-activate)
  ("C-c c d" . conda-env-deactivate))

;;jupyter notebook integration

;;>>> option 1
;; (use-package jupyter
;;  :defer t)
;;1. Require Emacs with module-support
;;2. run `pip install ipykernel` `python -m ipykernel install --user`
;;3. This package use zmq which makes Emacs very slow

;;>>> option 2
;; specify jupyter kernel in kernel.json file
;; if you don't know its path, run !jupyter kernelspec list in ipython
;; Other checking commands:
;; import sys; print(sys.executable); print(sys.path)
;; I also recommend to use mamba to manage packages
;; ---org-babel snippet---
;;#+BEGIN_SRC ein-python :session localhost:8889
;;#+END_SRC
;; Change cell type by C-c C-t

(use-package ein
  :defer 1
  :config
  ;;ein-babel config see the org-mode block
  (setq ein:use-company-backend t)
  (setq ein:worksheet-enable-undo t)
  (setq ein:output-area-inlined-images t)
  (add-hook 'poly-ein-mode-hook 'elpy-enable)
  (add-hook 'poly-ein-mode-hook (lambda ()
				  (display-line-numbers-mode nil))) ;;avoid grabled line-number
  (with-eval-after-load 'ein-notebook
    (define-key ein:notebook-mode-map "\C-c\C-d" 'ein:worksheet-delete-cell))
  (setq ein:worksheet-enable-undo t))

(use-package elpy :defer t) 		;;a completion system for ein

;;==============================
;;           Rlang            ;;
;;==============================
;; require ESS installed
;;Lazy load ess-r-mode (ESS doesn't like use-package pretty much)
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(use-package ess
  :defer t
  :custom
  (inferior-ess-r-program "R")
  )

(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))
(with-eval-after-load 'ess-r-mode
  (defun ess-insert-pipe ()
    "Insert a R pipe (%>%)"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (just-one-space 1)
    ;;(reindent-then-newline-and-indent)
    )

  ;; disable flycheck because lsp has linter already
  (add-hook 'ess-r-mode-hook (lambda () (flycheck-mode -1)))

  (defun ess-clear-REPL-buffer ()
    "Clear outputs in the REPL buffer"
    (interactive)
    (let ((r-repl-buffer (seq-find (lambda (buf)
				     (string-prefix-p "*R" (buffer-name buf)))
				   (buffer-list))))
      (if r-repl-buffer
	  (with-current-buffer r-repl-buffer
	    (comint-clear-buffer))
	(user-error "No R REPL buffers found"))))

  (define-key ess-r-mode-map (kbd "C-l") 'ess-clear-REPL-buffer)
  (define-key inferior-ess-r-mode-map (kbd "C-l") 'ess-clear-REPL-buffer) ;;inferior-* is the shell one
  (define-key ess-r-mode-map (kbd "M--") 'ess-insert-assign)
  (define-key inferior-ess-r-mode-map (kbd "M--") 'ess-insert-assign)
  (define-key ess-r-mode-map (kbd "M-p") 'ess-insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "M-p") 'ess-insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "C-p") 'comint-previous-input)
  ;; (define-key inferior-ess-r-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key inferior-ess-r-mode-map (kbd "C-n") 'comint-next-input)
  ;; (define-key inferior-ess-r-mode-map (kbd "<down>") 'comint-next-input)
  )

;; view R data frame
;; https://github.com/ShuguangSun/ess-view-data
(use-package ess-view-data
  :defer t)

;;C-c C-a to turn on csv-align-fields
(use-package csv-mode
  :mode
  "\\.csv\\'"
  "\\.CSV\\'")

;;display color of RGB code
(use-package rainbow-mode
  :hook
  (ess-r-mode . rainbow-mode)
  (js-mode . rainbow-mode))

;;;;;;;;;;;;
;; Matlab ;;
;;;;;;;;;;;;

;; cd /path/to/matlab-emacs -> make
;; Homepage: https://sourceforge.net/p/matlab-emacs/src/ci/documentation/tree/
(use-package matlab-mode
  :defer t
  :mode "\\.[mM]\\'")

;;;;;;;;;;
;; Yaml ;;
;;;;;;;;;;

(use-package yaml-mode
  :mode "\\.yml\\'"
  :bind
  (:map yaml-mode-map
	("\C-m" . newline-and-indent)))


;;; Text-mode: Markdown/org-mode/TeX

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;

;; Major mode for markdown
;; preview included but reply on multimarkdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . (lambda ()
		     (display-line-numbers-mode -1)
		     (visual-line-mode 1))))

;; Advanced preview
;; Dependency: npm/node, livedown npm package
(use-package emacs-livedown
  :after markdown-mode
  :straight (:type git
		   :host github
		   :repo "shime/emacs-livedown")
  :custom
  (livedown-autostart t) ; automatically open preview when opening markdown files
  (livedown-open t)	 ; automatically open the browser window
  (livedown-port 1337)	 ; port for livedown server
  (livedown-browser nil) ; browser to use
  :bind
  (:map markdown-mode-map
	("C-c c p" . livedown-preview)
	("C-c c k" . livedown-kill)))

;;add table of content for md/org
;;Add :TOC: tag for org (C-c C-c) and <-- :TOC: --> for md
;;then toc-org-insert-toc
(use-package toc-org
  :hook
  (markdown-mode . toc-org-mode)
  (org-mode . toc-org-mode)
  :config
  (global-set-key (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
  (add-to-list 'org-tag-alist '("TOC" . ?T)))

;;;;;;;;;;;;;;
;; Org-mode ;;
;;;;;;;;;;;;;;
(use-package smartparens
  :ensure t
  :hook
  (xenops-mode . smartparens-mode)
  (org-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'org-mode "\\[" " \\]")
  (sp-local-pair 'org-mode "\\{" " \\}")
  (sp-local-pair 'LaTeX-mode "$" "$")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'xenops-mode "$" "$")
  (sp-local-pair 'org-cdlatex-mode "$" "$")
  (sp-local-pair 'org-mode "'" "'" :actions '(rem))
  (sp-local-pair 'org-mode "=" "=" :actions '(rem))
  (sp-local-pair 'org-mode "\\left(" "\\right)" :trigger "\\l(" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left[" "\\right]" :trigger "\\l[" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left\\{" "\\right\\}" :trigger "\\l{" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left|" "\\right|" :trigger "\\l|" :post-handlers '(sp-latex-insert-spaces-inside-pair)))




;; special arrow \to
(use-package org
  :straight (:type built-in)
  :after counsel
  :config
  (setq org-startup-indented t
	org-startup-with-inline-images t
	org-hide-emphasis-markers t
	org-latex-prefer-user-labels t
	org-image-actual-width nil)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@")))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/Documents/org/inbox.org" "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+datetree "~/Documents/org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-default-notes-file "~/Documents/org/notes.org")
  (setq org-archive-location "~/Documents/org/archives.org::* From %s")
  (setq org-agenda-files '("~/Documents/org/agenda.org"
			   ;; "~/Documents/org/gcal.org"
			   ))
  (setq org-agenda-include-diary t)


  ;; not display _ and ^ as sub/superscript
  (setq org-use-sub-superscripts nil)

  ;;src setting
  (setq org-src-fontify-natively t)

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  (setq org-latex-listings 'minted)
  (setq org-export-latex-listings 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")
				   ("fontsize" "\\scriptsize")))

  (require 'ox-latex )
  ;; sett latex to xelatex
  ;; (setq org-latex-pdf-process
  ;; 	'("xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
  ;; 	  "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
  ;; 	  "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"))
  (setq org-latex-pdf-process
           '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
             "bibtex %b"
	     ;; "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
             ;; "biber %b"
             "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
             "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; (setq org-latex-pdf-process
  ;;     '("pdflatex -interaction nonstopmode -output-directory %o %f"
  ;;       "biber %b"
  ;;       "pdflatex -interaction nonstopmode -output-directory %o %f"
  ;;       "pdflatex -interaction nonstopmode -output-directory %o %f"))
  ;; Mimore class is a latex class for writing articles.
  (add-to-list 'org-latex-classes
               '("mimore"
                 "\\documentclass{mimore}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; Mimosis is a class I used to write my Ph.D. thesis.
  (add-to-list 'org-latex-classes
               '("mimosis"
                 "\\documentclass{mimosis}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]
\\newcommand{\\mboxparagraph}[1]{\\paragraph{#1}\\mbox{}\\\\}
\\newcommand{\\mboxsubparagraph}[1]{\\subparagraph{#1}\\mbox{}\\\\}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\mboxparagraph{%s}" . "\\mboxparagraph*{%s}")
                 ("\\mboxsubparagraph{%s}" . "\\mboxsubparagraph*{%s}")))

  ;; Elsarticle is Elsevier class for publications.
  (add-to-list 'org-latex-classes
               '("elsarticle"
                 "\\documentclass{elsarticle}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  (load "~/.emacs.d/.local/straight/repos/org-mode/lisp/ox-extra.el" t t)
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))
  :custom
  (org-support-shift-select 'always)
  (org-babel-load-languages '((emacs-lisp . t)
			      (python . t)
			      (R . t)
			      (latex . t)))

  ;;local keybinding

  :bind
  (:map org-mode-map
	("s-<up>" . org-previous-visible-heading)
	("s-<down>" . org-next-visible-heading)
	("C-c a" . org-agenda)
	("C-c c" . org-capture)
	("C-c C-r" . org-archive-subtree)
	("C-c t" . counsel-org-tag)
	("C-c l" . counsel-org-link)
	("C-x j" . org-cdlatex-mode)
	("C-c k" . org-mark-ring-goto))




  :hook
  (org-mode . (lambda ()
		(variable-pitch-mode 1)
		(visual-line-mode 1)
		(display-line-numbers-mode -1)
		(org-toggle-pretty-entities)
		(flycheck-mode 1)
		(smartparens-mode 1)
		(company-mode 1)
		)))

(add-hook 'org-log-buffer-setup-hook
	  (lambda()
	    (setq ispell-local-dictionary "en_US")))
(add-hook 'org-mode-hook
	  (lambda ()
            (LaTeX-add-environments "equation")
	    (LaTeX-add-environments "equation*")
	    (LaTeX-add-environments "lemma")
	    (LaTeX-add-environments "theorem")
	    (LaTeX-add-environments "example")
	    (LaTeX-add-environments "Remark")
	    (LaTeX-add-environments "proposition")))
(add-hook 'org-beamer-mode-hook
	  (lambda()
	    (add-to-list 'org-beamer-environments-extra
			 '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))
	    ;; export snippet translations
	    (add-to-list 'org-export-snippet-translation-alist
			 '("b" . "beamer"))))


;; stop org from prettifying sub and superscripts
(add-hook 'org-cdlatex-mode-hook
	  (lambda () (when (eq major-mode 'org-mode)
		       (make-local-variable 'org-pretty-entities-include-sub-superscripts)
		       (setq org-pretty-entities-include-sub-superscripts nil))))


(defun my-org-mark-ring-goto ()
  (interactive)
  (org-mark-ring-goto 2))


(use-package scimax-latex
  :straight (scimax-latex :type git
                          :host github
                          :repo "jkitchin/scimax"
                          :files ("scimax-latex.el"))
  :commands (scimax-latex-setup
             kpsewhich
             texdoc))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(require 'org-agenda)
;; agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;;(setq org-agenda-compact-blocks t)
;;(setq org-agenda-span 'day)
;; (setq org-agenda-span 14)

(setq org-agenda-compact-blocks nil)


;; this is to have several buffers (e.g., review and day/week mode)
(setq org-agenda-sticky t)
(setq calendar-week-start-day 1) ;; start Monday

;; opens agenda in current window!
;; (setq org-agenda-window-setup 'current-window)

;; various
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-deadline-warning-days 15)
(setq org-agenda-start-on-weekday nil)
(setq org-reverse-note-order t)
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)


;; custom agenda views
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))


;;
(defun org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "R"))


(defun org-agenda-show-projects (&optional arg)
  (interactive "P")
  (org-agenda arg "P"))

(defun org-agenda-show-agenda (&optional arg)
  (interactive "P")
  (org-agenda arg "X"))

;;French holidays
(setq aus-holiday
      '((holiday-fixed 1 1 "New Year's Day")
        (holiday-fixed 1 26 "Australia Day")
	(holiday-easter-etc -2 "Good Friday")
        (holiday-easter-etc 1 "Easter Monday")
	(holiday-easter-etc 2 "Easter Tuesday")
        (holiday-fixed 2 14 "Valentine's Day")
        (holiday-fixed 4 25 "ANZAC Day")
        (holiday-fixed 10 31 "Halloween")
        (holiday-fixed 12 24 "Christmas Eve")
        (holiday-fixed 12 25 "Christmas Day")
        (holiday-fixed 12 26 "Boxing Day")
        (holiday-fixed 12 31 "New Year's Eve")
        ;; (holiday-float 6 0 3 "Fête des pères") ;; troisième dimanche de juin
        ;; ;; Fête des mères
        ;; (holiday-sexp
        ;;  '(if (equal
        ;;        ;; Pentecôte
        ;;        (holiday-easter-etc 49)
        ;;        ;; Dernier dimanche de mai
        ;;        (holiday-float 5 0 -1 nil))
        ;;       ;; -> Premier dimanche de juin si coïncidence
        ;;       (car (car (holiday-float 6 0 1 nil)))
        ;;     ;; -> Dernier dimanche de mai sinon
        ;;     (car (car (holiday-float 5 0 -1 nil))))
        ;;  "Fête des mères")
	))

(setq calendar-holidays (append aus-holiday)
      calendar-mark-diary-entries-flag nil) ;calendar-mark-holidays-flag t

;; help
(defun show-org-help ()
  (interactive)
  (message-box "
ORG
---
<f3>:\t\t\t export agenda to ICS
<f4>:\t\t\t update todo
<f5>:\t\t\t update category
<f6>:\t\t\t update URL (C-c F6 to go)
C-c <f10>:\t\t\t calendar
<f9>:\t\t\t archive
C-u C-c C-l:\t\t create file link
C-S-mouse-1:\t open link in new frame / open mu4e mail
"))

(defun show-org-agenda-help ()
  (interactive)
  (message-box "\nAGENDA\n------\nr:   refresh+export (R: refresh)\nb,f: dates before/after\n.:   today\nj:   jump\n"))


;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("C" "clojure" "sh" "R" "latex"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("rr" . "src R"))
  ;; open org export pdf in Skim
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "open -a Skim %s"))
  )

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(use-package org-super-agenda
  :hook (after-init . org-super-agenda-mode)
  :init
  ;;Disable the super agenda header map.
  ;;don't do if using specific shortcuts
  (require 'org-habit)
  (setq org-super-agenda-header-map nil)
  (setq org-agenda-start-day nil)

  (setq org-agenda-block-separator ?\═) ;line

  (defun my-org-hour-specification-p (item)
    (s-matches? "[0-9+:hdwmy/ 	.-]" item))

  (setq org-agenda-custom-commands
        '(("R" "Review"
           (
            (todo "ACTIVE!" ((org-agenda-overriding-header (concat "Active projects -- pending actions"))))
            (todo "WAITING" ((org-agenda-overriding-header (concat "Waiting items"))))
            (todo "HOLD!" ((org-agenda-overriding-header (concat "Projects on hold -- no pending actions"))))
            ;;(todo "STARTED" ((org-agenda-overriding-header (concat "ITEMS in progress"))))
                                        ;(agenda "" ((org-agenda-ndays 21))) ;; review upcoming deadlines and appointments
                                        ;                                 ;; type "l" in the agenda to review logged items
            ;;(agenda "" ((org-super-agenda-groups '((:auto-group t)))) (org-agenda-list)) ;;(:auto-group t) (:name "Scheduled Today" :scheduled today)
            (agenda "" ((org-super-agenda-groups '(
                                                   (:name "⏰ Today               " :time-grid t)
                                                   (:name "⭐⭐⭐  Due Today ⭐⭐⭐    " :deadline today :scheduled today)
                                                   (:name "⇒ Due soon             " :deadline future)
                                                   (:name "⚠ OVERDUE! ⚠           " :deadline past)
                                                   (:name "⇒ Consider soon        " :scheduled future)
                                                   (:name "⚠ Behind               " :and (:scheduled past :not (:category "catchup")))
                                                   (:name "⚠ Catchup              " :category "catchup")
                                                   )))
                    (org-agenda nil "a")
                    )
            (todo "TODO" ((org-agenda-overriding-header (concat "All to-do ITEMS")))) ;; review waiting items
            (todo "1DAY!" ((org-agenda-overriding-header (concat "Inactive projects"))))
            ;;(stuck "") ;; review stuck projects as designated by org-stuck-projects
            ;; ...other commands here
            )
           )
          ("P" "Projects"
           (
            (todo "ACTIVE!" ((org-agenda-overriding-header (concat "Active projects -- pending actions"))))
            (todo "WAITING" ((org-agenda-overriding-header (concat "Waiting items"))))
            (todo "HOLD!" ((org-agenda-overriding-header (concat "Projects on hold -- no pending actions"))))
            ;;(todo "STARTED" ((org-agenda-overriding-header (concat "ITEMS in progress"))))
                                        ;(agenda "" ((org-agenda-ndays 21))) ;; review upcoming deadlines and appointments
                                        ;                                 ;; type "l" in the agenda to review logged items
            ;;(agenda "" ((org-super-agenda-groups '((:auto-group t)))) (org-agenda-list)) ;;(:auto-group t) (:name "Scheduled Today" :scheduled today)
            (todo "TODO" ((org-agenda-overriding-header (concat "All to-do ITEMS")))) ;; review waiting items
            (todo "1DAY!" ((org-agenda-overriding-header (concat "Inactive projects"))))
            ;;(stuck "") ;; review stuck projects as designated by org-stuck-projects
            ;; ...other commands here
            )
           )

          ("A" "Agenda"
           (
            (agenda "" ((org-super-agenda-groups '(
                                                   (:name "⏰ Today              " :and (:time-grid t :not (:scheduled future)))
                                                   (:name "⭐⭐⭐ Due Today ⭐⭐⭐    " :deadline today :scheduled today)
                                                   (:name "⇒ Due soon            " :deadline future)
                                                   (:name "⚠ OVERDUE! ⚠          " :deadline past)
                                                   (:name "⇒ Consider soon       " :scheduled future)
                                                   (:name "⚠ Behind              " :and (:scheduled past :not (:category "catchup")))
                                                   (:name "⚠ Catchup             " :category "catchup")
                                                   )))
                    (org-agenda nil "a")
                    )	  )
           )


          ))

  )
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;               NEW
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(use-package xenops
  :after org
  :bind (
         (("C-c C-g C-c" . xenops-reveal-at-point))
         (("C-c C-g C-i" . xenops-increase-size)))
  :config
  (setq xenops-math-image-scale-factor 1.8)
  )
(use-package pdf-tools
  :ensure t
  :commands (pdf-view-mode pdf-loader-install)
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (define-pdf-cache-function pagelables)
  ;;In case of high-resolution screen like Mac
  (setq pdf-view-use-scaling t
	pdf-view-use-imagemagick nil)
  :hook
  (pdf-view-mode . (lambda ()
		     (display-line-numbers-mode -1)))
  (pdf-view-mode . pdf-tools-enable-minor-modes)
  )
;; (setq doc-view-continuous t)

;; (use-package org-pdfview
;;   :ensure t)

;; (eval-after-load 'org '(require 'org-pdfview))

;; (add-to-list 'org-file-apps
;;              '("\\.pdf\\'" . (lambda (file link)
;;                                      (org-pdfview-open link))))

;; (defun md-compile ()
;;   "Compiles the currently loaded markdown file using pandoc into a PDF"
;;   (interactive)
;;   (save-buffer)
;;   (shell-command (concat "pandoc " (buffer-file-name) " -o "
;;                          (replace-regexp-in-string "md" "pdf" (buffer-file-name)))))

(defun update-other-buffer ()
  (interactive)
  (other-window 1)
  (pdf-view-revert-buffer nil t)
  (image-toggle-hex-display)
  (hexl-mode-exit)
  (other-window -1))

;; (defun md-compile-and-update-other-buffer ()
;;   "Has as a premise that it's run from a markdown-mode buffer and the
;;    other buffer already has the PDF open"
;;   (interactive)
;;   (md-compile)
;;   (update-other-buffer))

(defun latex-compile-and-update-other-buffer ()
  "Has as a premise that it's run from a latex-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (save-buffer)
  (shell-command (concat "pdflatex " (buffer-file-name)))
  (switch-to-buffer (other-buffer))
  (kill-buffer)
  ;; (update-other-buffer)
  )

(defun org-compile-beamer-and-update-other-buffer ()
  "Has as a premise that it's run from an org-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (org-beamer-export-to-pdf)
  ;; (update-other-buffer)
  )

(defun org-compile-latex-and-update-other-buffer ()
  "Has as a premise that it's run from an org-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (org-latex-export-to-pdf)
  ;; (update-other-buffer)
  )

(eval-after-load 'TeX-mode
  '(define-key TeX-mode-map (kbd "C-c k") 'latex-compile-and-update-other-buffer))

(define-key org-mode-map (kbd "C-c h r") 'org-compile-latex-and-update-other-buffer)
(define-key org-mode-map (kbd "C-c h b") 'org-compile-beamer-and-update-other-buffer)
(define-key org-mode-map (kbd "C-c k") 'my-org-mark-ring-goto)


(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "C-c r") 'md-compile-and-update-other-buffer))


;; inkscape-figure-manager
(defun inkscape-watch ()
  "watch for figures"
  (interactive)
  (shell-command "inkscape-figures watch"))

(defun inkscape-create ()
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'word)
    (let ((str (buffer-substring-no-properties beg end)))
      (shell-command (concat "inkscape-figures create " str " " (file-name-directory (buffer-file-name)) "/figures"))
      )))

(defun inkscape-edit ()
  "watch for figures"
  (interactive)
  (shell-command (concat "inkscape-figures edit " (file-name-directory (buffer-file-name)) "figures/")))

(define-key org-mode-map (kbd "C-c h w") 'inkscape-watch);
(define-key org-mode-map (kbd "C-c h c") 'inkscape-create);
(define-key org-mode-map (kbd "C-c h e") 'inkscape-edit);


(use-package org-pdftools
  :after pdf-tools
  :config (setq org-pdftools-root-dir (concat org-directory "prints")
                org-pdftools-search-string-separator "??")
  (with-eval-after-load 'org
    (org-link-set-parameters "pdftools"
                             :follow #'org-pdftools-open
                             :complete #'org-pdftools-complete-link
                             :store #'org-pdftools-store-link
                             :export #'org-pdftools-export)
    (add-hook 'org-store-link-functions 'org-pdftools-store-link)))

(use-package org-noter
  :after pdf-tools
  :commands (org-noter)
  :after (org)
  :config
  (with-eval-after-load 'pdf-tools
    (setq pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))
  (setq org-noter-notes-mode-map (make-sparse-keymap)))

(use-package org-noter-pdftools
  :after pdf-tools
  :after (org-noter)
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
;; }} org-noter



;;use org-superstar-mode to replace org-bullets
(use-package org-superstar
  :after org
  :config
  (setq org-superstar-special-todo-items t)
  :hook
  (org-mode . org-superstar-mode)
  :custom
  (org-ellipsis "⚡"))

;;prettify-symbols-mode setting
(add-hook 'org-mode-hook 'prettify-symbols-mode)
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "➤")
				       ("#+END_SRC" . "➤")
				       ("#+begin_src" . "➤")
				       ("#+end_src" . "➤")
				       ("#+begin_example" . "⁈")
				       ("#+end_example" . "⁈")
				       (">=" . "≥")
				       ("=>" . "⇨")
				       ("[-]" . "❍" )
				       ("[ ]" .  "☐")
				       ("[X]" . "☑" )))
(setq prettify-symbols-unprettify-at-point 'right-edge)

(use-package org-fancy-priorities
  :defer t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("❗❗❗" "❗❗" "❗")))

;;Image drag-and-drop for org-mode
(use-package org-download
  :after org
  :hook
  (dired-mode . org-download-enable))

;;(use-package org-super-agenda)
(use-package org-graph-view
  :defer t
  :straight (org-graph-view :type git :host github
			    :repo "alphapapa/org-graph-view"))

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package org-mind-map
  :defer t
  :straight (org-mind-map :type git :host github
			    :repo "the-ted/org-mind-map")
  :config
  (setq org-mind-map-engine "dot")	; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  )

;; A personal knowlege database tool, lateral linking
;; require sqlite, check org-oram--sqlite-available-p
(use-package emacsql-sqlite3
  :ensure t)
(use-package org-roam
  :defer t
  :custom
  (org-roam-directory "~/Documents/roamnotes")
  (add-hook 'after-init-hook 'org-roam-mode)
  (org-roam-capture-templates
   '(("m" "main" plain
      "%?"
      :if-new (file+head "main/${slug}.org"
			 "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("r" "reference" plain
      "\n* Source\nTitle: ${Title}\nAuthor: %^{Author}\n"
      :if-new
      (file+head "reference/${title}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("a" "article" plain "%?"
      :if-new
      (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
      :immediate-finish t
      :unnarrowed t)))

  :init
  (setq org-roam-v2-ack t)

  :bind
  (("C-c n l" . org-roam-buffer-toggle) ;;Backlink
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n s" . org-roam-db-autosync-mode)
   :map org-mode-map
   ("C-M-i" . completion-at-point)
   ;;create an ID for heading
   ("C-c n k" . org-id-get-create))

  :config
  (setq org-roam-database-connector 'sqlite3)
  (org-roam-setup)
  (setq org-roam-complete-everywhere t)
  )


(straight-use-package '(simple-httpd :type git :host github :repo "ruiying-ocean/simple-httpd" :local-repo "simple-httpd"))

;; a mindmap-like visualiser for org-roam
(use-package org-roam-ui
  :defer t
  :requires (simple-httpd)
  :straight
  (:host github :repo "org-roam/org-roam-ui"
	 :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; perfectly alian English/CJK fonts in the same table
(use-package valign
  :hook (org-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))

;; ==============================
;;             LaTeX           ;;
;; ==============================
(setq font-latex-fontify-script nil)
;; company
(use-package company-math)
(use-package company-auctex)
(use-package company-reftex)


;; AucTeX settings - almost no changes
(use-package auctex
  :ensure t
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
         ("C-S-e" . latex-math-from-calc))
  :config
  (message "auctex loaded")
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad)))))))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq Tex-file-line-error t)
  (setq-default TeX-master nil)
  (setq-default TeX-engine XeTex) ;;default engine

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              ;;(setq TeX-command-default "latexmk")
              (rainbow-delimiters-mode t)
              (company-mode t)
              (smartparens-mode t)
              (reftex-isearch-minor-mode t)
	      ;; (LaTeX-add-environments "equation*")
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'auto)
	      (setq TeX-source-correlate-mode t)
              (setq TeX-source-correlate-start-server t))))

  ;; (setq TeX-command-extra-options "-shell-escape")


;; CDLatex settings
;; (use-package cdlatex
;;   :ensure t
;;   :hook (LaTeX-mode . turn-on-cdlatex)
;;   :bind (:map cdlatex-mode-map
;;               ("<tab>" . cdlatex-tab))
;;   )

;; Yasnippet settings
(use-package yasnippet
  :ensure t
  :hook ((LaTeX-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

  (setq yas-triggers-in-field t)

  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas
;; fields
(use-package cdlatex
  :defer t
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field)
	 (LaTeX-mode . turn-on-cdlatex))
  :config
  (setq cdlatex-sub-super-scripts-outside-math-mode nil)
  (setq cdlatex-math-modify-alist '((?d "\\mathbb" nil t nil nil)
				    (?f "\\mathfrak" nil t nil nil)
				    (?s "\\mathscr" nil t nil nil)
				    (?l "\\textsl" nil t nil nil)
				    (?t "\\text" nil t nil nil)))
  (setq cdlatex-math-symbol-alist '((?. ("\\cdot" "\\circ"))
				    (?i ("\\in" "\\not\\in" "\\imath"))
				    (?j (nil nil "\\jmath"))
				    (?F ("\\Phi"))
				    (?9 ("\\cap" "\\bigcap"))
				    (?+ ("\\cup" "\\bigcup"))
				    (?V ("\\Vee"))
				    (?W ("\\Xi" "\\Wedge"))
				    (?w ("\\xi" "\\wedge"))))


  (use-package yasnippet
    :bind (:map yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB" . yas-next-field-or-cdlatex)
	   :map cdlatex-mode-map
	   ("<tab>" . cdlatex-tab))
    :config
    ;; (add-to-list 'cdlatex-math-modify-alist
    ;; 	       '(?d "\\mathbb" nil t nil nil))
    ;; (add-to-list 'cdlatex-math-modify-alist
    ;; 	       '(?f "\\mathfrak" nil t nil nil))
    ;; (Add-to-list 'cdlatex-math-modify-alist
    ;; 	       '(?l "\\textsl" nil t nil nil))
    ;; (add-to-list 'cdlatex-math-symbol-alist
    ;; 	       '(?F ("\\Phi")))
    ;; (add-to-list 'cdlatex-math-symbol-alist
    ;; 	       '(?. ("\\cdot" "\\circ")))
    ;; (add-to-list 'cdlatex-math-symbol-alist
    ;; 		 '(?9 ("\\cap" "\\bigcap")))
    ;; (add-to-list 'cdlatex-math-symbol-alist
    ;; 		 '(?+ ("\\cup" "\\bigcup")))
    ;; (add-to-list 'cdlatex-math-symbol-alist
    ;; 	       '(?V ("\\Vee")))
    ;; (add-to-list 'cdlatex-math-symbol-alist
    ;; 	       '(?W ("\\Xi" "\\Wedge")))
    ;; (add-to-list 'cdlatex-math-symbol-alist
    ;; 	       '(?w ("\\x" "\\wedge")))

    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

(defvar my/bib-file-location "~/Documents/monash/writing/ref/references.bib"
  "Where I keep my bib file.")

(use-package ivy-bibtex
  :after org-ref
  :init
  (setq bibtex-completion-bibliography '("~/Documents/monash/writing/ref/references.bib"
					 ;; "~/Dropbox/emacs/bibliography/dei.bib"
					 ;; "~/Dropbox/emacs/bibliography/master.bib"
					 ;; "~/Dropbox/emacs/bibliography/archive.bib"
					 )
	bibtex-completion-library-path '("~/Documents/monash/reference_papers/")
	;; bibtex-completion-notes-path "~/Dropbox/emacs/bibliography/notes/"
	bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	bibtex-completion-pdf-open-function
	(lambda (fpath)
	  (call-process "open" nil 0 nil "-a" "Skim" fpath)))
  )

(require 'bibtex)

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)

;; ox-pandoc needed for org-ref
(use-package ox-pandoc
  :ensure t)

(use-package org-ref
  :after org
  :straight (:type git :host github
		   :repo "jkitchin/org-ref")
  ;; :load-path (lambda () (expand-file-name "org-ref" scimax-dir))
  :init
  ;; (add-to-list 'load-path
  ;; 	       (expand-file-name "org-ref" scimax-dir))
  ;; (define-key org-mode-map (kbd "s-]") 'org-ref-cite-insert-ivy)
  (define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)
  (define-key org-mode-map (kbd "s-<return>") 'org-ref-citation-hydra/body)

  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos))

(require 'org-ref-ivy)
(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))
      org-ref-default-ref-type "Cref")
(defhydra+ org-ref-insert-link-hydra (:color red :hint nil)
  ("q" nil "Quit"))

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)
  (setq reftex-default-bibliography '("~/Documents/monash/writing/ref/references.bib"))
  (setq reftex-cite-format 'natbib)) ; Prompt for empty optional arguments in cite
(defvar my/reftex-cite-format-biblatex
  '((?\C-m . "\\cite[][]{%l}")
    (?C    . "\\cites[][]{%l}")
    (?t    . "\\textcite[][]{%l}")
    (?T    . "\\textcites[][]{%l}")
    (?p    . "\\parencite[][]{%l}")
    (?P    . "\\parencites[][]{%l}")
    (?f    . "\\footcite[][]{%l}")
    (?F    . "\\footcites[][]{%l}")
    (?s    . "\\shortcite[][]{%l}")
    (?S    . "\\shortcites[][]{%l}")
    (?a    . "\\citeauthor{%l}")
    (?A    . "\\citeauthor*{%l}")
    (?u    . "\\autocite[][]{%l}")
    (?U    . "\\autocite*[][]{%l}")
    (?i    . "\\citetitle{%l}")
    (?I    . "\\citetitle*{%l}")
    (?y    . "\\citeyear{%l}")
    (?Y    . "\\citeyear*{%l}")
    (?n    . "\\nocite{%l}"))
  "My personal set of biblatex citation commands for use with RefTeX.
Based on the biblatex set of `reftex-cite-format-builtin'.")

(setq reftex-cite-format my/reftex-cite-format-biblatex)

(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)

;; writing tool
(use-package academic-phrases :ensure t)

(use-package synosaurus
  :diminish synosaurus-mode
  :init    (synosaurus-mode)
  :config  (setq synosaurus-choose-method 'popup) ;; 'ido is default.
  (global-set-key (kbd "M-#") 'synosaurus-choose-and-replace)
  )
(use-package wordnut
  :bind ("M-!" . wordnut-lookup-current-word))

(use-package auto-dictionary
  :ensure t
  :defer t
  :init(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))



;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; org-table package not found
;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; Array/tabular input with org-tables and cdlatex
;; (use-package org-table
;;   :after cdlatex
;;   :bind (:map orgtbl-mode-map
;;               ("<tab>" . lazytab-org-table-next-field-maybe)
;;               ("TAB" . lazytab-org-table-next-field-maybe))
;;   :init
;;   (add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
;;   ;; Tabular environments using cdlatex
;;   (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
;;                                        "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil nil t))
;;   (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
;;                                        "\\begin{bmatrix} ? \\end{bmatrix}"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil nil t))
;;   (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
;;                                        "\\begin{pmatrix} ? \\end{pmatrix}"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil nil t))
;;   (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
;;                                         "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil t nil))
;;   :config
;;   ;; Tab handling in org tables
;;   (defun lazytab-position-cursor-and-edit ()
;;     ;; (if (search-backward "\?" (- (point) 100) t)
;;     ;;     (delete-char 1))
;;     (cdlatex-position-cursor)
;;     (lazytab-orgtbl-edit))

;;   (defun lazytab-orgtbl-edit ()
;;     (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
;;     (orgtbl-mode 1)
;;     (open-line 1)
;;     (insert "\n|"))

;;   (defun lazytab-orgtbl-replace (_)
;;     (interactive "P")
;;     (unless (org-at-table-p) (user-error "Not at a table"))
;;     (let* ((table (org-table-to-lisp))
;;            params
;;            (replacement-table
;;             (if (texmathp)
;;                 (lazytab-orgtbl-to-amsmath table params)
;;               (orgtbl-to-latex table params))))
;;       (kill-region (org-table-begin) (org-table-end))
;;       (open-line 1)
;;       (push-mark)
;;       (insert replacement-table)
;;       (align-regexp (region-beginning) (region-end) "\\([:space:]*\\)& ")
;;       (orgtbl-mode -1)
;;       (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)))

;;   (defun lazytab-orgtbl-to-amsmath (table params)
;;     (orgtbl-to-generic
;;      table
;;      (org-combine-plists
;;       '(:splice t
;;                 :lstart ""
;;                 :lend " \\\\"
;;                 :sep " & "
;;                 :hline nil
;;                 :llend "")
;;       params)))

;;   (defun lazytab-cdlatex-or-orgtbl-next-field ()
;;     (when (and (bound-and-true-p orgtbl-mode)
;;                (org-table-p)
;;                (looking-at "[[:space:]]*\\(?:|\\|$\\)")
;;                (let ((s (thing-at-point 'sexp)))
;;                  (not (and s (assoc s cdlatex-command-alist-comb)))))
;;       (call-interactively #'org-table-next-field)
;;       t))

;;   (defun lazytab-org-table-next-field-maybe ()
;;     (interactive)
;;     (if (bound-and-true-p cdlatex-mode)
;;         (cdlatex-tab)
;;       (org-table-next-field))))






;; (use-package tex ;;not auctex instead!
;;   :straight auctex
;;   :defer t
;;   :mode ("\\.tex\\'" . latex-mode)
;;   :config
;;   (setq TeX-auto-save t
;; 	TeX-parse-self t)
;;   (setq-default TeX-engine 'xetex) ;;default engine
;;   (setq-default TeX-PDF-mode t)	   ;;PDF output
;;   (setq-default TeX-master nil)

;;   ;;auctex preview C-c C-p C-p (recommend use math-preview instead)
;;   (setq preview-pdf-color-adjust-method t)
;;   (set-default 'preview-scale-function 1.0) ;;preview scale
;;   ;;  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0)) ;;preview in org-mode
;;   ;; (custom-set-faces
;;   ;;  '(preview-reference-face ((t (:background "gray" :foreground "black")))))

;;   ;;sync latex <-> pdf
;;   (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;;auto revert PDF buffer

;;   ;;C-c C-v to sync forward, double click to sync backward
;;   (setq TeX-view-program-selection '((output-pdf "PDF Tools")
;; 				     (output-dvi "DVI Viewer"))
;; 	TeX-source-correlate-start-server t
;; 	TeX-source-correlate-method 'auto) ;;Method to use for enabling forward and inverse search

;;   (add-hook 'LaTeX-mode-hook
;; 	    (lambda ()
;; 	      (projectile-mode -1)
;; 	      (rainbow-delimiters-mode 1)
;; 	      (visual-line-mode 1)
;; 	      (LaTeX-math-mode 1)
;; 	      (flycheck-mode 1) 	;enable grammarly
;; 	      (variable-pitch-mode 1)
;; 	      (TeX-source-correlate-mode 1) ;;Needed to sync TeX and PDF
;; 	      )))

;; Third-party preview for LaTeX
;; npm install -g git+https://gitlab.com/matsievskiysv/math-preview
;; There's also org-latex-impatient for org-mode
(use-package math-preview
  :custom
  (math-preview-command "/opt/homebrew/bin/math-preview")
  (math-preview-scale 1.0)
  (math-preview-marks
   '(("\\begin{equation}" . "\\end{equation}")
     ("\\begin{equation*}" . "\\end{equation*}")
     ("\\[" . "\\]")
     ("\\(" . "\\)")
     ("$$" . "$$")
     ("$" . "$")
     ("\\begin{align}" . "\\end{align}")
     ("\\begin{align*}" . "\\end{align*}")))
  :bind
  (:map TeX-mode-map
	("C-c p p" . math-preview-at-point)
	("C-c p a" . math-preview-all)
	("C-c c p" . math-preview-clear-at-point)
	("C-c c a" . math-preview-clear-all)))

;; (use-package magic-latex-buffer
;;   :hook
;;   (LaTeX-mode . magic-latex-buffer)
;;   :config
;;   (setq magic-latex-enable-block-highlight nil
;; 	magic-latex-enable-suscript t
;; 	magic-latex-enable-pretty-symbols t
;; 	magic-latex-enable-block-align nil
;; 	magic-latex-enable-inline-image nil
;; 	magic-latex-enable-minibuffer-echo nil))

;; LaTeX instant preview functionality based on pyQt5 (bug existed in arm64 Mac)
;; need symbolic link python file from straight/repo to straight/build
;; `ln -s ~/.emacs.d/straight/build/popweb/popweb.py ./popweb.py`
;; (use-package popweb
;;   :straight (popweb :type git
;; 		    :host github
;; 		    :repo "manateelazycat/popweb")
;;   :config
;;   (add-hook 'latex-mode-hook #'popweb-latex-mode))

;;; PDF READER

;; Compile and install: https://github.com/politza/pdf-tools
;; Install dependencies: cask, poppler, automake and setenv
;; Download source code and compile
;; (pdf-tools-install)

(add-hook 'TeX-mode-hook
	  #'(lambda ()
	      (setq TeX-view-program-selection
		    '((output-pdf "PDF Tools"))
		    TeX-source-correlate-start-server t)
              (setq TeX-view-program-list
                    '(("PDF Tools" TeX-pdf-tools-sync-view)))
	      ))
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(add-hook 'TeX-mode-hook
          #'(lambda ()
              (setq TeX-source-correlate-mode t)
              (with-eval-after-load "pdf-sync"
                (define-key TeX-source-correlate-map (kbd "C-c C-g")
                  'pdf-sync-forward-search))))
;; (add-hook 'pdf-view-mode-hook (lambda () (auto-revert-mode 1)))
;; (add-hook 'pdf-tools-enabled-hook (lambda () (auto-revert-mode 1)))
(add-hook 'TeX-mode-hook 'TeX-source-correlate-mode)


;;allow you to view pdf continuously
(use-package pdf-continuous-scroll-mode
  :after pdf-tools
  :straight (pdf-continuous-scroll-mode :type git :host github
					:repo "dalanicolai/pdf-continuous-scroll-mode.el")
  ;; :hook
  ;; (add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)
  :config
  (setq pdf-continuous-suppress-introduction t)
  (evil-define-key 'normal pdf-view-mode-map
  "j" 'pdf-continuous-scroll-forward
  (kbd "<down>") 'pdf-continuous-scroll-forward
  "gt" 'pdf-view-goto-page)
  )
(add-hook 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)

(require 'smtpmail)
;; managing emails with mu4e
(use-package mu4e
  :straight
  (:local-repo "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
	       :type built-in)
  :commands (mu4e)
  :bind
  ("C-x y" . mu4e)
  :config
  ;; (setq mu4e-mu-binary (executable-find "mu"))
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)
  ;; this command is called to sync imap servers:
  (setq mu4e-get-mail-command "mbsync -a")
  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))

  (setq mu4e-maildir "~/Mail")

  (setq mu4e-drafts-folder "/monash/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/monash/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/monash/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/monash/[Gmail]/Bin")
  (setq mu4e-spam-folder "/monash/[Gmail]/Spam")


  ;; (setq mu4e-maildir-shortcuts
  ;; 	'((:maildir "/monash/Inbox"    :key ?i)
  ;;       (:maildir "/monash/[Gmail]/Sent Mail" :key ?s)
  ;;       (:maildir "/monash/[Gmail]/Bin"     :key ?t)
  ;;       (:maildir "/monash/[Gmail]/Drafts"    :key ?d)
  ;;       (:maildir "/monash/[Gmail]/All Mail"  :key ?a)))
  (add-to-list 'mu4e-bookmarks
	       (make-mu4e-bookmark
		:name "Inbox - Monash"
                :query "maildir:/monash/INBOX"
                :key ?i))
  (add-to-list 'mu4e-bookmarks
	       (make-mu4e-bookmark
		:name "Sent Mail - Monash"
                :query "maildir:/monash/[Gmail]/Sent Mail"
                :key ?s))
  (add-to-list 'mu4e-bookmarks
	       (make-mu4e-bookmark
		:name "Drafts - Monash"
                :query "maildir:/monash/[Gmail]/Drafts"
                :key ?d))
  (add-to-list 'mu4e-bookmarks
	       (make-mu4e-bookmark
		:name "Spam - Monash"
                :query "maildir:/monash/[Gmail]/Spam"
                :key ?t))


  (setq mu4e-contexts
      `(,(make-mu4e-context
          :name "monash"
          :enter-func
          (lambda () (mu4e-message "Enter shidan.liu@monash.edu context"))
          :leave-func
          (lambda () (mu4e-message "Leave shidan.liu@monash.edu context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "shidan.liu@monash.edu")))
          :vars '((user-mail-address . "shidan.liu@monash.edu" )
                  (user-full-name . "Shidan Liu")
                  (mu4e-drafts-folder . "/monash/[Gmail]/Drafts")
                  (mu4e-refile-folder . "monash/[Gmail]/All Mail")
                  (mu4e-sent-folder . "/monash/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/monash/[Gmail]/Bin")))))

  (setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
  (setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; no need to confirm
  (setq mu4e-confirm-quit nil)

  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; save attachment to desktop by default
  ;; or another choice of yours:
  (setq mu4e-attachment-dir "~/Downloads")
)

(require 'mu4e-icalendar)
(mu4e-icalendar-setup)
(setq mu4e-view-use-gnus t)

;; gpg encryptiom & decryption:
;; this can be left alone
(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)
(auth-source-forget-all-cached)

;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

;; send program:
;; this is exeranal. remember we installed it before.
(setq sendmail-program (executable-find "msmtp"))

;; select the right sender email from the context.
(setq message-sendmail-envelope-from 'header)

;; chose from account before sending
;; this is a custom function that works for me.
;; well I stole it somewhere long ago.
;; I suggest using it to make matters easy
;; of course adjust the email adresses and account descriptions
(defun my/set-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "shidan.liu@monash.edu" from) "gmail"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(add-hook 'message-send-mail-hook 'timu/set-msmtp-account)

;; mu4e cc & bcc
;; this is custom as well
(add-hook 'mu4e-compose-mode-hook
          (defun my/add-cc-and-bcc ()
            "My Function to automatically add Cc & Bcc: headers.
    This is in the mu4e compose mode."
            (save-excursion (message-add-header "Cc:\n"))
            (save-excursion (message-add-header "Bcc:\n"))))

;; mu4e address completion
(add-hook 'mu4e-compose-mode-hook 'company-mode)

;;; setup org-msg
;; (setq mail-user-agent 'mu4e-user-agent)
;; (use-package org-msg
;;   :config
;;   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng"
;; 	org-msg-startup "hidestars indent inlineimages"
;; 	org-msg-greeting-fmt "\nHi *%s*,\n\n"
;; 	org-msg-greeting-name-limit 3
;; 	org-msg-default-alternatives '((new . (text html))
;; 				       (reply-to-html . (text html))
;; 				       (reply-to-text . (text)))
;; 	org-msg-signature "

;;  Regards,

;;  #+begin_signature
;;  -- *Shidan* \\\\
;;  #+end_signature")
;;   (org-msg-mode))



;;garbage collection
(add-hook 'focus-out-hook #'garbage-collect)

;; save history for minibuffer
(setq savehist-additional-variables        ;; also save...
      '(search-ring regexp-search-ring)    ;; ... my search entries
      ;; savehist-file "~/.emacs.d/savehist"
      ) ;; keep my home clean
(savehist-mode t)                          ;; do customization before activate


;;; End

;; modifiers
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(make-backup-files nil)
 '(ns-alternate-modifier (quote super))
 '(ns-right-alternate-modifier (quote hyper))
 '(ns-fuction-modifier (quote alt)))



;; Back to normal GC level
(defun my-cleanup-gc ()
  "Clean up gc.  From user redguardtoo."
  (setq gc-cons-threshold 100000000)
  (setq gc-cons-percentage 0.1))

;; Collect gc during free time
(run-with-idle-timer 4 nil #'my-cleanup-gc)

(setq max-specpdl-size 32000
      max-lisp-eval-depth 16000)

(defun print-init-info ()
  "Print init time of Emacs, a wrapper of 'emacs-init-time."
  (interactive)
  (message
   (format "Start up in %.2fs with %d features and %d GC(s)"
	   (float-time (time-subtract after-init-time before-init-time))
	   (length features)
	   gcs-done)))
(add-hook 'after-init-hook #'print-init-info)

(provide 'init)
;;; Init.el ends here
