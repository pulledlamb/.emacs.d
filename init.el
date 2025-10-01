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

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;              recompilation temp fix @emac29.4
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(defun fixed-native-compile-async-skip-p
        (native-compile-async-skip-p file load selector)
    (let* ((naive-elc-file (file-name-with-extension file "elc"))
           (elc-file       (replace-regexp-in-string
                               "\\.el\\.elc$" ".elc" naive-elc-file)))
        (or (gethash elc-file comp--no-native-compile)
            (funcall native-compile-async-skip-p file load selector))))

(advice-add 'native-compile-async-skip-p
    :around 'fixed-native-compile-async-skip-p)
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;                end of temp fix
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;                dvorak remap
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(define-key key-translation-map "\C-t" "\C-x")


(defvar native-comp-deferred-compilation-deny-list nil)
;; Customize when to check package modification (much much faster)
(setq-default straight-check-for-modifications '(check-on-save find-when-checking))

;; Cause straight.el to cache the autoloads of all used packages in a single
;; file on disk thus reduce IO operations
(setq-default straight-cache-autoloads t)

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
(setq-default straight-use-package-by-default t)

;; Emacs Native Compilation Feature support
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (progn
    (setq-default native-comp-async-report-warnings-errors nil)
    (setq-default comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

;; Update user load path
;; Optimize: Force "lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path`"
  (dolist (dir '("extra-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(when (eq system-type 'darwin)
  (defvar brew-parent-dir "/opt/homebrew/")
  (defvar brew-bin-dir (expand-file-name "bin/" brew-parent-dir))
  (defvar emacs-path "/opt/homebrew/bin/emacs"))

;; Avoid matching file name with regrex list during startup
(let ((file-name-handler-alist nil)) "~/.emacs.d/init.el")

;; Custom file
(setq-default custom-file (concat user-emacs-directory "extra-lisp/custom.el"))
(load custom-file :noerror)

;; Benchmark init time
(use-package esup
  :custom
  (esup-depth 0))


;; restart emacs
(use-package restart-emacs)

(use-package no-littering)
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq no-littering-etc-directory
      (expand-file-name "ect/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "var/" user-emacs-directory))
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;                  orgmode w/ latex preview
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; special arrow \to
(use-package org
  ;; :straight (:type built-in)
  :defer
  :straight `(org
              :fork (:host nil
                     :repo "https://git.tecosaur.net/tec/org-mode.git"
                     :branch "dev"
                     :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
               (require 'lisp-mnt)
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents "lisp/org.el")
                        (lm-header "version")))
                     (git-version
                      (string-trim
                       (with-temp-buffer
                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                         (buffer-string)))))
                (insert
                 (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                 (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                 "(provide 'org-version)\n")))
              :pin nil)
  :config
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)

  ;; Use dvisvgm to generate previews
  ;; ;; You don't need this, it's the default:
  ;; (setq org-latex-preview-process-default 'dvisvgm)

  (require 'ox-latex )
  ;; ;; set latex to xelatex
  ;; (setq org-latex-pdf-process
  ;; 	'("xelatex --shell-escape  -interaction nonstopmode -output-directory %o %f"
  ;; 	  "bibtex %b"
  ;; 	  "xelatex --shell-escape  -interaction nonstopmode -output-directory %o %f"
  ;; 	  "xelatex --shell-escape  -interaction nonstopmode -output-directory %o %f"))

  ;; (setq org-latex-pdf-process
  ;;       '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "bibtex %b"
  ;; 	  ;; "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         ;; "biber %b"
  ;;         "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;; 	  "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-pdf-process
    '("export PATH=\"/Library/TeX/texbin/:$PATH\";latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
  ;; (setq org-latex-pdf-process
  ;; 	'("pdflatex -interaction nonstopmode -output-directory %o %f"
  ;;         "bibtex %b"
  ;;         "pdflatex -interaction nonstopmode -output-directory %o %f"
  ;; 	  "pdflatex -interaction nonstopmode -output-directory %o %f"))

  ;; (setq org-latex-compiler "xelatex")
  ;; (setq org-latex-pdf-process
  ;;     (list (concat "latexmk -"
  ;;                   org-latex-compiler
  ;;                   " -recorder -synctex=1 -bibtex-cond %b")))

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  ;; (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)
  (setq org-latex-preview-live t)

  (setq org-highlight-latex-and-related '(native))
					; Highlight inline LaTeX code

  (let ((factor (- (/ (face-attribute 'default :height)
                      100.0)
                   0.025)))
    (plist-put org-latex-preview-appearance-options :scale factor)
    (plist-put org-latex-preview-appearance-options :zoom  factor))

  (let ((dvisvgm (alist-get 'dvisvgm org-latex-preview-process-alist))
	(libgs "/opt/homebrew/opt/ghostscript/lib/libgs.dylib"))
    (plist-put dvisvgm :image-converter
               `(,(concat "dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts"
                          " --libgs=" libgs

                          ;; Default is "-v3" here, but it does not seem to work correctly
                          ;; on my client.
                          " --bbox=preview -v4 -o %B-%%9p.svg %f"))))



  ;; ;; Default LaTeX preview image directory
  ;; (setq org-preview-latex-image-directory
  ;; 	(expand-file-name "ltximg/" user-emacs-directory))

  ;; (setq org-persist-directory (expand-file-name "org-persist" user-emacs-directory))

  ;; ;; ;; Preview functions

  ;; (defun lsd/org-preview-fragments ()
  ;;   (interactive)
  ;;   (call-interactively 'org-latex-preview-clear-cache)
  ;;   (org-latex-preview 'buffer)
  ;;   (org-redisplay-inline-images))
  ;; (bind-keys :map org-mode-map
  ;; 	     ("C-c p" . lsd/org-preview-fragments))

  (setq org-startup-indented t
	org-latex-precompile nil
	org-startup-with-inline-images t
	org-hide-emphasis-markers t
	org-latex-prefer-user-labels t
	org-image-actual-width nil
	org-export-allow-bind-keywords t)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/Documents/org/monash.org" "Tasks")
	   "* TODO %?\n  %i\n  %a")
	  ("j" "Journal" entry (file+olp+datetree "~/Documents/org/journal.org")
	   "* %?\nEntered on %U\n  %i\n  %a")
	  ("m" "Email Workflow")
	  ("mf" "Follow Up" entry (file+olp "~/Documents/org/monash.org" "Follow Up")
           "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i")
	  ("mr" "Read Later" entry (file+olp "~/Documents/org/monash.org" "Read Later")
	   "* TODO Read %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i")
	  ))
  (setq org-default-notes-file "~/Documents/org/notes.org")
  (setq org-archive-location "~/Documents/org/archives.org::* From %s")
  (setq org-agenda-files
	(file-expand-wildcards "~/Documents/org/*.org"))
  (setq org-agenda-include-diary t)
  (setq org-agenda-diary-file "~/Documents/org/Diary") ;;2020-03-02 10:47:06
  (setq diary-file "~/Documents/org/Diary")


  ;; not display _ and ^ as sub/superscript
  (setq org-use-sub-superscripts nil)

  ;; allow alphabetical plain lists
  (setq org-list-allow-alphabetical t)

  ;; turn off org-element-cache
  (setq org-element-use-cache  nil)

  ;;src setting
  (setq org-src-fontify-natively t)

  (setq org-latex-listings 'minted)
  ;; (setq org-latex-src-block-backend 'minted)
  (setq org-export-latex-listings 'minted)
  ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")
				   ("fontsize" "\\scriptsize")))

  (add-to-list 'org-latex-classes
               '("amsart"
                 "\\documentclass[a4paper]{amsart}
\\let\\email\\relax
                     "
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
                 )
               )
  (add-to-list 'org-latex-classes
               '("svjour3" "\\documentclass{svjour3}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
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

  (add-to-list 'org-latex-classes
               '("memoir"
		 "\\documentclass{memoir}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
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


  ;; (load "~/.emacs.d/.local/straight/repos/org-mode/lisp/ox-extra.el" t t)
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
	("C-c <deletechar>" . org-deadline)
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
		;; (company-mode 1)
		;; (org-cdlatex-mode 1)
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
	    (add-to-list 'org-beamer-environments-extra '("only" "o" "\\only%a{" "}"))
	    (add-to-list 'org-beamer-environments-extra '("action" "A" "\\action%a{" "}"))
	    ;; export snippet translations
	    (add-to-list 'org-export-snippet-translation-alist
			 '("b" . "beamer"))))

;; Numbering
(defun org-export-get-headline-number (headline info)
    "Return numbered HEADLINE numbering as a list of numbers.
    INFO is a plist holding contextual information."
    (and (org-export-numbered-headline-p headline info)
         (let* (
                (nums (cdr (assq headline (plist-get info :headline-numbering))))
                (root-heading (let ((parent headline)(temp)) (while (and (setq temp (org-element-property :parent parent)) (eq 'headline (org-element-type temp))) (setq parent temp)) parent))
                (appendix (member "appendix" (org-element-property :tags root-heading))))
           (if (eq 1 (length nums))
               ;; if it's a part get roman numbers
               (list (nth (car nums) '("Z" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X")))
               (let ((nums (cdr nums))) ; remove part number
		 (cons (if appendix
			   (concat "A." (substring (number-to-string nums) 1 -1))
                         )
                       (cdr nums))
		 )))))
(defun number-to-string (number)
  (format "%s" number))

;; Break inheritance of UNNUMBERED
(defun org-export-numbered-headline-p (headline info)
  "Return a non-nil value if HEADLINE element should be numbered.
INFO is a plist used as a communication channel."
  (unless (org-not-nil (org-export-get-node-property :UNNUMBERED headline  ))
                                                      ; removing `t` here ↑
                                                      ; removes inheritance
    (let ((sec-num (plist-get info :section-numbers))
      (level (org-export-get-relative-level headline info)))
      (if (wholenump sec-num) (<= level sec-num) sec-num))))


;; place CAPTION and LABEL in the right order
(defun org-latex--caption/label-string (element info)
  "Return caption and label LaTeX string for ELEMENT.

  INFO is a plist holding contextual information.  If there's no
  caption nor label, return the empty string.

  For non-floats, see `org-latex--wrap-label'."
  (let* ((label (org-latex--label element info nil t))
     (main (org-export-get-caption element))
     (attr (org-export-read-attribute :attr_latex element))
     (type (org-element-type element))
     (nonfloat (or (and (plist-member attr :float)
                (not (plist-get attr :float))
                main)
               (and (eq type 'src-block)
                (not (plist-get attr :float))
                (null (plist-get info :latex-listings)))))
     (short (org-export-get-caption element t))
     (caption-from-attr-latex (plist-get attr :caption)))
    (cond
     ((org-string-nw-p caption-from-attr-latex)
      (concat caption-from-attr-latex "\n"))
     ((and (not main) (equal label "")) "")
     ((not main) label)
     ;; Option caption format with short name.
     (t
      (format (if nonfloat "\\captionof{%s}%s{%s}\n%s"
        "\\caption%s%s{%s}\n%s")
          (let ((type* (if (eq type 'latex-environment)
                   (org-latex--environment-type element)
                 type)))
        (if nonfloat
            (cl-case type*
              (paragraph "figure")
              (image "figure")
              (special-block "figure")
              (src-block (if (plist-get info :latex-listings)
                     "listing"
                   "figure"))
              (t (symbol-name type*)))
          ""))
          (if short (format "[%s]" (org-export-data short info)) "")
          (org-export-data main info)
          label)))))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;                   end of orgmode
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; window split
(setq split-height-threshold 1)
(setq split-width-threshold nil)


;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;                     centaur tabs
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;; (use-package centaur-tabs
;;   :init
;;   (setq centaur-tabs-enable-key-bindings t)
;;   :config
;;   (setq centaur-tabs-style "bar"
;;         centaur-tabs-height 32
;;         centaur-tabs-set-icons t
;;         centaur-tabs-show-new-tab-button t
;;         centaur-tabs-set-modified-marker t
;;         centaur-tabs-show-navigation-buttons t
;;         centaur-tabs-set-bar 'under
;;         centaur-tabs-show-count nil
;;         ;; centaur-tabs-label-fixed-length 15
;;         ;; centaur-tabs-gray-out-icons 'buffer
;;         ;; centaur-tabs-plain-icons t
;;         x-underline-at-descent-line t
;;         centaur-tabs-left-edge-margin nil)
;;   (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
;;   (centaur-tabs-headline-match)
;;   ;; (centaur-tabs-enable-buffer-alphabetical-reordering)
;;   ;; (setq centaur-tabs-adjust-buffer-order t)
;;   (centaur-tabs-mode t)
;;   (setq uniquify-separator "/")
;;   (setq uniquify-buffer-name-style 'forward)
;;   (defun centaur-tabs-buffer-groups ()
;;     "`centaur-tabs-buffer-groups' control buffers' group rules.

;; Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
;; All buffer name start with * will group to \"Emacs\".
;; Other buffer group by `centaur-tabs-get-group-name' with project name."
;;     (list
;;      (cond
;;       ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
;;       ;; "Remote")
;;       ((or (string-equal "*" (substring (buffer-name) 0 1))
;;            (memq major-mode '(magit-process-mode
;;                               magit-status-mode
;;                               magit-diff-mode
;;                               magit-log-mode
;;                               magit-file-mode
;;                               magit-blob-mode
;;                               magit-blame-mode
;;                               )))
;;        "Emacs")
;;       ((derived-mode-p 'prog-mode)
;;        "Editing")
;;       ((derived-mode-p 'dired-mode)
;;        "Dired")
;;       ((memq major-mode '(helpful-mode
;;                           help-mode))
;;        "Help")
;;       ((memq major-mode '(org-mode
;;                           org-agenda-clockreport-mode
;;                           org-src-mode
;;                           org-agenda-mode
;;                           org-beamer-mode
;;                           org-indent-mode
;;                           org-bullets-mode
;;                           org-cdlatex-mode
;;                           org-agenda-log-mode
;;                           diary-mode))
;;        "OrgMode")
;;       (t
;;        (centaur-tabs-get-group-name (current-buffer))))))
;;   :hook
;;   (dashboard-mode . centaur-tabs-local-mode)
;;   (term-mode . centaur-tabs-local-mode)
;;   (calendar-mode . centaur-tabs-local-mode)
;;   (org-agenda-mode . centaur-tabs-local-mode)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward)
;;   ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
;;   ("C-S-<next>" . centaur-tabs-move-current-tab-to-right)
;;   (:map evil-normal-state-map
;;         ("g t" . centaur-tabs-forward)
;;         ("g T" . centaur-tabs-backward)))


;;; EDITOR

;;Coding system
;; In some old machines, you might need specify these in .bashrc
;; export LAGNUAGE=en_US.UTF-8
;; export LANG=en_US.UTF-8
;; export LC_ALL=en_US.UTF-8
;; export LC_CTYPE=en_US.UTF-8

;; (defvar default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Improved global minor mode for scrolling in Emacs 29
(if (> emacs-major-version 28)
    (pixel-scroll-precision-mode)
  ;; else use third-party package
  (use-package good-scroll
    :config
    (good-scroll-mode 1)
    (global-set-key [next] #'good-scroll-up-full-screen)
    (global-set-key [prior] #'good-scroll-down-full-screen)))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;We are lazy human :)
(if (> emacs-major-version 27)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;;No more backup files~
(setq-default make-backup-files nil)

;;No more strange ring bell
(setq ring-bell-function 'ignore)

(setq confirm-kill-processes t)

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

;; ---Edit keybinding style---
;; A better undo/redo mode
(use-package undo-tree
  :hook
  (after-init . global-undo-tree-mode)
  :bind
  ("C-c u" . undo-tree-visualize)
  ("M-/" . undo-tree-redo)
  ("C-/" . undo-tree-undo))


;; undo and redo changes in the *window configuration*
(use-package winner
  :straight (:type built-in)
  :hook
  (after-init . winner-mode)
  :bind
  (:map winner-mode-map
	("C-M-b" . winner-undo)
	("C-M-f" . winner-redo)))

;; move within buffers
(use-package mosey
  :bind
  ("C-a" . mosey-backward-bounce)
  ("C-e" . mosey-forward-bounce)
  :config
  (defmosey '(beginning-of-line
	      back-to-indentation
	      sp-backward-sexp		; Moves across lines
	      sp-forward-sexp		; Moves across lines
	      mosey-goto-end-of-code
	      mosey-goto-beginning-of-comment-text)
    :prefix "lisp"))

;;; Auto-save buffer
(use-package real-auto-save
  :hook
  (prog-mode . real-auto-save-mode)
  (text-mode . real-auto-save-mode)
  :custom
  ;; configure time gap (in sec)
  (real-auto-save-interval 120))


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
;; (use-package ws-butler
;;   :defer t
;;   :straight (:type git :host github
;; 		   :repo "lewang/ws-butler")
;;   :hook
;;   (prog-mode . ws-butler-mode))

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

(use-package ediff
  :defer 1
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-highlight-all-diffs t)
  :custom
  (ediff-forward-word-function 'forward-char) ;; from https://emacs.stackexchange.com/a/9411/17066
  (ediff-highlight-all-diffs t)
  (ediff-diff-options "-w")
  (ediff-keep-variants nil)
  (ediff-window-setup-function 'ediff-setup-windows-plain))


;;; TERMINAL, COMPLETION, LINT/SPELL CHECKER, SNIPPET
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
  (define-key evil-insert-state-map (kbd "C-0") 'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-$") 'evil-end-of-line)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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

(use-package eshell-syntax-highlighting
  :hook
  ;; Enable in all Eshell buffers.
  (eshell-mode . eshell-syntax-highlighting-mode))


;; Enable this to get a superior terminal emulator (a true application like iTerm)
;; read more on https://github.com/akermu/emacs-libvterm to see the external dependencies
;; remember to check the exec-path as well
(use-package vterm
  :bind
  ("C-x t" . vterm)
  ("C-x s" . vterm-shell)
  (:map vterm-mode-map
	("C-c C-t" . vterm-copy-mode))
  :custom
  (vterm-kill-buffer-on-exit t))

;; a comint extension, e.g., :view *.jpg to view a plot in shell
;; other useful cmd: :e (edit), :ssh,
(use-package shx
  :hook
  (after-init . shx-global-mode))


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


(use-package which-key
  :hook
  (after-init . which-key-mode)
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2))

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


(use-package flycheck-inline
  :hook
  (flycheck-mode . flycheck-inline-mode))

(use-package flycheck-grammarly
  :config
  (setq flycheck-grammarly-check-time 0.8)
  ;; :hook
  ;; (flycheck-mode . flycheck-grammarly-setup)
  )

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
  (setq ispell-program-name "aspell"
	ispell-dictionary "en_US"
	;; ispell-quietly t
	;; ispell-silently-savep t
	ispell-local-dictionary "en_US"
	ispell-extra-args '("--sug-mode=fast" "--lang=en_US"
			    "--camel-case" "--run-together"))
  (setq flyspell-default-dictionary "en_US")
  )

;; Quickly switch dictionaries
;; Adapted from DiogoRamos' snippet on https://www.emacswiki.org/emacs/FlySpell#h5o-5

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
;; (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
;; (add-to-list 'ispell-skip-region-alist '(":noexport:"))
;; (add-to-list 'ispell-skip-region-alist '("#\\+LATEX:"))
;; (add-to-list 'ispell-skip-region-alist '("#\\+LANGUAGE:" . "\n"))
;; (add-to-list 'ispell-skip-region-alist '("#\\+LATEX_HEADER:" . "\n"))
;; (add-to-list 'ispell-skip-region-alist '("#\\+OPTIONS:"))
;; (add-to-list 'ispell-skip-region-alist '("#\\+SETUPFILE:"))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; Flyspell interface
;; Use M-o to do words action (e.g., save)
(use-package flyspell-correct-ivy
  :after flyspell-correct)

(use-package consult-yasnippet
  :after (consult yasnippet))

;;Git + Emacs = boom!
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x c" . magit-checkout))

;;a magit prefix help page
(use-package transient
  :defer t)

;; git-forge support: fetches issues, pull-requests etc.
(use-package forge
  :after magit)

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


;(use-package use-package-ensure-system-package
;  :after exec-path-from-shell) ;;extend use-package, put after exec-path-from-shell

(use-package popwin
  :hook
  (after-init . popwin-mode))

;; better isearch, choose this or consult-line
(use-package ctrlf
  :hook
  (after-init . ctrlf-mode)
  :config
  (setq ctrlf-default-search-style 'fuzzy))

;; completion UI
(use-package vertico
  :bind (:map vertico-map
	("C-j" . vertico-next)
	("C-k" . vertico-previous)
	("C-q" . vertico-exit)
	:map minibuffer-local-map
	("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :hook
  (after-init . vertico-mode))

;; displays current match and total matches in search
(use-package anzu
  :hook
  (prog-mode . global-anzu-mode))

;; use posframe (in the centre of buffer) for vertico
(use-package vertico-posframe
  :hook
  (vertico-mode . vertico-posframe-mode))

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; save minibuffer history
(use-package savehist
  :config
  (savehist-mode))

;; completion strategy
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; rich annotations of minibuffer
(use-package marginalia
  ;; change more or less info
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :hook
  (after-init . marginalia-mode))

;;;; Code Completion
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 4)
  (corfu-auto-delay 0.75)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  (corfu-quit-no-match t)

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("s-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("s-<return>" . corfu-insert)
              ("s-q"        . corfu-quit))

  :init
  (global-corfu-mode)
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;; documentation
;; (use-package corfu-doc
;;   :straight (:type git :host github
;; 		   :repo "galeo/corfu-doc")
;;   :hook
;;   (corfu-mode . corfu-doc-mode))

;; icon like all-the-icons
(use-package kind-icon
  :straight (:type git :host github
		   :repo "jdtsmith/kind-icon")
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; (use-package embark
;;   :bind
;;   (("<f6>" . embark-act)	;; pick some comfortable binding
;;    ("C-;" . embark-dwim)	;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

;;   :init
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;; 	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))


;; jump to definition
(use-package dumb-jump
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  ;; xref as backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; customized xref to use `completing-read' to select a target
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; a bunch of advanced commands: buffer switching, imenu, search commands etc.
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)

	 ;; C-x bindings (ctl-x-map)
	 ("C-x b" . consult-buffer)	;; orig. switch-to-buffer
	 ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump

	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)	;; orig. yank-pop
	 ("<help> a" . consult-apropos) ;; orig. apropos-command

	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ;; ("M-g f" . consult-flycheck) ;; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)	 ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line) ;; orig. goto-line
	 ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)

	 ;; M-s bindings (search-map)
	 ;; ("M-s l" . consult-line)
	 ;; use consult-line to replac
	 ("C-s" . consult-line)
	 ("M-s f" . affe-find)
	 ("M-s s" . affe-grep)
	 ("M-s r" . consult-ripgrep)
	 ("M-s L" . consult-line-multi)
	 ("M-s m" . consult-multi-occur)
	 ("M-s k" . consult-keep-lines)
	 ("M-s u" . consult-focus-lines)
	 ("C-x r r" . consult-recent-file)
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
	 ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
	 ;; Minibuffer history
	 :map minibuffer-local-map
	 ("M-s" . consult-history) ;; orig. next-matching-history-element
	 ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  :hook
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  (completion-list-mode . consult-preview-at-point-mode)

  :config
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

;; asynchronous fuzzy find/grep (with just 200+ lines)
;; used in project
(use-package affe
  :defer 0.5)

;; consult extensions
(use-package consult-flycheck
  :after (consult flycheck))

;; project buffer/file
(use-package consult-projectile
  :after (consult projectile))


(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
(setq recentf-exclude '(".*-autoloads\\.el\\'"
                        "[/\\]\\elpa/"
                        "bookmark"
                        ))

;; Project management tool
(use-package projectile
  :after ivy
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


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

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  ("C-x o" . ace-swap-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 2)))))
  )
;; (global-set-key (kbd "M-o") 'ace-window)


;;; KEYBINDING

;; Set meta command for Mac OS
;; If you are using a external Windows keyboard, remeber to choose
;; USB keyboard in Preference -> Keyboard -> modify keyboard -> select keyboard
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))


(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x ,") 'beginning-of-buffer)
(global-set-key (kbd "C-x .") 'end-of-buffer)
;; globally go to previous position; "C-u C-SPC" to do same locally
(global-set-key (kbd "C-c C-SPC") 'pop-global-mark)
;; (define-key key-translation-map (kbd "C-d") (kbd "<deletechar>"))
;; evil insert mode
(evil-define-key 'insert global-map (kbd "C-n") 'evil-next-line)
(evil-define-key 'insert global-map (kbd "C-p") 'evil-previous-line)
;; repeat command
(global-set-key (kbd "<f4>") #'repeat)

;; (use-package goto-last-change
;;   :bind
;;   ("C-x C-x" . goto-last-change))

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

;; deleting a whitespace character will delete all whitespace
;; (use-package hungry-delete
;;   :hook
;;   (after-init . global-hungry-delete-mode)
;;   :config
;;   ;; left one last whitespace
;;   (setq hungry-delete-join-reluctantly t))

;; history across buffers
;; (use-package dogears
;;   :straight (:host github :repo "alphapapa/dogears.el")
;;   ;; These bindings are optional, of course:
;;   :bind (:map global-map
;; 	      ("M-g d" . dogears-go)
;; 	      ("M-g b" . dogears-back)
;; 	      ("M-g f" . dogears-forward)
;; 	      ("M-g D" . dogears-list)))

;; Mark set
;; C-x C-x -> set mark and move back to previous position
;; C-x h to select all


(use-package general
  :config
  ;; leader key
  (defconst leader "\\")
  (general-create-definer my/leader-def
    :prefix leader)

  ;; double press to input `\`
  (defun quote-backslash ()
    (interactive)
    (insert "\\"))

  ;; ------ Global Keybindings ------
  (my/leader-def
    "" nil
    "\\" 'quote-backslash

    "g l" '(avy-goto-line :which-key "goto-line")
    "g g" '(goto-line :which-key "goto-line-number")
    "g m" '(exchange-point-and-mark :which-key "go-back-and-mark")
    ;; "g b" '(goto-last-change :which-key "go-back")
    "g b" '(dogear-back :which-key "go-back")

    "m" '(indent-rigidly :which-key "move code")

    "p f" '(affe-find :which-key "project find file")
    "p s" '(affe-grep :which-key "project search text")
    "p b" '(consult-projectile :which-key "project buffer/file")
    "p c" '(projectile-compile-project :which-key "project compile")
    "p p" '(projectile-switch-project :which-key "project switch")
    "p v" '(projectile-run-vterm :which-key "project vterm")
    "p x" '(projectile-run-shell :which-key "project shell")
    "p e" '(projectile-run-eshell :which-key "project eshell")

    "e b" '(ediff-buffers :which-key "compare buffers")
    "e f" '(ediff-files :which-key "compare files")

    "h a" '(mark-whole-buffer :which-key "select all")

    "." 'mc/mark-next-like-this
    "," 'mc/mark-previous-like-this

    "1" 'beginning-of-buffer
    "2" 'end-of-buffer

    "v" 'vterm

    "f" 'consult-file-externally
    "k" 'kill-this-buffer
    "q" 'save-buffers-kill-terminal)

  ;; ------ Mode-specific Keybindings ------
  (my/leader-def prog-mode-map
    "b" 'consult-imenu
    "s" 'shell
    "t" 'vterm
    "d" 'ediff-buffers
    "c" 'consult-flycheck
    "%" 'query-replace)

  (my/leader-def text-mode-map
    "d" 'define-word-at-point)

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
    "e" 'dired-toggle-read-only
    "r" 'dired-rsync
    ))


;; folding your code
(use-package hideshow
  :straight (:type built-in)
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  ("<f5>" . hs-toggle-hiding))

;; adjust font size
(setq-default text-scale-mode-step 1.1)

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

;;Cursor
(setq-default cursor-type 'hollow)
(blink-cursor-mode 0)

;;Display time in the mode line
(add-hook 'after-init-hook 'display-time-mode)
(setq display-time-format "%B %d %H:%M %p")
(setq system-time-locale nil)

;;Don't display load average percentage
(setq display-time-default-load-average nil)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 15)))
;;If use doom-mode-line, then display battery
(add-hook 'doom-modeline-mode-hook 'display-battery-mode)

;; (use-package mood-line
;;   :ensure t
;;   :hook
;;   (after-init . mood-line-mode))

(use-package nyan-mode
  :hook
  (doom-modeline-mode . nyan-mode)
  :config
  (setq nyan-animate-nyancat t
	nyan-wavy-trail t)
  )

;;Highlight current line, based on default hl-line-mode
(use-package lin
  :hook
  (prog-mode . lin-mode)
  (text-mode . lin-mode))

;; highlight cursor when scroll window
(use-package beacon
  :straight (:type git :host github
		   :repo "Malabarba/beacon")
  :hook
  (after-init . beacon-mode))


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
(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-listing-switches "-alFhv")
  (setq dired-dwim-target t)
  (setq dired-dwim-target t)
  :bind
  ;; % - m to mark regex
  (:map dired-mode-map
	("o" . dired-display-file)
	("<mouse-2>" . dired-mouse-find-file)))

(use-package dired-hacks-utils
  :hook
  (dired-mode . dired-utils-format-information-line-mode)
  :bind
  (:map dired-mode-map
	("j" . dired-hacks-next-file)
	("k" . dired-hacks-previous-file)))

;; to replace slow tramp copy
;; maybe need to reinstall rsync in MacOS
(use-package dired-rsync
  :bind
  (:map dired-mode-map
	("C-c C-r" . dired-rsync)))

;; (use-package pulsing-cursor
;;   :straight (:type git :host github
;; 		   :repo "jasonjckn/pulsing-cursor")
;;   :hook
;;   (after-init . pulsing-cursor-mode))


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
;; (electric-pair-mode t)
;; (setq electric-pair-pairs '((?\" . ?\")
;; 			    (?\` . ?\`)
;; 			    (?\( . ?\))
;; 			    (?\{ . ?\})))

;; use lispy-mode (a vi-like editing) for lisp parentheses
;; remove electric-pair-mode first
;; (add-hook 'emacs-lisp-mode-hook (lambda ()
;; 				  (electric-pair-local-mode -1)))

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

(require 'nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; all the icons for completion framework (e.g. vertico)
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


;; defer if it's slow
(use-package dashboard
  :if (and (< (length command-line-args) 2)
	   (fboundp 'native-comp-available-p))
  :config
  (setq dashboard-set-init-info nil)
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Hahahahahah??	 ಠ_ಠ")
  ;;    (setq dashboard-startup-banner 3)
  (setq dashboard-startup-banner "~/.emacs.d/fancy-splash/world_origin.png")

  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)

	                  ;; (bookmarks . 5)
			  ;; (projects . 5)
			  ;; (agenda . 5)
			  ;; (registers . 5)
                          )) ;;add org-agenda could slow start-up speed

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))) ;; show Dashboard in frames created with emacsclient -c
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  ;; (define-key dashboard-mode-map (kbd "m") 'next-line)
  ;; (define-key dashboard-mode-map (kbd "p") 'previous-line)
  )

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


(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq inhibit-compacting-font-caches t))


(use-package all-the-icons-dired
  :if window-system
  ;;need to run all-the-icons-install-fonts first to avoid grabled icon
  :hook
  (dired-mode . all-the-icons-dired-mode))

(global-tab-line-mode t)

;; beautify hydra
(use-package pretty-hydra
  :after all-the-icons
  :config
  ;; (defvar hydra-ui-title (s-concat (s-repeat 20 " ")
  ;; 				   (all-the-icons-faicon "windows")
  ;; 				   " Apperance"))
  ;; (pretty-hydra-define hydra-ui
  ;;   (:foreign-keys warn :title hydra-ui-title :quit-key "q")
  ;;   ("Theme"
  ;;    (("d" night-theme "dark-theme")
  ;;     ("l" light-theme "light-theme")
  ;;     ("t" counsel-load-theme "choose"))))
  )

;; a center-floating posframe for hydra
(use-package hydra-posframe
  :after hydra
  :straight
  (:type git :host github
	 :repo "Ladicle/hydra-posframe")
  :hook
  (after-init . hydra-posframe-mode))

;; to display ^L page break
(use-package form-feed
  :hook
  (emacs-lisp-mode . form-feed-mode))


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
	(set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "Iosevka" 15))
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
(use-package solo-jazz-theme
  :ensure t
  :config
  (load-theme 'solo-jazz t))
(use-package twilight-bright-theme :defer t )
(use-package ample-theme :defer t )
;; (use-package eziam-theme :defer t ) ;;almost perfect light theme
;; (use-package eziam-light-theme
;;     :ensure eziam-theme))
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
;; (setq-default custom-enabled-themes '(humanoid-light))

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

;; Varialble/fixed pictch font setting, essential for org-mode
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
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-ellipsis ((t (:foreground "#D2B38C"))))
 )


(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.2 :weight bold :background "#cad9d3" :overline "#A7A6AA" :foreground "#080808"))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

(custom-set-faces
 '(org-block-begin-line
   ((t (:foreground "#6a6f73" :background "#e5e5e5" :box (:line-width 1 :style released-button) :slant italic
		    :weight semi-bold))))
 '(org-block
   ((t (:background "#fdfff4"))))
 '(org-block-end-line
   ((t (:foreground "#6a6f73" :background "#e5e5e5"))))
 '(org-meta-line
   ((t (:background "#E7E7E7" :slant italic))))
 )

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

;; Option 1: Per buffer
(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))
(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t
 org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
 org-modern-list '((43 . "➤")
		   (45 . "–")
                   (42 . "•"))

 ;; Org styling, hide markup etc.
 ;; org-hide-emphasis-markers t

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "◀── now ─────────────────────────────────────────────────")
(setq org-ellipsis " ▿")

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
;; LSP-bridge
(add-to-list 'load-path "~/.emacs.d/lsp-bridge")
(require 'lsp-bridge)
;; (global-lsp-bridge-mode)
(setq acm-enable-icon t)
(setq lsp-bridge-complete-manually nil)
;; (define-key lsp-bridge-mode-map (kbd "<return>") 'acm-complete)

;; requires install `sbcl`
;; REPL for lisp
;; (use-package slime
;;   :config
;;   (setq inferior-lisp-program "sbcl"))

;; (use-package stan-mode
;;   :mode ("\\.stan\\'" . stan-mode)
;;   :hook (stan-mode . stan-mode-setup)
;;   ;;
;;   :config
;;   ;; The officially recommended offset is 2.
;;   (setq stan-indentation-offset 2))

;; sticky header of code block
(use-package topsy
  :straight (topsy :fetcher github :repo "alphapapa/topsy.el")
  :hook (prog-mode . topsy-mode))

;;==============================
;;           Python           ;;
;;==============================

;; Interpreter choice
(setq python-shell-interpreter "/usr/bin/python3")
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
  ;; (setq ein:use-company-backend t)
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

(use-package cmake-mode
  :mode ("\\.cmake\\'" "CMakeLists\\.txt\\'"))


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
  (sp-local-pair 'org-mode "\\{" " \\}")
  (sp-local-pair 'LaTeX-mode "$" "$")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "*" "* ")
  (sp-local-pair 'org-mode "/" "/ ")
  (sp-local-pair 'org-mode "~" "~ ")
  (sp-local-pair 'org-mode "_" "_ ")
  (sp-local-pair 'org-mode "\"" "\" ")
  (sp-local-pair 'xenops-mode "$" "$")
  (sp-local-pair 'org-cdlatex-mode "$" "$")
  (sp-local-pair 'org-mode "'" "'" :actions '(rem))
  (sp-local-pair 'org-mode "=" "=" :actions '(rem))
  (sp-local-pair 'xenops-mode "\\bigl(" "\\bigr)" :trigger "\\l(" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left[" "\\right]" :trigger "\\l[" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left\\{" "\\right\\}" :trigger "\\l{" :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'xenops-mode "\"" "\" " :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-cdlatex-mode "\"" "\" " :post-handlers '(sp-latex-insert-spaces-inside-pair))
  (sp-local-pair 'org-mode "\\left|" "\\right|" :trigger "\\l|" :post-handlers '(sp-latex-insert-spaces-inside-pair)))

;; (with-eval-after-load 'org
;;  (modify-syntax-entry ?/ "(/ " org-mode-syntax-table)
;;  (modify-syntax-entry ?~ "(~ " org-mode-syntax-table)
;;  (modify-syntax-entry ?_ "(_ " org-mode-syntax-table)
;;   (modify-syntax-entry ?* "(* " org-mode-syntax-table))

;; location
(setq calendar-longitude 144.946457)
(setq calendar-latitude -37.840935)

;;Sunrise and Sunset
;;日出而作, 日落而息
(defun diary-sunrise ()
  (let ((dss (diary-sunrise-sunset)))
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ",")
      (buffer-substring (point-min) (match-beginning 0)))))

(defun diary-sunset ()
  (let ((dss (diary-sunrise-sunset))
        start end)
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ", ")
      (setq start (match-end 0))
      (search-forward " at")
      (setq end (match-beginning 0))
      (goto-char start)
      (capitalize-word 1)
      (buffer-substring start end))))

(use-package leetcode
  :defer t
  :init
  (setq leetcode-prefer-language "python3"
        leetcode-prefer-sql "mysql"
        leetcode-save-solutions t
        leetcode-directory "~/SLIU/randomness/leetcode")
  (add-hook 'leetcode-solution-mode-hook
            (lambda() (flycheck-mode -1))))



;; (with-eval-after-load 'ox-latex
;;   ;; cleanup org-mode export intermediary files
;;   (add-to-list 'org-latex-logfiles-extensions "bbl")
;;   (add-to-list 'org-latex-logfiles-extensions "bib")
;;   )


(defun copy-figures-from-subsubdirs ()
  "Copy .svg and .pdf_tex files from subsubdirectories named 'figures' into a local './figures' directory next to the current buffer's file. Prompts before overwriting."
  (interactive)
  (let* ((buffer-file (buffer-file-name)))
    (unless buffer-file
      (user-error "This buffer is not visiting a file."))
    (let* ((base-dir (file-name-directory buffer-file))
           (target-dir (expand-file-name "figures" base-dir))
	   (found-figures-dirs '())
           (found-files '())
           (copied-files '())
	   (overwrite-all nil))
      ;; Ensure target directory exists
      (unless (file-directory-p target-dir)
        (make-directory target-dir t))

      ;; Find all directories ending in '/figures' at least two levels deep
      (dolist (dir1 (directory-files base-dir t "^[^.]"))
        (when (file-directory-p dir1)
          (dolist (dir2 (directory-files dir1 t "^[^.]"))
            (when (and (file-directory-p dir2)
                       (string= (file-name-nondirectory dir2) "figures"))
              (message "Found figures directory: %s" dir2)
              (push dir2 found-figures-dirs)))))
      ;; Step 2: Match with regex
      (dolist (fig-dir found-figures-dirs)
        (dolist (f (directory-files-recursively fig-dir ".*\\(\\.svg\\|\\.pdf_tex\\|\\.pdf\\)$" t)))
        (setq found-files (append found-files
                                  (directory-files-recursively fig-dir ".*\\(\\.svg\\|\\.pdf_tex\\|\\.pdf\\)$" t))))

      ;; Print found files for debugging
      (if found-files
          (progn
            (message "Found %d file(s):" (length found-files))
            (dolist (f found-files)
              (message "Found: %s" f)))
        (message "No matching files found in any subsubdirectory named 'figures'."))

      ;; Copy files with prompt
      ;; (dolist (file found-files)
      ;;   (let ((target-path (expand-file-name (file-name-nondirectory file) target-dir)))
      ;;     (if (file-exists-p target-path)
      ;;         (when (yes-or-no-p (format "File %s exists. Overwrite?" (file-name-nondirectory file)))
      ;;           (copy-file file target-path t)
      ;;           (push (cons file target-path) copied-files))
      ;;       (copy-file file target-path)
      ;;       (push (cons file target-path) copied-files))))

      ;; Step 4: Copy with default "Yes" overwrite
      ;; (dolist (file found-files)
      ;;   (let ((target-path (expand-file-name (file-name-nondirectory file) target-dir)))
      ;;     (if (file-exists-p target-path)
      ;;         (if (y-or-n-p (format "File %s exists. Overwrite? " (file-name-nondirectory file)))
      ;;             (copy-file file target-path t))
      ;;       (copy-file file target-path))
      ;;     (push (cons file target-path) copied-files))

      ;; Step 4: Ask once if the user wants to overwrite all
      (setq overwrite-all (yes-or-no-p "Do you want to overwrite all existing files in the target directory?"))

	(dolist (file found-files)
           (let ((target-path (expand-file-name (file-name-nondirectory file) target-dir)))
	     (if (file-exists-p target-path)
	      (if overwrite-all
		  (progn
		    (copy-file file target-path t) ;; Overwrite all without prompting
		    (message "Automatically overwrote: %s" file))
		(if (y-or-n-p (format "File %s exists. Overwrite? " (file-name-nondirectory file)))
		    (progn
		      (copy-file file target-path t)
		      (message "Overwritten: %s" file))
		  (message "Skipped: %s" file)))
	    (copy-file file target-path)) ;; If file doesn't exist, just copy
	  (push (cons file target-path) copied-files))


      ;; Report copied files
      (if copied-files
          (progn
            (message "Copied %d file(s) to %s:" (length copied-files) target-dir)
            (dolist (pair copied-files)
              (message "Copied: %s -> %s" (car pair) (cdr pair))))
        (message "No files copied."))))))
;; ========================
;;       thesis hydra    ;;
;; ========================
;; thesis hydra.
(defhydra thesis-menu-transient-state (:color pink
				       :hint nil)
  "
                 P.h.D Thesis Menu
^Main Files^       ^Chapters^       ^Actions^
^^^^^^^^-------------------------------------------
_m_: Thesis        _1_: Research 1  _o_: Open Thesis.pdf externally
_t_: Title page    _2_: Research 2  _c_: Async compile file
_i_: Introduction  _3_: Research 3  _q_: Cancel
_s_: thesis.setup  ^ ^              _u_: Update figures
_d_: Summary       ^ ^              ^ ^
"
      ("q" nil :exit t)
      ("m" (find-file "~/Documents/monash/writing/org_thesis/thesis/thesis.org") :exit t)
      ("t" (find-file "~/Documents/monash/writing/org_thesis/thesis/title.org") :exit t)
      ("s" (find-file "~/Documents/monash/writing/org_thesis/setup/thesis.setup") :exit t)
      ("i" (find-file "~/Documents/monash/writing/org_thesis/thesis/introduction/introduction.org") :exit t)
      ("d" (find-file "~/Documents/monash/writing/org_thesis/thesis/summary/summary.org") :exit t)
      ("1" (find-file "~/Documents/monash/writing/org_thesis/thesis/ch1/research.org") :exit t)
      ("2" (find-file "~/Documents/monash/writing/org_thesis/thesis/ch2/research.org") :exit t)
      ("3" (find-file "~/Documents/monash/writing/org_thesis/thesis/ch3/research.org") :exit t)
      ("u" (copy-figures-from-subsubdirs) :exit t)
      ("o" (shell-command "open ~/Documents/monash/writing/org_thesis/thesis/thesis.pdf" :exit t))
      ("c" (org-latex-export-to-pdf :async t) :exit t))

(global-set-key (kbd "s-t") 'thesis-menu-transient-state/body)


;; ========================
;;       cover letter    ;;
;; ========================
;; https://orgmode.org/manual/LaTeX-specific-export-settings.html
(add-to-list 'org-latex-packages-alist
             '("AUTO" "babel" t ("pdflatex")))
;; (add-to-list 'org-latex-packages-alist
;;              '("AUTO" "polyglossia" t ("xelatex" "lualatex")))
(eval-after-load 'ox '(require 'ox-koma-letter))
 ;; KOMA-SCript letter
(eval-after-load 'ox-koma-letter
    '(progn
       (add-to-list 'org-latex-classes
                    '("my-koma-letter"
                      "\\documentclass\{scrlttr2\}
\\usepackage[hidelinks,unicode]{hyperref}
[NO-DEFAULT-PACKAGES]"))

       (setq org-koma-letter-default-class "my-koma-letter")))

;; (setq org-koma-letter-closing "Yours sincerely,")


;; stop org from prettifying sub and superscripts
(add-hook 'org-cdlatex-mode-hook
	  (lambda () (when (eq major-mode 'org-mode)
		       (make-local-variable 'org-pretty-entities-include-sub-superscripts)
		       (setq org-pretty-entities-include-sub-superscripts nil))))

(add-hook 'org-cdlatex-mode-hook
	  (lambda () (when (eq major-mode 'org-msg-mode)
		       (make-local-variable 'org-pretty-entities-include-sub-superscripts)
		       (setq org-pretty-entities-include-sub-superscripts nil))))

;; ==============================================================================
;;            export example blocks to example environment in latex            ;;
;; ==============================================================================

(defun my-latex-export-example-blocks (text backend info)
  "Export example blocks as listings env."
  (when (org-export-derived-backend-p backend 'latex)
    (with-temp-buffer
      (insert text)
      ;; replace verbatim env by listings
      (goto-char (point-min))
      (replace-string "\\begin{verbatim}" "\\begin{Example}")
      (replace-string "\\end{verbatim}" "\\end{Example}")
      (buffer-substring-no-properties (point-min) (point-max)))))

(add-to-list 'org-export-filter-example-block-functions
         'my-latex-export-example-blocks)

;; ========================
;;       org-pomodoro    ;;
;; ========================

 (use-package org-pomodoro
   :config
   (setq
    org-pomodoro-manual-break t
    org-pomodoro-ticking-sound-p nil

    org-pomodoro-length 25
    ;; org-pomodoro-length 1
    org-pomodoro-start-sound-p t
    org-pomodoro-start-sound (expand-file-name "etc/pomodoro/ringHigh.wav" user-emacs-directory)
    org-pomodoro-overtime-sound-p t
    org-pomodoro-overtime-sound (expand-file-name "etc/pomodoro/chimeTriple.wav" user-emacs-directory)
    org-pomodoro-finished-sound-p t
    org-pomodoro-finished-sound (expand-file-name "etc/pomodoro/ringMiddle.wav" user-emacs-directory)                 ;;;; same as break-sound

    org-pomodoro-finished-before-long-break-sound-p t
    org-pomodoro-finished-before-long-break-sound (expand-file-name "etc/pomodoro/chimeLow.wav" user-emacs-directory) ;;;; same as long-break-sound
    org-pomodoro-clock-break t                                                                                         ;;;; clock breaks, too
    org-pomodoro-short-break-length 5
    ;; org-pomodoro-short-break-length 1
    org-pomodoro-short-break-sound-p t
    org-pomodoro-short-break-sound (expand-file-name "etc/pomodoro/ringMiddle.wav" user-emacs-directory)

    org-pomodoro-long-break-frequency 4
    org-pomodoro-long-break-length 35
;; org-pomodoro-long-break-length 1
    org-pomodoro-long-break-sound-p t
    org-pomodoro-long-break-sound (expand-file-name "etc/pomodoro/chimeLow.wav" user-emacs-directory)

  ;;;; modeline
    org-pomodoro-time-format "%.2m:%.2s"
    org-pomodoro-format "P %s"
    ;; org-pomodoro-format " %s"
    org-pomodoro-overtime-format "+ %s"
    org-pomodoro-short-break-format "S %s"
    org-pomodoro-long-break-format "L %s"
    )
   )


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


;; (require 'org-agenda)
;; agenda
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-tags-column 17)
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
(setq org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-deadline-warning-days 5
      org-agenda-start-on-weekday nil
      org-reverse-note-order t
      org-agenda-start-with-log-mode t
      org-log-done 'time
      org-log-into-drawer t
      org-agenda-include-deadlines t)


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
  )

;; Set the times to display in the time grid
;;---------------------------------------------
;;org-agenda-time-grid
;;--------------------------------------------
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   (300
                                    600
                                    900
                                    1200
                                    1500
                                    1800
                                    2100
                                    2400)
                                   "......"
                                   "-----------------------------------------------------"
                                   )))
;; Compact the block agenda view (disabled)
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
        '((" " "Review"
	   (
	    (agenda "" ((org-agenda-overriding-header "Today's Schedule:")
			(org-agenda-span 'day)
			(org-agenda-ndays 1)
			(org-agenda-start-on-weekday nil)
			(org-agenda-start-day "+0d")
			(org-agenda-entry-text-mode t)))

	    (agenda "" ((org-agenda-overriding-header "Week At A Glance:")
 					    (org-agenda-ndays 5)
 					    (org-agenda-start-day "+1d")
 					    ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
 					    (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %s [%b] ")))
					    (org-agenda-include-diary nil)))
           ))
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
           ((agenda "" ((org-super-agenda-groups '(
                                                   ;; (:name "⏰ Today              " :and (:not (:deadline furture) :not (:scheduled future)))
                                                   (:name "⭐⭐⭐ Due Today ⭐⭐⭐    " :deadline today :scheduled today)
                                                   (:name "⇒ Due soon            " :deadline future)
                                                   (:name "⚠ OVERDUE! ⚠          " :order 1 :deadline past)
                                                   (:name "⇒ Consider soon       " :order 2 :scheduled future)
                                                   (:name "⚠ Behind              " :and (:scheduled past :not (:category "catchup")))
                                                   (:name "⚠ Catchup             " :category "catchup")))
			(org-agenda-time-grid nil)
			(org-agenda-include-diary nil)
			)
		    (org-agenda nil "a")))
	   )
	  )
	)
  )
;; ==============================
;;      display math in org    ;;
;; ==============================
(use-package xenops
  :after org
  :bind (
         (("C-c C-g C-c" . xenops-reveal-at-point))
         (("C-c C-g C-i" . xenops-increase-size)))
  :config
  (setq xenops-math-image-scale-factor 1.8
	xenops-reveal-on-entry nil)
  (add-hook 'xenops-mode-hook 'xenops-render)
  )

(setq xdvsvgm
        '(xdvsvgm
          :programs ("latex" "dvisvgm")
          :description "dvi > svg"
          :message "you need to install the programs: latex and dvisvgm."
          :use-xcolor t
          :image-input-type "dvi"
          :image-output-type "svg"
          :image-size-adjust (1.7 . 1.5)
          :latex-compiler ("latex -no-pdf -interaction nonstopmode -output-directory %o %f")
          :image-converter ("dvisvgm %f -n -b %B -c %S -o %O --libgs=/opt/homebrew/Cellar/ghostscript/10.02.1/lib/libgs.10.dylib")))
(with-eval-after-load 'xenops
  (add-to-list 'xenops-math-latex-process-alist xdvsvgm)
  (setq xenops-math-latex-process 'xdvsvgm))


;; ============================
;;       org2latex           ;;
;; ============================

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
  (shell-command (concat "lualatex -shell-escape -interaction nonstopmode -output-directory %o" (buffer-file-name)))
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
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "C-c r") 'md-compile-and-update-other-buffer))

(define-key org-mode-map (kbd "C-c h r") 'org-compile-latex-and-update-other-buffer)
(define-key org-mode-map (kbd "C-c h b") 'org-compile-beamer-and-update-other-buffer)
(define-key org-mode-map (kbd "C-c k") 'my-org-mark-ring-goto)



;; =======================
;;       inkscape       ;;
;; =======================
;; inkscape-figure-manager
(defun inkscape-watch ()
  "watch for figures"
  (interactive)
  (shell-command "inkscape-figures watch")
  ;; (shell-command (concat "cd " (file-name-directory (buffer-file-name)) "figures/" "&& inkscape-figures-manager start"))
  )

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

(defun inkscape-quit ()
  "kill the inkscape figures manager"
  (interactive)
  (shell-command "pkill inkscape-figures-manager start"))

(define-key org-mode-map (kbd "C-c h w") 'inkscape-watch);
(define-key org-mode-map (kbd "C-c h c") 'inkscape-create);
(define-key org-mode-map (kbd "C-c h e") 'inkscape-edit);
(define-key org-mode-map (kbd "C-c h s") 'inkscape-quit);

(setq org-link-file-path-type 'noabbrev)
(delete '("\\.pdf\\'" . default) org-file-apps)
(add-to-list 'org-file-apps '("\\.pdf\\'" . "open -a Skim %s"))
(add-to-list 'org-file-apps
             '("\\.pdf::\\([0-9]+\\)\\'" . "open skim://%s#page=%1"))

;; (use-package pdf-tools
;;   :straight
;;   (:type git :host github :repo "dalanicolai/pdf-tools"
;; 	 :branch "pdf-roll"
;; 	 :files ("lisp/*.el"
;; 		 "README"
;; 		 ("build" "Makefile")
;; 		 ("build" "server")
;; 		 (:exclude "lisp/tablist.el" "lisp/tablist-filter.el")))
;;   ;; :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
;;   :magic ("%PDF" . pdf-view-mode)
;;   :hook
;;   (pdf-view-mode . pdf-view-roll-minor-mode))

;; (use-package image-roll
;;   :straight
;;   (:type git :host github :repo "dalanicolai/image-roll.el"))

;; This is to use pdf-tools instead of doc-viewer
(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  :hook
  (pdf-view-mode . (lambda ()
		     (display-line-numbers-mode -1))))


;; (use-package org-noter
;;   :after (:any org pdf-view)
;;   :straight
;;   (:type git :host github :repo "petermao/org-noter")
;;   :config
;;   (setq
;;    org-noter-notes-search-path "~/org/org-roam/references/"
;;    org-noter-hide-other nil
;;    org-noter-separate-notes-from-heading t
;;    org-noter-always-create-frame t)
;;   )

;; (use-package pdf-tools-org-noter-helpers :ensure t
;;     :straight (
;;                :type git :repo "https://github.com/analyticd/pdf-tools-org-noter-helpers")
;;     :config
;;     (require 'pdf-tools-org-noter-helpers))

;; (use-package org-noter-pdftools
;;   :after pdf-tools
;;   :after (org-noter)
;;   :config
;;   ;; Add a function to ensure precise note is inserted
;;   (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((org-noter-insert-note-no-questions (if toggle-no-questions
;;                                                    (not org-noter-insert-note-no-questions)
;;                                                  org-noter-insert-note-no-questions))
;;            (org-pdftools-use-isearch-link t)
;;            (org-pdftools-use-freestyle-annot t))
;;        (org-noter-insert-note (org-noter--get-precise-info)))))

;;   ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
;;   (defun org-noter-set-start-location (&optional arg)
;;     "When opening a session with this document, go to the current location.
;; With a prefix ARG, remove start location."
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((inhibit-read-only t)
;;            (ast (org-noter--parse-root))
;;            (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
;;        (with-current-buffer (org-noter--session-notes-buffer session)
;;          (org-with-wide-buffer
;;           (goto-char (org-element-property :begin ast))
;;           (if arg
;;               (org-entry-delete nil org-noter-property-note-location)
;;             (org-entry-put nil org-noter-property-note-location
;;                            (org-noter--pretty-print-location location))))))))
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
;; }} org-noter



;;use org-superstar-mode to replace org-bullets
;; (use-package org-superstar
;;   :ensure t
;;   :after org
;;   :config
;;   (setq org-superstar-leading-bullet " ")
;;   (setq org-superstar-special-todo-items t)
;;   (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
;;   (setq org-superstar-headline-bullets-list
;;         '("◉" ?* "○" "" "✿" "✸"))
;;   :custom
;;   ;; (org-ellipsis " ⚡")
;;   (org-ellipsis " ▿")
;;   )

;;prettify-symbols-mode setting
;; (add-hook 'org-mode-hook 'prettify-symbols-mode)
;; (setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "➤")
;; 				       ("#+END_SRC" . "➤")
;; 				       ("#+begin_src" . "➤")
;; 				       ("#+end_src" . "➤")
;; 				       ;; ("#+BEGIN_EXAMPLE" . "💡")
;; 				       ;; ("#+END_EXAMPLE" . " ")
;; 			      	       ;; ("#+BEGIN_Remark" . "📝")
;; 				       ;; ("#+END_Remark" . " ")
;; 				       ;; ("#+TITLE" . "📓")
;; 				       ;; ("#+LATEX_HEADER" . "⚒")
;; 				       ;; ("#+SETUPFILE" . "⚒")
;; 				       ;; ("#+LATEX_CLASS" . "⚒")
;; 				       ;; ("#+LATEX_CLASS_OPTIONS" . "⚒")
;; 				       ;; (">=" . "≥")
;; 				       ;; ("=>" . "⇨")
;; 				       ("[-]" . "❍" )
;; 				       ("[ ]" .  "☐")
;; 				       ("[X]" . "☑" )))
(setq prettify-symbols-unprettify-at-point 'right-edge)

(use-package org-fancy-priorities
  :defer t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("❗❗❗" "❗❗" "❗")))

;; pretty symbol for org-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (push '("[ ]" . "☐") prettify-symbols-alist)
	    (push '("[X]" . "☑") prettify-symbols-alist)
	    (push '("[-]" . "◫") prettify-symbols-alist)
	    (prettify-symbols-mode)))

(use-package svg-tag-mode
  :init
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
		(svg-lib-progress-bar (/ (string-to-number value) 100.0)
				      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		(svg-lib-tag (concat value "%")
			     nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
	   (count (float (car seq)))
	   (total (float (cadr seq))))
      (svg-image (svg-lib-concat
		  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
		  (svg-lib-tag value nil
			       :stroke 0 :margin 0)) :ascent 'center)))

  (setq svg-tag-tags
	`(
	  ;; Org tags
	  (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
	  (":\\([A-Za-z0-9_]+\\)" . ((lambda (tag) (svg-tag-make tag))))

	  ;; Task priority
	  ("\\[#[A-Z]\\]" . ((lambda (tag)
			       (svg-tag-make tag :face 'org-priority
					     :beg 2 :end -1 :margin 0))))

	  ;; Progress
	  ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
					      (svg-progress-percent (substring tag 1 -2)))))
	  ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
					    (svg-progress-count (substring tag 1 -1)))))

	  ;; TODO / DONE
	  ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
	  ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


	  ;; Citation of the form [cite:@Knuth:1984]
	  ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
					    (svg-tag-make tag
							  :inverse t
							  :beg 7 :end -1
							  :crop-right t))))
	  ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
						     (svg-tag-make tag
								   :end -1
								   :crop-left t))))

	  ;; org-ref citation
	  ("\\(\\[\\[cite:&[A-Za-z0-9_]+\\)" . ((lambda (tag)
					    (svg-tag-make tag
							  :inverse t
							  :beg 8))))
	  ("\\(;&[A-Za-z0-9_]+\\)" . ((lambda (tag)
					    (svg-tag-make tag
							  :inverse t
							  :beg 2))))

	  ;; Active date (with or without day name, with or without time)
	  (,(format "\\(<%s>\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0))))
	  (,(format "\\(<%s \\)%s>" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
	  (,(format "<%s \\(%s>\\)" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

	  ;; Inactive date  (with or without day name, with or without time)
	  (,(format "\\(\\[%s\\]\\)" date-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
	  (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
	  (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
	   ((lambda (tag)
	      (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

  :hook org-mode)


;;Image drag-and-drop for org-mode
(use-package org-download
  :after org
  :hook
  (dired-mode . org-download-enable)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  (org-download-screenshot-method "/opt/homebrew/bin/pngpaste %s")
  :bind
  ("C-M-y" . org-download-screenshot)
  :config
  (require 'org-download)
  )

;;(use-package org-super-agenda)
(use-package org-graph-view
  :defer t
  :straight (org-graph-view :type git :host github
			    :repo "alphapapa/org-graph-view"))


;; A personal knowlege database tool, lateral linking
;; require sqlite, check org-oram--sqlite-available-p
(use-package emacsql-sqlite3
  :ensure t)
(use-package org-roam
  :defer t
  :custom
  (org-roam-directory "~/Documents/monash/writing/org_thesis/research_chapter/roamnotes")
  (add-hook 'after-init-hook 'org-roam-mode)
  (org-roam-capture-templates
   '(("m" "main" plain
      "%?"
      :if-new (file+head "main/${slug}.org"
			 "#+title: ${title}\n#+LATEX_CLASS: article\n#+LATEX_CLASS_OPTIONS: [a4paper, 11pt]
#+SETUPFILE: ~/Documents/monash/writing/org_thesis/setup/manuscript.setup
#+LATEX_HEADER:\\addbibresource{~/Documents/monash/writing/ref/references.bib}")
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
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-mode-sections
      '((org-roam-backlinks-section :unique t)
        org-roam-reflinks-section))
  )

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))


(use-package simple-httpd)
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
        org-roam-ui-open-on-start t)
  (setq org-roam-ui-custom-theme
    '((bg . "#1E2029")
        (bg-alt . "#282a36")
        (fg . "#f8f8f2")
        (fg-alt . "#6272a4")
        (red . "#ff5555")
        (orange . "#f1fa8c")
        (yellow ."#ffb86c")
        (green . "#50fa7b")
        (cyan . "#8be9fd")
        (blue . "#ff79c6")
        (violet . "#8be9fd")
        (magenta . "#bd93f9")))
  )

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
  (setq-default TeX-engine 'luatex) ;;default engine

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              ;;(setq TeX-command-default "latexmk")
              (rainbow-delimiters-mode t)
              ;; (company-mode t)
              (smartparens-mode t)
              (reftex-isearch-minor-mode t)
	      ;; (LaTeX-add-environments "equation*")
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'auto)
	      (setq TeX-source-correlate-mode t)
              (setq TeX-source-correlate-start-server t))))


;; Yasnippet settings
(use-package yasnippet
  :ensure t
  :hook ((after-init . yas-global-mode)
	 (LaTeX-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (setq yas-triggers-in-field t)

  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

(require 'warnings)

(with-eval-after-load 'yasnippet
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

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
  (setq cdlatex-math-symbol-alist '((?. ("\\cdot" "\\circ" "\\odot"))
				    (?i ("\\in" "\\not\\in" "\\imath"))
				    (?B ("\\iota"))
				    (?j (nil nil "\\jmath"))
				    (?F ("\\Phi"))
				    (?\; ("\\;"))
				    (?& (nil))
				    (?9 ("\\cap" "\\bigcap"))
				    (?+ ("\\cup" "\\bigcup"))
				    (?V ("\\bigvee"))
				    (?W ("\\Xi" "\\Wedge"))
				    (?w ("\\xi" "\\wedge"))))


  (use-package yasnippet
    :bind (:map yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB" . yas-next-field-or-cdlatex)
	   :map cdlatex-mode-map
	   ("<tab>" . cdlatex-tab))
    :config
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
  (setq bibtex-completion-bibliography '("~/Documents/monash/writing/ref/references.bib")
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
      bibtex-autokey-name-year-separator "_"
      bibtex-autokey-year-title-separator "_"
      bibtex-autokey-titleword-separator "_"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(define-key bibtex-mode-map (kbd "s-b") 'org-ref-bibtex-hydra/body)

;; ox-pandoc needed for org-ref
(use-package ox-pandoc
  :ensure t)

(use-package org-ref
  :after org
  :straight (:type git :host github
		   :repo "jkitchin/org-ref")
  :init
  (define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)
  (define-key org-mode-map (kbd "s-<return>") 'org-ref-citation-hydra/body)

  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos)
  (require 'doi-utils))


(require 'org-ref-ivy)
(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))
      org-ref-default-ref-type "Cref")
(defhydra+ org-ref-insert-link-hydra (:color red :hint nil)
  ("q" nil "Quit"))

;; Marking candidates
(defun scimax-ivy-toggle-mark ()
  "Toggle the mark"
  (interactive)
  (if (ivy--marked-p)
      (ivy-unmark)
    (ivy-mark))
  (ivy-previous-line))

(define-key ivy-minibuffer-map (kbd "S-<tab>")
  #'scimax-ivy-toggle-mark)

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


;;; PDF READER

;; Compile and install: https://github.com/politza/pdf-tools
;; Install dependencies: cask, poppler, automake and setenv
;; Download source code and compile
;; (pdf-tools-install)
;;allow you to view pdf continuously
;; (use-package pdf-continuous-scroll-mode
;;   :after pdf-tools
;;   :straight (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el")
;;   :hook
;;   (pdf-view-mode-hook . pdf-continuous-scroll-mode))

;; Compile and install: https://github.com/politza/pdf-tools
;; Install dependencies: cask, poppler, automake and setenv
;; Download source code and compile
;; (pdf-tools-install)
;; (use-package pdf-tools
;;   :ensure t
;;   :commands (pdf-view-mode pdf-loader-install)
;;   :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
;;   :magic ("%PDF" . pdf-view-mode)
;;   :config
;;   (pdf-tools-install)
;;   (define-pdf-cache-function pagelables)
;;   ;;In case of high-resolution screen like Mac
;;   (setq pdf-view-use-scaling t
;; 	pdf-view-use-imagemagick nil)
;;   :hook
;;   (pdf-view-mode . (lambda ()
;; 		     (display-line-numbers-mode -1)))
;;   (pdf-view-mode . pdf-tools-enable-minor-modes)
;;   )



;; ======================
;;   email with mu4e  ;;
;; ======================

(require 'smtpmail)
;; managing emails with mu4e
(use-package mu4e
  :straight
  (:local-repo "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
	       :type built-in)
  :commands (mu4e)
  :bind
  ("C-x y" . mu4e)
  ("C-c C-c" . org-capture)
  :config
  :config
  (setq mu4e-headers-flagged-mark   `("F" . "!"))
  (setq mu4e-headers-trashed-mark   `("T" . "♻"))
  (setq mu4e-headers-attach-mark    `("a" . "#"))
  (setq mu4e-headers-encrypted-mark `("x" . "X"))
  (setq mu4e-headers-signed-mark    '("s" . "☡"))
  (setq mu4e-headers-unread-mark    `("u" . "⚑"))
  (setq mu4e-headers-new-mark       '("N" . "⚐"))
  (setq mu4e-headers-draft-mark     '("D" . "⚒"))
  (setq mu4e-headers-passed-mark    '("P" . "F"))
  (setq mu4e-headers-replied-mark   '("R" . "R"))
  (setq mu4e-headers-seen-mark      '("S" . ""))
  ;; (setq mu4e-mu-binary (executable-find "mu"))
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)
  ;; this command is called to sync imap servers:
  (setq mu4e-get-mail-command "mbsync -a")
  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-mu-binary "/opt/homebrew/bin/mu")

  (setq mu4e-maildir "~/Mail")

  (setq mu4e-drafts-folder "/monash/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/monash/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/monash/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/monash/[Gmail]/Bin")
  (setq mu4e-spam-folder "/monash/[Gmail]/Spam")

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
                  (mu4e-refile-folder . "/monash/[Gmail]/All Mail")
                  (mu4e-sent-folder . "/monash/[Gmail]/Sent Mail")
                  (mu4e-trash-folder . "/monash/[Gmail]/Bin")
		  ;; (mu4e-compose-signature . (concat "Regards,\n" "Shidan"))
		  ))))
  (setq mu4e-maildir-shortcuts
	'(("/monash/[Gmail]/Drafts" . ?d)
	  ("/monash/[Gmail]/Sent Mail" . ?s)
	  ("/monash/INBOX" . ?i)))

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
  (setq mu4e-attachment-dir "~/Documents")
  ;; ;; convert org mode to HTML automatically
  ;; (setq org-mu4e-convert-to-html t)

  ;; ;; need this to convert some e-mails properly
  ;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
  )


;; (load "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/org-mu4e.el") ;; best not to include the ending “.el” or “.elc”
;; (require 'org-mu4e)

;; (defalias 'org-mail 'org-mu4e-compose-org-mode)

(setq mail-user-agent 'mu4e-user-agent)
;;; setup org-msg
(use-package org-msg
  :straight t
  :after mu4e
  :config
  (setq
   org-msg-options "html-postamble:nil num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:dvipng eval:nil"
   org-msg-startup "hidestars indent inlineimages"
   org-msg-default-alternatives '((new . (text html))
                                  (reply-to-html . (text html))
                                  (reply-to-text . (text)))
   org-msg-convert-citation t
   org-pretty-entities-include-sub-superscripts nil
   org-msg-signature "

 Regards,

 #+begin_signature
  *Shidan* \\\\
 #+end_signature"
   ;; The default attachment matcher gives too many false positives,
   ;; it's better to be more conservative. See https://regex101.com/r/EtaiSP/4.
   org-msg-attached-file-reference
   "see[ \t\n]\\(?:the[ \t\n]\\)?\\(?:\\w+[ \t\n]\\)\\{0,3\\}\\(?:attached\\|enclosed\\)\\|\
(\\(?:attached\\|enclosed\\))\\|\
\\(?:attached\\|enclosed\\)[ \t\n]\\(?:for\\|is\\)[ \t\n]")
  (org-msg-mode)
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
