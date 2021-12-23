;;; package --- Guillaume Anciaux emacs config
;;; Commentary:

;;; package management
(require 'package)
;; we use use-package to do this for us
;;; Code:
(setq package-enable-at-startup nil)
;; use https for both melpa and gelpa if available
(if (gnutls-available-p)
    (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                             ("melpa" . "https://melpa.org/packages/")))
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/"))))

(package-initialize)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; uncomment to debug package loading times
;; (setq use-package-verbose t)

;; Bootstrap `use-package' from melpa
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package alert
  :ensure t
  :init (when (eq system-type 'gnu/linux)
          (setq alert-default-style 'notifications)))

(when (version< emacs-version "24.4")
  (alert "Emacs version too old - please run 24 or newer"
         :severity 'high))

;;; --------------- use ansi-colors for compilation -----------------------
(defun colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(use-package ansi-color
  :ensure t
  :config (add-hook 'compilation-filter-hook 'colorize-compilation))

;;; --------------- translate input characters ----------------------------
(use-package iso-transl)


;;; --------------- projectile---------------------------------------------
(use-package projectile
	     :ensure t
	     :config (progn
		       (add-to-list 'projectile-project-root-files "configure.ac")
		       (add-to-list 'projectile-project-root-files ".clang_complete")
		       (add-to-list 'projectile-project-root-files ".clang_complete.in")
		       (add-to-list 'projectile-project-root-files "AndroidManifest.xml")
		       (add-to-list 'projectile-project-root-files ".clang-format")))

;;; --------------- jedi --------------------------------------------------
(use-package jedi
	     :ensure t)
;; (global-auto-complete-mode t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;;; --------------- ein --------------------------------------------------
(use-package ein
	     :ensure t
	     :config (progn
		       (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
		       (global-set-key [M-f9] 'ein:worksheet-execute-all-cell)
		       (global-set-key [f6] 'ein:worksheet-split-cell-at-point)
		       (global-set-key [M-f6] 'ein:worksheet-merge-cell)
		       (global-set-key [M-f7] 'ein:worksheet-toggle-slideshow-view)
		       ))

;;(setq ein:use-auto-complete-superpack t)
;;(setq ein:complete-on-dot nil)

;;; --------------- js2-mode --------------------------------------------------
(use-package js2-mode
	     :ensure t
	     :defer t
	     :mode ("\\.js\\'" . js2-mode)
	     :init (progn
		     (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
		     ;; Add NodeJS error format
		     (setq compilation-error-regexp-alist-alist
			   (cons '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
					1 ;; file
					2 ;; line
					3 ;; column
					)
				 compilation-error-regexp-alist-alist))
		     (setq compilation-error-regexp-alist (cons 'node compilation-error-regexp-alist))))
;;; --------------- auctex ref tex --------------------------------------------------
(defun apm-latex-mode-setup ()
  "Tweaks and customisations for LaTeX mode."
  ;; smartparens latex support
  ;; (use-package smartparens-latex)
  ;; Enable source-correlate for Control-click forward/reverse search.
  (TeX-source-correlate-mode 1)
  ;; enable math mode in latex
  (LaTeX-math-mode 1)
  ;; Enable reftex
  (turn-on-reftex)
  ;; integrate with company
  (company-auctex-init))

(use-package auctex
  :ensure t
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init (progn
          (setq-default TeX-auto-save t)
          (setq-default TeX-parse-self t)
          (setq-default TeX-PDF-mode t)
          (setq-default TeX-master nil)
          (setq-default reftex-plug-into-AUCTeX t)
          (setq-default TeX-source-correlate-start-server t)

          (add-hook 'LaTeX-mode-hook #'apm-latex-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.ptex\\'" . latex-mode))
;;; --------------- encryption tool --------------------------------------------------
(require 'epa-file)
(epa-file-enable)
(setq epa-armor t)
;;; --------------- correction mode -----------------------------------------------
(defun my-corrector-mode()
  (interactive)
  (flyspell-mode 1)
  (flyspell-buffer)
  )

(global-set-key [f5] 'my-corrector-mode)
;;; --------------- shortcut keys --------------------------------------------------
    					; to go to next error with f10
(global-set-key [f10] 'next-error)
					; next match to f7
(global-set-key [f7] 'next-match)
					; gotoline with M-g
(global-set-key "\M-g" 'goto-line)

(global-set-key [M-f3] 'count-words)

;;; --------------- myrevert --------------------------------------------------

(defun my-revert-buffer()
  "Small function to bind revert action to a key."
  (interactive)
  (revert-buffer t t)
  )

(global-set-key [f8] 'my-revert-buffer)

;;; --------------- flycheck-pyflakes ----------------------------------------------
(add-hook 'python-mode-hook 'flycheck-mode)
(use-package py-autopep8
  :ensure t
  :config (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  )
(defun apm-pep8-hook()
  "Setup the key for formatting code in python."
  (global-set-key [M-f12] 'py-autopep8)
  )
(add-hook 'python-mode-hook 'apm-pep8-hook)

;;; --------------- jinja mode ----------------------------------------------
(setq auto-mode-alist
            (append '(("\\.tpl" . jinja2-mode)) auto-mode-alist))
(add-hook 'jinja-mode-hook 'jinja2-mode)
;;; --------------- git-gutter ----------------------------------------------
(use-package git-gutter
	     :ensure t
	     :init (progn
		     (global-git-gutter-mode +1)
					; to make smerge easier
		     (setq smerge-command-prefix "\C-cv")))
;;; --------------- akantu ----------------------------------------------

(c-add-style "llvm.org"
             '((fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((arglist-intro . ++)
                                   (innamespace . 0)
                                   (member-init-intro . ++)
                                   ))
               ))

(defconst akantu-c-style
  '("llvm.org")
  "Akantu C Programming Style.")

(c-add-style "akantu" akantu-c-style)
(setq c-default-style "akantu")

;;; --------------- company ----------------------------------------------
(use-package company
  :ensure t
  :commands global-company-mode
  ;; Use Company for completion
  :bind (:map company-mode-map ([remap completion-at-point] . company-complete))
  :init (progn
          ;; set default lighter as nothing so in general it is not displayed
          ;; but will still be shown when completion popup is active to show the
          ;; backend which is in use
          (setq company-lighter-base "")
          (global-company-mode 1))
  :config (progn
            ;; some better default values
            (setq company-idle-delay 0.5)
            (setq company-tooltip-limit 10)
            (setq company-minimum-prefix-length 2)

            ;; align annotations in tooltip
            (setq company-tooltip-align-annotations t)

            ;; nicer keybindings
            (define-key company-active-map (kbd "C-n") 'company-select-next)
            (define-key company-active-map (kbd "C-p") 'company-select-previous)
            (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

            (define-key company-active-map [tab] 'company-complete-common-or-cycle)
            (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)

            ;; put most often used completions at stop of list
            (setq company-transformers '(company-sort-by-occurrence))))

;;; --------------- company-irony ----------------------------------------------
(use-package company-irony
  :ensure t
  :after company
  :init (progn
	  (add-to-list 'company-backends 'company-irony)
	  (setq irony-additional-clang-options '("-std=c++17"))
	  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
	  ))

;;; --------------- flycheck-irony ---------------------------------------
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config (global-flycheck-mode 1))

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :config (progn
            (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
            (flycheck-add-next-checker 'irony '(warning . c/c++-cppcheck))))

;;; --------------- irony ----------------------------------------------


(defun apm-irony-mode-setup ()
  "Setup irony-mode."
  (irony-cdb-autosetup-compile-options)
  (with-eval-after-load 'company-irony
    (company-irony-setup-begin-commands))
  (with-eval-after-load 'irony-eldoc
    (irony-eldoc)))

;; autogenerate a .clang_complete if there is an associated .clang_complete.in
(defun apm-autogenerate-clang-complete ()
  "Autogenerate a .clang_complete if needed when opening a project."
  (when (and (fboundp 'projectile-project-root)
	     ;; handle if not in project by returning nil
	     (not (null (condition-case nil
			    (projectile-project-root)
			  (error nil))))
	     (file-exists-p (concat (file-name-as-directory
				     (projectile-project-root))
				    ".clang_complete.in")))
    (projectile-with-default-dir (projectile-project-root)
				 (shell-command "make .clang_complete"))))

(defun apm-irony-cdb-clang-complete--auto-generate-clang-complete (command &rest args)
  "Try and autogenerate a .clang_complete (COMMAND ARGS are ignored)."
  (apm-autogenerate-clang-complete))


(use-package irony
  :ensure t
  :diminish irony-mode
  :commands (irony-mode)
  :bind (:map irony-mode-map ([remap completion-at-point] . irony-completion-at-point-async)
              ([remap complete-symbol] . irony-completion-at-point-async))
  :init (progn
          (advice-add 'irony-cdb-clang-complete :before 'apm-irony-cdb-clang-complete--auto-generate-clang-complete)
          (add-hook 'c-mode-hook 'irony-mode)
          (add-hook 'c++-mode-hook 'irony-mode)
          (add-hook 'irony-mode-hook 'apm-irony-mode-setup)))

;;; --------------- spaceline ----------------------------------------------
(use-package spaceline-config           ; A beautiful mode line
  :ensure spaceline
  :init (setq spaceline-workspace-numbers-unicode t
              spaceline-window-numbers-unicode t)
  :config
  (spaceline-compile
   'lunaryorn
   ;; Left side of the mode line (all the important stuff)
   '(((buffer-modified buffer-size input-method) :face highlight-face)
     anzu
     '(buffer-id remote-host buffer-encoding-abbrev)
     ((point-position line-column buffer-position hud) :separator " | ")
     major-mode
     process
     (flycheck-error flycheck-warning flycheck-info)
     (python-pyvenv :fallback python-pyenv)
     ((which-function projectile-root) :separator " @ ")
     ((minor-modes :separator spaceline-minor-modes-separator) :when active))
   ;; Right segment (the unimportant stuff)
   '((workspace-number window-number)
     (battery :when active)
     (version-control :when active)))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-lunaryorn)))))

(use-package powerline                  ; The work-horse of Spaceline
  :ensure t
  :after spaceline-config
  :config (progn
            (setq powerline-height (truncate (* 1.0 (frame-char-height))))
            (setq powerline-default-separator 'utf-8)))

;;; --------------- ivy ----------------------------------------------

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :commands (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         ([remap switch-to-buffer] . ivy-switch-buffer))
  :init (progn
          (setq ivy-count-format ""
                ivy-display-style 'fancy)
          (ivy-mode 1)))

(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
;;; --------------- compile ----------------------------------------------

(use-package compile
    :bind ([f9] . recompile)
    ;; automatically scroll to first error on output
    :config (setq compilation-scroll-output 'first-error))

;;; --------------- magit ----------------------------------------------
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :bind ("C-x C-e" . magit-ediff-resolve)
)

;;; --------------- yaml ----------------------------------------------
(use-package yaml-mode
  :ensure t
  :defer t
)

(use-package flycheck-yamllint
  :ensure t
  :after flycheck
)
;;; --------------- clang-format ----------------------------------------------

(defun apm-clang-hook()
  "Setup the key for formatting code in C/C++"
  (global-set-key [M-f12] 'clang-format-buffer)
  (add-hook (make-local-variable 'before-save-hook)
            'clang-format-buffer)
  )

(use-package clang-format
  :ensure t
  :config (progn
    (setq clang-format-fallback-style "llvm")
    (add-hook 'c-mode-hook (function(lambda ()
			     (add-hook (make-local-variable 'before-save-hook)
				       'clang-format-buffer))))
    )
  )
(add-hook 'c-mode-hook 'apm-clang-hook)
(add-hook 'c++-mode-hook 'apm-clang-hook)

;; Added by Hnat Lesiv

;; Minimalistic look
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; That annoying bell sound
(setq ring-bell-function 'ignore)
;; My theme
(use-package jetbrains-darcula-theme
  :config
  (load-theme 'jetbrains-darcula t))

;; In order for pdf-tools to work correctly
;; (use-package pdf-tools
;;   :ensure t
;;   :pin manual ;; don't reinstall when package updates
;;   :mode  ("\\.pdf\\'" . pdf-view-mode)
;;   :config
;;   (setq-default pdf-view-display-size 'fit-page)
;;   (setq pdf-annot-activate-created-annotations t)
;;   (pdf-tools-install :no-query)
;;   (require 'pdf-occur))

;; Highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; Map to meta
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;;; --------------------------------------------------------------
;; added by emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clang-format-fallback-style "llvm")
 '(flycheck-python-flake8-executable "/usr/bin/python3")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(jetbrains pdf-tools highlight-numbers jetbrains-darcula-theme irony epa-file yasnippet use-package projectile modern-cpp-font-lock jedi git-gutter flymake-jslint flycheck-irony ein company-irony-c-headers auctex alert)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; emacs.el ends here
(put 'erase-buffer 'disabled nil)
