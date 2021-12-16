(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; --------------------------------------------------------------------------
;; Obtained from: https://linuxhint.com/c_emacs_configuration/
;; --------------------------------------------------------------------------


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(use-package try :ensure t)
(use-package which-key :ensure t :config (which-key-mode))

;; Snippets
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; Autocomplete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

;; Syntax checks and warnings
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

;; Syntax highlight for modern C++
(use-package modern-cpp-font-lock
  :ensure t)

;; Git integration
(use-package magit
  :ensure t
  :init
  (progn
    (bind-key "C-x g" 'magit-status)))

;; Compiling and running C++ Code
(defun code-compile ()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
     (let ((file (file-name-nondirectory buffer-file-name)))
       (format "%s -o %s %s"
           (if  (equal (file-name-extension file) "cpp") "g++" "gcc" )
           (file-name-sans-extension file)
           file)))
    (compile compile-command)))

(global-set-key [f9] 'code-compile)

;; --------------------------------------------------------------------------
;; Do not touch this
;; --------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5185a285365a768a30ac274bdbc4437e7fd2fbe3107a1b0f2b60e900181905e0" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "3b8284e207ff93dfc5e5ada8b7b00a3305351a3fb222782d8033a400a48eca48" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" default)))
 '(package-selected-packages
   (quote
    (doom-themes gruvbox-theme solarized-theme zenburn-theme markdown-preview-mode markdown-mode magit modern-cpp-font-lock auto-complete yasnippet-classic-snippets yasnippet-snippets company ggtags irony-eldoc irony use-package pdf-tools jetbrains-darcula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; --------------------------------------------------------------------------
;; My modifications
;; --------------------------------------------------------------------------

;; Startup
(setq inhibit-startup-message t)

;; Minimalistic look
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Nice font
;; (set-face-attribute 'default nil :font "Fira Code Retina" :height 115)

;; That annoying bell sound
(setq ring-bell-function 'ignore)

;; Theme
;; (load-theme 'zenburn t)
;; In order for pdf-tools to work correctly
(use-package pdf-tools
  :ensure t
  :pin manual ;; don't reinstall when package updates
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

;; My custom modifications
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; My theme
(use-package jetbrains-darcula-theme
  :config
 (load-theme 'jetbrains-darcula t))

;; Check spelling
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
