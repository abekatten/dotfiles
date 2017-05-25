;;; init.el --- JJ's main init file for emacs

;;; Commentary:

;;; Code:

;;
;; Set up package management with 'use-package'
;; -------------------------------------------------------

(require 'package)

;; Repositories
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(setq package-enable-at-startup nil)

(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Require and/or install this now as it enables :diminish inside use-package statement
(use-package diminish :ensure t)


;;
;; Essential base configuration
;; ------------------------------------------------

(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message t)


(setq ring-bell-function 'ignore)
(menu-bar-mode +1) ;; I want it visible for now
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(subword-mode +1) ;; make motions respect camelCase

;;(setq custom-safe-themes t) ;; I put this here to know that it exists

;; Move settings done through customize to separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Move backups to a single folder
;; Copied from https://www.emacswiki.org/emacs/BackupDirectory
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))
(setq
   backup-by-copying t      ; don't clobber symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;(require 'init-global-functions) - take a look a what he does


;; Load larger package specific configurations in correct order
;; ---------------------------------------------------------------
(require 'init-evil)
(require 'init-org)


;; Setup tiny-menu
;; --------------------------------------------------
(use-package tiny-menu :ensure t)
;; This has yes to be set up, but it looks very useful


;; Utilities
;; --------------------------------------------------

;; Better navigation inside the elisp documentation system
(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :config
  (elisp-slime-nav-mode)
  (eldoc-mode))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
	(list (cons "." "~/emacs.d/undo-tree-history"))))

(use-package linum-relative
 :ensure t
 :config
 (global-linum-mode 1)
 (global-set-key (kbd "<f12>") 'linum-relative-toggle))

(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired :ensure t)


;; Navigation, searching, finding, managing
;; Helm, dired, projectile, ace-window, winner, .....
;; -------------------------------------------------------------

(use-package ace-window
    :ensure t
    :config
    (global-set-key (kbd "s-$") 'ace-window)
    (set-face-attribute 'aw-leading-char-face nil
                        :foreground "grey"
                        :weight 'bold
                        :height 5.0)
;;    (add-hook 'ace-window-mode (lambda() 'evil-echo-state))
;;    (add-to-list 'evil-emacs-state-modes 'ace-window-mode)
    (winner-mode 1)
    (setq aw-dispatch-always t)
    (setq aw-dispatch-alist
          '((?x aw-delete-window      "AW - Window to delete")
            (?s aw-swap-window        "AW - Window to swap current with")
            (?- aw-split-window-vert  "AW - Window to split with horisontal line") ;; horisontal line
            (?i aw-split-window-horz  "AW - Window to split with vertical line" )  ;; vertical line
            (?m delete-other-windows  "AW - Window to maximize")                   ;; maximize
            (?b balance-windows)
            (?u winner-undo)
            (?r winner-redo))))

(use-package avy :ensure t)

(use-package dired
  :config
  (require 'dired-x)
  ;; dired-omit-files exists - check it out when clutter arises
  (add-hook 'dired-mode-hook (lambda ()
			       (dired-omit-mode t)
			       (all-the-icons-dired-mode t)))
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^")   (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "C-.") 'dired-omit-mode)
  (define-key dired-mode-map (kbd "c")   'find-file)
  (define-key dired-mode-map (kbd "/")   'counsel-grep-or-swiper)
  (define-key dired-mode-map (kbd "?")   'evil-search-backward)) 
  
(use-package projectile
  :ensure t
  :defer 1
  :config
  (projectile-global-mode)
  ;;(setq projectile-enable-cachin t) this one exists if it becomes slow
  )

(use-package helm
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark) 
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile helm-projectile-switch-project))
(use-package helm-ag :ensure t)

;; Autocompletion and syntax highlighting
;; --------------------------------------------
(use-package company
  :ensure t
  :diminish company-mode
  :init (global-company-mode)
  :config
  
  (defun org-keyword-backend (command &optional arg &rest ignored)
  "Company backend for org keywords.

COMMAND, ARG, IGNORED are the arguments required by the variable
`company-backends', which see."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'org-keyword-backend))
      (prefix (and (eq major-mode 'org-mode)
                   (let ((p (company-grab-line "^#\\+\\(\\w*\\)" 1)))
                     (if p (cons p t)))))
      (candidates (mapcar #'upcase
                          (cl-remove-if-not
                           (lambda (c) (string-prefix-p arg c))
                           (pcomplete-completions))))
      (ignore-case t)
      (duplicates t)))
  
  (add-to-list 'company-backends 'org-keyword-backend)

  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "ESC") 'company-abort)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package company-jedi :ensure t)
(use-package company-lua :ensure t)
(use-package lua-mode :ensure t)

(use-package counsel :ensure t)
(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . counsel-grep-or-swiper)
  :config
  (require 'counsel)
  (setq counsel-grep-base-command "grep -niE \"%s\" %s")
  (setq ivy-height 20))

(use-package flycheck
  :ensure t
  :commands flycheck-mode)

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  ;;(elpy-use-ipython)
  ;;(elpy-clean-modeline)
  )

(use-package tex
  :defer t
  :ensure auctex)

(use-package haskell-mode :ensure t)


(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish ""
  :config
  (setq-default highlight-symbol-idle-delay 1.5))

;; Searching - stolen directly from Aaron Bieber
;; ---------------------------------------------------

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  (defadvice wgrep-change-to-wgrep-mode (after wgrep-set-normal-state)
    (if (fboundp 'evil-normal-state)
        (evil-normal-state)))
  (ad-activate 'wgrep-change-to-wgrep-mode)

  (defadvice wgrep-finish-edit (after wgrep-set-motion-state)
    (if (fboundp 'evil-motion-state)
        (evil-motion-state)))
  (ad-activate 'wgrep-finish-edit))

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup))

(use-package ag
  :ensure t
  :commands (ag ag-project)
  :config
  (add-hook 'ag-mode-hook
            (lambda ()
              (wgrep-ag-setup)
              (define-key ag-mode-map (kbd "n") 'evil-search-next)
              (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
  (setq ag-executable "/usr/local/bin/ag")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))





;;--------------------------------------------------------
;; Install non-default themes
;; --------------------------------------------------
(use-package zenburn-theme :ensure t)
(use-package sublime-themes :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package color-theme-solarized :ensure t)
(use-package ample-theme :ensure t)
;;(load-theme 'gruvbox)
;;(load-theme 'zenburn)
;;(load-theme 'sanityinc-tomorrow-eighties)



;; Hooks for different modes
;; ------------------------------------------------------

;;; Flycheck mode:
(add-hook 'flycheck-mode-hook
          (lambda ()
            (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
            (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; All programming modes
(defun air--set-up-prog-mode ()
  "Configure global prog-mode."
  (setq-local comment-auto-fill-only-comments t)
  (electric-pair-local-mode))
(add-hook 'prog-mode-hook 'air--set-up-prog-mode)


;; ------------------------
;; Magit  
(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (magit-define-popup-switch 'magit-log-popup ?f "first parent" "--first-parent"))

;;; Magit mode (which does not open in evil-mode):
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

;;; Git Commit Mode (a Magit minor mode):
(add-hook 'git-commit-mode-hook 'evil-insert-state)



;;--------------------------------------------------------
;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;; used in OS X. Disable sRGB before setting up Powerline.
(when (memq window-system '(mac ns))
  (setq ns-use-srgb-colorspace nil))


(use-package powerline
  :ensure t)

(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (require 'powerline)
  (setq powerline-default-separator 'arrow-fade)
  (setq sml/theme 'powerline)
  (sml/setup)
  ;; These colors are more pleasing (for gruvbox)
  (custom-theme-set-faces
   'user
   '(powerline-evil-normal-face ((t (:inherit powerline-evil-base-face :background "chartreuse3"))))
   '(sml/folder ((t (:inherit sml/global :background "grey22" :foreground "grey50" :weight normal))) t)
   '(sml/git ((t (:background "grey22" :foreground "chartreuse3"))) t)))

(setq server-socket-dir (expand-file-name "server" user-emacs-directory))
(server-start)

(provide 'init)
