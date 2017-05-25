(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)

  ;; Configure evil leader mode - we are still inside use-package
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "b"  'helm-mini
    "d"  'kill-this-buffer
    "e"  'eval-last-sexp
    "f"  'helm-imenu
    "w"  'save-buffer
    "x"  'helm-M-x
    "l"  'whitespace-mode
    ","  'other-window))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package evil-indent-textobject :ensure t)

(use-package evil
  :ensure t
  :config 
  (evil-mode t)
  ;; Put evil into emacs state when entering ace-window to not override its keybindings
  (evil-set-initial-state 'ace-window-mode 'emacs)
  
  ;; Unset some that i don't use and which interferes with other custom keybindings
  ;; This one interferes with helm-find-files
  (define-key evil-motion-state-map (kbd "C-o") nil)

  ;; Make escape quit everything possible (from Aaron Bieber)
  ;; Sometimes it is necessary to use C-g
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))
  
  ;; first one below was mapped to keyboard-escape-quit - but this is nicer as is doesn't close windows in my base layout
  (define-key evil-normal-state-map [escape] 'keyboard-quit) 
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  
  ;; More bindings from Aaron Bieber
  (evil-define-key 'normal global-map (kbd "-")     'helm-find-files)
  (evil-define-key 'normal global-map (kbd "C-p")   'helm-projectile)
  (evil-define-key 'normal global-map (kbd "C-S-p") 'helm-projectile-switch-project)

  ;; Make parens and co. easily available using M and S in insert state
  (evil-define-key 'insert global-map (kbd "M-8")     "[")
  (evil-define-key 'insert global-map (kbd "M-9")     "]")
  (evil-define-key 'insert global-map (kbd "M-S-8")   "{")
  (evil-define-key 'insert global-map (kbd "M-S-9")   "}")
  (evil-define-key 'insert global-map (kbd "M-i")     "|")
  
  
  ;; Trying things from nathantypanski.com
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
    'elisp-slime-nav-describe-elisp-thing-at-point)
  )

(provide 'init-evil)
