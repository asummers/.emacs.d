;;; package --- Summary: init file
;;; Commentary:
;;; Code:

(setq gc-cons-threshold 100000000)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter (selected-frame) 'alpha '(93 93))
(add-to-list 'default-frame-alist '(alpha 93 93))

(setq custom-file "~/.emacs.d/emacs-custom.el")
; (load custom-file)

(require 'bind-key)
(require 'diminish)
(require 'use-package)

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-eighties t))

(use-package sane-defaults
  :load-path "~/.emacs.d/plugins/")

(use-package alchemist
  :load-path "~/.emacs.d/plugins/alchemist.el")

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)))

(use-package company
  :ensure t
  :diminish company-mode
  :hook
  (prog-mode . global-company-mode)
  :config
  (add-to-list 'company-backends '(company-capf company-dabbrev))
  (setq company-idle-delay nil))

(use-package company-statistics
  :ensure t
  :after company
  :init
  (company-statistics-mode))

(use-package company-lsp
  :ensure t
  :after company
  :config
  (push 'company-lsp company-backends))

(use-package crux
  :ensure t
  :bind
  (("s-r" . crux-rename-file-and-buffer)
   ("C-j" . crux-top-join-line)
   ("C-`" . crux-switch-to-previous-buffer)
   ("M-;" . comment-or-uncomment-region)
   ("C-a" . crux-move-beginning-of-line)
   ([S-return] . crux-smart-open-line))
  :config
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)

  (crux-with-region-or-line kill-ring-save)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-line kill-region)

  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-buffer indent-region)

  (defadvice kill-ring-save (after kill-ring-save activate)
    (message "Marked")))

(use-package dumb-jump
  ;; :disabled
  :ensure t
  :diminish dumb-jump
  :bind
  (("M-." . dumb-jump-go)
   ("M-," . dumb-jump-back))
  :init
  (dumb-jump-mode))

(use-package guru-mode
  :ensure t
  :defer t
  :diminish guru-mode
  :init
  (guru-global-mode))

(use-package dockerfile-mode
  :ensure t
  :mode
  ("Dockerfile\\'" . dockerfile-mode)
  :config
  (setq-default docker-use-sudo nil))

(use-package elixir-mode
  :ensure t
  :mode
  (("\\.exs$" . elixir-mode)
   ("\\.ex$" . elixir-mode)))

(use-package expand-region
  :ensure t
  :bind
  (("C-]" . er/expand-region)
   ("C-}" . er/contract-region))
  :config
  (setq expand-region-fast-keys-enabled nil))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (setq flycheck-idle-change-delay 5)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint
                          javascript-gjshint
                          javascript-jscs
                          javascript-standard))))

(use-package gitignore-mode
  :ensure t
  :mode
  (("/\\.gitignore_global\\'" . gitignore-mode)
   ("/\\.gitignore\\'" . gitignore-mode)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind
  (("C-c h" . helm-command-prefix)
   ("C-h a" . helm-apropos)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-c p s r" . helm-resume)
   ("C-c r" . helm-resume)
   :map helm-map
   ([tab] . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))
  :init
  (helm-mode 1)
  :config
  (global-unset-key (kbd "C-x c"))

  (helm-autoresize-mode t)

  (setq helm-split-window-inside-p nil) ; t to open helm buffer inside current window, not occupy whole other window)
  (setq helm-move-to-line-cycle-in-source t) ; move to end or beginning of source when reaching top or bottom of source.
  (setq helm-ff-search-library-in-sexp t) ; search for library in `require' and `declare-function' sexp.
  (setq helm-scroll-amount 8) ; scroll 8 lines other window using M-<next>/M-<prior>
  (setq helm-ff-file-name-history-use-recentf t)

  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-ag-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq-default helm-input-idle-delay 0.01)

  (set-face-attribute 'helm-selection nil
                      :background "grey"
                      :foreground "black")

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (defun helm-hide-minibuffer-maybe ()
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe))

(use-package helm-flx
  :ensure t
  :defer t
  :after helm
  :init
  (helm-flx-mode t)
  :config
  (setq helm-flx-for-helm-find-files t)
  (setq helm-flx-for-helm-locate t))

(use-package helm-company
  :ensure t
  :after company
  :bind
  ("M-?" . complete-symbol))
  ;; (:map company-active-map

  ;;  :map company-mode-map
  ;;  ;; ("M-?" . helm-company)))

(use-package helm-projectile
  :ensure t
  :bind
  (("C-c f" . helm-projectile-find-file)
   ("C-c s" . helm-projectile-ag))
  :after helm
  :init
  (helm-projectile-on))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind
  (("C-c o" . helm-swoop)
   ("C-c M-o" . helm-multi-swoop)))

(use-package helm-xref
  :ensure t
  :after helm
  :commands helm-xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package highlight-indentation
  :ensure t
  :hook
  (prog-mode . highlight-indentation-current-column-mode))

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :hook
  ((prog-mode . highlight-symbol-mode)
   (prog-mode . highlight-symbol-nav-mode))
  :config
  (setq highlight-symbol-on-navigation-p t)
  (setq highlight-symbol-idle-delay 0.01)
  (setq highlight-symbol-foreground-color "white")
  (setq next-line-add-newlines nil))

(use-package hippie-exp
  :ensure t
  :bind
  ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line)))

(use-package hl-line
  :commands hl-line-mode
  :ensure t
  :init
  (global-hl-line-mode 1)
  :config
  (set-face-background hl-line-face "gray10"))

(use-package hl-todo
  :ensure t
  :defer t
  :init
  (global-hl-todo-mode)
  :config
  (setq hl-todo-activate-in-modes '(prog-mode)))

(use-package lsp-mode
  :disabled
  :ensure t
  :diminish lsp-mode
  :bind
  ("<f7>" . lsp-format-buffer)
  :init (lsp-mode))

(use-package lsp-imenu
  :after lsp-mode
  :hook
  (lsp-after-open . lsp-enable-imenu))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :diminish lsp-ui
  :init (lsp-ui-mode)
  :config
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-enable-eldoc nil)
  (setq lsp-ui-flycheck-enable t))

(use-package lsp-elixir
  :disabled
  :load-path "~/.emacs.d/plugins/"
  :after lsp-mode
  :hook
  (elixir-mode . lsp-elixir-enable)
  :config
  (setq lsp-elixir-ls-command "sh")
  (setq lsp-elixir-ls-args '("/Users/asummers/Code/elixir-ls/language_server.sh")))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun my/projectile-invalidate-cache ()
    (ignore-errors
      (with-current-buffer buffer
        (projectile-invalidate-cache (projectile-project-root)))))

  (add-hook 'magit-post-refresh-hook #'my/projectile-invalidate-cache)

  (defadvice git-commit-commit (after delete-window activate)
    (delete-window))

  (defadvice git-commit-abort (after delete-window activate)
    (delete-window))

  (defun magit-quit-session ()
    "Restore the previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package midnight
  :init
  (midnight-mode)
  :config
  (setq clean-buffer-list-delay-general 1))

(use-package move-text
  :ensure t
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down)))

(use-package org-mode
  :bind
  (("C-c l" . org-store-link)
   ("C-c C-a" . org-agenda))
  :mode
  ("\\.org$" . org-mode)
  :init
  (setq org-src-fontify-natively t)
  (setq org-return-follows-link t)
  (setq org-log-done 'time)
  (setq org-use-fast-todo-selection t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s)" "WANT(w)" "|" "DONE(d)" "CANCELLED(c)"))))

(use-package pair-mode
  :diminish pair-mode
  :load-path "~/.emacs.d/plugins/")

(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :init
  (projectile-mode)
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".zprezto")
  (setq projectile-enable-caching t)

  (with-eval-after-load 'helm
    (setq projectile-completion-system 'helm))

  (with-eval-after-load 'helm-projectile
    (setq projectile-switch-project-action 'helm-projectile)))

(use-package rainbow-identifiers
  :ensure t
  :hook
  (prog-mode . rainbow-identifiers-mode)
  :config
  (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)
  (setq rainbow-identifiers-cie-l*a*b*-lightness 85)
  (setq rainbow-identifiers-cie-l*a*b*-saturation 38)
  (setq rainbow-identifiers-cie-l*a*b*-color-count 50)

  (setq rainbow-identifiers-faces-to-override '(highlight-quoted-symbol
                                        ;font-lock-variable-name-face
                                        ;font-lock-function-name-face
                                        ;font-lock-type-face
                                                web-mode-function-call-face
                                                web-mode-function-name-face
                                                web-mode-param-name-face
                                                web-mode-variable-name-face
                                                font-lock-variable-name-face
                                                font-lock-function-name-face)))

(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (rainbow-mode t))

(use-package restclient
  :ensure t
  :defer t)

(use-package savehist
  :ensure t
  :defer t
  :init
  (savehist-mode 1)
  :config
  (setq history-length 1000))

(use-package saveplace
  :ensure t
  :init
  (save-place-mode 1)
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package shrink-whitespace
  :ensure t
  :bind
  ("M-\\" . shrink-whitespace))

(use-package super-save
  :ensure t
  :diminish super-save-mode
  :init
  (super-save-mode t)
  :config
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))

(use-package unbound
  :ensure t
  :commands describe-unbound-keys)

(use-package unfill
  :ensure t
  :commands
  (unfill-region unfill-paragraph unfill-toggle)
  :bind
  ("M-q" . unfill-toggle))

(use-package web-mode
  :ensure t
  :mode
  (("\\.ios\\.js$" . web-mode)
   ("\\.android\\.js$" . web-mode)
   ("\\.react\\.js$" . web-mode)
   ("\\.js$" . web-mode))
  :config
  (add-to-list 'magic-mode-alist '("^import React" . web-mode))
  (add-to-list 'magic-mode-alist '("React.Component" . web-mode))
  (add-to-list 'magic-mode-alist '("from 'react';$" . web-mode))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode))

  (add-hook 'web-mode-hook
            (lambda ()
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx"))))
  (setq-local web-mode-enable-auto-quoting nil)

  (setq-default js-indent-level 4)
  (defun set-indentation ()
    (setq web-mode-markup-indent-offset (symbol-value 'js-indent-level))
    (setq web-mode-attr-indent-offset (symbol-value 'js-indent-level))
    (setq web-mode-css-indent-offset (symbol-value 'js-indent-level))
    (setq web-mode-code-indent-offset (symbol-value 'js-indent-level)))

  (add-hook 'web-mode-hook 'set-indentation))

(use-package yaml-mode
  :ensure t
  :mode
  ("\\.sls$" . yaml-mode))

(load "server")
(unless (server-running-p)
  (add-hook 'after-init-hook 'server-start t))

(provide 'init)
;;; init.el ends here
