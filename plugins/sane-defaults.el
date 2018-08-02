;;; package-summary -- Sane Defaults
;;; Commentary:
;;; Code:

; We have lots of RAM, let's use it
(setq gc-cons-threshold 20000000)

; Turn off all the noise.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; OSX
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq ns-function-modifier 'hyper))

; y/n is much better than typing yes/no
(fset 'yes-or-no-p 'y-or-n-p)

; Delete active region when you type
(delete-selection-mode 1)

; Enable CamelCase aware deletion for all programming modes
(add-hook 'prog-mode-hook 'subword-mode)

; Spaces not tabs
(setq-default indent-tabs-mode nil)

; Removes whitespace at end of lines
(setq-default truncate-lines t)

; Disable startup noise
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

; Disable the bell sound on errors
(setq ring-bell-function #'ignore)

; Put deleted files in the trash
(setq delete-by-moving-to-trash t)

; If at end of line, add new line when pressing C-n
(setq next-line-add-newlines t)

; When exiting emacs, prompt to prevent accidental closings.
(setq confirm-kill-emacs 'y-or-n-p)

; Open new
(setq ns-pop-up-frames nil)

(setq vc-follow-symlinks t)

(setq sentence-end-double-space nil)

(setq-default dired-listing-switches "-alh")

; Full path in title
(setq-default frame-title-format
              '((:eval (if (buffer-file-name)
                           (abbreviate-file-name (buffer-file-name)) "%f"))))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

; Show line and column in modeline
(setq line-number-mode t)
(setq column-number-mode t)

; Lines are 80 cols not 72
(setq fill-column 80)

; Show keystrokes in minibuffer pretty much immediately.
(setq echo-keystrokes 0.01)

(setq disabled-command-function nil)

; Disable tooltips
(tooltip-mode nil)

(setq hscroll-margin 0)
(setq hscroll-step 1)

; Scroll settings
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1)))

(setenv "PAGER" "/bin/cat")

; Highlight matching brace
(show-paren-mode 1)
(setq show-paren-delay 0)

(recentf-mode 1)
(setq recentf-max-saved-items 100)

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)

(setq completion-ignore-case t)

(global-display-line-numbers-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default mode-line-buffer-identification
              '(:eval
                (if (buffer-file-name)
                    (propertize (abbreviate-file-name (buffer-file-name)) 'face '(:foreground "#C4A1CE"))
                  "")))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(defun split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
	       (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'create-non-existent-directory)

(defun copy-file-name-to-clipboard ()
  "Put the current file name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (or (zerop i) (forward-line))
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-L" #'mark-line)
(bind-key "M-S" #'mark-sentence)

; I keep closing emacs on accident.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

; Ctrl-click/right click opens a popup menu. Eww.
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-unset-key (kbd "C-<down-mouse-3>"))

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(global-set-key (kbd "M-#") 'sort-lines)
(global-set-key (kbd "C-x 2") 'split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'split-window-right-and-switch)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(global-unset-key (kbd "M-+"))
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-unset-key (kbd "M--"))
(global-set-key (kbd "M--") 'text-scale-decrease)

(provide 'sane-defaults)
;;; sane-defaults.el ends here
