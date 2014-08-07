;;;;;;;;;;;;;;;;;;;;;;;;;
;; local settings

(load (expand-file-name "~/emacs/elisp/pre-local-settings.el"))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
(package-refresh-contents)

(mapc (lambda (package) 
	(or (package-installed-p package)
	    (package-install package)))
      '(magit 
	paredit 
	color-theme-sanityinc-solarized
	auto-complete
	org
	ac-slime
	cider
	ac-nrepl
        s))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(if window-system
    (load-theme 'sanityinc-solarized-dark t))

(set-frame-font (if (eq system-type 'windows-nt)
                    "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
                  "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
(set-face-attribute 'default nil :height 140)

;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(when (eq system-type 'darwin)
  (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#"))))

;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp

(load (expand-file-name "~/quicklisp/slime-helper.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple settings

(setq-default truncate-lines t)
(setq ring-bell-function #'ignore)
(ido-mode)
(setq ido-enable-flex-matching t)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq-default indent-tabs-mode nil)
(require 's)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups 

(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(global-set-key (kbd "C-c m") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit

(defun my-magit-default-tracking-name-fn (remote branch)
  "Just use the remote branch name directly"
  branch)

(setq magit-default-tracking-name-function #'my-magit-default-tracking-name-fn)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ack

(defvar ack-history nil
  "History for the `ack' command.")

(defun ack (command-args)
  (interactive
   (let ((ack-command "ack-grep --nogroup --with-filename --all "))
     (list (read-shell-command "Run ack (like this): "
                               ack-command
                               'ack-history))))
  (let ((compilation-disable-input t))
    (compilation-start (concat command-args " < " null-device)
                       'grep-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ediff

;; Store window config so that it can be restored when ediff quits.
(defvar my-pre-ediff-window-config)
(add-hook 'ediff-before-setup-hook (lambda () (setq my-pre-ediff-window-config (current-window-configuration))))
(add-hook 'ediff-quit-hook (lambda () (set-window-configuration my-pre-ediff-window-config)) 'append)

;;;;;;;;;;;;;;;
;; Paredit

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

;;;;;;;;;;;;;;;;;;;;;
;; Auto Complete

(require 'auto-complete-config)
(ac-config-default)

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org

;;;;;;;;;;;;;;;;;;;;;;;;;
;; local settings

(load (expand-file-name "~/emacs/elisp/post-local-settings.el"))

