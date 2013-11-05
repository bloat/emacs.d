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
	color-theme-solarized
	auto-complete
	org
	ac-slime
	cider
	ac-nrepl))

;;;;;;;;;;;;;;;
;; Paredit

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'color-theme)
(color-theme-initialize)

(defun apply-color-theme (frame)
  "Apply color theme to a frame based on whether its a 'real'
   window or a console window."
  (select-frame frame)
  (if (window-system frame)
      (color-theme-solarized-dark)
    (color-theme-charcoal-black)))

(setq color-theme-is-global nil)
(add-hook 'after-make-frame-functions 'apply-color-theme)

(defun apply-font (frame)
  (select-frame frame)
  (when (window-system frame)
    (set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
    (set-face-attribute 'default nil :height 160)))

(add-hook 'after-make-frame-functions 'apply-font)

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(global-set-key (kbd "C-c m") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; local settings

(load (expand-file-name "~/emacs/elisp/local-settings.el"))

