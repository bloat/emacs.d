;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(mapc (lambda (package) 
	(or (package-installed-p package)
	    (package-install package)))
      '(magit 
	paredit 
	color-theme-solarized
	auto-complete))

;;;;;;;;;;;;;;;
;; Paredit

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

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
    (set-frame-font "-unknown-Ubuntu Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
    (set-face-attribute 'default nil :height 160)))

(add-hook 'after-make-frame-functions 'apply-font)


