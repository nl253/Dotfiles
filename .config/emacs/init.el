(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("eecacf3fb8efc90e6f7478f6143fd168342bbfa261654a754c7d47761cec07c8" default)))
 '(package-selected-packages
   (quote
    (flymake-shell flymake-css flycheck-ycmd company-ycmd company-web company-ycm company-shell company org-bullets markdown-mode magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "grey16" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 160 :width condensed :foundry "SRC" :family "Hack"))))
 '(font-lock-comment-face ((t (:foreground "light sea green"))))
 '(font-lock-constant-face ((t (:foreground "light goldenrod"))))
 '(font-lock-function-name-face ((t (:foreground "dark orange" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "deep pink" :weight bold))))
 '(font-lock-string-face ((t (:foreground "SeaGreen2"))))
 '(font-lock-type-face ((t (:foreground "deep sky blue" :weight normal))))
 '(ido-only-match ((t (:foreground "SpringGreen2"))))
 '(ido-subdir ((t (:foreground "DodgerBlue2"))))
 '(markdown-header-face ((t (:inherit (font-lock-function-name-face font-lock-type-face) :weight bold))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.0))))
 '(mode-line ((t (:background "grey75" :foreground "black"))))
 '(mode-line-highlight ((t (:background "black" :foreground "grey20"))))
 '(mode-line-inactive ((t (:background "grey10" :foreground "grey80" :weight light)))))

;; Enable company mode:                               
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-ycmd)
(require 'flycheck-ycmd)
(flycheck-ycmd-setup)
(require 'flymake-css)
(add-hook 'css-mode-hook 'flymake-css-load)
(require 'flymake-shell)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)

;; Enable company-ycm.
(add-to-list 'company-backends 'company-web-html)
(add-to-list 'company-backends 'company-ycm)           
(add-to-list 'company-begin-commands 'c-electric-colon)
(add-to-list 'company-begin-commands 'c-electric-lt-gt)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element)

(setq scroll-margin 12)
(setq hscroll-margin 12)

(add-hook 'org-mode-hook (lambda () (flyspell-buffer)))
(add-hook 'ielm-mode-hook (lambda () (electric-pair-mode)))
(add-hook 'emacs-lisp-mode-hook (lambda () (electric-pair-mode)))

(require 'ido)
(ido-mode t)

(setq ido-find-file 'selected-window)
(setq ido-create-new-buffer 'prompt)
(setq ido-toggle-regexp t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq initial-buffer-choice "~/Documents/Revision")
