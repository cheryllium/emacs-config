(setq package-check-signature nil)
(setq create-lockfiles nil)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

(setq-default indent-tabs-mode nil)
;(setq-default tab-width 4)
(setq-default tab-width 2)
;(setq indent-line-function 'insert-tab)

(add-to-list 'load-path "~/.emacs/d")
(add-to-list 'default-frame-alist '(foreground-color . "white"))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(defvar zenburn-override-colors-alist
  '(("zenburn-bg" . "#111111")))
(load-theme 'zenburn t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (markdown-preview-eww markdown-mode rjsx-mode elm-mode rust-mode nyan-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(nyan-mode)

(defun build-cpp () (interactive) 
       (let ((command-text
              (format "g++ -o out -Wall %s"
                      (shell-quote-argument buffer-file-name)))
             (output-buffer (get-buffer-create "*CPP Build Output*")))
         (with-output-to-temp-buffer output-buffer
           (shell-command command-text output-buffer output-buffer)
           (switch-to-buffer-other-window output-buffer))))


(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil))

(setq js-indent-level 2)
