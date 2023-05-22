;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path "~/.emacs.d/options/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/options/themes")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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

;; copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
;; you can utilize :map :hook and :config to customize copilot
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(require 'web-mode)
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq package-check-signature nil)
(setq create-lockfiles nil)
(menu-bar-mode -1)
(tool-bar-mode -1)

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
   '(handlebars-mode go-mode haskell-mode lsp-mode markdown-preview-eww markdown-mode rjsx-mode elm-mode rust-mode nyan-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(nyan-mode)
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil))

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)

(setq org-startup-indented t)

(setq inferior-lisp-program "sbcl")

;; wbl slime/swank integration?
;;(slime-connect \"localhost\" 4208)

;; Manually cloned lsp-mode... 
;; (add-to-list 'load-path "~/.emacs.d/pkgsource/lsp-mode")
;; (load "lsp-mode")

;; build CPP file lol
(defun build-cpp () (interactive) 
       (let ((command-text
              (format "g++ -o out -Wall %s"
                      (shell-quote-argument buffer-file-name)))
             (output-buffer (get-buffer-create "*CPP Build Output*")))
         (with-output-to-temp-buffer output-buffer
           (shell-command command-text output-buffer output-buffer)
           (switch-to-buffer-other-window output-buffer))))


;; Some PHP IDE functionality that I wanted to have lol

;; helpers
(defun symfony-find-project-root (buffer-filename)
  (locate-dominating-file buffer-filename "src/"))
(defun symfony-find-class-source (classname project-root)
  (car (directory-files-recursively project-root (concat classname ".php"))))

;; interactive functions
(defun symfony-open-class-source (point mark)
  "Opens the source file for highlighted class name (if can be found locally)"
  (interactive "r")
  (let* (
         (highlighted-classname (buffer-substring point mark))
         (symfony-project-root (symfony-find-project-root (buffer-file-name)))
         (php-class-filepath
          (symfony-find-class-source highlighted-classname symfony-project-root)))
    (find-file-other-window php-class-filepath)))

(defun symfony-insert-class-import (point mark)
  "Inserts use statement for the highlighted classname if it can be found"
  (interactive "r")
  (let* ((highlighted-classname (buffer-substring point mark))
         (symfony-project-root (symfony-find-project-root (buffer-file-name)))
         (php-class-filepath
          (symfony-find-class-source highlighted-classname symfony-project-root))
         (use-statement
          (save-current-buffer
            (let ((temp-buffer (set-buffer (find-file php-class-filepath)))
                  (use-statement 
                   (concat "use" 
                           (buffer-substring 
                            (re-search-forward "namespace")
                            (1- (re-search-forward ";")))
                           "\\" highlighted-classname ";")))
              (kill-buffer temp-buffer)
              use-statement))))
    (save-excursion
      (end-of-buffer)
      (re-search-backward "use [a-zA-Z0-9_\\]*;")
      (move-end-of-line nil)
      (insert (concat "\n" use-statement)))))


(define-minor-mode jekyll-watch-mode
  "Automatically triggers Jekyll build on file save")
(defun rebuild-jekyll ()
  "Rebuilds Jekyll source if in jekyll_source directory"
  (when (bound-and-true-p jekyll-watch-mode)
    (let ((default-directory "~/content/generator/"))
      (message "%s" (shell-command-to-string "./build.sh")))))
(add-hook 'after-save-hook #'rebuild-jekyll)

;; Add hook to web-mode to check and auto-enable minor mode if necessary
(defun auto-enable-jekyll-watch-mode ()
    "Automatically enables Jekyll watch minor mode if directory path contains jekyll_source"
  (and buffer-file-name
       (cl-search "jekyll_source" buffer-file-name)
       (jekyll-watch-mode 1)))
(add-hook 'web-mode-hook #'auto-enable-jekyll-watch-mode)
