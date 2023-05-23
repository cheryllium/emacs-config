;;; copilot-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "copilot" "copilot.el" (0 0 0 0))
;;; Generated autoloads from copilot.el

(autoload 'copilot-complete "copilot" "\
Complete at the current point." t nil)

(autoload 'copilot-mode "copilot" "\
Minor mode for Copilot.

If called interactively, enable Copilot mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-copilot-mode 'globalized-minor-mode t)

(defvar global-copilot-mode nil "\
Non-nil if Global Copilot mode is enabled.
See the `global-copilot-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-copilot-mode'.")

(custom-autoload 'global-copilot-mode "copilot" nil)

(autoload 'global-copilot-mode "copilot" "\
Toggle Copilot mode in all buffers.
With prefix ARG, enable Global Copilot mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Copilot mode is enabled in all buffers where
`copilot-mode' would do it.
See `copilot-mode' for more information on Copilot mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "copilot" '("copilot-")))

;;;***

(provide 'copilot-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; copilot-autoloads.el ends here
