(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'smartparens)

(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'swiper)
(straight-use-package 'projectile)
(straight-use-package 'company)

(straight-use-package 'ivy-rich)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ivy-rich-path-style 'abbrev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modes from Installed Packages
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(smartparens-global-mode t)

(ivy-mode t)
(counsel-mode t)
(global-company-mode t)
(ivy-rich-mode t)

;; Builtin Modes
(global-cwarn-mode t)
(global-prettify-symbols-mode t)
(global-visual-line-mode t)
(global-display-line-numbers-mode t)
(xterm-mouse-mode t)
