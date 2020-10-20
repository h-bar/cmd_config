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


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t))

;; (use-package solarized-theme
;;   :config
;;   (load-theme 'solarized-dark t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ripgrep)
(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys t))

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :config
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-indent-info t))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode t))
(use-package counsel
  :config
  (counsel-mode t))
(use-package swiper)
(use-package hydra)

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-dynamic-mode-line t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  (projectile-mode t))

(use-package company
  :config
  (global-company-mode t))

;; (use-package lsp-ui
;;   :config
;;   (lsp-ui-peek-enable t)
;;   (lsp-ui-mode t)
;;   (lsp-ui-sideline-mode t))

;; (use-package lsp-mode
;;   :config
;;   (lsp-completion-mode t)
;;   (lsp-diagnostics-mode t)
;;   (lsp-diagnostics-modeline-mode t))



(use-package ivy-hydra)

(use-package ivy-rich
  :config
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode))

(use-package ivy-xref
  :config
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel-projectile)

;; (use-package company-lsp
;;   :config
;;   (push 'conpany-lsp company-backends))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; (use-package flycheck-color-mode-line
;;   :hook (flycheck-mode . flycheck-color-mode-line-mode))

(load (concat user-emacs-directory "comphy_fw"))

(global-cwarn-mode t)
(global-prettify-symbols-mode t)
(global-visual-line-mode t)
(global-display-line-numbers-mode t)
(xterm-mouse-mode t)
(which-function-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(auto-save-visited-mode t)
(display-time-mode t)
(auto-save-mode nil)
(global-hl-line-mode t)

(setq scroll-conservatively most-positive-fixnum)
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-s" 'swiper)
;; (global-set-key "\C-x b" 'split-window-horizontally)
;; (global-set-key "\C-x \\" 'split-window-vertically')

(global-set-key (kbd "M-p g") 'counsel-projectile-rg)
(global-set-key (kbd "M-p t") 'projectile-find-tag)
(global-set-key (kbd "M-p f") 'counsel-projectile-find-file)
(global-set-key (kbd "M-p r") 'xref-find-references)
(global-set-key (kbd "M-p d") 'xref-find-definitions)
(global-set-key (kbd "M-p p") 'counsel-projectile-switch-project)

(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)
