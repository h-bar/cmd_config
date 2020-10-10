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
(straight-use-package 'ripgrep)
(straight-use-package 'evil-nerd-commenter)

(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)

(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'swiper)
(straight-use-package 'projectile)
(straight-use-package 'company)
(straight-use-package 'hydra)

(straight-use-package 'ivy-hydra)
(straight-use-package 'ivy-rich)
(straight-use-package 'ivy-xref)
(straight-use-package 'counsel-projectile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy, Counsel and Swiper
(setq ivy-use-virtual-buffers t)
(setq ivy-rich-path-style 'abbrev)

;; Projectile
(setq projectile-completion-system 'ivy)
(setq projectile-dynamic-mode-line t)
(setq projectile-indexing-method 'alien)
(setq projectile-sort-order 'recently-active)
(setq projectile-enable-caching t)
;; (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))

;; Modeline
(setq doom-modeline-window-width-limit fill-column)
(setq doom-modeline-project-detection 'projectile)
(setq doom-modeline-buffer-file-name-style 'truncate-except-project)
(setq doom-modeline-indent-info t)
(setq find-file-visit-truename t)

;; Ivy-xref
(when (>= emacs-major-version 27)
  (setq xref-show-definitions-function #'ivy-xref-show-defs))
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)


(load (concat user-emacs-directory "comphy_fw"))

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
(doom-modeline-mode t)

;; Builtin Modes
(global-cwarn-mode t)
(global-prettify-symbols-mode t)
(global-visual-line-mode t)
(global-display-line-numbers-mode t)
(xterm-mouse-mode t)
(which-func-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(auto-save-visited-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-conservatively most-positive-fixnum)
(setq backup-directory-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-s" 'swiper)
(global-set-key "\M-;" 'evilnc-comment-or-uncomment-lines)

(global-set-key (kbd "M-p g") 'counsel-projectile-rg)
(global-set-key (kbd "M-p t") 'projectile-find-tag)
(global-set-key (kbd "M-p f") 'counsel-projectile-find-file)
(global-set-key (kbd "M-p r") 'xref-find-references)
(global-set-key (kbd "M-p d") 'xref-find-definitions-other-window)
(global-set-key (kbd "M-p s") 'counsel-projectile-switch-project)

(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

