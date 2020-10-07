(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (linum-relative company-lsp company-ctags ivy-xref counsel-gtags counsel-projectile lsp-ivy ivy-rich ripgrep counsel lsp-ui magit flycheck))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package magit)

(use-package ivy
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "")
  ;; Do not show "./" and "../" in the `counsel-find-file' completion list
  (setq ivy-extra-directories nil)    ;Default value: ("../" "./")
  :bind(
	("C-s" . counsel-grep-or-swiper)
	("C-x b" . ivy-switch-buffer)
	))

(use-package company-ctags
  :after company
  :config
  (company-ctags-auto-setup)
  (setq company-ctags-support-etags 1)
  (setq company-ctags-fuzzy-match-p 1))

(use-package company-lsp
  :after company
  :config
  (push 'company-lsp company-backends))

(use-package counsel
  :after ivy
  :config
  (counsel-mode t)
  :bind(
	("C-c b" . counsel-bookmark)
	))

(use-package counsel-projectile
  :after counsel)

(use-package swiper)


(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode-enable))

(use-package projectile)

(use-package lsp-mode
  :config
  (lsp-mode t))

(use-package lsp-ui
  :config
  (lsp-ui-mode t))

(use-package smartparens
  :config
  (smartparens-global-mode t))

(use-package company
  :config
  (global-company-mode t))

(use-package ripgrep)

(use-package ivy-rich
  :after (ivy)
  :config
  (ivy-rich-mode t)
  (setq ivy-rich-path-style 'abbrev))

(use-package lsp-ivy)
(use-package counsel-gtags)

(use-package ivy-xref
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package linum-relative
  :config
  (linum-relative-global-mode t))

(global-cwarn-mode t)
;;(global-display-line-numbers-mode t)
(global-prettify-symbols-mode t)
(global-visual-line-mode t)
(xterm-mouse-mode t)
