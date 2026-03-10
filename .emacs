					; a brand new emacs
;; add MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; disable ui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; install evil-mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  (evil-mode 1))

;; compatibility layer for evil-mode
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; increase the default font size
(set-face-attribute 'default nil :height 150)

;; install sly for common lisp
(use-package sly
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))

;; better navigation in the minibuffer
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; best git interface out there
(use-package magit
  :ensure t
  :defer t)

;; save minibuffer history across sessions
(savehist-mode 1)

;; better deleting, copying, renaming files
(use-package embark
  :ensure t
  :bind ("C-." . embark-act))

;; navigation tool for jumping around
(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char-2))

;; native terminal emulator
(use-package eat
  :ensure t
  :defer t)

; disable the key binding that makes me turn evil mode off by mistake
(define-key evil-normal-state-map (kbd "C-z") nil)
(define-key evil-visual-state-map (kbd "C-z") nil)
(define-key evil-insert-state-map (kbd "C-z") nil)
(define-key evil-motion-state-map (kbd "C-z") nil)

;; better undo history
(use-package vundo
  :ensure t
  :defer t)

(define-key evil-normal-state-map (kbd "u") 'vundo)

;; better window jumping
(use-package ace-window
  :ensure t
  :defer t
  :bind ("M-o" . ace-window))
