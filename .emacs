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
  (setq sly-contribs (remove 'sly-quicklisp (remove 'sly-asdf sly-contribs)))
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
(define-key evil-normal-state-map (kbd "C-.") 'embark-act)

;; navigation tool for jumping around
(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char-2))

;; native terminal emulator
(use-package eat
  :ensure t
  :defer t
  )

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

;; awesome searching across files
(use-package deadgrep
  :ensure t
  :defer t)

(define-key evil-normal-state-map (kbd ",") 'deadgrep)

;; move backups to a better place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; nice utility i wrote for listing unsaved buffers
(defun list-unsaved-buffers ()
  "List unsaved buffers and let the user select one to visit."
  (interactive)
  (let* ((unsaved (seq-filter (lambda (buf)
                                (and (buffer-modified-p buf)
                                     (buffer-file-name buf)))
                              (buffer-list)))
         (names (mapcar #'buffer-name unsaved)))
    (if names
        (let ((choice (completing-read "Unsaved buffers: " names nil t)))
          (when choice
            (switch-to-buffer choice)))
      (message "No unsaved buffers."))))

;; load my experiments package
(use-package experiment
  :ensure nil
  :load-path "~/Repos/experiment"
  :commands (cluster-open))

;; comment/uncomment with ; in normal mode
(defun comment-or-uncomment-selection ()
  "Comment or uncomment the current line or selected region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(define-key evil-normal-state-map (kbd ";") #'comment-or-uncomment-selection)
(define-key evil-visual-state-map (kbd ";") #'comment-or-uncomment-selection)

;; remove pg-up/pg-down behaviour
;; -- never needed it, never wanted it.
(global-set-key (kbd "<prior>") 'ignore)
(global-set-key (kbd "<next>") 'ignore)
