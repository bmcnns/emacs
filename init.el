(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(tool-bar-mode -1)
(set-window-scroll-bars (minibuffer-window) nil nil)

(setq make-backup-files nil)

(setq evil-want-keybinding nil)

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))

;; Enable Evil
(require 'evil)
(evil-mode 1)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; Optional: Enable ivy-rich for better project and buffer descriptions
(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t)

;; Optional: Add keybindings for projectile and counsel-git-grep
(global-set-key (kbd "C-c p f") 'counsel-git)  ; Find file in Git repo
(global-set-key (kbd "C-c p p") 'counsel-git)  ; Switch to Git project
(global-set-key (kbd "C-c p g") 'counsel-git-grep) ; Search in Git repo

(use-package swiper
  :ensure t
  :bind
  (:map evil-normal-state-map
        ("/" . my-swiper))
  :config
  (defun my-swiper ()
    "Run swiper unless in minibuffer."
    (interactive)
    (if (minibufferp)
        (self-insert-command 1)
      (swiper))))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(tool-bar-mode -1)
(set-window-scroll-bars (minibuffer-window) nil nil)

(use-package org-bullets
  :ensure t
  :hook ((org-mode) . org-bullets-mode))
