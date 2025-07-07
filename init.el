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

(use-package counsel
  :ensure t)

;; Optional: Add keybindings for projectile and counsel-git-grep
(global-set-key (kbd "C-c p f") 'counsel-git)  ; Find file in Git repo
(global-set-key (kbd "C-c p p") 'counsel-git)  ; Switch to Git project
(global-set-key (kbd "C-c p g") 'counsel-git-grep) ; Search in Git repo

(use-package swiper
  :ensure t
  :config
  (defun my/swiper-with-occur ()
    "Run `swiper` and immediately trigger `ivy-occur`."
    (interactive)
    (let ((ivy-initial-inputs-alist nil)) ; Don't prefill with symbol at point
      (swiper)
      (ivy-occur)))

  ;; Replace swiper with our version everywhere
  (define-key evil-normal-state-map (kbd "/") #'my/swiper-with-occur))

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

(use-package elpy
  :after flycheck
  :after py-autopep8
  :ensure t
  :init
  (elpy-enable)
  :custom
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))


(use-package flycheck
  :ensure t)

(use-package py-autopep8
  :ensure t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("8dbbcb2b7ea7e7466ef575b60a92078359ac260c91fe908685b3983ab8e20e3f" default))
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(ob-sly org-download org-modern dashboard hyperbole macrostep embark-consult embark sly-asdf sly-quicklisp sly straight straight-el eldoc-box cape corfu rainbow-delimiters hl-todo which-key doom-themes monokai-theme github-theme gruvbox-theme ef-themes modus-themes undo-tree wgrep deadgrep chatgpt-shell treemacs orderless consult marginalia vertico org-fragtog py-autopep8 flycheck elpy org-bullets magit ivy-rich evil-collection counsel))
 '(python-shell-interpreter "/home/bryce/anaconda3/envs/gp-is-good-for-fqe/bin/python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq org-latex-classes nil)

(add-to-list 'org-latex-classes
                 '("acmart" "\\documentclass{acmart}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
                 '("acmartconf" "\\documentclass[sigconf, nonacm]{acmart}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq inferior-lisp-program "sbcl")

(set-face-attribute 'default nil :height 130)

(setq inferior-lisp-program "sbcl")
(use-package org-fragtog
    :ensure t
    :after org
    :hook
    (org-mode . org-fragtog-mode)
    :custom
    (org-format-latex-options
     (plist-put org-format-latex-options :scale 2.5)
     (plist-put org-format-latex-options :foreground 'auto)
     (plist-put org-format-latex-options :background 'auto)))
(setq inferior-lisp-program "sbcl --dynamic-space-size 4096")

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
    '("gptp"
      "\\documentclass[graybox]{svmult}
% Base fonts
\\usepackage{mathptmx}
\\usepackage{helvet}
\\usepackage{courier}
\\usepackage{type1cm}

% Springer core
\\usepackage{makeidx}
\\usepackage{graphicx}
\\usepackage{multicol}
\\usepackage[bottom]{footmisc}

% === Additional packages added by user ===
\\usepackage{amsmath}
\\usepackage{cite}
\\interdisplaylinepenalty=2500
\\usepackage{url}
\\usepackage{tikz}
\\usepackage{tikz-qtree}
\\usetikzlibrary{matrix}
\\usepackage[caption=false,font=footnotesize]{subfig}
\\usepackage{float}
\\usepackage{algorithm}
\\usepackage{algpseudocode}
\\DeclareMathOperator*{\\argmin}{argmin}
\\newcommand{\\code}{\\texttt}

\\makeindex
"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}"
                 ;; This maps headings to Beamer frames:
                 ("\\begin{frame}{%s}" "\\end{frame}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}"))))

;; Set tab width to 4 spaces
(setq-default tab-width 4)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Make TAB insert 4 spaces
(setq-default standard-indent 4)

;; Optional: apply to all programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq standard-indent 4)))

;; Optional: fix electric-indent if it's messing with things
(electric-indent-mode 1)

;; Use system clipboard on macOS
(setq select-enable-clipboard t)
(setq select-enable-primary t)

(setq tramp-default-method "ssh")

(defun brycelab1 ()
  "Quick access to Bryce's lab machine 1."
  (interactive)
  (find-file "/ssh:bryce@129.173.67.115:/home/bryce/"))

(defun brycelab2 ()
  "Quick access to Bryce's lab machine 2."
  (interactive)
  (find-file "/ssh:bryce@129.173.67.123:/home/bryce/"))

(setq enable-recursive-minibuffers t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))


(use-package consult
  :ensure t
  :bind (("C-c t" . consult-theme)))

                                        ; or (kbd "C-<down>") for manual preview
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package treemacs
  :ensure t
  :bind
  ([f5] . treemacs)  ;; Use F5 to toggle the Treemacs file tree
  :config
  (setq treemacs-width 30))

(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[11pt]{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("letter"
               "\\documentclass[11pt]{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "OPENAI_API_KEY"))

(use-package chatgpt-shell
  :ensure t
  :after exec-path-from-shell evil
  :init
  (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY")
        chatgpt-shell-models (chatgpt-shell-openai-models)
        chatgpt-shell-model-version "gpt-4o"
        chatgpt-shell-default-interface 'openai)
  :config
  (define-key chatgpt-shell-mode-map (kbd "RET") nil)
  (evil-define-key 'normal chatgpt-shell-mode-map
    (kbd "RET") #'chatgpt-shell-submit)
  (evil-define-key 'normal 'global (kbd "`") #'chatgpt-shell))
    

(use-package deadgrep
  :ensure t
  :after evil`
  :bind (:map evil-normal-state-map
              ("," . deadgrep))
  :config
  (with-eval-after-load 'wgrep
    (setq deadgrep-edit-mode-line-string "  [wgrep: edit-mode]")
    (add-hook 'deadgrep-finished-hook #'deadgrep-edit-mode)))

(use-package wgrep
  :ensure t
  :config
  (add-hook 'wgrep-mode-hook #'evil-normalize-keymaps)
  (add-hook 'wgrep-mode-hook (lambda () (evil-normal-state))))

(with-eval-after-load 'deadgrep
  ;; Always show results buffer in same window
  (defun deadgrep--visit-search-buffer-same-window ()
    (let ((buffer (get-buffer deadgrep--search-buffer-name)))
      (when buffer
        (switch-to-buffer buffer))))
  (advice-add 'deadgrep--visit-search-buffer :override
              #'deadgrep--visit-search-buffer-same-window)

  ;; Also use same window when visiting matches
  (setq deadgrep-display-buffer-function #'switch-to-buffer))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff nil
        undo-tree-visualizer-timestamps nil
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo" user-emacs-directory))))

  ;; Auto-close visualizer on RET
  (defun my/undo-tree-quit-after-set (&rest _) (undo-tree-visualizer-quit))
  (advice-add 'undo-tree-visualizer-set :after #'my/undo-tree-quit-after-set)

  ;; Evil integration
  (with-eval-after-load 'evil
    (define-key undo-tree-visualizer-mode-map (kbd "h") #'undo-tree-visualize-switch-branch-left)
    (define-key undo-tree-visualizer-mode-map (kbd "l") #'undo-tree-visualize-switch-branch-right)
    (define-key undo-tree-visualizer-mode-map (kbd "j") #'undo-tree-visualize-redo)
    (define-key undo-tree-visualizer-mode-map (kbd "k") #'undo-tree-visualize-undo)
    (define-key undo-tree-visualizer-mode-map (kbd "q") #'undo-tree-visualizer-quit)
    (define-key evil-normal-state-map (kbd "u") #'undo-tree-visualize)))


(defun bryce/toggle-comment ()
  "Comment or uncomment the current line or selected region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd ";") #'bryce/toggle-comment)
  (define-key evil-visual-state-map (kbd ";") #'bryce/toggle-comment))



(use-package modus-themes :ensure t)
(use-package ef-themes :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package doom-themes :ensure t)
(use-package monokai-theme :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package avy
  :ensure t
  :bind ("C-s" . avy-goto-char-timer))

(use-package rainbow-delimiters
  :ensure t
  :hook (lisp-mode . rainbow-delimiters-mode))

(use-package eldoc
  :ensure t
  :hook ((lisp-mode) . eldoc-mode) ((sly-mode) . eldoc-mode))

(use-package eldoc-box
  :ensure t
  :hook (eldoc-mode . eldoc-box-hover-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                 ;; Enable auto-completion
  (corfu-cycle t)                ;; Allow cycling through candidates
  (corfu-preview-current nil)   ;; Disable live preview
  (corfu-quit-no-match 'separator))

(use-package sly
  :ensure t
  :hook ((sly-mode . eldoc-mode))
  :config
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))

(use-package sly-quicklisp :ensure t)
(use-package sly-asdf :ensure t)

(use-package embark
  :ensure t
  :bind (("C-." . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(use-package eat
  :ensure t
  :config
  (setq eat-terminal-type "xterm-256color"))

(defalias 'shell 'eat)

(use-package hyperbole
  :ensure t
  :init
  (hyperbole-mode 1))


(use-package dashboard
  :ensure t
  :init
  (setq inhibit-startup-screen t) ; Disable default splash screen
  :config
  (dashboard-setup-startup-hook))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-hide-stars nil               ;; Keep leading stars visible
        org-modern-todo-faces
        '(("TODO" :inherit warning :weight bold)
          ("DONE" :inherit success :strike-through t))
        org-modern-table nil                    ;; Optional: keep tables classic
        org-modern-priority nil                 ;; Optional: turn off priority boxes
        org-modern-checkbox nil))               ;; Optional: keep classic checkboxes

(use-package org-download
  :ensure t
  :hook (org-mode . org-download-enable)
  :config
  ;; Save images relative to the org file's directory
  (setq org-download-method 'directory
        org-download-image-dir "images"
        org-download-heading-lvl nil  ;; Don't nest images under heading
        org-download-screenshot-method "screencapture -i %s" ;; macOS specific
        org-download-delete-image-after-download nil))

(defun org-download-resize-last-image (&optional width)
  "Resize the last downloaded image to WIDTH px (default 600)."
  (interactive "nResize to width (px): ")
  (let ((last-img org-download-last-file)
        (target-width (or width 600)))
    (when last-img
      (shell-command (format "convert %s -resize %d %s"
                             (shell-quote-argument last-img)
                             target-width
                             (shell-quote-argument last-img)))
      (message "Resized image to %dpx" target-width))))


(defun my/sly-repl-here ()
  "Start SLY and switch to the REPL in the current window."
  (interactive)
  (sly)
  (let ((repl (sly-mrepl--find-create (sly-connection))))
    (when repl
      (switch-to-buffer repl))))

(advice-add 'sly :after
            (lambda (&rest _)
              (let ((repl (sly-mrepl--find-create (sly-connection))))
                (when repl
                  (switch-to-buffer repl)))))

(setq display-buffer-alist
      '(("\\*sly-mrepl.*\\*" . (display-buffer-same-window))))
