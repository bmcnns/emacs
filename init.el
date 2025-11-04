
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

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

(use-package evil
  :ensure t)

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
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "b99ff6bfa13f0273ff8d0d0fd17cc44fab71dfdc293c7a8528280e690f084ef0" "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0" "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0" "d609d9aaf89d935677b04d34e4449ba3f8bbfdcaaeeaab3d21ee035f43321ff1" "b1791a921c4f38cb966c6f78633364ad880ad9cf36eef01c60982c54ec9dd088" "ac893acecb0f1cf2b6ccea5c70ea97516c13c2b80c07f3292c21d6eb0cb45239" "6af300029805f10970ebec4cea3134f381cd02f04c96acba083c76e2da23f3ec" "c038d994d271ebf2d50fa76db7ed0f288f17b9ad01b425efec09519fa873af53" "aff0396925324838889f011fd3f5a0b91652b88f5fd0611f7b10021cc76f9e09" "4c16a8be2f20a68f0b63979722676a176c4f77e2216cc8fe0ea200f597ceb22e" "90185f1d8362727f2aeac7a3d67d3aec789f55c10bb47dada4eefb2e14aa5d01" "ffa78fc746f85d1c88a2d1691b1e37d21832e9a44a0eeee114a00816eabcdaf9" "cee5c56dc8b95b345bfe1c88d82d48f89e0f23008b0c2154ef452b2ce348da37" "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2" "19b62f442479efd3ca4c1cef81c2311579a98bbc0f3684b49cdf9321bd5dfdbf" "fae5872ff90462502b3bedfe689c02d2fa281bc63d33cb007b94a199af6ccf24" "b3ba955a30f22fe444831d7bc89f6466b23db8ce87530076d1f1c30505a4c23b" "71b688e7ef7c844512fa7c4de7e99e623de99a2a8b3ac3df4d02f2cd2c3215e7" "3d9938bbef24ecee9f2632cb25339bf2312d062b398f0dfb99b918f8f11e11b1" "541282f66e5cc83918994002667d2268f0a563205117860e71b7cb823c1a11e9" "da69584c7fe6c0acadd7d4ce3314d5da8c2a85c5c9d0867c67f7924d413f4436" "a0e9bc5696ce581f09f7f3e7228b949988d76da5a8376e1f2da39d1d026af386" "2551f2b4bc12993e9b8560144fb072b785d4cddbef2b6ec880c602839227b8c7" "b41d0a9413fb0034cea34eb8c9f89f6e243bdd76bccecf8292eb1fefa42eaf0a" "b9f44212b4be6f0466811c5d8a297dda3c40dbf4c4cfd97c1686fceb2043b617" "8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378" "5a0ddbd75929d24f5ef34944d78789c6c3421aa943c15218bac791c199fc897d" "51fa6edfd6c8a4defc2681e4c438caf24908854c12ea12a1fbfd4d055a9647a3" "75b371fce3c9e6b1482ba10c883e2fb813f2cc1c88be0b8a1099773eb78a7176" "5aedf993c7220cbbe66a410334239521d8ba91e1815f6ebde59cecc2355d7757" "18a1d83b4e16993189749494d75e6adb0e15452c80c431aca4a867bcc8890ca9" "d5fd482fcb0fe42e849caba275a01d4925e422963d1cd165565b31d3f4189c87" "8ba8918be4bb12c57cae812f8f9543e7a4b59a3cc1d5d4a4f97dc26a397c94e3" "7235b77f371f46cbfae9271dce65f5017b61ec1c8687a90ff30c6db281bfd6b7" "a9028cd93db14a5d6cdadba789563cb90a97899c4da7df6f51d58bb390e54031" "1c2fb3448ce245f18c62fde3c7cfd008e69a27e88ae8a03fbb62857f13d0b6fe" "c5975101a4597094704ee78f89fb9ad872f965a84fb52d3e01b9102168e8dc40" "6bf350570e023cd6e5b4337a6571c0325cec3f575963ac7de6832803df4d210a" "0adcffc4894e2dd21283672da7c3d1025b5586bcef770fdc3e2616bdb2a771cd" "8529b6ff705d30e6df50734db169b376e9de050ed56ce4e59ff98d774a710847" "3f0b3692ad48f88c0006ddd1543175376aff7a621d95739a02142f9081478e90" "d35afe834d1f808c2d5dc7137427832ccf99ad2d3d65d65f35cc5688404fdf30" "2a36b8d0abc07a341a90a3a5997ea143d5f1f02f5cff2901078e9cd04796b5a0" "72d9086e9e67a3e0e0e6ba26a1068b8b196e58a13ccaeff4bfe5ee6288175432" "4d714a034e7747598869bef1104e96336a71c3d141fa58618e4606a27507db4c" "3f24dd8f542f4aa8186a41d5770eb383f446d7228cd7a3413b9f5e0ec0d5f3c0" "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a" "28b9703810da2df6fb8667b681cea47d6fbfa5df6a7f23d5dee41d25acca38ba" "8dbbcb2b7ea7e7466ef575b60a92078359ac260c91fe908685b3983ab8e20e3f" default))
 '(display-time-default-load-average nil)
 '(notmuch-address-command 'internal)
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(all-the-icons doom-modeline ob-sly org-download org-modern dashboard macrostep embark-consult embark sly-asdf sly-quicklisp sly straight straight-el cape rainbow-delimiters hl-todo which-key doom-themes monokai-theme github-theme gruvbox-theme ef-themes modus-themes undo-tree wgrep deadgrep chatgpt-shell treemacs orderless consult marginalia vertico org-fragtog py-autopep8 flycheck elpy org-bullets magit ivy-rich evil-collection counsel))
 '(python-shell-interpreter "/home/bryce/anaconda3/envs/gp-is-good-for-fqe/bin/python3")
 '(safe-local-variable-values '((buffer-read-only . 1))))
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


(set-face-attribute 'default nil :height 130)

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

(setq inferior-lisp-program "sbcl --dynamic-space-size 8192")

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
(require 'minibuffer)
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
        chatgpt-shell-model-version "gpt-4o"
        chatgpt-shell-default-interface 'openai)
  :config
  (define-key chatgpt-shell-mode-map (kbd "RET") nil)
  (evil-define-key 'normal chatgpt-shell-mode-map
    (kbd "RET") #'chatgpt-shell-submit)
  (evil-define-key 'normal 'global (kbd "C-`") #'chatgpt-shell))
    

(use-package deadgrep
  :ensure t
  :after evil
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

(use-package sly
  :ensure t
  :hook ((sly-mode . eldoc-mode)))

(use-package sly-quicklisp :ensure t)
(use-package sly-asdf :ensure t)

(use-package eldoc
  :after sly
  :ensure t
  :hook ((lisp-mode) . eldoc-mode) ((sly-mode) . eldoc-mode))

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

;; Ensure all-the-icons package is installed
(use-package all-the-icons
  :ensure t)

;; Install and configure doom-modeline
(use-package doom-modeline
  :after all-the-icons
  :ensure t
  :init
  ;; Set custom variables for the doom-modeline
  (setq doom-modeline-modal-icon t               ;; Show/hide evil state icon
        doom-modeline-major-mode-icon nil        ;; Hide major mode icon
        doom-modeline-minor-modes nil            ;; Hide minor modes from modeline
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-buffer-encoding nil
        doom-modeline-time t
        doom-modeline-time-icon nil
        doom-modeline-time-live-icon nil
        doom-modeline-project-name t
        doom-modeline-total-line-number t
        doom-modeline-buffer-state-icon nil))

 ;; Install straight.el (package manager)
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
  (load bootstrap-file nil 'nomessage))          ;; Load straight.el

;; Use straight.el as the backend for use-package
(straight-use-package 'use-package)

;; Install and configure nerd-icons
(use-package nerd-icons
  :straight (nerd-icons
             :type git
             :host github
             :repo "rainstormstudio/nerd-icons.el"
             :files (:defaults "data"))
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")) ;; Set default Nerd Font for GUI

(doom-modeline-mode 1)

(display-time-mode t)
(setq display-time-default-load-average nil)

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

(define-prefix-command 'my-additional-prefix)
(global-set-key (kbd "C-a") 'my-additional-prefix)
(global-set-key (kbd "C-a s") #'list-unsaved-buffers)

(setq org-babel-lisp-eval-fn #'sly-eval)

(setq org-babel-python-command "/usr/bin/python3")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (python .t)))

(use-package htmlize :ensure t)

;; Load local/latest org
(use-package org
  :pin org
  :ensure t)

(use-package orderless :ensure t)

(set-face-attribute 'minibuffer-prompt nil :height 140)
(add-hook 'minibuffer-setup-hook
          (lambda () (setq line-spacing 0.2)))

;; Org mode setup

(setq org-directory "~/Repos/orgfiles")

(setq org-agenda-files '("~/Repos/orgfiles/journal.org"
                         "~/Repos/orgfiles/meetings.org"
                         "~/Repos/orgfiles/todo.org"
                         "~/Repos/orgfiles/calendar.org"))

;; Make capture quick and easy to use
(global-set-key (kbd "C-c c") #'org-capture)

;; Show entries with inactive timestamps in the agenda
(setq org-agenda-include-inactive-timestamps nil)

(setq org-capture-templates
      '(("j" "Journal" entry
         (file+olp+datetree "~/Repos/orgfiles/journal.org")
         "* %?\n:PROPERTIES:\n:Created: %U\n:END:\n"
         :empty-lines 1)
        ("c" "Check-in" entry
         (file+olp+datetree "~/Repos/orgfiles/check-ins.org")
         "* %?\n:PROPERTIES:\n:Created: %U\n:END:\n"
         :empty-lines 1)
        ("t" "Task" entry
         (file+headline "~/Repos/orgfiles/todo.org" "Inbox")
         "* TODO %?\n:PROPERTIES:\n:Created: %U\n:END:\n"
         :empty-lines 1)
        ("m" "Meeting" entry
         (file+headline "~/Repos/orgfiles/meetings.org" "Meetings")
         "* %^{Title}\n%^{When|Timestamp or range|<%Y-%m-%d %a %H:%M>|<%Y-%m-%d %a %H:%M>--<%Y-%m-%d %a %H:%M>}\n:PROPERTIES:\n:Created: %U\n:SUMMARY: %\\1\n:LOCATION: %^{Location|Online}\n:TIMEZONE: %^{TZ|America/Halifax}\n:CLASS: %^{Class|PUBLIC|CONFIDENTIAL|PRIVATE}\n:END:\n%^{Description}\n"
         :empty-lines 1)))
    

(global-set-key (kbd "C-c a") #'org-agenda)

(use-package calfw
  :ensure t)

(use-package calfw-org
  :after calfw
  :ensure t)

(global-set-key (kbd "C-c C-d") #'cfw:open-org-calendar)

(setq org-agenda-todo-ignore-without-schedules t)

(defun sync-org-agenda-with-remote-server (&optional calendar-file remote-path)
  "Copy CALENDAR-FILE to bryce@REMOTE-IP:~/bryce.ics via TRAMP.
Defaults: /tmp/bryce.ics and 129.173.67.123."
  (interactive)
  (let* ((calendar-file (or calendar-file "~/Repos/orgfiles/bryce.ics"))
         (remote-path (or remote-path "/ssh:bmacinnis@timberlea.cs.dal.ca:~/public_html/bryce.ics")))
    (when (file-exists-p remote-path)
      (delete-file remote-path))
    (copy-file calendar-file remote-path t)))

(defun publish-org-agenda ()
  (org-icalendar-combine-agenda-files)
  (sync-org-agenda-with-remote-server "~/Repos/orgfiles/bryce.ics" "/ssh:bmacinnis@timberlea.cs.dal.ca:~/public_html/bryce.ics"))
  
;; Where your meetings file lives
(defconst bryce-meetings-file
  (expand-file-name "~/Repos/orgfiles/meetings.org"))

(defun bryce--maybe-publish-agenda ()
  "If the just-saved buffer is meetings.org, publish the agenda & sync."
  (when (and buffer-file-name
             (string= (file-truename buffer-file-name)
                      (file-truename bryce-meetings-file)))
    (message "Publishing iCal…")
    (publish-org-agenda)
    (message "Publishing iCal…done")))

(add-hook 'after-save-hook #'bryce--maybe-publish-agenda)

(setq org-icalendar-combined-agenda-file "~/Repos/orgfiles/bryce.ics")

(setq erc-autojoin-channels-alist '(("" "#lispgames")))

(use-package crux
  :ensure t)

(global-set-key (kbd "s-r") #'crux-recentf-find-file)

(global-set-key [remap keyboard-quit] #'crux-keyboard-quit-dwim)

(global-set-key (kbd "C-c C-9") #'crux-find-user-init-file)

(global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer)

(global-set-key (kbd "C-x 4 t") #'crux-transpose-windows)

(use-package notmuch
  :ensure t)

(setq sendmail-program "msmtp"
      message-sendmail-f-is-evil t
      message-send-mail-function 'message-send-mail-with-sendmail)

(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-f-is-evil t)

(define-prefix-command 'my-mail-map)
(global-set-key (kbd "C-c m") 'my-mail-map)
(global-set-key (kbd "C-c m n") #'notmuch-mua-new-mail)

(global-set-key (kbd "C-c m h") #'notmuch)

(global-set-key (kbd "C-c m g") (lambda ()
                                  (interactive)
                                  (shell-command-to-string "mbsync -a -j 4")
                                  (shell-command-to-string "notmuch new")))

(global-set-key (kbd "C-c m i") (lambda ()
                                  (interactive)
                                  (notmuch-search "tag:inbox")))

;; use notmuch’s built-in address source

(setq notmuch-search-oldest-first nil)
(setq-default notmuch-search-oldest-first nil)

(use-package khalel
  :after org
  :config (khalel-add-capture-template)
  :ensure t)
(setq khalel-vdirsyncer-command "vdirsyncer")
(setq khalel-khal-command "khal")
(setq khalel-capture-key "e")
(setq khalel-import-org-file (concat "~/Repos/orgfiles/" "/" "calendar.org"))
(setq khalel-import-end-date "+30d")
(setq khalel-import-org-file-confirm-overwrite nil)

(use-package doom-modeline-now-playing
  :straight (doom-modeline-now-playing :host github :repo "elken/doom-modeline-now-playing")
  :after doom-modeline)

(setq doom-modeline-now-playing t)

(doom-modeline-def-modeline 'main
  '(bar workspace-name window-number modals matches buffer-info remote-host
        buffer-position parrot selection-info)
  '(misc-info persp-name lsp irc mu4e github debug repl
        minor-modes input-method indent-info buffer-encoding major-mode
        process vcs now-playing))   ;; <-- add here

(doom-modeline-set-modeline 'main t)

(setq mail-user-agent 'message-user-agent)

(use-package org-msg
  :ensure t
  :hook
  (message-mode . org-msg-mode)
  (notmuch-message-mode . org-msg-edit-mode)
  :config
  (org-msg-mode 1))



(defun my/notmuch-sync-before (&rest args)
  "Always sync mail before running a notmuch command."
  (start-process-shell-command
   "mail-sync" "*mail-sync*"
   "mbsync -a && notmuch new"))

(advice-add 'notmuch :before #'my/notmuch-sync-before)

(use-package corfu
  :ensure t
  ;; Optional customizations
  ;;:custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )

(setq corfu-auto t
      corfu-auto-prefix 1
      corfu-auto-delay 0.5)

(use-package sly-repl-ansi-color
  :straight (:host github :repo "PuercoPop/sly-repl-ansi-color")
  :after sly
  :config
  (push 'sly-repl-ansi-color sly-contribs))

(use-package origami
  :ensure t
  :init
  (global-origami-mode))

(evil-define-key 'normal 'global (kbd "TAB") #'origami-toggle-node)
(evil-define-key 'normal 'global (kbd "<S-tab>") #'origami-open-all-nodes)
(evil-define-key 'normal 'global (kbd "<C-tab>") #'origami-close-all-nodes)

(use-package demap
  :ensure t)


(global-set-key (kbd "<f2>") #'demap-toggle)

(setq magit-show-long-lines-warning nil)

(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger\\'" . ledger-mode))

(use-package scad-mode
  :ensure t)

