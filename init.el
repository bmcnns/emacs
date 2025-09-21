(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
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
 '(display-time-default-load-average nil)
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(all-the-icons doom-modeline ob-sly org-download org-modern dashboard macrostep embark-consult embark sly-asdf sly-quicklisp sly straight straight-el cape rainbow-delimiters hl-todo which-key doom-themes monokai-theme github-theme gruvbox-theme ef-themes modus-themes undo-tree wgrep deadgrep chatgpt-shell treemacs orderless consult marginalia vertico org-fragtog py-autopep8 flycheck elpy org-bullets magit ivy-rich evil-collection counsel))

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
        chatgpt-shell-model-version "gpt-4o"
        chatgpt-shell-default-interface 'openai)
  :config
  (define-key chatgpt-shell-mode-map (kbd "RET") nil)
  (evil-define-key 'normal chatgpt-shell-mode-map
    (kbd "RET") #'chatgpt-shell-submit)
  (evil-define-key 'normal 'global (kbd "`") #'chatgpt-shell))
    

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

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package sly
  :ensure t
  :hook ((sly-mode . eldoc-mode))
  :config
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))

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

(global-set-key (kbd "C-c C-s") #'list-unsaved-buffers)

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
                         "~/Repos/orgfiles/todo.org"))

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

(global-set-key (kbd "C-c i") #'crux-find-user-init-file)

(global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer)

(global-set-key (kbd "C-x 4 t") #'crux-transpose-windows)


(use-package doom-modeline-now-playing
  :straight (doom-modeline-now-playing :host github :repo "elken/doom-modeline-now-playing")
  :ensure t
  :after doom-modeline)

(doom-modeline-def-modeline 'main
  '(bar workspace-name window-number modals matches buffer-info remote-host
        buffer-position parrot selection-info)
  '(misc-info persp-name lsp irc mu4e github debug repl
        minor-modes input-method indent-info buffer-encoding major-mode
        process vcs now-playing))

(doom-modeline-set-modeline 'main t)

(setq doom-modeline-now-playing-max-length 50)
