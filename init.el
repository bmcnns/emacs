;;; init.el --- Bryce's Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Cleaned-up config with deferred loading for fast startup.
;; Organized into logical sections.

;;; Code:

;;;; ============================================================
;;;; Package Management
;;;; ============================================================

(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("org"    . "http://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Uncomment to profile startup:
;; (setq use-package-compute-statistics t)

;;;; ============================================================
;;;; General Settings
;;;; ============================================================

(setq inhibit-startup-screen t
      make-backup-files nil
      enable-recursive-minibuffers t
      select-enable-clipboard t
      select-enable-primary t)

(setq-default tab-width 4
              indent-tabs-mode nil
              standard-indent 4)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode t)
(electric-indent-mode 1)
(set-window-scroll-bars (minibuffer-window) nil nil)
(set-face-attribute 'default nil :height 130)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 4
                  standard-indent 4)))

;;;; ============================================================
;;;; macOS Clipboard (works in terminal & GUI)
;;;; ============================================================

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta)
  ;; GUI Emacs on macOS should handle clipboard natively,
  ;; but for terminal Emacs, use pbcopy/pbpaste:
  (unless (display-graphic-p)
    (defun pbcopy ()
      (interactive)
      (let ((text (buffer-substring-no-properties
                   (region-beginning) (region-end))))
        (with-temp-buffer
          (insert text)
          (call-process-region (point-min) (point-max) "pbcopy"))))
    (defun pbpaste ()
      (interactive)
      (insert (shell-command-to-string "pbpaste")))
    ;; Integrate with evil yanking/pasting
    (setq interprogram-cut-function
          (lambda (text &optional _push)
            (let ((process-connection-type nil))
              (let ((proc (start-process "pbcopy" nil "pbcopy")))
                (process-send-string proc text)
                (process-send-eof proc))))
          interprogram-paste-function
          (lambda ()
            (shell-command-to-string "pbpaste")))))

;;;; ============================================================
;;;; Evil Mode
;;;; ============================================================

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config (evil-collection-init))

;;;; ============================================================
;;;; Completion Framework (Vertico + Orderless + Marginalia + Consult)
;;;; ============================================================

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-c t" . consult-theme)))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.5))

(use-package cape :ensure t)

(set-face-attribute 'minibuffer-prompt nil :height 140)
(add-hook 'minibuffer-setup-hook
          (lambda () (setq line-spacing 0.2)))

;;;; ============================================================
;;;; Navigation & Search
;;;; ============================================================

(use-package avy
  :ensure t
  :bind ("C-s" . avy-goto-char-timer))

(use-package swiper
  :ensure t
  :config
  (defun my/swiper-with-occur ()
    "Run `swiper` and immediately trigger `ivy-occur`."
    (interactive)
    (let ((ivy-initial-inputs-alist nil))
      (swiper)
      (ivy-occur)))
  (define-key evil-normal-state-map (kbd "/") #'my/swiper-with-occur))

(use-package counsel
  :ensure t
  :bind (("C-c p f" . counsel-git)
         ("C-c p p" . counsel-git)
         ("C-c p g" . counsel-git-grep)))

(use-package deadgrep
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map ("," . deadgrep))
  :config
  (with-eval-after-load 'wgrep
    (setq deadgrep-edit-mode-line-string " [wgrep: edit-mode]")
    (add-hook 'deadgrep-finished-hook #'deadgrep-edit-mode))
  ;; Show results in same window
  (defun deadgrep--visit-search-buffer-same-window ()
    (let ((buffer (get-buffer deadgrep--search-buffer-name)))
      (when buffer (switch-to-buffer buffer))))
  (advice-add 'deadgrep--visit-search-buffer
              :override #'deadgrep--visit-search-buffer-same-window)
  (setq deadgrep-display-buffer-function #'switch-to-buffer))

(use-package wgrep
  :ensure t
  :config
  (add-hook 'wgrep-mode-hook #'evil-normalize-keymaps)
  (add-hook 'wgrep-mode-hook (lambda () (evil-normal-state))))

;;;; ============================================================
;;;; File Tree & Window Management
;;;; ============================================================

(use-package treemacs
  :ensure t
  :bind ([f5] . treemacs)
  :config (setq treemacs-width 30))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;;; ============================================================
;;;; Git
;;;; ============================================================

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config (setq magit-show-long-lines-warning nil))

;;;; ============================================================
;;;; Undo
;;;; ============================================================

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff nil
        undo-tree-visualizer-timestamps nil
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo" user-emacs-directory))))
  (defun my/undo-tree-quit-after-set (&rest _)
    (undo-tree-visualizer-quit))
  (advice-add 'undo-tree-visualizer-set :after #'my/undo-tree-quit-after-set)
  (with-eval-after-load 'evil
    (define-key undo-tree-visualizer-mode-map (kbd "h") #'undo-tree-visualize-switch-branch-left)
    (define-key undo-tree-visualizer-mode-map (kbd "l") #'undo-tree-visualize-switch-branch-right)
    (define-key undo-tree-visualizer-mode-map (kbd "j") #'undo-tree-visualize-redo)
    (define-key undo-tree-visualizer-mode-map (kbd "k") #'undo-tree-visualize-undo)
    (define-key undo-tree-visualizer-mode-map (kbd "q") #'undo-tree-visualizer-quit)
    (define-key evil-normal-state-map (kbd "u") #'undo-tree-visualize)))

;;;; ============================================================
;;;; Themes & Modeline
;;;; ============================================================

(use-package modus-themes :ensure t :defer t)
(use-package ef-themes :ensure t :defer t)
(use-package gruvbox-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)

(use-package all-the-icons :ensure t)

(use-package doom-modeline
  :ensure t
  :after all-the-icons
  :init
  (setq doom-modeline-modal-icon t
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-buffer-encoding nil
        doom-modeline-time t
        doom-modeline-time-icon nil
        doom-modeline-time-live-icon nil
        doom-modeline-project-name t
        doom-modeline-total-line-number t
        doom-modeline-buffer-state-icon nil)
  :config
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches
      buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp irc mu4e github debug repl
      minor-modes input-method indent-info buffer-encoding
      major-mode process vcs))
  (doom-modeline-set-modeline 'main t)
  (doom-modeline-mode 1))

(display-time-mode t)
(setq display-time-default-load-average nil)

(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook))

;;;; ============================================================
;;;; Which Key
;;;; ============================================================

(use-package which-key
  :ensure t
  :init (which-key-mode))

;;;; ============================================================
;;;; Common Lisp (SLY)
;;;; ============================================================

(setq inferior-lisp-program "sbcl --dynamic-space-size 8192")


 (setq 
(use-package sly
  :ensure t
  :defer t
  :hook (sly-mode . eldoc-mode)
  :config
  (setq display-buffer-alist
        '(("\\*sly-mrepl.*\\*" . (display-buffer-same-window))))
  (advice-add 'sly :after
              (lambda (&rest _)
                (let ((repl (sly-mrepl--find-create (sly-connection))))
                  (when repl (switch-to-buffer repl))))))

(use-package sly-quicklisp :ensure t :after sly)
(use-package sly-asdf :ensure t :after sly)

(use-package macrostep
  :ensure t
  :bind ("C-c e" . macrostep-expand))

(use-package rainbow-delimiters
  :ensure t
  :hook (lisp-mode . rainbow-delimiters-mode))

(use-package eldoc
  :ensure t
  :hook ((lisp-mode . eldoc-mode)
         (sly-mode . eldoc-mode)))

;;;; ============================================================
;;;; Scheme (Geiser + MIT)
;;;; ============================================================

(use-package geiser-mit
  :ensure t
  :defer t
  :config (setq geiser-mit-binary "scheme"))

(add-hook 'scheme-mode-hook 'geiser-mode)

;;;; ============================================================
;;;; Python (deferred — only loads when opening .py files)
;;;; ============================================================

(use-package flycheck :ensure t :defer t)
(use-package py-autopep8 :ensure t :defer t)

(use-package elpy
  :ensure t
  :defer t
  :hook (python-mode . elpy-enable)
  :config
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (flycheck-mode 1))

;;;; ============================================================
;;;; Jupyter (deferred — very heavy, only load on demand)
;;;; ============================================================

(use-package jupyter
  :ensure t
  :defer t)

;;;; ============================================================
;;;; Org Mode
;;;; ============================================================

(use-package org
  :ensure t
  :pin org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-directory "~/Repos/orgfiles"
        org-agenda-files '("~/Repos/orgfiles/journal.org"
                           "~/Repos/orgfiles/meetings.org"
                           "~/Repos/orgfiles/todo.org"
                           "~/Repos/orgfiles/calendar.org")
        org-agenda-include-inactive-timestamps nil
        org-agenda-todo-ignore-without-schedules t
        org-babel-lisp-eval-fn #'sly-eval
        org-babel-python-command "/usr/bin/python3"
        org-icalendar-combined-agenda-file "~/Repos/orgfiles/bryce.ics")

  ;; Babel languages (jupyter loaded lazily)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (python . t)))

  ;; Load jupyter babel support only when needed
  (with-eval-after-load 'jupyter
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages '((jupyter . t)))))

  ;; Capture templates
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
           "* %^{Title}
%^{When|Timestamp or range|<%Y-%m-%d %a %H:%M>|<%Y-%m-%d %a %H:%M>--<%Y-%m-%d %a %H:%M>}
:PROPERTIES:
:Created: %U
:SUMMARY: %\\1
:LOCATION: %^{Location|Online}
:TIMEZONE: %^{TZ|America/Halifax}
:CLASS: %^{Class|PUBLIC|CONFIDENTIAL|PRIVATE}
:END:
%^{Description}
"
           :empty-lines 1)))

  ;; LaTeX classes
  (setq org-latex-classes nil)

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

  (add-to-list 'org-latex-classes
               '("acmart"
                 "\\documentclass{acmart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("acmartconf"
                 "\\documentclass[sigconf, nonacm]{acmart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("gptp"
                 "\\documentclass[graybox]{svmult}
\\usepackage{mathptmx}
\\usepackage{helvet}
\\usepackage{courier}
\\usepackage{type1cm}
\\usepackage{makeidx}
\\usepackage{graphicx}
\\usepackage{multicol}
\\usepackage[bottom]{footmisc}
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
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}"
                 ("\\begin{frame}{%s}" "\\end{frame}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}"))))

;; Org extension packages
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-hide-stars nil
        org-modern-todo-faces '(("TODO" :inherit warning :weight bold)
                                ("DONE" :inherit success :strike-through t))
        org-modern-table nil
        org-modern-priority nil
        org-modern-checkbox nil))

(use-package org-fragtog
  :ensure t
  :after org
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2.5)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

(use-package org-download
  :ensure t
  :hook (org-mode . org-download-enable)
  :config
  (setq org-download-method 'directory
        org-download-image-dir "images"
        org-download-heading-lvl nil
        org-download-screenshot-method "screencapture -i %s"
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

(use-package htmlize :ensure t :defer t)

;;;; ============================================================
;;;; Org Agenda Publishing (iCal sync)
;;;; ============================================================

(defun sync-org-agenda-with-remote-server (&optional calendar-file remote-path)
  "Copy CALENDAR-FILE to remote server via TRAMP."
  (interactive)
  (let* ((calendar-file (or calendar-file "~/Repos/orgfiles/bryce.ics"))
         (remote-path (or remote-path "/ssh:bmacinnis@timberlea.cs.dal.ca:~/public_html/bryce.ics")))
    (when (file-exists-p remote-path)
      (delete-file remote-path))
    (copy-file calendar-file remote-path t)))

(defun publish-org-agenda ()
  "Export agenda to iCal and sync to remote."
  (org-icalendar-combine-agenda-files)
  (sync-org-agenda-with-remote-server))

(defconst bryce-meetings-file
  (expand-file-name "~/Repos/orgfiles/meetings.org"))

(defun bryce--maybe-publish-agenda ()
  "If the saved buffer is meetings.org, publish the agenda."
  (when (and buffer-file-name
             (string= (file-truename buffer-file-name)
                      (file-truename bryce-meetings-file)))
    (message "Publishing iCal…")
    (publish-org-agenda)
    (message "Publishing iCal…done")))

(add-hook 'after-save-hook #'bryce--maybe-publish-agenda)

;;;; ============================================================
;;;; Calendar (khalel)
;;;; ============================================================

(use-package calfw :ensure t :defer t)
(use-package calfw-org :ensure t :after calfw)
(global-set-key (kbd "C-c C-d") #'cfw:open-org-calendar)

(use-package khalel
  :ensure t
  :defer t
  :after org
  :config
  (khalel-add-capture-template)
  (setq khalel-vdirsyncer-command "vdirsyncer"
        khalel-khal-command "khal"
        khalel-capture-key "e"
        khalel-import-org-file "~/Repos/orgfiles/calendar.org"
        khalel-import-end-date "+30d"
        khalel-import-org-file-confirm-overwrite nil))

;;;; ============================================================
;;;; Email (notmuch + msmtp)
;;;; ============================================================

(use-package notmuch
  :ensure t
  :defer t
  :config
  (setq notmuch-search-oldest-first nil)
  (setq-default notmuch-search-oldest-first nil))

(setq sendmail-program "msmtp"
      message-sendmail-f-is-evil t
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-extra-arguments '("--read-envelope-from")
      mail-specify-envelope-from t
      mail-envelope-from 'header
      mail-user-agent 'message-user-agent)

;; Async mail sync before notmuch
(defun my/notmuch-sync-before (&rest _args)
  "Async sync mail before running notmuch."
  (start-process-shell-command "mail-sync" "*mail-sync*"
                               "mbsync -a && notmuch new"))
(advice-add 'notmuch :before #'my/notmuch-sync-before)

(use-package org-msg
  :ensure t
  :defer t
  :hook ((message-mode . org-msg-mode)
         (notmuch-message-mode . org-msg-edit-mode))
  :config (org-msg-mode 1))

;; Mail keybindings
(define-prefix-command 'my-mail-map)
(global-set-key (kbd "C-c m") 'my-mail-map)
(global-set-key (kbd "C-c m n") #'notmuch-mua-new-mail)
(global-set-key (kbd "C-c m h") #'notmuch)
(global-set-key (kbd "C-c m g")
                (lambda () (interactive)
                  (shell-command-to-string "mbsync -a -j 4")
                  (shell-command-to-string "notmuch new")))
(global-set-key (kbd "C-c m i")
                (lambda () (interactive)
                  (notmuch-search "tag:inbox")))

;;;; ============================================================
;;;; ChatGPT Shell
;;;; ============================================================

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-copy-env "OPENAI_API_KEY"))

(use-package chatgpt-shell
  :ensure t
  :defer t
  :after exec-path-from-shell evil
  :init
  (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY")
        chatgpt-shell-model-version "gpt-4o"
        chatgpt-shell-default-interface 'openai)
  :config
  (define-key chatgpt-shell-mode-map (kbd "RET") nil)
  (evil-define-key 'normal chatgpt-shell-mode-map
    (kbd "RET") #'chatgpt-shell-submit)
  (evil-define-key 'normal 'global
    (kbd "C-`") #'chatgpt-shell))

;;;; ============================================================
;;;; Terminal
;;;; ============================================================

(use-package eat
  :ensure t
  :defer t
  :config (setq eat-terminal-type "xterm-256color"))

(defalias 'shell 'eat)

;;;; ============================================================
;;;; TRAMP & Remote Access
;;;; ============================================================

(setq tramp-default-method "ssh")

(defun brycelab1 ()
  "Quick access to Bryce's lab machine 1."
  (interactive)
  (find-file "/ssh:bryce@129.173.67.115:/home/bryce/"))

(defun brycelab2 ()
  "Quick access to Bryce's lab machine 2."
  (interactive)
  (find-file "/ssh:bryce@129.173.67.123:/home/bryce/"))

;;;; ============================================================
;;;; Folding & Minimap
;;;; ============================================================

(use-package origami
  :ensure t
  :init (global-origami-mode)
  :config
  (evil-define-key 'normal 'global (kbd "TAB") #'origami-toggle-node)
  (evil-define-key 'normal 'global (kbd "<backtab>") #'origami-open-all-nodes)
  (evil-define-key 'normal 'global (kbd "<C-tab>") #'origami-close-all-nodes))

(use-package demap
  :ensure t
  :bind ("<f6>" . demap-toggle))

;;;; ============================================================
;;;; Misc Packages
;;;; ============================================================

(use-package crux
  :ensure t
  :bind (("s-r"     . crux-recentf-find-file)
         ("C-c C-9" . crux-find-user-init-file)
         ("C-c r"   . crux-rename-file-and-buffer)
         ("C-x 4 t" . crux-transpose-windows))
  :config
  (global-set-key [remap keyboard-quit] #'crux-keyboard-quit-dwim))

(use-package ledger-mode
  :ensure t
  :defer t
  :mode ("\\.ledger\\'" . ledger-mode))

(use-package scad-mode
  :ensure t
  :defer t)

(use-package gnuplot
  :ensure t
  :defer t)

(use-package hl-todo
  :ensure t
  :defer t)

;;;; ============================================================
;;;; Custom Functions
;;;; ============================================================

(defun bryce/toggle-comment ()
  "Comment or uncomment the current line or selected region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd ";") #'bryce/toggle-comment)
  (define-key evil-visual-state-map (kbd ";") #'bryce/toggle-comment))

(defun list-unsaved-buffers ()
  "List unsaved buffers and let the user select one to visit."
  (interactive)
  (let* ((unsaved (seq-filter
                   (lambda (buf)
                     (and (buffer-modified-p buf)
                          (buffer-file-name buf)))
                   (buffer-list)))
         (names (mapcar #'buffer-name unsaved)))
    (if names
        (let ((choice (completing-read "Unsaved buffers: " names nil t)))
          (when choice (switch-to-buffer choice)))
      (message "No unsaved buffers."))))

(define-prefix-command 'my-additional-prefix)
(global-set-key (kbd "C-a") 'my-additional-prefix)
(global-set-key (kbd "C-a s") #'list-unsaved-buffers)

(defun my/sly-repl-here ()
  "Start SLY and switch to the REPL in the current window."
  (interactive)
  (sly)
  (let ((repl (sly-mrepl--find-create (sly-connection))))
    (when repl (switch-to-buffer repl))))

;;;; ============================================================
;;;; Who-Calls-Me Minor Mode
;;;; ============================================================

(use-package ov :ensure t :defer t)

(defface bryce-usage-face
  '((t :foreground "#ff00ff" :weight light))
  "Face for usage overlays.")

(defface bryce-not-used-face
  '((t :foreground "#ff0000" :weight light))
  "Face for unused function overlays.")

(defun bryce/count-symbol-refs (symbol)
  "Count occurrences of SYMBOL in buffer (excluding its defun)."
  (let ((count 0)
        (regex (concat "\\_<" (regexp-quote symbol) "\\_>")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (unless (save-excursion
                  (beginning-of-line)
                  (looking-at (concat "^(defun \\_<"
                                      (regexp-quote symbol) "\\_>")))
          (setq count (1+ count)))))
    count))

(defun bryce/who-cls-me ()
  "Show reference count overlays for all defuns in buffer."
  (interactive)
  (require 'ov)
  (ov-clear 'bryce-hello)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^(defun \\([^ )]+\\)" nil t)
      (let ((fn (match-string 1)))
        (save-excursion
          (beginning-of-defun)
          (forward-line -1)
          (let* ((count (bryce/count-symbol-refs fn))
                 (face (if (= count 0) 'bryce-not-used-face 'bryce-usage-face))
                 (ov (ov-make (point) (point))))
            (ov-set ov 'after-string
                    (propertize (format "\n(%d references)" count) 'face face))
            (ov-set ov 'bryce-hello t)))))))

(define-minor-mode who-calls-me-mode
  "Toggle function reference overlays."
  :lighter " WhoCalls"
  :group 'bryce
  (if who-calls-me-mode
      (bryce/who-cls-me)
    (ov-clear 'bryce-hello)))

;;;; ============================================================
;;;; IRC
;;;; ============================================================

(setq erc-autojoin-channels-alist '(("" "#lispgames")))

;;;; ============================================================
;;;; Custom-set (managed by Emacs — do not edit manually)
;;;; ============================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes '(default))
 '(display-time-default-load-average nil)
 '(notmuch-address-command 'internal)
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(all-the-icons cape chatgpt-shell consult counsel dashboard deadgrep
                   doom-modeline doom-themes ef-themes elpy embark
                   embark-consult evil-collection flycheck geiser-mit
                   gnuplot gptel gruvbox-theme hl-todo ivy-rich
                   jupyter latex-preview-pane macrostep magit
                   marginalia modus-themes monokai-theme ob-sly
                   orderless org-bullets org-download org-fragtog
                   org-margin org-modern pdf-tools py-autopep8
                   rainbow-delimiters sly sly-asdf sly-quicklisp
                   treemacs undo-tree vertico vs-light-theme wgrep
                   which-key))
 '(package-vc-selected-packages
   '((org-margin :url "https://github.com/rougier/org-margin")))
 '(python-shell-interpreter "/home/bryce/anaconda3/envs/gp-is-good-for-fqe/bin/python3")
 '(safe-local-variable-values '((buffer-read-only . 1))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

                                        ; org mode reimagined



;;; init.el ends here

(setq TeX-PDF-mode t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)


(pdf-tools-install)

(use-package gptel
  :ensure t)

(setq
 gptel-model 'claude-sonnet-4-6 ;"claude-3-opus-20240229" also available
 gptel-backend (gptel-make-anthropic "Claude"
                 :stream t :key ""))
