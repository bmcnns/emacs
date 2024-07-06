(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.
;; See `package-archive-priorities` and `package-pinned-packages`.
;; Most users will not need or want to do this.
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-2.4.5/"
  (require 'use-package)))

(setq inhibit-startup-message t)

;; Include a manually installed powershell mode
(add-to-list 'load-path "~/.emacs.d/packages/powershell.el")

(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(tool-bar-mode -1)
(set-window-scroll-bars (minibuffer-window) nil nil)

(setq cursor-type nil)  ; or nil for a box cursor, or bar for a vertical bar cursor
(blink-cursor-mode 1) ; or 0 to disable cursor blinking

(set-face-attribute 'default nil :family "Consolas" :height 130)

(use-package ef-themes
  :ensure t)
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(setq evil-want-keybinding nil)

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init))

;; Enable Evil
(require 'evil)
(evil-mode 1)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; Optional: Add keybindings for projectile and counsel-git-grep
(global-set-key (kbd "C-c p f") 'counsel-git)  ; Find file in Git repo
(global-set-key (kbd "C-c p p") 'counsel-git)  ; Switch to Git project
(global-set-key (kbd "C-c p g") 'counsel-git-grep) ; Search in Git repo

;; Optional: Enable ivy-rich for better project and buffer descriptions
(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

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
        (self-insert-command 1)
      (swiper))))

(defconst my-msbuild-path
  "\"C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Enterprise\\MSBuild\\15.0\\Bin\\msbuild.exe\"")

(defconst change-auditor-path
  "\"C:\\Repos\\ChangeAuditor\\Roq.sln\"")
  
(defun nuget-package-restore ()
  "Run NuGet package restore for Change Auditor."
  (interactive)
  (compile (format "nuget restore \"%s\" -ConfigFile \"C:\\Repos\\ChangeAuditor\\.nuget\\NuGet.Config\" -NoCache -Verbosity Normal" change-auditor-path)))

(defun build-change-auditor ()
  "Run msbuild for Change Auditor with a user-selected build option."
  (interactive)
  (let ((build-options '("Debug" "Release")))
    (setq chosen-option (completing-read "Select build option: " build-options))
    (compile (format "%s \"C:\\Repos\\ChangeAuditor\\Roq.sln\" /nologo /nr:false /nologo /p:platform=\"Any CPU\" /p:configuration=\"%s\"" my-msbuild-path chosen-option))))

(global-set-key (kbd "C-c b") 'build-change-auditor)
(global-set-key (kbd "C-c n r") 'nuget-package-restore)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(global-set-key (kbd "C-c b") 'build-change-auditor)


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(add-to-list 'load-path "~/.emacs.d/packages/powershell.el")
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))


(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))


(use-package treemacs
  :ensure t
  :bind ("<f5>" . treemacs)
  :custom
  (treemacs-is-never-other-window t)
  :hook
  (treemacs-mode . treemacs-project-follow-mode))


(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "openai-key")))))

(setq chatgpt-shell-openai-chatgpt-shell-model-versionkey (getenv "OPENAI_API_KEY"))
(setq chatgpt-shell-model-version "3.5t-16k-0613")

(global-set-key (kbd "C-x p") 'chatgpt-shell)

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)
                             (jupyter . t)
                             (shell . t)))
 
(setq org-babel-python-command "/Users/bmcnns/anaconda3/envs/pytpg/bin/ipython")

(use-package org-bullets
  :ensure t
  :hook ((org-mode) . org-bullets-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode t))

(use-package notmuch
  :ensure t)

(setq org-babel-shell-names '("bash" "python" "ipython" "python3" "ipython3" "conda"))

(use-package ob-async
  :ensure t)

(setq ob-async-no-async-languages-alist
      '("jupyter-python"))

(global-set-key (kbd "C-c a") 'org-agenda)


(setq org-agenda-files (directory-files-recursively "~/Repos/journal/" "\\.org$"))

(toggle-truncate-lines)

(add-hook 'org-mode-hook 'org-indent-mode)

;; Define a function to set the mail-default-headers variable based on the email address
(defun set-mail-default-headers ()
  (setq mail-default-headers (concat "From: " (message-field-value "From") "\n")))

;; Set msmtp as the mail sending program
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it
      mail-specify-envelope-from t
      mail-envelope-from 'header)

(setq sendmail-program "/opt/homebrew/Cellar/msmtp/1.8.25/bin/msmtp")
;; Hook the function to set mail-default-headers before sending mail
(add-hook 'message-send-mail-hook 'set-mail-default-headers)

(use-package org-mime
  :ensure t)

(setq org-export-babel-evaluate nil)

(use-package yapfify
  :ensure t)

(add-hook 'python-mode-hook 'yapf-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#fcfcfc" "#800000" "#008000" "#808000" "#000080" "#800080" "#000080" "#545454"])
 '(chart-face-color-list
   '("#b52c2c" "#0fed00" "#f1e00a" "#2fafef" "#bf94fe" "#47dfea" "#702020" "#007800" "#b08940" "#1f2f8f" "#5f509f" "#00808f"))
 '(custom-safe-themes
   '("73c55f5fd22b6fd44f1979b6374ca7cc0a1614ee8ca5d4f1366a0f67da255627" "5e05db868f138062a3aedcccefe623eee18ec703ae25d4e5aebd65f892ac5bcc" "5f92b9fc442528b6f106eaefa18bb5e7bfa0d737164e18f1214410fef2a6678d" "46a843168cc83b28b740735516e6eea4f97769d848c79b5acab32f7a278f793a" "01aef17f41edea53c665cb57320bd80393761f836be5ab0bd53292afc94bd14d" "7ca04d620046f5807d0740f265844d45e53b864138c246f48f663bea8fba5c5d" "ae7aede615d12f126a7b6b0d8c1e0af2af1ab63fb21d80a130a5f701fdbb23fe" "7a259016d8fe61ef450895a01d842c4353dabc873ee46e250c933995f7f004cf" "704f75d46620d87bb246e2ec1abb129437764b0e84ac0fff6b968311cc046918" "7d10494665024176a90895ff7836a8e810d9549a9872c17db8871900add93d5c" "ca934a76aae4ff950288e082be75a68eb7bac6e8d3dd58b28649993540412ed6" "38cb7d03fec58c860d5e92ee756aa80bdb2d78ef7ac726317e78c5c844e04f3a" "4de156ea6aa06640e3d16da41dfbdee85aea2fb969f620e601a982c6e237b396" "a73cd17e5b490a5558a0ca13186cccb85ff74b2160f77e65ff36024bc66089b4" "65809263a533c5151d522570b419f1a653bfd8fb97e85166cf4278e38c39e00e" "c49f79bfea991ed384cc0dc33328c8d812c413bf27cff0bc24ad58af2cdbccb4" "65af8e8d704bcd9745a4f191db756995de6b1fdd15cf2eb41befaae75f7b045d" "91fed8cade0d85d1f5a5621e72ac6e720945be79493465a79f020e673f7e2d24" "8ea6a46120abb34bf6a664b76d78014e0dd1f2b740a0976ec41313612421712f" "c9bc12faeadf400bd85c286069dff1d3bb3b537be00fff8b5873fb8dbaf4c3eb" "d4d7039292ed380b0fb8c1535b5bc7cf2f2bf9ff745e33a0e1268a6680ba0d20" "4aee0a939c49baa99948d9f685c134a274f3e6a46e23b76fe30dc0ddc9b0748c" "63184b70451ce1eb3b6d09969a1f990f68e0ec1bfdd07f94ba453ac9096008ee" "a0163d99111c1e4810f78bcdb819d8c40067ce48c6ce00c642ec0a7e7c45f26a" "630d2fb3e5bdfadd60b251c72218b61b3142a6c8a186df7c9c23b1a32faf9128" "bd98f90e425caa3219e9b2e44794130aefcb243bfcecdb8843e37df7f635bbf3" "03183bd97449f3695456fc5c8b5dea62fad3cfcdeb8534340b72105821de1dc1" "6cfdddbbd572c60ebb563a321eec8704237c718a4df1814b63d2ae96953320b5" "b9476e7046307b57f20f5e06a466d04559fb59124d041a58a1caba8ee16cb08d" "0672d7fbe7fa24344bbe98461a8db26cb52a074354f9a4529cea4ba1af456044" "0430ba14a3cc147f924a5e0d24c4fd21b377c6ebe2924855969023430705500c" "154a0469224ee6b1cb91796aec23a0d3ae76b50aced14af0e8352677c7482c0f" "f146b2935f915fc1f9cee018199fbb93c2887aed85c7051fb8dd57f5a05f123f" "855b8dee99b96b726a57790eb904544c623cb39caa0846c8afc008d6ee48aec1" "3f48acc0b6cbedc3126e25eaad128f580d445654eab6602373061cb793ce58c7" "7349fd2dd0671b4dd0fad1ad8d20f032b72b2b0dc34e7566f5e5bea5f045c038" "711df73c2d73a038f3e4f9327fa96244f6b081a0ef631fafdc5d3a859d7257b7" "bd17d0f6495c2626c493322ff37c64dc76ddb06c10dbadfead6a4f92775f00f3" "4ca2b8e7ce031be552d1e896f82607af064ac4abe402868dff98759f5970e2b4" "8de91e43d21b9d5cb79a146b267d29d3e377ed73888324912c7f6e514a76e5d7" "e0b2f460b6bee1d7a09b90635b2995e801ff5ac1a7aa66ea9c5674cd0bf1bb7a" "cbc8efdcd8e957c9ede1b4a82fed7fa1f3114ff6e7498c16f0cccb9509c1c17c" "e223120256455daba01b6c68510b48fac813acab05c314510e47aea377b23634" "039112154ee5166278a7b65790c665fe17fd21c84356b7ad4b90c29ffe0ad606" "7819a530936ad2204a35340498da6341f66d816a92b8c71af138f73814b269ec" "67a0f6cdf628610987cf7f0d38f0ae68ed3a5ab6816ff3da5f66d7b09d03f470" "4c326abbf8b85c85e114691d3892cbbfe889b2b064dadd284cf5eccca3eecbff" "c73ce0e82bfbf5f186756001eeaaf26f0b461851a8b421d1ff5ec35e10f300ed" "7dc1c6210efe106a8c6cd47009a2ffd0069826b550dda379e8e4ef6105384cba" "a6a979c8b7ccb1d4536f4fa74a6e47674a3ce65feea3fecdf1d9dc448fac47e0" "df42062cdd672acecac9b5a1229c45f74c0cc2bc0362f9ad41054af6ac355021" "b0b29575dda28e1948e31f40e7693fa8483d7a9b1b7aff069fa8a1b7193cd1ab" "35335d6369e911dac9d3ec12501fd64bc4458376b64f0047169d33f9d2988201" "bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" "896e4305e7c10f3217c5c0a0ef9d99240c3342414ec5bfca4ec4bff27abe2d2d" default))
 '(exwm-floating-border-color "#9f9f9f")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark ef-themes-mark-delete))
 '(flymake-note-bitmap '(exclamation-mark ef-themes-mark-select))
 '(flymake-warning-bitmap '(exclamation-mark ef-themes-mark-other))
 '(highlight-changes-colors nil)
 '(highlight-changes-face-list '(success warning error bold bold-italic))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#7a4f2f")
     ("TODO" . "#884900")
     ("NEXT" . "#193668")
     ("THEM" . "#193668")
     ("PROG" . "#0031a9")
     ("OKAY" . "#0031a9")
     ("DONT" . "#7a4f2f")
     ("FAIL" . "#884900")
     ("BUG" . "#884900")
     ("DONE" . "#0031a9")
     ("NOTE" . "#7a4f2f")
     ("KLUDGE" . "#7a4f2f")
     ("HACK" . "#7a4f2f")
     ("TEMP" . "#7a4f2f")
     ("FIXME" . "#884900")
     ("XXX+" . "#884900")
     ("REVIEW" . "#0031a9")
     ("DEPRECATED" . "#0031a9")))
 '(ibuffer-deletion-face 'ef-themes-mark-delete)
 '(ibuffer-filter-group-name-face 'bold)
 '(ibuffer-marked-face 'ef-themes-mark-select)
 '(ibuffer-title-face 'default)
 '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(htmlize highlight-indentation indent-tools yasnippet yapfify use-package treemacs projectile preview-dvisvgm pdf-tools pandoc org-mime org-bullets ob-async notmuch modus-themes magit jupyter ivy-rich format-all evil-visual-mark-mode evil-collection ein ef-themes dashboard counsel chatgpt-shell centered-window blacken base16-theme all-the-icons))
 '(pdf-view-midnight-colors '("#000000" . "#f0f0f0"))
 '(rcirc-colors
   '(modus-themes-fg-red modus-themes-fg-green modus-themes-fg-blue modus-themes-fg-yellow modus-themes-fg-magenta modus-themes-fg-cyan modus-themes-fg-red-warmer modus-themes-fg-green-warmer modus-themes-fg-blue-warmer modus-themes-fg-yellow-warmer modus-themes-fg-magenta-warmer modus-themes-fg-cyan-warmer modus-themes-fg-red-cooler modus-themes-fg-green-cooler modus-themes-fg-blue-cooler modus-themes-fg-yellow-cooler modus-themes-fg-magenta-cooler modus-themes-fg-cyan-cooler modus-themes-fg-red-faint modus-themes-fg-green-faint modus-themes-fg-blue-faint modus-themes-fg-yellow-faint modus-themes-fg-magenta-faint modus-themes-fg-cyan-faint modus-themes-fg-red-intense modus-themes-fg-green-intense modus-themes-fg-blue-intense modus-themes-fg-yellow-intense modus-themes-fg-magenta-intense modus-themes-fg-cyan-intense)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

