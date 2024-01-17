(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(ef-arbutus))
 '(custom-safe-themes
   '("91d9dd2bc02a71a018738c815ba64be0ac0ec12815324fb28733c95d414f3f00" "e8915dac580af7a4a57bd38a0638a5a7714b47714529b1a66bd19e7aa155818d" "95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692" "01aef17f41edea53c665cb57320bd80393761f836be5ab0bd53292afc94bd14d" "b0b29575dda28e1948e31f40e7693fa8483d7a9b1b7aff069fa8a1b7193cd1ab" "7ca04d620046f5807d0740f265844d45e53b864138c246f48f663bea8fba5c5d" "ae7aede615d12f126a7b6b0d8c1e0af2af1ab63fb21d80a130a5f701fdbb23fe" "df42062cdd672acecac9b5a1229c45f74c0cc2bc0362f9ad41054af6ac355021" "7a259016d8fe61ef450895a01d842c4353dabc873ee46e250c933995f7f004cf" "a6a979c8b7ccb1d4536f4fa74a6e47674a3ce65feea3fecdf1d9dc448fac47e0" "7dc1c6210efe106a8c6cd47009a2ffd0069826b550dda379e8e4ef6105384cba" "704f75d46620d87bb246e2ec1abb129437764b0e84ac0fff6b968311cc046918" "c73ce0e82bfbf5f186756001eeaaf26f0b461851a8b421d1ff5ec35e10f300ed" "4c326abbf8b85c85e114691d3892cbbfe889b2b064dadd284cf5eccca3eecbff" "205bb9accecaf0ae5e8cb5f09773be4f1175aca71322ba4fd44b539cd48463b6" "28d5b595e4724ad44b52eb1ab9bd6fbf1cca747109e2dc5ff4073a07d1b50946" "67a0f6cdf628610987cf7f0d38f0ae68ed3a5ab6816ff3da5f66d7b09d03f470" "91fed8cade0d85d1f5a5621e72ac6e720945be79493465a79f020e673f7e2d24" "65af8e8d704bcd9745a4f191db756995de6b1fdd15cf2eb41befaae75f7b045d" "c49f79bfea991ed384cc0dc33328c8d812c413bf27cff0bc24ad58af2cdbccb4" "65809263a533c5151d522570b419f1a653bfd8fb97e85166cf4278e38c39e00e" "4de156ea6aa06640e3d16da41dfbdee85aea2fb969f620e601a982c6e237b396" "e546768f3da4b394adc8c460106a7d220af130a3a2a0518d265c832d015a4385" "38cb7d03fec58c860d5e92ee756aa80bdb2d78ef7ac726317e78c5c844e04f3a" "a73cd17e5b490a5558a0ca13186cccb85ff74b2160f77e65ff36024bc66089b4" "ca934a76aae4ff950288e082be75a68eb7bac6e8d3dd58b28649993540412ed6" "7d10494665024176a90895ff7836a8e810d9549a9872c17db8871900add93d5c" "46a843168cc83b28b740735516e6eea4f97769d848c79b5acab32f7a278f793a" "8ea6a46120abb34bf6a664b76d78014e0dd1f2b740a0976ec41313612421712f" "5f92b9fc442528b6f106eaefa18bb5e7bfa0d737164e18f1214410fef2a6678d" "7819a530936ad2204a35340498da6341f66d816a92b8c71af138f73814b269ec" "5e05db868f138062a3aedcccefe623eee18ec703ae25d4e5aebd65f892ac5bcc" "73c55f5fd22b6fd44f1979b6374ca7cc0a1614ee8ca5d4f1366a0f67da255627" default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(vs-light-theme vs-dark-theme monokai-theme ivy-rich magit evil-collection evil-leader counsel ef-themes use-package)))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-2.4.5/"
  (require 'use-package)))

(setq inhibit-startup-message t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(tool-bar-mode -1)
(set-window-scroll-bars (minibuffer-window) nil nil)

(setq cursor-type nil)  ; or nil for a box cursor, or bar for a vertical bar cursor
(blink-cursor-mode 1) ; or 0 to disable cursor blinking

(set-face-attribute 'default nil :family "Consolas" :height 110)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

