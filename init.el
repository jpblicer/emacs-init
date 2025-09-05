;; Performance
(setq gc-cons-threshold (* 50 1000 1000))

(defun start/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'start/org-babel-tangle-config)))

;; Package Managers
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
			 ))

;; Better Defaults
(use-package emacs
  :bind
  (("M-o" . other-window)
   ("S-<up>" . windmove-up)
   ("S-<down>" . windmove-down)
   ("S-<left>" . windmove-left)
   ("S-<right>" . windmove-right)
   ("C-." . duplicate-dwim)
   ("C-x C-b" . ibuffer))
  :custom
  (menu-bar-mode nil)
  (ring-bell-function 'ignore)
  (inhibit-startup-message t)
  (initial-scratch-message nil)
  (scroll-bar-mode nil)
  (tool-bar-mode nil)  
  (delete-selection-mode t)
  (electric-indent-mode t)
  (electric-pair-mode t)    
  (blink-cursor-mode nil)   
  (global-auto-revert-mode t)
  (global-auto-revert-non-file-buffers t)
  (delete-by-moving-to-trash t)
  (winner-mode t)
  (x-select-enable-clipboard t)
  (mouse-wheel-progressive-speed nil)
  (scroll-conservatively 10)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (scroll-margin 8)
  (make-backup-files nil)
  (auto-save-default nil)
  (column-number-mode)
  (text-mode-ispell-word-completion nil)
  :hook
  (prog-mode . (lambda () (hs-minor-mode t)))
  :config
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage))


;; Modeline
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-format "%Y年%m月%d%a %H:%M"
      display-time-default-load-average nil)

(display-time-mode t)
(display-battery-mode t)

(setq-default mode-line-format
	      `("%e" mode-line-front-space
	       (:propertize
		(
		 ""
		 mode-line-mule-info
		 mode-line-client
		 mode-line-modified
		 mode-line-remote
		 mode-line-window-dedicated
		 )
		display (min-width (6.0)))
	       mode-line-frame-identification
	       mode-line-buffer-identification
	       ;;"   "
	       ;;mode-line-position (project-mode-line project-mode-line-format)
	       (vc-mode vc-mode)
	       ;;"  "
	       ;;mode-line-modes
	       mode-line-format-right-align
	       mode-line-misc-info
	       "")
	      )

;; Modus Operandi Theme
(use-package modus-themes
  :config
  (load-theme 'modus-operandi t))

;; Font and Font-Size
(set-face-attribute 'default nil 
		    :family "Iosevka Comfy"
		    :height 130
		    :weight 'regular)

(set-fontset-font t 'japanese-jisx0208 "Noto Sans CJK JP-13")

;; Line numbers and autoclose parentheses when programming
(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode)
                            (electric-pair-mode)))

;; Zoom in / out
(use-package emacs
  :bind
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("<C-wheel-up>" . text-scale-increase)
  ("<C-wheel-down>" . text-scale-decrease))

;; proced
(use-package proced
  :ensure nil
  :defer t
  :custom
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  (proced-auto-update-flag 'visible)
  (proced-auto-update-interval 1)
  (proced-descent t)
  (proced-filter 'user)
  :config
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

;; eww
(use-package url-cookie
  :ensure nil
  :commands (url-cookie-list)
  :bind
  ("C-c b" . eww)
  :config
  (setq url-cookie-untrusted-urls '(".*")))

(defun eww-reddit-redirect(url)
  "Redirect reddit.com to old.reddit.com automatically."
  (replace-regexp-in-string "https://www.reddit.com" "https://old.reddit.com" url))

(setq eww-url-transformers '(eww-remove-tracking eww-reddit-redirect))

;; Start Syncthing process
(defun start-syncthing ()
  "Start the Syncthing process."
  (interactive)
  (start-process "syncthing" "*syncthing*" "syncthing"))

(global-set-key (kbd "C-c s") 'start-syncthing)

;; Project.el
(use-package project
  :ensure nil)

(setq project--default-search-method 'git)

;; mise
(use-package mise
  :hook (after-init . global-mise-mode))

;; tramp
(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Use exec-path-from-shell to set up PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; vTerm
(use-package vterm
  :ensure t
  :bind
  (("C-c t" . (lambda () (interactive) (vterm 'new))))
    :config
  (setq vterm-timer-delay 0.01))

;; eshell
(use-package eshell
  :ensure nil
  :bind (("C-c e" . eshell)))

;; Go Translate
(use-package gt
  :ensure t
  :bind
  (([muhenkan] . gt-translate)
   ([henkan] . gt-translate)
   ("C-c w" . gt-translate))
  :config
  (setq gt-langs '(en ja))
  (setq gt-lang-rules
        '((ja . "[\u3040-\u309F\u30A0-\u30FF\u3400-\u4DBF\u4E00-\u9FFF]"))) ;; better kanji recognition
  (setq gt-default-translator
	(gt-translator
	 :taker (gt-taker :langs '(en ja) :text 'word)
	 :engines (gt-google-engine)
	 :render (gt-buffer-render))))

;; Dired
(use-package dired
  :ensure nil
  :defer t
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-make-directory-clickable t)
  (setq dired-mouse-drag-files t)
  (setq dired-dwim-target t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-listing-switches "-ahl --group-directories-first"))
(setopt dired-mouse-drag-files                   t
        mouse-drag-and-drop-region-cross-program t)
(put 'dired-find-alternate-file 'disabled nil)

;; eglot configuration
(use-package eglot
  :ensure nil
  :hook ((c-ts-mode c++-ts-mode lua-ts-mode ruby-ts-mode go-ts-mode
                    js-ts-mode typescript-ts-mode python-ts-mode) . eglot-ensure)
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  (eglot-report-progress nil)
  :config
  (add-to-list 'eglot-server-programs
               '(ruby-ts-mode . ("solargraph" "stdio"))
               '(typescript-ts-mode . ("typescript-language-server" "--stdio"))))

;; tree-sit auto
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Docker
(use-package docker
  :ensure t
  :bind ("C-c d" . docker)
  :config	(setq docker-use-sudo t))

;; Verb
(use-package verb
  :ensure t
  :config
  (define-key org-mode-map (kbd "C-c r") verb-command-map))

;; impostman
(use-package impostman
  :ensure t)

;; Rails / Ruby
(use-package ruby-ts-mode
  :mode ("\\.rb\\'"      
         "\\.rake\\'"    
         "\\.gemspec\\'" 
         "Gemfile\\'"    
         "Capfile\\'"))

;; Geiser
(use-package geiser-mit :ensure t)

;; Python Virtual Enviornment Support
(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Org-mode
(use-package org
  :ensure nil
  :custom
  (org-edit-src-content-indentation 2)
  (org-startup-folded t) 
  :hook
  (org-mode . org-indent-mode)
  (org-mode . toggle-truncate-lines)
  (org-mode . word-wrap-whitespace-mode)
  (org-log-done . 'time)
  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  :config
  (setq org-capture-templates
	'(
	  ("c" "Add a Contact"
	   entry (file "~/Documents/Org/20250112173941-contacts.org")
	   "* %^{First} %^{Last} :%^{Select a Tag |career|friend|family}: %?
:PROPERTIES:
:First: %\\1
:Last: %\\2
:DOB: [%^{Date of Birth YYYY-MM-DD}]
:Phone: %^{Phone}
:Email: %^{Email}
:LinkedIn: [[%^{LinkedIn Page}]]
:Country: %^{Country}
:Address: %^{Address}
:Company:
:TITLE: %^{Title}
:END:
** (tel:%\\5)
** (mailto:%\\6)
** DOB: [%\\4]
*** Reminder <%\\4 .+1y}>
** Notes")
	  ("C" "Add a Company"
	   entry (file"~/Documents/Org/20250112182718-companies.org")
	   "* %^{Company} :%^{Select a Tag |career|service|info}: %?
:PROPERTIES:
:Company: %\\1
:Website: [[%^{Website}]]
:END:
** Notes")
	  ("t" "Create a Todo"
	   entry (file "~/Documents/Org/20250715185317-agenda.org")
	   "* TODO [#%^{Priority|B|A|C|D}] %^{Task}"
	   :prepend t)
	  ("a" "Create an Appointment"
	   entry (file "~/Documents/Org/20250715185317-agenda.org")
	   "* %^{Appointment}\nSCHEDULED: %^t\n"
	   :prepend t)
	  )))

(use-package toc-org
  :commands toc-org-enable
  :hook (org-mode . toc-org-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;; Org-roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/Org")
  :bind(("C-c n l" . org-roam-buffer-toggle)
	("C-c n f" . org-roam-node-find)
	("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; Devdocs
(use-package devdocs
  :ensure t
  :bind
  (("C-h D" . (lambda () (interactive) (devdocs-lookup 'new)))))

;; Nerd Icons
(use-package nerd-icons
  :if (display-graphic-p))

(use-package nerd-icons-dired
  :hook (dired-mode . (lambda () (nerd-icons-dired-mode t))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Magit
(use-package magit
  :commands magit-status)

(use-package diff-hl
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init (global-diff-hl-mode))

;; Corfu
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum length of prefix for auto completion.
  (corfu-popupinfo-mode t)       ;; Enable popup information
  (corfu-popupinfo-delay 0.5)    ;; Lower popupinfo delay to 0.5 seconds from 2 seconds
  (corfu-separator ?\s)          ;; Orderless field separator, Use M-SPC to enter separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (completion-ignore-case t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (corfu-preview-current nil) ;; Don't insert completion without confirmation
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Vertico
(use-package vertico
  :init
  (vertico-mode))

(savehist-mode)

;; Marginalia
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode-hook . 'nerd-icons-completion-marginalia-setup))

;;Consult
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;; consult-theme :preview-key '(:debounce 0.2 any)
  ;; consult-ripgrep consult-git-grep consult-grep
  ;; consult-bookmark consult-recent-file consult-xref
  ;; consult--source-bookmark consult--source-file-register
  ;; consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  ;; :preview-key '(:debounce 0.4 any))

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
   ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
   ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
   ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
   ;;;; 4. projectile.el (projectile-project-root)
  ;;(autoload 'projectile-project-root "projectile")
  ;;(setq consult-project-function (lambda (_) (projectile-project-root)))
   ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

;;Which Key
(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-min-display-lines 6)
  (which-key-idle-delay 0.8)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil))

;; Ledger
(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger\\'" . ledger-mode)
  :config
  (setq ledger-clear-whole-transactions 1)
  (setq ledger-reconcile-default-commodity
	"￥"))

;; Org Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
     (ledger . t)
     (ruby . t)
     (screen . nil)
     (shell . t)
     (sql . nil)
     (sqlite . t)))

;; Runtime Performance
(setq gc-cons-threshold (* 2 1000 1000))
(setq read-process-output-max (* 1024 1024))
