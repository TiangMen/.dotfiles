(setq gc-cons-threshold (* 100 1024 1024)) ;; 100 MB
(setq read-process-output-max (* 1 1024 1024)) ;; 1 MB

;; The default is 800 kilobytes.  Measured in bytes.
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(set-fringe-mode 10)        ; Give some breathing room

(recentf-mode 1) ;; remembers recently edited files

;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 120)

(defvar bootstrap-version)
    (let ((bootstrap-file
            (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
            (bootstrap-version 5))
        (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
                "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq use-package-always-ensure nil)
;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(use-package guix-emacs
   :straight nil)

(use-package general
  :straight nil
  :config
  (general-evil-setup t)

  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(rune/leader-keys
  "cc" 'compile
  "." 'find-file
  "," 'ido-switch-buffer
  "oa" 'org-agenda
  "oe" 'eshell
  "ov" 'vterm
  "hrr" '((lambda () (interactive) (load-file "~/.config/emacs/init.el"))
          :which-key "Reload Emacs config")
  "fr" '(recentf-open-files :which-key "Recent files")
  "fp" '((lambda () (interactive) (find-file (expand-file-name "~/.config/emacs/config.org")))
         :which-key "edit config")
  "fP" '(find-file "~/.config/emacs" :which-key "edit config") ; doom emacs config dir
  "<" 'list-buffers) 

(rune/leader-keys
  "G"  '(:ignore t :which-key "Guix")
  "Gg" '(guix :which-key "Guix")
  "Gi" '(guix-installed-user-packages :which-key "user packages")
  "GI" '(guix-installed-system-packages :which-key "system packages")
  "Gp" '(guix-packages-by-name :which-key "search packages")
  "GP" '(guix-pull :which-key "pull"))

(use-package vertico
    :straight nil
    :bind (:map vertico-map
           ("C-j" . vertico-next)
           ("C-k" . vertico-previous)
           ("C-f" . vertico-exit)
           :map minibuffer-local-map
           ("C-w" . backward-kill-word))
    :custom
    (vertico-cycle t)
    :init
    (vertico-mode))

  (use-package savehist
    :init
    (savehist-mode))

  (use-package marginalia
    :after vertico
    :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package company
    :straight t
    :after lsp-mode
    :hook (lsp-mode . company-mode)
    :bind (:map company-active-map
                ("<tab>" . company-complete-selection))
    (:map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0))

  (use-package company-box
    :straight t
    :hook (company-mode . company-box-mode))

  (use-package company-prescient
    :straight t
    :after company
    :config
    (company-prescient-mode 1)
    (prescient-persist-mode))

  (add-hook 'after-init-hook 'global-company-mode)

 (setq ispell-program-name "hunspell")
  ;; you could set `ispell-dictionary` instead but `ispell-local-dictionary' has higher priority
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US,en_US-med") nil utf-8)))
  ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
  ;; If it's nil, Emacs tries to automatically set up the dictionaries.
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))  

  (defun my-text-mode-hook ()
  (setq-local company-backends
              '((company-dabbrev company-ispell :separate)
                company-files)))

(add-hook 'text-mode-hook #'my-text-mode-hook)

(use-package all-the-icons
  :straight nil)

(use-package doom-modeline
  :straight nil
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Or if you use use-package
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-startup-banner "~/.config/screenshots/example.png")

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                vterm-mode-hook
                twittering-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package winner
  :after evil
  :config
  (winner-mode))

(rune/leader-keys
        "w"  '(:ignore t :which-key "evil window")
        "ws" 'evil-window-split
        "wv" 'evil-window-vsplit
        "ww" 'evil-window-next
        "wo" 'delete-other-windows
        "wq" 'evil-quit
        "wu" 'winner-undo ;; pop in and out of window history
        "wU" 'winner-redo ;; pop in and out of window history
)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
  ;;(set-frame-parameter (selected-frame) 'alpha <both>)
  (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
  (add-to-list 'default-frame-alist '(alpha . (85 . 50)))
 ;; Set frame transparency

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))

    (rune/leader-keys
        "ct" 'toggle-transparency)

(use-package undo-tree
  :straight nil
  :init
  (global-undo-tree-mode 1))

(use-package evil
  :straight nil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight nil
  :after evil
  :config
  (evil-collection-init))

(use-package evil-tutor
  :straight t)

(use-package key-chord
  :straight nil
  :config 
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package evil-multiedit
  :straight nil
  :config 
  (evil-multiedit-default-keybinds))

(use-package evil-org
  :straight t
  :after org
  :hook ((org-mode . evil-org-mode)

         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package doom-snippets
  :straight nil
  :after yasnippet)

(use-package flymake-shellcheck
  :straight nil
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas/expand-snippet (buffer-string) (point-min) (point-max)))

(custom-set-variables
 '(auto-insert 'other)
 '(auto-insert-directory "~/Templates/")
 '(auto-insert-alist '((("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand])
                       (("\\.el\\'" . "Emacs Lisp") . ["template.el" my/autoinsert-yas-expand])
                       (("\\.py\\'" . "Python script") . ["template.py" my/autoinsert-yas-expand])
                       (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
                       )))

(use-package flymake-shellcheck
  :straight t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package pyvenv
  :straight nil
  :init
  (setenv "WORKON_HOME" "~/.venvs/")
  :config
  ;; (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(setq python-shell-interpreter "python3")
(use-package blacken
  :straight t
  :init
  (setq-default blacken-fast-unsafe t)
  (setq-default blacken-line-length 80)
  )
(use-package python-mode
  :straight t
  :hook
  (python-mode . pyvenv-mode)
  (python-mode . flycheck-mode)
  (python-mode . flymake-mode)
  (python-mode . company-mode)
  (python-mode . blacken-mode)
  (python-mode . yas-minor-mode)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  :config
  )

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package ccls
  :straight t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  (c-mode . flycheck-mode)
  (c-mode . flymake-mode)
  (c-mode . yas-minor-mode)
  (c-mode . company-mode)
  (c++-mode . flycheck-mode)
  (c++-mode . yas-minor-mode)
  (c++-mode . company-mode))

(use-package clang-format
  :straight t
  :config
  (setq clang-format-style-option "google"))

(global-set-key [C-M-tab] 'clang-format-region)

(use-package go-mode
  :straight t
  :hook
  (go-mode . lsp-deferred)
  (go-mode . flycheck-mode)
  (go-mode . company-mode))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

(use-package yasnippet-snippets
  :straight t)
(use-package yasnippet
  :straight nil
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (yas-global-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package flycheck
  :straight nil
  :diminish flycheck-mode
  :init
  (setq flycheck-check-syntax-automatically '(save new-line)
        flycheck-idle-change-delay 5.0
        flycheck-display-errors-delay 0.9
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode 'left-fringe
        flycheck-standard-error-navigation t
        flycheck-deferred-syntax-check nil)
  )

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (dot . t)
   (latex . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'org-babel-tangle
                               :append :local)))

;; Configure the Modus Themes' appearance
(setq modus-themes-mode-line '(accented borderless)
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-completions 'opinionated
      modus-themes-org-blocks 'tinted-background
      modus-themes-scale-headings t
      modus-themes-region '(bg-only)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
        (2 . (rainbow background 1.3))
        (3 . (rainbow bold 1.2))
        (t . (semilight 1.1))))

;; Load the dark theme by default
(load-theme 'modus-vivendi t)


(set-face-attribute 'default nil :font "JetBrains Mono" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height efs/default-variable-font-size :weight 'regular)

;; Make sure org-indent face is available
(require 'org-indent)

(use-package org-superstar
  :straight nil
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.3)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :hook (org-mode . efs/org-mode-visual-fill))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org :straight (:type built-in)
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  (org-mode . flyspell-mode)
  :config

  (setq org-directory "~/Projects/Code/OrgFiles")
  (setq org-agenda-files '("Tasks.org" "Birthdays.org"))


  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  ;; Configure custom agenda views
  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))
  (setq org-ellipsis " ▾")

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Projects/Code/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Projects/Code/OrgFiles/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

                                        ;     (efs/org-font-setup)
  )

(use-package org-roam
  :straight nil
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(rune/leader-keys
  "nc"  '(:ignore t :which-key "Org Roam")
  "ncl"  'org-roam-buffer-toggle
  "ncf" 'org-roam-node-find
  "nci" 'org-roam-node-insert)

(use-package org-make-toc
  :straight t
  :hook (org-mode . org-make-toc-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))

(use-package org-pomodoro
  :straight t
  :after org
  :config
  (setq org-pomodoro-start-sound "~/.dotfiles/.config/emacs/sounds/focus_bell.wav")
  (setq org-pomodoro-short-break-sound "~/.dotfiles/.config/emacs/sounds/three_beeps.wav")
  (setq org-pomodoro-long-break-sound "~/.dotfiles/.config/emacs/sounds/three_beeps.wav")
  (setq org-pomodoro-finished-sound "~/.dotfiles/.config/emacs/sounds/meditation_bell.wav")

  (rune/leader-keys
    "op"  '(org-pomodoro :which-key "pomodoro")))

(use-package olivetti
  :straight t
  :diminish
  :hook (text-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 100))

(use-package outshine
  :straight nil
  :config
  (setq LaTeX-section-list '(
                             ("part" 0)
                             ("chapter" 1)
                             ("section" 2)
                             ("subsection" 3)
                             ("subsubsection" 4)
                             ("paragraph" 5)
                             ("subparagraph" 6)
                             ("begin" 7)
                             )
        )
  (add-hook 'LaTeX-mode-hook #'(lambda ()
                                 (outshine-mode 1)
                                 (setq outline-level #'LaTeX-outline-level)
                                 (setq outline-regexp (LaTeX-outline-regexp t))
                                 (setq outline-heading-alist
                                       (mapcar (lambda (x)
                                                 (cons (concat "\\" (nth 0 x)) (nth 1 x)))
                                               LaTeX-section-list))))

  )

(general-define-key
 :states '(normal visual)
 :keymaps 'LaTeX-mode-map
 "TAB"  '(outshine-cycle :which-key "outshine-cycle")
 )

;; latexmk
(straight-use-package
 '(auctex-latexmk :type git :host github :repo "tom-tan/auctex-latexmk"))
;; company
(use-package company-math :straight t)
(use-package company-auctex :straight t)
(use-package company-reftex :straight t)


;;  use cdlatex
(use-package cdlatex :straight t)

;; https://gist.github.com/saevarb/367d3266b3f302ecc896
;; https://piotr.is/2010/emacs-as-the-ultimate-latex-editor/

(use-package auctex
  :defer t
  :custom
  (olivetti-body-width 100)
  (cdlatex-simplify-sub-super-scripts nil)
  :bind (:map LaTeX-mode-map
              ("C-c C-e" . cdlatex-environment)
              )
  :hook
  (LaTeX-mode . olivetti-mode)
  (LaTeX-mode . TeX-PDF-mode)
  (LaTeX-mode . company-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . flycheck-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . try/latex-mode-setup)
  (LaTeX-mode . turn-on-cdlatex)

  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-save-query nil)

  (setq reftex-plug-into-AUCTeX t)

  ;; pdftools
  ;; https://emacs.stackexchange.com/questions/21755/use-pdfview-as-default-auctex-pdf-viewer#21764
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
  ;; to have the buffer refresh after compilation,
  ;; very important so that PDFView refesh itself after comilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; latexmk
  (require 'auctex-latexmk)
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  )

(use-package ledger-mode
  :straight nil)

(use-package haskell-mode)

(use-package dired
  :straight nil)

(use-package all-the-icons-dired
  :straight nil
  :hook (dired-mode . all-the-icons-dired-mode))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;(use-package dired-hide-dotfiles
;  :hook (dired-mode . dired-hide-dotfiles-mode)
;  :config
;  (evil-collection-define-key 'normal 'dired-mode-map
;    "H" 'dired-hide-dotfiles-mode))

;; NOTE: If you want to move everything out of the ~/.config/emacs folder
;; reliably, set `user-emacs-directory` before loading no-littering!
					;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering
  :straight nil )

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(server-start)

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell))

(use-package eshell-git-prompt
  :straight t
  :config
  (eshell-git-prompt-use-theme 'robbyrussell))

(use-package vterm
  :straight nil
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package rustic
  :straight t)

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :straight t
  :after projectile
  :config
  (counsel-projectile-mode 1))

(use-package magit
  :straight nil
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook 
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-diagnostics-provider :capf)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(project file symbols))
  (lsp-lens-enable nil)
  (lsp-disabled-clients '((python-mode . pyls)))
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  :config
  )
(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :after lsp-mode
  :custom
  (lsp-ui-doc-show-with-cursor nil)
  :config
  (setq lsp-ui-doc-position 'bottom)
  )


(general-define-key
 :states '(normal visual)
 :keymaps 'lsp-mode-map
 :prefix "SPC"
  "d" '(lsp-find-definition :which-key "find-definitions")
  "r" '(lsp-find-references :which-key "find-references")
  "h" '(lsp-describe-thing-at-point :which-key "help-detailed")
  "e" '(lsp-ui-flycheck-list :which-key "flycheck-list")
  "o" 'counsel-imenu
  "x" 'lsp-execute-code-action)



(use-package pdf-tools
  :straight nil
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(use-package epub
  :load-path "~/.guix-profile/share/emacs/site-lisp/nov-el-0.3.4"
  :straight nil)

(use-package simple-httpd
  :straight t)

(use-package htmlize
  :straight t)

(use-package password-store
  :straight t
  :config
  (setq password-store-password-length 12))

(use-package auth-source-pass
  :straight t
  :config
  (auth-source-pass-enable))

(rune/leader-keys
  "ap" '(:ignore t :which-key "pass")
  "app" 'password-store-copy
  "api" 'password-store-insert)

(use-package elfeed-tube
  :straight (:host github :repo "karthink/elfeed-tube")
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
  ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)
              :map elfeed-search-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :straight (:host github :repo "karthink/elfeed-tube")
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))

(use-package elfeed
  :straight nil
  :commands elfeed
  :config
  (setq elfeed-feeds
        '("https://nullprogram.com/feed/"
          "https://ambrevar.xyz/atom.xml"
          "https://guix.gnu.org/feeds/blog.atom"
          "https://xkcd.com/atom.xml"
          "https://valdyas.org/fading/feed/"
          "https://www.reddit.com/r/emacs/.rss")))

(use-package shrface
:straight t
:config
(shrface-basic)
(shrface-trial)
(shrface-default-keybindings) ; setup default keybindings
(setq shrface-href-versatile t))
(use-package twittering-mode
  :straight t
  :custom
  (setq twittering-use-master-password t)
  (setq twittering-allow-insecure-server-cert t)
  :config
  (require 'shrface)
  )

(use-package elcord
  :straight t
  :custom
  (elcord-display-buffer-details nil)
  :config
  (elcord-mode))

(use-package leetcode
  :straight t
  :config
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/leetcode")
  (setq leetcode-prefer-language "python3")
  )

(use-package ctable
  :straight nil)

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(use-package writegood-mode
  :config 
  (global-set-key "\C-cg" 'writegood-mode))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
