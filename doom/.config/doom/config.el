;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Josh Blais"
      user-mail-address "josh@joshblais.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
(setq doom-font (font-spec :family "Source Code Pro" :size 15))
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(add-to-list 'custom-theme-load-path "~/.config/doom/themes/")
(load-theme 'doom-nord t)

;; Maintain terminal transparency in Doom Emacs
(after! doom-themes
  (unless (display-graphic-p)
    (set-face-background 'default "undefined")))

;; remove top frame bar in emacs
(add-to-list 'default-frame-alist '(undecorated . t))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Vaults/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Line wrap
(global-visual-line-mode t)

;; Performance optimizations
(setq gc-cons-threshold (* 256 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq comp-deferred-compilation t)
(setq comp-async-jobs-number 8)

;; Garbage collector optimization
(setq gcmh-idle-delay 5)
(setq gcmh-high-cons-threshold (* 1024 1024 1024))

;; Version control optimization
(setq vc-handled-backends '(Git))

;; trash files before deleting
(setq delete-by-moving-to-trash t)

;; Setup custom splashscreen
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(setq fancy-splash-image "~/Pictures/cath.jpg")
(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Welcome Home.")))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
                                        ;(require 'org-mime)

;; Evil-escape sequence
(setq-default evil-escape-key-sequence "kj")
(setq-default evil-escape-delay 0.1)

;; Vterm adjustemts
(setq vterm-environment '("TERM=xterm-256color"))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(custom-set-faces!
  '(vterm :family "Source Code Pro"))

;; open vterm in dired location
(after! vterm
  (setq vterm-buffer-name-string "vterm %s")
  (defadvice! +vterm/buffer-name-from-dired (fn &rest args)
    :around #'vterm
    (let ((default-directory (if (eq major-mode 'dired-mode)
                                 (dired-current-directory)
                               default-directory)))
      (apply fn args))))

;; Enable paste from system clipboard with C-v in insert mode
(evil-define-key 'insert global-map (kbd "C-v") 'clipboard-yank)

;; set specific browser to open links
;;(setq browse-url-browser-function 'browse-url-firefox)
;; set browser to zen-browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "zen-browser")  ; replace with actual executable name


;; Emmet remap
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(map! :map emmet-mode-keymap
      :n "<C-return>" #'emmet-expand-line)
(setq emmet-expand-jsx-className? t) ;; default nil
;;---------
(use-package org
  :ensure nil
  :custom (org-modules '(org-habit)))
;;---------
(after! org
  (map! :map org-mode-map
        :n "<M-left>" #'org-do-promote
        :n "<M-right>" #'org-do-demote)
  )

;; Auto-clock in when state changes to STRT
(defun my/org-clock-in-if-starting ()
  "Clock in when the task state changes to STRT"
  (when (and (string= org-state "STRT")
             (not (org-clock-is-active)))
    (org-clock-in)))

;; Auto-clock out when leaving STRT state
(defun my/org-clock-out-if-not-starting ()
  "Clock out when leaving STRT state"
  (when (and (org-clock-is-active)
             (not (string= org-state "STRT")))
    (org-clock-out)))

;; Add these functions to org-after-todo-state-change-hook
(add-hook 'org-after-todo-state-change-hook 'my/org-clock-in-if-starting)
(add-hook 'org-after-todo-state-change-hook 'my/org-clock-out-if-not-starting)

;; Show habits in agenda
(setq org-habit-show-all-today t)
(setq org-habit-graph-column 1)
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (setq truncate-lines 1)))

(after! org
  (use-package! org-fancy-priorities
    :hook
    (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))))

;; LSP Performance optimizations and settings
(after! lsp-mode
  (setq lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-completion-provider :capf
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil
        lsp-enable-snippet nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-links nil

        ;; Go-specific settings
        lsp-go-hover-kind "Synopsis"
        lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (unusedwrite . t)
                          (unusedparams . t))

        ;; Register custom gopls settings
        lsp-gopls-completeUnimported t
        lsp-gopls-staticcheck t
        lsp-gopls-analyses '((unusedparams . t)
                             (unusedwrite . t))))

;; LSP UI settings for better performance
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 0.5
        lsp-ui-sideline-enable nil
        lsp-ui-peek-enable t))

;; Company mode tweaks
(after! company
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-show-quick-access t
        company-tooltip-limit 20
        company-tooltip-align-annotations t)
  ;; Add file path completion
  (add-to-list 'company-backends 'company-files)
  (setq company-files-exclusions nil)
  (setq company-files-chop-trailing-slash t))

;; Tailwind CSS
(use-package! lsp-tailwindcss)

;; ;; Setup Minimap
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map) ;; experimental
;; (require 'sublimity-attractive)
;; ;; Minimap settings
;; (setq minimap-window-location 'right)
;; (map! :leader
;;       (:prefix ("t" . "toggle")
;;        :desc "Toggle minimap-mode" "m" #'minimap-mode))

;; Treemacs
(require 'treemacs-all-the-icons)
(setq doom-themes-treemacs-theme "all-the-icons")

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(96 . 97))
(add-to-list 'default-frame-alist '(alpha . (96 . 97)))

;; Aggresssive Indent
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)

;; Blink cursor
(blink-cursor-mode 1)

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (package-initialize)

;; (setq package-selected-packages
;;       '(dart-mode lsp-mode lsp-dart lsp-treemacs flycheck company
;;         ;; Optional packages
;;         lsp-ui company hover))

;; (when (cl-find-if-not #'package-installed-p package-selected-packages)
;;   (package-refresh-contents)
;;   (mapc #'package-install package-selected-packages))

;; Org-auto-tangle
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

;; ;; Trying to get pomodoro timer working
;; (require 'org)
;; (setq org-clock-sound "~/Downloads/Gong.wav")

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Spelling
(setq ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
(setq spell-fu-directory "~/+STORE/dictionary") ;; Please create this directory manually.
(setq ispell-personal-dictionary "~/+STORE/dictionary/.pws")

;; Dictionary
(setq +lookup-dictionary-provider 'define-word)

;;Snippets
(yas-global-mode 1)
(add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))

(after! projectile
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid))

;; Path completion
(projectile-add-known-project "~/Vaults/Writing")
(projectile-add-known-project "~/Vaults")
(projectile-add-known-project "~/go/src/github.com/jblais493/HTMXFrontend")
(projectile-add-known-project "~/go/src/github.com/jblais493/Citadel")
(projectile-add-known-project "~/Development/svelte-email")

;; Add to your config.el
(setq completing-read-function #'completing-read-default)
(setq read-file-name-function #'read-file-name-default)

;; Makes path completion more like find-file everywhere
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Use the familiar C-x C-f interface for directory completion
(map! :map minibuffer-mode-map
      :when (featurep! :completion vertico)
      "C-x C-f" #'find-file)

(after! vertico
  ;; Add file preview
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word))

;; Org Agenda
;; Set days viewed to 3, set start day to today, create seperator, and Dashboard view
(setq org-agenda-remove-tags t)
(setq org-agenda-block-separator 32)
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         (
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "\n HIGHEST PRIORITY")
                 (org-agenda-prefix-format "   %i %?-2 t%s")
                 )
                )
          (agenda ""
                  (
                   (org-agenda-start-day "+0d")
                   (org-agenda-span 1)
                   (org-agenda-time)
                   (org-agenda-remove-tags t)
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-scheduled-leaders '("" ""))
                   (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈┈┈ NOW")
                   (org-agenda-overriding-header "\n TODAY'S SCHEDULE")
                   (org-agenda-prefix-format "   %i %?-2 t%s")
                   )
                  )
          (tags-todo  "-STYLE=\"habit\""
                      (
                       (org-agenda-overriding-header "\n ALL TODO")
                       (org-agenda-sorting-strategy '(priority-down))
                       (org-agenda-remove-tags t)
                       (org-agenda-prefix-format "   %i %?-2 t%s")
                       )
                      )))))

;; Remove Scheduled tag
(setq org-agenda-scheduled-leaders '("" ""))

;; Refile Targets
(setq org-refile-targets '(("~/org/done.org" . (:level . 1))))

;; Send a daily email to myself with the days agenda:
(defun my/send-daily-agenda ()
  "Send daily agenda email using mu4e"
  (interactive)
  (let* ((date-string (format-time-string "%Y-%m-%d"))
         (subject (format "Daily Agenda: %s" (format-time-string "%A, %B %d")))
         (tmp-file (make-temp-file "agenda")))

    ;; Generate agenda and save to temp file
    (save-window-excursion
      (org-agenda nil "d")
      (with-current-buffer org-agenda-buffer-name
        (org-agenda-write tmp-file)))

    ;; Read the agenda content
    (let ((agenda-content
           (with-temp-buffer
             (insert-file-contents tmp-file)
             (buffer-string))))

      ;; Create and send email
      (with-current-buffer (mu4e-compose-new)
        (mu4e-compose-mode)
        ;; Set up headers
        (message-goto-to)
        (insert "josh@joshblais.com")
        (message-goto-subject)
        (insert subject)
        (message-goto-body)
        ;; Insert the agenda content
        (insert agenda-content)
        ;; Send
        (message-send-and-exit)))

    ;; Cleanup
    (delete-file tmp-file)))

;; Remove any existing timer
(cancel-function-timers 'my/send-daily-agenda)

;; Schedule for 4:30 AM
(run-at-time "04:30" 86400 #'my/send-daily-agenda)

;; Remove holidays from agenda
(setq org-agenda-include-diary nil)

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/org/todo.org" "Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        ("e" "Event" entry
         (file+headline "~/org/calendar.org" "Events")
         "* %^{Event}\n%^{SCHEDULED}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:CONTACT: %(org-capture-ref-link \"~/org/contacts.org\")\n:END:\n%?")
        ("d" "Deadline" entry
         (file+headline "~/org/calendar.org" "Deadlines")
         "* TODO %^{Task}\nDEADLINE: %^{Deadline}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        ("p" "Project" entry
         (file+headline "~/org/todo.org" "Projects")
         "* PROJ %^{Project name}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n** TODO %?")
        ("i" "Idea" entry
         (file+headline "~/org/ideas.org" "Ideas")
         "** IDEA %^{Idea}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")
        ("c" "Contact" entry
         (file+headline "~/org/contacts.org" "Inbox")
         "* %^{Name}

:PROPERTIES:
:CREATED: %U
:CAPTURED: %a
:EMAIL: %^{Email}
:PHONE: %^{Phone}
:BIRTHDAY: %^{Birthday +1y}u
:LOCATION: %^{Address}
:LAST_CONTACTED: %U
:END:
*** Communications
*** Notes
%?")
        ("n" "Note" entry
         (file+headline "~/org/notes.org" "Inbox")
         "* [%<%Y-%m-%d %a>] %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?"
         :prepend t)))

;; Helper function to select and link a contact
(defun org-capture-ref-link (file)
  "Create a link to a contact in contacts.org"
  (let* ((headlines (org-map-entries
                     (lambda ()
                       (cons (org-get-heading t t t t)
                             (org-id-get-create)))
                     t
                     (list file)))
         (contact (completing-read "Contact: "
                                   (mapcar #'car headlines)))
         (id (cdr (assoc contact headlines))))
    (format "[[id:%s][%s]]" id contact)))

;; Set archive location to done.org under current date
;; (defun my/archive-done-task ()
;;   "Archive current task to done.org under today's date"
;;   (interactive)
;;   (let* ((date-header (format-time-string "%Y-%m-%d %A"))
;;          (archive-file (expand-file-name "~/org/done.org"))
;;          (location (format "%s::* %s" archive-file date-header)))
;;     ;; Only archive if not a habit
;;     (unless (org-is-habit-p)
;;       ;; Add COMPLETED property if it doesn't exist
;;       (org-set-property "COMPLETED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
;;       ;; Set archive location and archive
;;       (setq org-archive-location location)
;;       (org-archive-subtree))))

;; Automatically archive when marked DONE, except for habits
;; (add-hook 'org-after-todo-state-change-hook
;;           (lambda ()
;;             (when (and (string= org-state "DONE")
;;                        (not (org-is-habit-p)))
;;               (my/archive-done-task))))

;; Optional key binding if you ever need to archive manually
(define-key org-mode-map (kbd "C-c C-x C-a") 'my/archive-done-task)

;; Enable arrow keys in org-read-date calendar popup
(define-key org-read-date-minibuffer-local-map (kbd "<left>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<right>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<up>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<down>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))

;;Org-Roam
(setq org-roam-directory "~/org/roam")

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; org-download customizations
(require 'org-download)
(setq-default org-download-screenshot-method "scrot -s %s")

;; Nov.el customizations and setup
(setq nov-unzip-program (executable-find "bsdtar")
      nov-unzip-args '("-xC" directory "-f" filename))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; EMMS
(emms-all)
(emms-default-players)
(emms-mode-line-mode 1)
(emms-playing-time-mode 1)
(setq emms-source-file-default-directory "~/Music"
      emms-browser-covers #'emms-browser-cache-thumbnail-async
      emms-browser-thumbnail-small-size 64
      emms-browser-thumbnail-medium-size 128
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(map! :leader
      (:prefix ("m" . "music/EMMS")  ;; Changed from 'a' to 'm' for music
       :desc "Play at directory tree"   "d" #'emms-play-directory-tree
       :desc "Go to emms playlist"      "p" #'emms-playlist-mode-go
       :desc "Shuffle"                  "h" #'emms-shuffle
       :desc "Emms pause track"         "x" #'emms-pause
       :desc "Emms stop track"          "s" #'emms-stop
       :desc "Emms play previous track" "b" #'emms-previous
       :desc "Emms play next track"     "n" #'emms-next))

;; Grab album artwork for dunst to display
(defun emms-cover-art-path ()
  "Return the path of the cover art for the current track."
  (let* ((track (emms-playlist-current-selected-track))
         (path (emms-track-get track 'name))
         (dir (file-name-directory path))
         (cover-files (directory-files dir nil ".*\\(jpg\\|png\\|jpeg\\)$")))
    (when cover-files
      (concat dir (car cover-files)))))

;; MU4E
(use-package mu4e
  :ensure nil
  :defer 20
  :config

  (load-file "~/.config/mu4e/mu4e-config.el")
  (setq mu4e-update-interval (* 10 60))
  (mu4e t))

;; Deft mode
;; (setq deft-extensions '("txt" "tex" "org"))
;; (setq deft-directory "~/Vaults/org/roam")
;; (setq deft-recursive t)
;; (setq deft-use-filename-as-title t)

(defun my/magit-stage-commit-push ()
  "Stage all, commit with quick message, and push with no questions"
  (interactive)
  (magit-stage-modified)
  (let ((msg (read-string "Commit message: ")))
    (magit-commit-create (list "-m" msg))
    (magit-run-git "push" "origin" (magit-get-current-branch))))

;; Custom keymaps
(map! :leader
      ;; Magit mode mappngs
      (:prefix ("g" . "magit")  ; Use 'g' as the main prefix
       :desc "Stage all files"          "a" #'magit-stage-modified
       :desc "Push"                     "P" #'magit-push
       :desc "Pull"                     "p" #'magit-pull
       :desc "Merge"                    "m" #'magit-merge
       :desc "Quick commit and push"    "z" #'my/magit-stage-commit-push
       )
      ;; Org mode mappings
      (:prefix("y" . "org-mode-specifics")
       :desc "MU4E org mode"                    "m" #'mu4e-org-mode
       :desc "Mail add attachment"              "a" #'mail-add-attachment
       :desc "Export as markdown"               "e" #'org-md-export-as-markdown
       :desc "Preview markdown file"            "p" #'markdown-preview
       :desc "Export as html"                   "h" #'org-html-export-as-html
       :desc "Org Roam UI"                      "u" #'org-roam-ui-mode
       :desc "Search dictionary at word"        "d" #'dictionary-lookup-definition
       :desc "Powerthesaurus lookup word"       "t" #'powerthesaurus-lookup-word-at-point
       :desc "Read Aloud This"                  "r" #'read-aloud-this
       :desc "Export as LaTeX then PDF"         "l" #'org-latex-export-to-pdf
       :desc "spell check"                      "z" #'ispell-word
       :desc "Find definition"                  "f" #'lsp-find-definition
       )
      ;; Mappings for Elfeed and ERC
      (:prefix("e" . "Elfeed and ERC")
       :desc "Open elfeed"              "e" #'elfeed
       :desc "Open ERC"                 "r" #'erc
       :desc "Open EWW Browser"         "w" #'eww
       :desc "Update elfeed"            "u" #'elfeed-update
       :desc "MPV watch video"          "v" #'elfeed-tube-mpv
       :desc "Open Elpher"              "l" #'elpher
       :desc "Open Pass"                "p" #'pass
       )
      ;; Various other commands
      (:prefix("o" . "open")
       :desc "Calendar"                  "c" #'=calendar
       :desc "Bookmarks"                 "l" #'list-bookmarks
       )
      (:prefix("b" . "+buffer")
       :desc "Save Bookmarks"                 "P" #'bookmark-save
       ))

;; Saving
(map! "C-s" #'save-buffer)

;; Moving between splits
(map! :map general-override-mode-map
      "C-<right>" #'evil-window-right
      "C-<left>"  #'evil-window-left
      "C-<up>"    #'evil-window-up
      "C-<down>"  #'evil-window-down
      ;; Window resizing with Shift
      "S-<right>" (lambda () (interactive)
                    (if (window-in-direction 'left)
                        (evil-window-decrease-width 5)
                      (evil-window-increase-width 5)))
      "S-<left>"  (lambda () (interactive)
                    (if (window-in-direction 'right)
                        (evil-window-decrease-width 5)
                      (evil-window-increase-width 5)))
      "S-<up>"    (lambda () (interactive)
                    (if (window-in-direction 'below)
                        (evil-window-decrease-height 2)
                      (evil-window-increase-height 2)))
      "S-<down>"  (lambda () (interactive)
                    (if (window-in-direction 'above)
                        (evil-window-decrease-height 2)
                      (evil-window-increase-height 2))))


(map! :n "<C-tab>"   #'centaur-tabs-forward    ; normal mode only
      :n "<C-iso-lefttab>" #'centaur-tabs-backward)  ; normal mode only

(define-key evil-normal-state-map "f" 'avy-goto-char-2)
(define-key evil-normal-state-map "F" 'avy-goto-char-2)

;; Drag and drop:
;; Function for mouse events
(defun my/drag-file-mouse (event)
  "Drag current file using dragon (mouse version)"
  (interactive "e")
  (let ((file (dired-get-filename nil t)))
    (when file
      (message "Click and drag the dragon window to your target location")
      (start-process "dragon" nil "/usr/local/bin/dragon"
                     "-x"          ; Send mode
                     "--keep"      ; Keep the window open
                     file))))

;; Function for keyboard shortcut with multiple files support
(defun my/drag-file-keyboard ()
  "Drag marked files (or current file) using dragon"
  (interactive)
  (let ((files (or (dired-get-marked-files)
                   (list (dired-get-filename nil t)))))
    (when files
      (message "Click and drag the dragon window to your target location")
      (apply 'start-process "dragon" nil "/usr/local/bin/dragon"
             (append (list "-x" "--keep") files)))))

;; Bind both versions
(after! dired
  (define-key dired-mode-map [drag-mouse-1] 'my/drag-file-mouse)
  (define-key dired-mode-map (kbd "C-c C-d") 'my/drag-file-keyboard))

;; /path/to/project/.dir-locals.el
;; ((sql-mode . ((sql-postgres-login-params
;;                '((user :default "postgres")
;;                  (database :default "postgres")
;;                  (server :default "localhost")
;;                  (port :default 54322))))))

;; Additional Consult bindings
(map! :leader
      (:prefix-map ("s" . "search")
       :desc "Search project" "p" #'consult-ripgrep
       :desc "Search buffer" "s" #'consult-line
       :desc "Search project files" "f" #'consult-find))

;; Enhanced marginalia annotations
(after! marginalia
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; Corrected Embark configuration
(map! :leader
      (:prefix ("k" . "embark")  ;; Using 'k' prefix instead of 'e' which conflicts with elfeed
       :desc "Embark act" "a" #'embark-act
       :desc "Embark dwim" "d" #'embark-dwim))

;; Optional: Make vertico use a more minimal display
(after! vertico
  (setq vertico-count 17
        vertico-cycle t))

;; Optional: Configure consult for better previews
(after! consult
  (setq consult-preview-key "M-.")
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip"))

(map! :leader
      :desc "Dirvish in current dir" "d" #'dirvish)

(use-package! svelte-mode
  :mode "\\.svelte\\'"
  :config
  (setq svelte-basic-offset 2)
  ;; Disable automatic reformatting
  (setq svelte-format-on-save nil)
  ;; Use prettier instead
  (add-hook 'svelte-mode-hook 'prettier-js-mode))

;; Configure prettier
(use-package! prettier-js
  :config
  (setq prettier-js-args
        '("--parser" "svelte"
          "--tab-width" "2"
          "--use-tabs" "true")))

;; Load various scripts and templates
(load! "templates/writing-template")
(load! "templates/note-template")

;; Basic elfeed setup first
(make-directory "~/.elfeed" t)

;; Force load elfeed-org
(require 'elfeed-org)
(elfeed-org)

;; Set org feed file
(setq rmh-elfeed-org-files '("~/.config/doom/elfeed.org"))

;; Configure elfeed
(after! elfeed
  (setq elfeed-db-directory "~/.elfeed")
  (setq elfeed-search-filter "@1-week-ago +unread -4chan -Reddit"))

;; Load extensions
;; (use-package! elfeed-goodies
;;   :after elfeed
;;   :config
;;   (elfeed-goodies/setup))

(use-package! elfeed-tube
  :after elfeed
  :config
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

;; (use-package! elysium
;;   :custom
;;   (elysium-window-size 0.33)
;;   (elysium-window-style 'vertical))

;; (use-package! gptel
;;   :custom
;;   (gptel-model 'claude-3-5-sonnet-20241022)
;;   :config
;;   (defun gptel-api-key ()
;;     "Read API key from file and ensure it's clean."
;;     (string-trim
;;      (with-temp-buffer
;;        (insert-file-contents "~/secrets/claude_key")
;;        (buffer-string))))

;;   (setq gptel-backend
;;         (gptel-make-anthropic "Claude"
;;                               :stream t
;;                               :key (gptel-api-key))))  ; Call the function immediately

;;;; TRAMP optimizations
(after! tramp
  (setq tramp-default-method "ssh"          ; Use SSH by default
        tramp-verbose 1                      ; Reduce verbosity
        tramp-use-ssh-controlmaster-options nil  ; Don't use control master
        tramp-chunksize 500                 ; Bigger chunks for better performance
        tramp-connection-timeout 10         ; Shorter timeout
        ;; Use SSH configuration
        tramp-use-ssh-controlmaster-options nil
        ;; Cache remote files
        remote-file-name-inhibit-cache nil
        ;; Enable file-name-handler cache
        tramp-cache-read-persistent-data t))

;; Additional performance settings
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Insert image into org from selection
(defun my/org-insert-image ()
  "Select and insert an image into org file."
  (interactive)
  (let ((selected-file (read-file-name "Select image: " "~/Pictures/" nil t)))
    (when selected-file
      (insert (format "[[file:%s]]\n" selected-file))
      (org-display-inline-images))))

;; Keybinds for org mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-i") #'my/org-insert-image)
  (define-key org-mode-map (kbd "C-c e") #'org-set-effort)
  (define-key org-mode-map (kbd "C-c i") #'org-clock-in)
  (define-key org-mode-map (kbd "C-c o") #'org-clock-out))

;; Emacs everywhere configuration
(after! emacs-everywhere
  ;; Set default frame parameters for emacs-everywhere
  (setq emacs-everywhere-frame-parameters
        '((name . "emacs-everywhere")
          (width . 80)
          (height . 24)
          (minibuffer . t)
          (menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (undecorated . t))))

;; setup development sql database
(setq sql-connection-alist
      '((dev-postgres
         (sql-product 'postgres)
         (sql-server "localhost")
         (sql-user "postgres")
         (sql-password "postgres")
         (sql-database "devdb")
         (sql-port 5432))))

;; Enable Treesitter for Go in org
(after! tree-sitter
  (require 'tree-sitter-langs)
  (add-to-list 'tree-sitter-major-mode-language-alist '(org-mode . go)))

(defun org-babel-edit-prep:go (babel-info)
  (when-let ((tangled-file (->> babel-info caddr (alist-get :tangle))))
    (let ((full-path (expand-file-name tangled-file)))
      ;; Don't actually create/modify the tangled file
      (setq-local buffer-file-name full-path)
      (lsp-deferred))))

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((go . t)))

  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        ;; Don't save source edits in temp files
        org-src-window-setup 'current-window))

(after! dap-mode
  (require 'dap-dlv-go)

  ;; Remove problematic hooks
  (remove-hook 'dap-stopped-hook 'dap-ui-repl-toggle)
  (remove-hook 'dap-session-created-hook 'dap-ui-mode))

(setq evil-move-cursor-back nil)      ; Don't move cursor back when exiting insert mode
(setq evil-want-fine-undo t) ;; granular undo with evil mode
(setq auto-save-default t) ;; autosave on

;; Attempt to setup org-gcal and tasks
;; Main calendar and tasks synchronization setup
;; (after! org
;;   ;; Configure org-gcal with enhanced security and error handling
;;   (use-package! org-gcal
;;     :config
;;     ;; Set up dedicated directory for token storage and lock files
;;     (setq org-gcal-token-directory (expand-file-name "org-gcal/" doom-etc-dir))
;;     (unless (file-directory-p org-gcal-token-directory)
;;       (make-directory org-gcal-token-directory t))

;;     ;; Configure OAuth2 authentication and file paths
;;     (setq org-gcal-client-id (my/get-env-or-warn "ORG_GCAL_CLIENT_ID")
;;           org-gcal-client-secret (my/get-env-or-warn "ORG_GCAL_CLIENT_SECRET")
;;           ;; Store tokens in the dedicated directory we created
;;           org-gcal-token-file (expand-file-name ".org-gcal-token" org-gcal-token-directory)
;;           org-gcal-file-alist
;;           `((,(my/get-env-or-warn "ORG_GCAL_EMAIL") . "~/org/calendar.org"))
;;           ;; Explicitly set OAuth2 as the authentication method
;;           org-gcal-request-method 'oauth2
;;           org-gcal-notify-p t
;;           org-gcal-reminder-time 30)

;;     ;; Enhanced synchronization function with improved lock file handling
;;     (defun my/google-sync ()
;;       "Synchronize with Google Calendar with comprehensive error handling.
;; Manages sync locks and provides detailed feedback on the synchronization process."
;;       (interactive)
;;       (when (and org-gcal-client-id org-gcal-client-secret)
;;         (condition-case err
;;             (progn
;;               ;; Check for lock file in our specified directory
;;               (let ((lock-file (expand-file-name "org-gcal.lock" org-gcal-token-directory)))
;;                 (when (file-exists-p lock-file)
;;                   (delete-file lock-file)
;;                   (message "Removed stale lock file")))
;;               ;; Perform calendar synchronization
;;               (org-gcal-sync)
;;               ;; Attempt tasks synchronization if available
;;               (when (featurep 'org-gtasks)
;;                 (org-gtasks-sync))
;;               (message "Google synchronization completed successfully"))
;;           ;; Handle and report any errors while ensuring locks are cleaned up
;;           (error
;;            (message "Sync error: %s" (error-message-string err))
;;            (let ((lock-file (expand-file-name "org-gcal.lock" org-gcal-token-directory)))
;;              (when (file-exists-p lock-file)
;;                (delete-file lock-file))))))))

;;   ;; Set up automatic synchronization triggers
;;   (run-with-timer 0 600 #'my/google-sync)  ; Sync every 10 minutes
;;   (add-hook 'org-agenda-mode-hook #'my/google-sync)
;;   (add-hook 'org-capture-after-finalize-hook #'my/google-sync)

;; ;; Configure org-gtasks when available
;; (after! org-gtasks
;;   (setq org-gtasks-file "~/org/tasks.org")
;;   ;; Add task capture template
;;   (after! org-capture
;;     (add-to-list 'org-capture-templates
;;                  '("t" "Task" entry
;;                    (file "~/org/tasks.org")
;;                    "* TODO %?\nDEADLINE: %^t\n%i"))))

;; Trying to save workspaces
(after! persp-mode
  ;; Auto-save workspaces when Emacs exits
  (setq persp-auto-save-opt 1)
  ;; Save all workspace info including window configurations
  (setq persp-set-last-persp-for-new-frames nil)
  (setq persp-reset-windows-on-nil-window-conf nil)
  ;; Load workspaces automatically on startup
  (setq persp-auto-resume-time -1))

;; pomodoro timer
(load! "lisp/pomodoro")
