;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Josh Blais"
      user-mail-address "josh@joshblais.com")

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

;; Set SSH_AUTH_SOCK from keychain
(let ((ssh-auth-sock (string-trim
                      (shell-command-to-string
                       "keychain --eval --quiet --agents ssh 2>/dev/null | grep SSH_AUTH_SOCK | sed 's/.*SSH_AUTH_SOCK=\\([^;]*\\).*/\\1/'"))))
  (when (and ssh-auth-sock (file-exists-p ssh-auth-sock))
    (setenv "SSH_AUTH_SOCK" ssh-auth-sock)))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
(setq doom-font (font-spec :family "GeistMono Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "Alegreya" :size 18)
      doom-big-font (font-spec :family "GeistMono Nerd Font" :size 22))
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

(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-lsp-icon t)
(setq doom-modeline-major-mode-color-icon t)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(96 . 97))
(add-to-list 'default-frame-alist '(alpha . (96 . 97)))

;; Aggresssive Indent
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)

;; Blink cursor
(blink-cursor-mode 1)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Line wrapping
(global-visual-line-mode t)

;; Send files to trash instead of fully deleting
(setq delete-by-moving-to-trash t)
;; Save automatically
(setq auto-save-default t)

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

;; Fix x11 issues
(setq x-no-window-manager t)
(setq frame-inhibit-implied-resize t)
(setq focus-follows-mouse nil)

;; Setup custom splashscreen
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(setq fancy-splash-image "~/Pictures/Wallpapers/emacsdoom.jpg")
(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Welcome Home, Joshua.")))

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

;; set specific browser to open links
;;(setq browse-url-browser-function 'browse-url-firefox)
;; set browser to zen-browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "zen-browser")  ; replace with actual executable name

;; Speed of which-key popup
(setq which-key-idle-delay 0.2)

;; Completion mechanisms
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

;; Save minibuffer history - enables command history in M-x
(use-package! savehist
  :config
  (setq savehist-file (concat doom-cache-dir "savehist")
        savehist-save-minibuffer-history t
        history-length 1000
        history-delete-duplicates t
        savehist-additional-variables '(search-ring
                                        regexp-search-ring
                                        extended-command-history))
  (savehist-mode 1))

(after! vertico
  ;; Add file preview
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  ;; Make vertico use a more minimal display
  (setq vertico-count 17
        vertico-cycle t
        vertico-resize t)
  ;; Enable alternative filter methods
  (setq vertico-sort-function #'vertico-sort-history-alpha)
  ;; Quick actions keybindings
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (define-key vertico-map (kbd "M-RET") #'vertico-exit-input)

  ;; History navigation
  (define-key vertico-map (kbd "M-p") #'vertico-previous-history)
  (define-key vertico-map (kbd "M-n") #'vertico-next-history)
  (define-key vertico-map (kbd "C-r") #'consult-history)

  ;; Configure orderless for better filtering
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion orderless))))

  ;; Customize orderless behavior
  (setq orderless-component-separator #'orderless-escapable-split-on-space
        orderless-matching-styles '(orderless-literal
                                    orderless-prefixes
                                    orderless-initialism
                                    orderless-regexp)))

;; Quick command repetition
(use-package! vertico-repeat
  :after vertico
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :leader
        (:prefix "r"
         :desc "Repeat completion" "v" #'vertico-repeat)))

;; TODO Not currently working
;; Enhanced sorting and filtering with prescient
;; (use-package! vertico-prescient
;;   :after vertico
;;   :config
;;   (vertico-prescient-mode 1)
;;   (prescient-persist-mode 1)
;;   (setq prescient-sort-length-enable nil
;;         prescient-filter-method '(literal regexp initialism fuzzy)))

;; Enhanced marginalia annotations
(after! marginalia
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  ;; Show more details in marginalia
  (setq marginalia-max-relative-age 0
        marginalia-align 'right))

;; Corrected Embark configuration
(map! :leader
      (:prefix ("k" . "embark")  ;; Using 'k' prefix instead of 'e' which conflicts with elfeed
       :desc "Embark act" "a" #'embark-act
       :desc "Embark dwim" "d" #'embark-dwim
       :desc "Embark collect" "c" #'embark-collect))

;; Configure consult for better previews
(after! consult
  (setq consult-preview-key "M-."
        consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip"
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  ;; More useful previews for different commands
  (consult-customize
   consult-theme consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any)))

;; Enhanced directory navigation
(use-package! consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map vertico-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

;; Add additional useful shortcuts
(map! :leader
      (:prefix "s"
       :desc "Command history" "h" #'consult-history
       :desc "Recent directories" "d" #'consult-dir))

(after! company
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-show-quick-access t
        company-tooltip-limit 20
        company-tooltip-align-annotations t)

  ;; Make company-files a higher priority backend
  (setq company-backends (cons 'company-files (delete 'company-files company-backends)))

  ;; Better file path completion settings
  (setq company-files-exclusions nil)
  (setq company-files-chop-trailing-slash t)

  ;; Enable completion at point for file paths
  (defun my/enable-path-completion ()
    "Enable file path completion using company."
    (setq-local company-backends
                (cons 'company-files company-backends)))

  ;; Enable for all major modes
  (add-hook 'after-change-major-mode-hook #'my/enable-path-completion)

  ;; Custom file path trigger
  (defun my/looks-like-path-p (input)
    "Check if INPUT looks like a file path."
    (or (string-match-p "^/" input)         ;; Absolute path
        (string-match-p "^~/" input)        ;; Home directory
        (string-match-p "^\\.\\{1,2\\}/" input))) ;; Relative path

  (defun my/company-path-trigger (command &optional arg &rest ignored)
    "Company backend that triggers file completion for path-like input."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-files))
      (prefix (when (my/looks-like-path-p (or (company-grab-line "\\([^ ]*\\)" 1) ""))
                (company-files 'prefix)))
      (t (apply 'company-files command arg ignored))))

  ;; Add the custom path trigger to backends
  (add-to-list 'company-backends 'my/company-path-trigger))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

(use-package org
  :ensure nil
  :custom (org-modules '(org-habit)))

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

;; (after! org
;;   (use-package! org-fancy-priorities
;;     :hook
;;     (org-mode . org-fancy-priorities-mode)
;;     :config
;;     (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "FUTURE"))))

;; Prevent clock from stopping when marking subtasks as done
(setq org-clock-out-when-done nil)

;; Org-auto-tangle
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

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
;; Remove holidays from agenda
(setq org-agenda-include-diary nil)

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/org/inbox.org" "Inbox")
         "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

        ("e" "Event" entry
         (file+headline "~/org/calendar.org" "Events")
         "* %^{Event}\n%^{SCHEDULED}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:CONTACT: %(org-capture-ref-link \"~/org/contacts.org\")\n:END:\n%?")

        ("d" "Deadline" entry
         (file+headline "~/org/calendar.org" "Deadlines")
         "* TODO %^{Task}\nDEADLINE: %^{Deadline}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

        ("p" "Project" entry
         (file+headline "~/org/projects.org" "Projects")
         "* PROJ %^{Project name}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n** TODO %?")

        ("i" "Idea" entry
         (file+headline "~/org/ideas.org" "Ideas")
         "** IDEA %^{Idea}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

        ("b" "Bookmark" entry
        (file+headline "~/org/bookmarks.org" "Inbox")
        "** [[%^{URL}][%^{Title}]]\n:PROPERTIES:\n:CREATED: %U\n:TAGS: %(org-capture-bookmark-tags)\n:END:\n\n"
        :empty-lines 0)

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
\\ *** Communications
\\ *** Notes
%?")

        ("n" "Note" entry
         (file+headline "~/org/notes.org" "Inbox")
         "* [%<%Y-%m-%d %a>] %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?"
         :prepend t)))

(defun org-capture-bookmark-tags ()
  "Get tags from existing bookmarks and prompt for tags with completion."
  (save-window-excursion
    (let ((tags-list '()))
      ;; Collect existing tags
      (with-current-buffer (find-file-noselect "~/org/bookmarks.org")
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^:TAGS:\\s-*\\(.+\\)$" nil t)
            (let ((tag-string (match-string 1)))
              (dolist (tag (split-string tag-string "[,;]" t "[[:space:]]"))
                (push (string-trim tag) tags-list))))))
      ;; Remove duplicates and sort
      (setq tags-list (sort (delete-dups tags-list) 'string<))
      ;; Prompt user with completion
      (let ((selected-tags (completing-read-multiple "Tags (comma-separated): " tags-list)))
        ;; Return as a comma-separated string
        (mapconcat 'identity selected-tags ", ")))))

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

;;Org-Roam
;; Org-Roam Configuration with SQLite Built-in Connector
(use-package! org-roam
  :custom
  ;; Set your org-roam directory
  (org-roam-directory "~/org/roam")

  ;; Explicitly use the built-in SQLite connector
  (org-roam-database-connector 'sqlite-builtin)

  ;; Set an absolute path for the database file
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))

  :config
  ;; Make sure the directory exists
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))

  ;; Add error handling for database operations
  (advice-add 'org-roam-db-query :around
              (lambda (fn &rest args)
                (condition-case err
                    (apply fn args)
                  (error
                   (message "Database error in org-roam: %S" err)
                   nil))))

  ;; Enable auto-sync mode to keep the database updated
  (org-roam-db-autosync-mode +1))

;; Org-Roam UI setup - only load after org-roam is properly initialized
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; org-download customizations
(require 'org-download)
(setq-default org-download-screenshot-method "scrot -s %s")

;; Debugging function for SQLite issues
(defun debug-org-roam-db ()
  "Debug function to test org-roam database connection."
  (interactive)
  (message "Testing org-roam database...")
  (message "Directory exists: %s" (file-exists-p org-roam-directory))
  (message "Database path: %s" org-roam-db-location)
  (message "Database connector: %s" org-roam-database-connector)
  (condition-case err
      (progn
        (org-roam-db-sync)
        (message "Database synced successfully!"))
    (error (message "Database sync error: %S" err))))

;; Keybinds for org mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-i") #'my/org-insert-image)
  (define-key org-mode-map (kbd "C-c e") #'org-set-effort)
  (define-key org-mode-map (kbd "C-c i") #'org-clock-in)
  (define-key org-mode-map (kbd "C-c o") #'org-clock-out))

;; Insert image into org from selection
(defun my/org-insert-image ()
  "Select and insert an image into org file."
  (interactive)
  (let ((selected-file (read-file-name "Select image: " "~/Pictures/" nil t)))
    (when selected-file
      (insert (format "[[file:%s]]\n" selected-file))
      (org-display-inline-images))))

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((go . t)))

  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        ;; Don't save source edits in temp files
        org-src-window-setup 'current-window))

;; Specifically for go-mode literate programming
(defun org-babel-edit-prep:go (babel-info)
  (when-let ((tangled-file (->> babel-info caddr (alist-get :tangle))))
    (let ((full-path (expand-file-name tangled-file)))
      ;; Don't actually create/modify the tangled file
      (setq-local buffer-file-name full-path)
      (lsp-deferred))))

;; Evil-escape sequence
(setq-default evil-escape-key-sequence "kj")
(setq-default evil-escape-delay 0.1)

; Don't move cursor back when exiting insert mode
(setq evil-move-cursor-back nil)
;; granular undo with evil mode
(setq evil-want-fine-undo t)
;; Enable paste from system clipboard with C-v in insert mode
(evil-define-key 'insert global-map (kbd "C-v") 'clipboard-yank)

;; Vterm adjustemts
(setq vterm-environment '("TERM=xterm-256color"))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(custom-set-faces!
  '(vterm :family "Geistmono Nerd Font"))

;; open vterm in dired location
(after! vterm
  (setq vterm-buffer-name-string "vterm %s")

  ;; Modify the default vterm opening behavior
  (defadvice! +vterm-use-current-directory-a (fn &rest args)
    "Make vterm open in the directory of the current buffer."
    :around #'vterm
    (let ((default-directory (or (and (buffer-file-name)
                                      (file-name-directory (buffer-file-name)))
                                 (and (eq major-mode 'dired-mode)
                                      (dired-current-directory))
                                 default-directory)))
      (apply fn args)))

  ;; Also modify Doom's specific vterm functions
  (defadvice! +vterm-use-current-directory-b (fn &rest args)
    "Make Doom's vterm commands open in the directory of the current buffer."
    :around #'+vterm/here
    (let ((default-directory (or (and (buffer-file-name)
                                      (file-name-directory (buffer-file-name)))
                                 (and (eq major-mode 'dired-mode)
                                      (dired-current-directory))
                                 default-directory)))
      (apply fn args))))

(defun open-vterm-in-current-context ()
  "Open vterm in the context of the current buffer/window."
  (interactive)
  (when-let ((buf (current-buffer)))
    (with-current-buffer buf
      (call-interactively #'+vterm/here))))

(defun my-open-vterm-at-point ()
  "Open vterm in the directory of the currently selected window's buffer.
This function is designed to be called via `emacsclient -e`."
  (interactive)
  (let* ((selected-window (selected-window))
         ;; Ensure selected-window is not nil before trying to get its buffer
         (buffer-in-window (and selected-window (window-buffer selected-window)))
         dir)

    (when buffer-in-window
      (setq dir
            ;; Temporarily switch to the target buffer to evaluate its context
            (with-current-buffer buffer-in-window
              (cond ((buffer-file-name buffer-in-window)
                     (file-name-directory (buffer-file-name buffer-in-window)))
                    ((and (eq major-mode 'dired-mode)
                          (dired-current-directory))
                     (dired-current-directory))
                    (t default-directory)))))

    ;; Fallback to the server's default-directory if no specific directory was found
    (unless dir (setq dir default-directory))

    (message "Opening vterm in directory: %s" dir) ; For debugging, check *Messages* buffer

    ;; Now, crucially, set 'default-directory' for the vterm call itself
    (let ((default-directory dir))
      ;; Call the plain 'vterm' function, which should respect 'default-directory'.
      ;; We are *not* passing 'dir' as an argument to 'vterm' here,
      ;; as it's often designed to pick up the current 'default-directory'.
      (vterm))))

;; Emmet remap
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(map! :map emmet-mode-keymap
      :n "<C-return>" #'emmet-expand-line)
(setq emmet-expand-jsx-className? t) ;; default nil

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

(add-to-list 'auto-mode-alist '("\\.astro\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.templ\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

(set-file-template! "\\.astro$" :trigger "__astro" :mode 'web-mode)
(set-file-template! "\\.templ$" :trigger "__templ" :mode 'web-mode)
(set-file-template! "\\.svelte$" :trigger "__svelte" :mode 'web-mode)

;; Enable Treesitter for Go in org
(after! tree-sitter
  (require 'tree-sitter-langs)
  (add-to-list 'tree-sitter-major-mode-language-alist '(org-mode . go)))

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

(use-package! gptel
  :custom
  (gptel-model 'claude-sonnet-4-20250514)
  :config
  (defun gptel-api-key ()
    "Read API key from file and ensure it's clean."
    (string-trim
     (with-temp-buffer
       (insert-file-contents "~/secrets/claude_key")
       (buffer-string))))
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
                             :stream t
                             :key #'gptel-api-key
                             :models '(claude-sonnet-4-20250514
                                     claude-opus-4-20250514
                                     claude-3-7-sonnet-20250219))))

;; Elysium provides a nicer UI for gptel
(use-package! elysium
  :after gptel
  :custom
  (elysium-window-size 0.33)
  (elysium-window-style 'vertical))

;; Aider for code editing
(use-package! aider
  :config
  ;; Use Claude Sonnet 4 (latest)
  (setq aider-args '("--model" "claude-sonnet-4-20250514" "--no-auto-accept-architect"))

  ;; Set the API key using your existing function
  (setenv "ANTHROPIC_API_KEY" (gptel-api-key))

  ;; Optional: Set a key binding for the transient menu
  (map! :leader
        :desc "Aider menu" "a" #'aider-transient-menu))

(defun my/magit-stage-commit-push ()
  "Stage all, commit with quick message, and push with no questions"
  (interactive)
  (magit-stage-modified)
  (let ((msg (read-string "Commit message: ")))
    (magit-commit-create (list "-m" msg))
    (magit-run-git "push" "origin" (magit-get-current-branch))))

(after! dap-mode
  (require 'dap-dlv-go)

  ;; Remove problematic hooks
  (remove-hook 'dap-stopped-hook 'dap-ui-repl-toggle)
  (remove-hook 'dap-session-created-hook 'dap-ui-mode))

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

;; Setup development SQL database
(setq sql-connection-alist
      '((dev-postgres
         (sql-product 'postgres)
         (sql-server "localhost")
         (sql-user "postgres")
         (sql-password "postgres")
         (sql-database "devdb")
         (sql-port 5432))))

;; Configure org-babel SQL connection parameters
(setq org-babel-default-header-args:sql
      '((:engine . "postgresql")
        (:dbhost . "localhost")
        (:dbuser . "postgres")
        (:dbpassword . "postgres")
        (:database . "devdb")))

;; Ensure we have org-babel SQL support
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t))))

;; PGmacs setup
(use-package pgmacs
  :after pg
  :commands (pgmacs pgmacs-open-string pgmacs-open-uri)
  :config
  ;; Define a function to quickly connect to your development database
  (defun my-pgmacs-connect ()
    "Connect to the development database using PGmacs."
    (interactive)
    (pgmacs-open-string "user=postgres password=postgres dbname=devdb host=localhost port=5432"))

  ;; Set PGmacs customization options
  (setq pgmacs-default-display-limit 100)  ;; Default number of rows to show
  (setq pgmacs-widget-use-proportional-font nil))  ;; Use fixed-width font in widgets

;; Modified function to use existing SQL connection when available
(defun pg-query-to-orgtable (query &optional buffer-name)
  "Execute PostgreSQL QUERY and insert results as an Org table."
  (interactive "sSQL Query: \nsBuffer name (default *SQL Results*): ")
  (let ((buffer (get-buffer-create (or buffer-name "*SQL Results*"))))
    ;; Check if we have an active SQL connection
    (if (and (boundp 'sql-buffer) (buffer-live-p sql-buffer))
        ;; Use the SQL buffer method if we have a connection
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (org-mode)
            (insert "#+TITLE: SQL Query Results\n")
            (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
            (insert "#+BEGIN_SRC sql\n")
            (insert query "\n")
            (insert "#+END_SRC\n\n"))

          ;; Format the SQL output for better parsing
          (sql-send-string "\\a")  ;; Unaligned mode
          (sql-send-string "\\t")  ;; Tuples only
          (sql-send-string "\\f '|'")  ;; Field separator
          (sit-for 0.3)

          ;; Execute the query
          (sql-send-string query)
          (sit-for 1.0)

          ;; Add a marker to find the end of results
          (sql-send-string "SELECT '---RESULT-END---';")
          (sit-for 0.5)

          ;; Parse results from SQL buffer
          (with-current-buffer sql-buffer
            (save-excursion
              (goto-char (point-max))
              (when (search-backward "---RESULT-END---" nil t)
                (let ((end-pos (match-beginning 0)))
                  (search-backward query nil t)
                  (forward-line 1)
                  (let ((result-text (buffer-substring-no-properties (point) end-pos)))
                    (with-current-buffer buffer
                      (goto-char (point-max))
                      (let ((lines (split-string result-text "\n" t)))
                        (dolist (line lines)
                          (unless (string-match-p "^\\(devdb\\|Output\\|Tuples\\|Field\\)" line)
                            (unless (string-equal "" (string-trim line))
                              (insert "| ")
                              (insert (mapconcat 'identity
                                                (split-string line "|")
                                                " | "))
                              (insert " |\n"))))
                        (when (search-backward "|" nil t)
                          (org-table-align)))))))))

          ;; Reset SQL formatting
          (sql-send-string "\\a")
          (sql-send-string "\\t"))

      ;; Otherwise use org-babel with explicit connection parameters
      (with-current-buffer buffer
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: SQL Query Results\n")
        (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
        (insert "#+begin_src sql :engine postgresql :dbhost localhost :dbuser postgres :dbpassword postgres :database devdb :exports both\n")
        (insert query)
        (insert "\n#+end_src\n\n")
        (goto-char (point-min))
        (search-forward "#+begin_src")
        (forward-line 1)
        (org-babel-execute-src-block)))

    (switch-to-buffer buffer)
    (goto-char (point-min))))

;; Bridge function to export PGmacs data to Org documents
(defun my-pg-export-table-to-org (table-name)
  "Export a table from database to an Org document with query results."
  (interactive "sTable name: ")
  (pg-query-to-orgtable (format "SELECT * FROM %s LIMIT 100;" table-name)))

;; All our existing functions kept for backward compatibility
(defun pg-table-to-orgtable (table-name &optional limit-rows where-clause)
  "Select data from TABLE-NAME and display as an Org table.
Optionally limit results with LIMIT-ROWS and/or filter with WHERE-CLAUSE."
  (interactive
   (list (read-string "Table name: ")
         (read-string "Limit rows (default 100): " nil nil "100")
         (read-string "WHERE clause (optional): ")))
  (let ((query (format "SELECT * FROM %s%s%s"
                      table-name
                      (if (and where-clause (not (string-empty-p where-clause)))
                          (format " WHERE %s" where-clause)
                        "")
                      (if (and limit-rows (not (string-empty-p limit-rows)))
                          (format " LIMIT %s" limit-rows)
                        ""))))
    (pg-query-to-orgtable query (format "*Table: %s*" table-name))))

(defun pg-browse-table (table-name)
  "Browse a PostgreSQL table in Org mode."
  (interactive "sTable name: ")
  (pg-table-to-orgtable table-name))

(defun pg-list-tables ()
  "List tables in the PostgreSQL database and make them clickable."
  (interactive)
  (if (and (boundp 'sql-buffer) (buffer-live-p sql-buffer))
      (let ((buf (get-buffer-create "*PG Tables*")))
        (with-current-buffer buf
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: PostgreSQL Tables\n\n")

          ;; Send command to list tables
          (sql-send-string "\\dt")
          (sit-for 0.5)

          ;; Capture the results
          (with-current-buffer sql-buffer
            (let ((tables-text (buffer-substring-no-properties
                               (save-excursion
                                 (goto-char (point-max))
                                 (forward-line -15)
                                 (point))
                               (point-max))))
              (with-current-buffer buf
                (insert "| Schema | Table | Action |\n")
                (insert "|--------+-------+--------|\n")
                ;; Parse the table list
                (let ((lines (split-string tables-text "\n" t)))
                  (dolist (line lines)
                    (when (string-match "^ *\\([^ |]*\\) *| *\\([^ |]*\\)" line)
                      (let ((schema (match-string 1 line))
                            (table (match-string 2 line)))
                        (unless (or (string= schema "Schema")
                                    (string-match-p "^--" schema)
                                    (string-match-p "^(" schema))
                          (insert (format "| %s | %s | [[elisp:(pg-browse-table \"%s\")][Browse]] | [[elisp:(my-pg-export-table-to-org \"%s\")][Export]] | [[elisp:(pgmacs-display-table \"%s\")][PGmacs]] |\n"
                                         schema table table table table))))))))))
          (org-table-align))
        (switch-to-buffer buf))
    ;; Use org-babel if no SQL connection
    (let ((buf (get-buffer-create "*PG Tables*")))
      (with-current-buffer buf
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: PostgreSQL Tables\n\n")
        (insert "#+begin_src sql :engine postgresql :dbhost localhost :dbuser postgres :dbpassword postgres :database devdb :exports both\n")
        (insert "SELECT table_schema, table_name FROM information_schema.tables WHERE table_schema='public' ORDER BY table_name;\n")
        (insert "#+end_src\n\n")
        (goto-char (point-min))
        (search-forward "#+begin_src")
        (forward-line 1)
        (org-babel-execute-src-block)

        ;; Create links for each table - with additional options
        (when (search-forward "#+RESULTS:" nil t)
          (forward-line 1)
          (let ((start (point)))
            (forward-line)  ;; Skip header row
            (while (and (not (eobp)) (looking-at "^| "))
              (when (looking-at "| *\\([^ |]+\\) *| *\\([^ |]+\\) *|")
                (let ((schema (match-string-no-properties 1))
                      (table (match-string-no-properties 2)))
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert (format "| %s | %s | [[elisp:(pg-browse-table \"%s\")][Browse]] | [[elisp:(my-pg-export-table-to-org \"%s\")][Export]] | [[elisp:(pgmacs-display-table \"%s\")][PGmacs]] |"
                                 schema table table table table))))
              (forward-line 1))
            (org-table-align))))
      (switch-to-buffer buf))))

(defun pg-describe-table (table-name)
  "Show detailed information about a table structure."
  (interactive "sTable name: ")
  (let ((buf (get-buffer-create (format "*Table Structure: %s*" table-name))))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert (format "#+TITLE: Table Structure: %s\n\n" table-name))

      ;; Column information
      (insert "* Columns\n\n")
      (let ((query (format "SELECT column_name, data_type, is_nullable, column_default
FROM information_schema.columns
WHERE table_name = '%s'
ORDER BY ordinal_position;" table-name)))
        (pg-query-to-orgtable query))

      ;; Constraints
      (insert "\n* Constraints\n\n")
      (let ((query (format "SELECT c.conname AS constraint_name,
       CASE c.contype
         WHEN 'c' THEN 'check'
         WHEN 'f' THEN 'foreign_key'
         WHEN 'p' THEN 'primary_key'
         WHEN 'u' THEN 'unique'
       END AS constraint_type,
       pg_get_constraintdef(c.oid) AS constraint_definition
FROM pg_constraint c
JOIN pg_namespace n ON n.oid = c.connamespace
JOIN pg_class t ON t.oid = c.conrelid
WHERE t.relname = '%s'
  AND n.nspname = 'public';" table-name)))
        (pg-query-to-orgtable query))

      ;; Indexes
      (insert "\n* Indexes\n\n")
      (let ((query (format "SELECT indexname, indexdef
FROM pg_indexes
WHERE tablename = '%s';" table-name)))
        (pg-query-to-orgtable query)))
    (switch-to-buffer buf)))

(defun pg-sample-data (table-name)
  "Show sample data from a table with ability to filter."
  (interactive "sTable name: ")
  (let* ((where (read-string "WHERE clause (optional): "))
         (limit (read-string "Limit (default 10): " nil nil "10"))
         (query (format "SELECT * FROM %s%s LIMIT %s;"
                      table-name
                      (if (string-empty-p where) "" (format " WHERE %s" where))
                      limit)))
    (pg-query-to-orgtable query (format "*Sample: %s*" table-name))))

(defun pg-execute-buffer-query ()
  "Execute the current SQL buffer as a query and show results."
  (interactive)
  (pg-query-to-orgtable (buffer-string)))

(defun pg-execute-statement-at-point ()
  "Execute the SQL statement at point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'paragraph))
         (statement (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (pg-query-to-orgtable statement)))

(defun pg-connect ()
  "Connect to PostgreSQL database."
  (interactive)
  (sql-connect 'dev-postgres))

;; Key bindings for SQL mode
(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-c") 'pg-execute-buffer-query)
  (define-key sql-mode-map (kbd "C-c C-r") 'pg-execute-statement-at-point)
  (define-key sql-mode-map (kbd "C-c t") 'pg-list-tables)
  (define-key sql-mode-map (kbd "C-c d") 'pg-describe-table))

;; Global key bindings for database operations
(map! :leader
      (:prefix-map ("e" . "custom")
       (:prefix ("d" . "database")
        :desc "Connect to PGmacs" "c" #'my-pgmacs-connect
        :desc "Open PGmacs" "p" #'pgmacs
        :desc "List tables" "t" #'pg-list-tables
        :desc "Connect to SQL" "s" #'pg-connect
        :desc "Execute SQL query" "q" #'pg-query-to-orgtable)))

;; LSP support for SQL files
(use-package lsp-sqls
  :after lsp-mode
  :hook (sql-mode . lsp-deferred)
  :config
  ;; Let sqls use the config file instead of hardcoded connections
  (setq lsp-sqls-workspace-config-path nil)) ; This tells it to look for .sqls.yml

(setq docker-command "podman")
(setq docker-compose-command "podman-compose")

(use-package! agenix
  :config
  (setq agenix-secrets-file "~/nixos-config/secrets.nix")

  ;; Override the command to always use your identity
  (defun agenix--age-command (action file &rest args)
    (append (list "age" action "--identity" (expand-file-name "~/.ssh/joshuakey") file) args)))

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

;; Setup writeroom width and appearance
(after! writeroom-mode
  ;; Set width for centered text
  (setq writeroom-width 40)

  ;; Ensure the text is truly centered horizontally
  (setq writeroom-fringes-outside-margins nil)
  (setq writeroom-center-text t)

  ;; Add vertical spacing for better readability
  (setq writeroom-extra-line-spacing 4)  ;; Adds space between lines

  ;; Improve vertical centering with visual-fill-column integration
  (add-hook! 'writeroom-mode-hook
    (defun my-writeroom-settings ()
      "Configure various settings when entering/exiting writeroom-mode."
      (if writeroom-mode
          (progn
            ;; When entering writeroom mode
            (display-line-numbers-mode -1)       ;; Turn off line numbers
            (setq cursor-type 'bar)              ;; Change cursor to a thin bar for writing
            (hl-line-mode -1)                    ;; Disable current line highlighting
            (setq left-margin-width 0)           ;; Let writeroom handle margins
            (setq right-margin-width 0)
            (text-scale-set 1)                   ;; Slightly increase text size

            ;; Improve vertical centering
            (when (bound-and-true-p visual-fill-column-mode)
              (visual-fill-column-mode -1))      ;; Temporarily disable if active
            (setq visual-fill-column-width 40)   ;; Match writeroom width
            (setq visual-fill-column-center-text t)
            (setq visual-fill-column-extra-text-width '(0 . 0))

            ;; Set top/bottom margins to improve vertical centering
            ;; These larger margins push content toward vertical center
            (setq-local writeroom-top-margin-size
                        (max 10 (/ (- (window-height) 40) 3)))
            (setq-local writeroom-bottom-margin-size
                        (max 10 (/ (- (window-height) 40) 3)))

            ;; Enable visual-fill-column for better text placement
            (visual-fill-column-mode 1))

        ;; When exiting writeroom mode
        (progn
          (display-line-numbers-mode +1)       ;; Restore line numbers
          (setq cursor-type 'box)              ;; Restore default cursor
          (hl-line-mode +1)                    ;; Restore line highlighting
          (text-scale-set 0)                   ;; Restore normal text size
          (when (bound-and-true-p visual-fill-column-mode)
            (visual-fill-column-mode -1))))))  ;; Disable visual fill column mode

  ;; Hide modeline for a cleaner look
  (setq writeroom-mode-line nil)

  ;; Add additional global effects for writeroom
  (setq writeroom-global-effects
        '(writeroom-set-fullscreen        ;; Enables fullscreen
          writeroom-set-alpha             ;; Adjusts frame transparency
          writeroom-set-menu-bar-lines
          writeroom-set-tool-bar-lines
          writeroom-set-vertical-scroll-bars
          writeroom-set-bottom-divider-width))

  ;; Set frame transparency
  (setq writeroom-alpha 0.95))

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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
      (:prefix("e" . "Elfeed/ERC/AI")
       :desc "Open elfeed"              "e" #'elfeed
       :desc "Open ERC"                 "r" #'erc
       :desc "Open EWW Browser"         "w" #'eww
       :desc "Update elfeed"            "u" #'elfeed-update
       :desc "MPV watch video"          "v" #'elfeed-tube-mpv
       :desc "Open Elpher"              "l" #'elpher
       :desc "Open Pass"                "p" #'pass
       :desc "Claude chat (gptel)"      "g" #'gptel
       :desc "Send region to Claude"    "s" #'elysium-add-context
       :desc "Elysium chat UI"          "i" #'elysium-query
       :desc "Aider code session"       "a" #'aider-session
       :desc "Aider edit region"        "c" #'aider-edit-regio
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

;; Enable arrow keys in org-read-date calendar popup
(define-key org-read-date-minibuffer-local-map (kbd "<left>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<right>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<up>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "<down>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))

;; Additional Consult bindings
(map! :leader
      (:prefix-map ("s" . "search")
       :desc "Search project" "p" #'consult-ripgrep
       :desc "Search buffer" "s" #'consult-line
       :desc "Search project files" "f" #'consult-find))

(after! projectile
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid))

;; Path completion
(projectile-add-known-project "~/Vaults/Writing")
(projectile-add-known-project "~/Vaults")
(projectile-add-known-project "~/go/src/github.com/jblais493/HTMXFrontend")
(projectile-add-known-project "~/go/src/github.com/jblais493/Citadel")
(projectile-add-known-project "~/Development/svelte-email")

;; Trying to save workspaces
(after! persp-mode
  ;; Auto-save workspaces when Emacs exits
  (setq persp-auto-save-opt 1)
  ;; Save all workspace info including window configurations
  (setq persp-set-last-persp-for-new-frames nil)
  (setq persp-reset-windows-on-nil-window-conf nil)
  ;; Load workspaces automatically on startup
  (setq persp-auto-resume-time -1))

;; EMMS full configuration with Nord theme, centered layout, and swaync notifications
(emms-all)
(emms-default-players)
(emms-mode-line-mode 1)
(emms-playing-time-mode 1)

;; Basic settings
(setq emms-source-file-default-directory "~/MusicOrganized"
      emms-browser-covers #'emms-browser-cache-thumbnail-async
      emms-browser-thumbnail-small-size 64
      emms-browser-thumbnail-medium-size 128
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

;; MPD integration
(require 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(setq emms-player-mpd-music-directory (expand-file-name "~/Music"))

;; Connect to MPD and add it to player list and info functions
(add-to-list 'emms-player-list 'emms-player-mpd)
(add-to-list 'emms-info-functions 'emms-info-mpd)
(emms-player-mpd-connect)

(defun my/update-emms-from-mpd ()
  "Update EMMS cache from MPD and refresh browser."
  (interactive)
  (message "Updating EMMS cache from MPD...")
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "EMMS cache updated. Refreshing browser...")
  (when (get-buffer "*EMMS Browser*")
    (with-current-buffer "*EMMS Browser*"
      (emms-browser-refresh))))

;; Ensure players are properly set up
(setq emms-player-list '(emms-player-mpd
                         emms-player-mplayer
                         emms-player-vlc
                         emms-player-mpg321
                         emms-player-ogg123))

;; Nord theme colors
(with-eval-after-load 'emms
  (custom-set-faces
   ;; Nord colors: https://www.nordtheme.com/docs/colors-and-palettes
   '(emms-browser-artist-face ((t (:foreground "#ECEFF4" :height 1.1))))  ;; Nord Snow Storm (bright white)
   '(emms-browser-album-face ((t (:foreground "#88C0D0" :height 1.0))))   ;; Nord Frost (blue)
   '(emms-browser-track-face ((t (:foreground "#A3BE8C" :height 1.0))))   ;; Nord Aurora (green)
   '(emms-playlist-track-face ((t (:foreground "#D8DEE9" :height 1.0))))  ;; Nord Snow Storm (lighter white)
   '(emms-playlist-selected-face ((t (:foreground "#BF616A" :weight bold))))))  ;; Nord Aurora (red)

;; Add margins and spacing for better layout
(defun emms-center-buffer-in-frame ()
  "Add margins to center the EMMS buffer in the frame."
  (let* ((window-width (window-width))
         (desired-width 80)  ;; Desired text width
         (margin (max 0 (/ (- window-width desired-width) 2))))
    (setq-local left-margin-width margin)
    (setq-local right-margin-width margin)
    ;; Add line spacing for better readability
    (setq-local line-spacing 0.2)
    (set-window-buffer (selected-window) (current-buffer))))

;; Set dark Nord background and center layout

(add-hook 'emms-browser-mode-hook
          (lambda ()
            (face-remap-add-relative 'default '(:background "#2E3440"))  ;; Nord Polar Night (dark blue-gray)
            (emms-center-buffer-in-frame)))

(add-hook 'emms-playlist-mode-hook
          (lambda ()
            (face-remap-add-relative 'default '(:background "#2E3440"))  ;; Nord Polar Night (dark blue-gray)
            (emms-center-buffer-in-frame)))

;; Add window resize hook to maintain centering
(add-hook 'window-size-change-functions
          (lambda (_)
            (when (or (eq major-mode 'emms-browser-mode)
                      (eq major-mode 'emms-playlist-mode))
              (emms-center-buffer-in-frame))))

;; Ensure browser functionality
(setq emms-browser-default-browse-type 'artist)
;; (add-to-list 'emms-info-functions 'emms-info-mp3info)
(add-to-list 'emms-info-functions 'emms-info-ogginfo)
;; (add-to-list 'emms-info-functions 'emms-info-metaflac)
(add-to-list 'emms-info-functions 'emms-info-tinytag)

;; Ensure tracks play when selected
(define-key emms-browser-mode-map (kbd "RET") 'emms-browser-add-tracks-and-play)
(define-key emms-browser-mode-map (kbd "SPC") 'emms-pause)

;; Your keybindings
(map! :leader
      (:prefix ("m" . "music/EMMS")
       :desc "Update from MPD" "u" #'my/update-emms-from-mpd
       :desc "Play at directory tree"   "d" #'emms-play-directory-tree
       :desc "Go to emms playlist"      "p" #'emms-playlist-mode-go
       :desc "Shuffle"                  "h" #'emms-shuffle
       :desc "Emms pause track"         "x" #'emms-pause
       :desc "Emms stop track"          "s" #'emms-stop
       :desc "Emms play previous track" "b" #'emms-previous
       :desc "Emms play next track"     "n" #'emms-next
       :desc "EMMS Browser"             "o" #'emms-browser))

;; Enhanced cover art function (used by both browser and notifications)
(defun emms-cover-art-path ()
  "Return the path of the cover art for the current track."
  (let* ((track (emms-playlist-current-selected-track))
         (path (emms-track-get track 'name))
         (dir (file-name-directory path))
         ;; Check standard cover filenames first
         (standard-files '("cover.jpg" "cover.png" "folder.jpg" "folder.png"
                         "album.jpg" "album.png" "front.jpg" "front.png"))
         (standard-cover (cl-find-if
                         (lambda (file)
                           (file-exists-p (expand-file-name file dir)))
                         standard-files)))
    (if standard-cover
        (expand-file-name standard-cover dir)
      ;; If standard files aren't found, try any image in the directory
      (let ((cover-files (directory-files dir nil ".*\\(jpg\\|png\\|jpeg\\)$")))
        (when cover-files
          (expand-file-name (car cover-files) dir))))))

;; Swaync notification function with cover art
(defun emms-notify-song-change-with-artwork ()
  "Send song change notification with album artwork to swaync via libnotify"
  (let* ((track (emms-playlist-current-selected-track))
         (artist (or (emms-track-get track 'info-artist) "Unknown Artist"))
         (title (or (emms-track-get track 'info-title) "Unknown Title"))
         (album (or (emms-track-get track 'info-album) "Unknown Album"))
         ;; Use our existing cover art function
         (cover-image (emms-cover-art-path)))

    ;; Send notification with artwork if available
    (apply #'start-process
           "emms-notify" nil "notify-send"
           "-a" "EMMS"               ;; Application name
           "-c" "music"              ;; Category
           (append
            ;; Add artwork if found
            (when cover-image
              (list "-i" cover-image))
            ;; Summary and body
            (list
             (format "Now Playing: %s" title)
             (format "Artist: %s\nAlbum: %s" artist album))))))

;; Add the notification function to hook
(add-hook 'emms-player-started-hook 'emms-notify-song-change-with-artwork)

;; Waybar MPD integration helper - optional signal to waybar on track change
(defun emms-signal-waybar-mpd-update ()
  "Signal waybar to update its MPD widget"
  (start-process "emms-signal-waybar" nil "pkill" "-RTMIN+8" "waybar"))

;; Add the waybar signal function to hook (optional, uncomment if using waybar)
;; (add-hook 'emms-player-started-hook 'emms-signal-waybar-mpd-update)

;; Nov.el customizations and setup
(setq nov-unzip-program (executable-find "bsdtar")
      nov-unzip-args '("-xC" directory "-f" filename))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; In config.el
(use-package! calibredb
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/Library"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/Library"))
        calibredb-format-all-the-icons t)

  ;; Set up key bindings for calibredb-search-mode
  (map! :map calibredb-search-mode-map
        :n "RET" #'calibredb-find-file
        :n "?" #'calibredb-dispatch
        :n "a" #'calibredb-add
        :n "d" #'calibredb-remove
        :n "j" #'calibredb-next-entry
        :n "k" #'calibredb-previous-entry
        :n "l" #'calibredb-open-file-with-default-tool
        :n "s" #'calibredb-set-metadata-dispatch
        :n "S" #'calibredb-switch-library
        :n "q" #'calibredb-search-quit))

;; Add mu4e to load path
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; MU4E configuration
(after! mu4e
  (setq mu4e-mu-binary "/usr/bin/mu")
  (setq mu4e-update-interval (* 10 60))
  (load (expand-file-name "private/mu4e-config.el" doom-private-dir)))

;; Load elfeed-download package
(load! "lisp/elfeed-download")

(make-directory "~/.elfeed" t)

;; Force load elfeed-org
(require 'elfeed-org)
(elfeed-org)

;; Set org feed file
(setq rmh-elfeed-org-files '("~/.config/doom/elfeed.org"))

;; Configure elfeed - consolidate all elfeed config in one after! block
(after! elfeed
  (setq elfeed-db-directory "~/.elfeed")
  (setq elfeed-search-filter "@1-week-ago +unread -4chan -news -Reddit")

  ;; Set up elfeed-download
  (elfeed-download-setup)

  ;; Key bindings
  (map! :map elfeed-search-mode-map
        :n "d" #'elfeed-download-current-entry
        :n "O" #'elfeed-search-browse-url))

;; Update hourly
(run-at-time nil (* 60 60) #'elfeed-update)

;; Elfeed-tube configuration
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

;; Load private org-gcal credentials if the file exists
(load! "lisp/org-gcal-credentials")

;; Open dirvish
(map! :leader
      :desc "Dirvish in current dir" "d" #'dirvish)

(defun my/dired-copy-file-directory ()
  "Copy directory of file at point and switch to workspace 2"
  (interactive)
  (let ((file (dired-get-filename)))
    ;; Copy directory
    (call-process "~/.config/scripts/upload-helper.sh" nil 0 nil file)
    ;; Switch workspace using shell command (like your working binding)
    (shell-command "hyprctl dispatch workspace 2")
    (message "File's directory copied, switched to workspace 2")))

;; Bind to "yu"
(after! dired
  (map! :map dired-mode-map
        :n "yu" #'my/dired-copy-file-directory))

(after! dirvish
  (map! :map dirvish-mode-map
        :n "yu" #'my/dired-copy-file-directory))

;; Open file manager in place dirvish/dired
(defun open-thunar-here ()
  "Open thunar in the current directory shown in dired/dirvish."
  (interactive)
  (let ((dir (cond
              ;; If we're in dired mode
              ((derived-mode-p 'dired-mode)
               default-directory)
              ;; If we're in dirvish mode (dirvish is derived from dired)
              ((and (featurep 'dirvish)
                    (derived-mode-p 'dired-mode)
                    (bound-and-true-p dirvish-directory))
               (or (bound-and-true-p dirvish-directory) default-directory))
              ;; Fallback for any other mode
              (t default-directory))))
    (message "Opening thunar in: %s" dir)  ; Helpful for debugging
    (start-process "thunar" nil "thunar" dir)))
;; Bind it to Ctrl+Alt+f in both dired and dirvish modes
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-M-f") 'open-thunar-here))
;; For dirvish, we need to add our binding to its special keymap if it exists
(with-eval-after-load 'dirvish
  (if (boundp 'dirvish-mode-map)
      (define-key dirvish-mode-map (kbd "C-M-f") 'open-thunar-here)
    ;; Alternative approach if dirvish uses a different keymap system
    (add-hook 'dirvish-mode-hook
              (lambda ()
                (local-set-key (kbd "C-M-f") 'open-thunar-here)))))

(defun thanos/wtype-text (text)
  "Process TEXT for wtype, handling newlines properly."
  (let* ((has-final-newline (string-match-p "\n$" text))
         (lines (split-string text "\n"))
         (last-idx (1- (length lines))))
    (string-join
     (cl-loop for line in lines
              for i from 0
              collect (cond
                       ;; Last line without final newline
                       ((and (= i last-idx) (not has-final-newline))
                        (format "wtype -s 350 \"%s\""
                                (replace-regexp-in-string "\"" "\\\\\"" line)))
                       ;; Any other line
                       (t
                        (format "wtype -s 350 \"%s\" && wtype -k Return"
                                (replace-regexp-in-string "\"" "\\\\\"" line)))))
     " && ")))

(define-minor-mode thanos/type-mode
  "Minor mode for inserting text via wtype."
  :keymap `((,(kbd "C-c C-c") . ,(lambda () (interactive)
                                   (call-process-shell-command
                                    (thanos/wtype-text (buffer-string))
                                    nil 0)
                                   (delete-frame)))
            (,(kbd "C-c C-k") . ,(lambda () (interactive)
                                   (kill-buffer (current-buffer))))))

(defun thanos/type ()
  "Launch a temporary frame with a clean buffer for typing."
  (interactive)
  (let ((frame (make-frame '((name . "emacs-float")
                             (fullscreen . 0)
                             (undecorated . t)
                             (width . 70)
                             (height . 20))))
        (buf (get-buffer-create "emacs-float")))
    (select-frame frame)
    (switch-to-buffer buf)
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (flyspell-mode)
      (thanos/type-mode)
      (setq-local header-line-format
                  (format " %s to insert text or %s to cancel."
                          (propertize "C-c C-c" 'face 'help-key-binding)
			  (propertize "C-c C-k" 'face 'help-key-binding)))
      ;; Make the frame more temporary-like
      (set-frame-parameter frame 'delete-before-kill-buffer t)
      (set-window-dedicated-p (selected-window) t))))

;; Load private IRC configuration
(load! "private/irc-config" nil t)

(after! circe

  ;; Rest of your configuration remains the same
  (setq circe-format-self-say "{nick}: {body}")
  (setq circe-format-server-topic "*** Topic: {topic-diff}")
  (setq circe-use-cycle-completion t)
  (setq circe-reduce-lurker-spam t)

  (setq lui-max-buffer-size 30000)
  (enable-lui-autopaste)
  (enable-lui-irc-colors)

  (tracking-mode 1)
  (setq tracking-faces-priorities '(circe-highlight-nick-face))
  (setq tracking-ignored-buffers '("*circe-network-Rizon*"))

  (setq circe-highlight-nick-type 'all)

  (setq circe-directory "~/.doom.d/circe-logs")
  (setq lui-logging-directory "~/.doom.d/circe-logs")
  (setq lui-logging-file-format "{buffer}/%Y-%m-%d.txt")
  (setq lui-logging-format "[%H:%M:%S] {text}")
  (enable-lui-logging-globally)

  (unless (file-exists-p "~/.doom.d/circe-logs")
    (make-directory "~/.doom.d/circe-logs" t)))

(defun my/irc-connect-rizon ()
  "Connect to Rizon IRC."
  (interactive)
  (circe "Rizon"))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Connect to Rizon IRC" "i" #'my/irc-connect-rizon))

(define-minor-mode my/audio-recorder-mode
  "Minor mode for recording audio in Emacs."
  :lighter " Audio"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c a r") 'my/record-audio)
            (define-key map (kbd "C-c a s") 'my/stop-audio-recording)
            map))

(defun my/org-return-and-maybe-elpher ()
  "Handle org-return and open gemini/gopher links in elpher when appropriate."
  (interactive)
  (let ((context (org-element-context)))
    (if (and (eq (org-element-type context) 'link)
             (member (org-element-property :type context) '("gemini" "gopher")))
        ;; If it's a gemini/gopher link, open in elpher
        (let ((url (org-element-property :raw-link context)))
          (elpher-go url))
      ;; Otherwise, do the normal org-return behavior
      (org-return))))

;; Override the Return key in org-mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "RET") 'my/org-return-and-maybe-elpher)

  ;; Register protocols with org-mode
  (org-link-set-parameters "gemini" :follow
                          (lambda (path) (elpher-go (concat "gemini://" path))))
  (org-link-set-parameters "gopher" :follow
                          (lambda (path) (elpher-go (concat "gopher://" path)))))

;; Remove EWW from popup rules to make it open in a full buffer
(after! eww
  (set-popup-rule! "^\\*eww\\*" :ignore t))

;; lisp functions
(load! "lisp/nm")
(load! "lisp/pomodoro")
(load! "lisp/done-refile")
(load! "lisp/mu4e-contact")
(load! "lisp/post-to-blog")
(load! "lisp/popup-scratch")
(load! "lisp/popup-dirvish-browser")
(load! "lisp/meeting-assistant")
(load! "lisp/create-daily")
(load! "lisp/audio-record")
(load! "lisp/universal-launcher")
(load! "lisp/weather")
(load! "lisp/termux-sms")
;; POSSE posting system
(load! "lisp/posse/posse-twitter")
(load! "lisp/posse/instagram-scheduler")

;; Load various scripts and templates
(load! "templates/writing-template")
(load! "templates/note-template")

;;;; Send a daily email to myself with the days agenda:
;;(defun my/send-daily-agenda ()
;;  "Send daily agenda email using mu4e"
;;  (interactive)
;;  (let* ((date-string (format-time-string "%Y-%m-%d"))
;;         (subject (format "Daily Agenda: %s" (format-time-string "%A, %B %d")))
;;         (tmp-file (make-temp-file "agenda")))
;;
;;    ;; Generate agenda and save to temp file
;;    (save-window-excursion
;;      (org-agenda nil "d")
;;      (with-current-buffer org-agenda-buffer-name
;;        (org-agenda-write tmp-file)))
;;
;;    ;; Read the agenda content
;;    (let ((agenda-content
;;           (with-temp-buffer
;;             (insert-file-contents tmp-file)
;;             (buffer-string))))
;;
;;      ;; Create and send email
;;      (with-current-buffer (mu4e-compose-new)
;;        (mu4e-compose-mode)
;;        ;; Set up headers
;;        (message-goto-to)
;;        (insert "josh@joshblais.com")
;;        (message-goto-subject)
;;        (insert subject)
;;        (message-goto-body)
;;        ;; Insert the agenda content
;;        (insert agenda-content)
;;        ;; Send
;;        (message-send-and-exit)))
;;
;;    ;; Cleanup
;;    (delete-file tmp-file)))
;;
;;;; Remove any existing timer
;;(cancel-function-timers 'my/send-daily-agenda)
;;
;;;; Schedule for 5:30 AM
;;(run-at-time "05:30" 86400 #'my/send-daily-agenda)

;; Deft mode
;; (setq deft-extensions '("txt" "tex" "org"))
;; (setq deft-directory "~/Vaults/org/roam")
;; (setq deft-recursive t)
;; (setq deft-use-filename-as-title t)

;; Drag and drop:
;; Function for mouse events
;;(defun my/drag-file-mouse (event)
;;  "Drag current file using dragon (mouse version)"
;;  (interactive "e")
;;  (let ((file (dired-get-filename nil t)))
;;    (when file
;;      (message "Click and drag the dragon window to your target location")
;;      (start-process "dragon" nil "/usr/local/bin/dragon"
;;                     "-x"          ; Send mode
;;                     "--keep"      ; Keep the window open
;;                     file))))
;;
;;;; Function for keyboard shortcut with multiple files support
;;(defun my/drag-file-keyboard ()
;;  "Drag marked files (or current file) using dragon"
;;  (interactive)
;;  (let ((files (or (dired-get-marked-files)
;;                   (list (dired-get-filename nil t)))))
;;    (when files
;;      (message "Click and drag the dragon window to your target location")
;;      (apply 'start-process "dragon" nil "/usr/local/bin/dragon"
;;             (append (list "-x" "--keep") files)))))
;;
;;;; Bind both versions
;;(after! dired
;;  (define-key dired-mode-map [drag-mouse-1] 'my/drag-file-mouse)
;;  (define-key dired-mode-map (kbd "C-c C-d") 'my/drag-file-keyboard))
