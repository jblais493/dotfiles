;;; universal-launcher.el --- Optimized universal launcher

;;; Commentary:
;; Ultra-optimized version for instantaneous popup display

;;; Code:

(require 'all-the-icons)
(require 'json)

;; Pre-create the frame
(defvar universal-launcher--frame nil "Pre-created launcher frame.")
(defvar universal-launcher--initialized nil "Whether the launcher has been initialized.")

;; Pre-grouped category structure for aesthetic grouping
(defvar universal-launcher--categories
  '((:name "Active" :icon "device-desktop" :types (buffer running))
    (:name "Files & Apps" :icon "apps" :types (file app flatpak))
    (:name "Web" :icon "globe" :types (bookmark firefox-action))
    (:name "System" :icon "terminal" :types (command)))
  "Category definitions for the launcher.")

;; Enhanced cache system
(defvar universal-launcher--all-candidates nil "Pre-computed candidates.")
(defvar universal-launcher--last-update 0 "Last time candidates were updated.")
(defvar universal-launcher--update-interval 30 "Update interval in seconds.")

;; Icon cache with category-specific icons
(defvar universal-launcher--icon-cache
  (let ((cache (make-hash-table :test 'equal)))
    (puthash 'buffer (all-the-icons-fileicon "elisp") cache)
    (puthash 'running (all-the-icons-octicon "device-desktop") cache)
    (puthash 'app (all-the-icons-faicon "rocket") cache)
    (puthash 'flatpak (all-the-icons-octicon "package") cache)
    (puthash 'firefox (all-the-icons-faicon "firefox") cache)
    (puthash 'bookmark (all-the-icons-faicon "bookmark") cache)
    (puthash 'file (all-the-icons-faicon "file") cache)
    (puthash 'command (all-the-icons-octicon "terminal") cache)
    ;; Category icons
    (puthash "Active" (all-the-icons-material "dashboard") cache)
    (puthash "Files & Apps" (all-the-icons-material "apps") cache)
    (puthash "Web" (all-the-icons-material "language") cache)
    (puthash "System" (all-the-icons-material "settings") cache)
    cache)
  "Pre-loaded icon cache.")

(defun universal-launcher--initialize ()
  "Initialize the launcher system."
  (unless universal-launcher--initialized
    ;; Pre-create the frame but keep it invisible
    (setq universal-launcher--frame
          (let* ((monitor-attrs (frame-monitor-attributes))
                 (monitor-workarea (cdr (assoc 'workarea monitor-attrs)))
                 (monitor-width (nth 2 monitor-workarea))
                 (monitor-height (nth 3 monitor-workarea))
                 (frame-height 10))
            (make-frame `((name . "launcher")
                          (minibuffer . only)
                          (width . ,(/ monitor-width (frame-char-width)))
                          (height . ,frame-height)
                          (left . 0)
                          (top . ,(- monitor-height (* frame-height (frame-char-height))))
                          (auto-raise . t)
                          (skip-taskbar . t)
                          (undecorated . t)
                          (internal-border-width . 0)
                          (vertical-scroll-bars . nil)
                          (menu-bar-lines . 0)
                          (tool-bar-lines . 0)
                          (visibility . nil)  ; Start invisible
                          (no-focus-on-map . t)
                          (no-accept-focus . nil)
                          (buffer-predicate . (lambda (_) nil))
                          (background-mode . 'dark)))))

    ;; Pre-compute candidates
    (universal-launcher--update-candidates t)

    ;; Set up background update timer
    (run-with-timer universal-launcher--update-interval
                    universal-launcher--update-interval
                    #'universal-launcher--update-candidates)

    (setq universal-launcher--initialized t)))

(defun universal-launcher--grouped-candidates ()
  "Return candidates grouped by category."
  (let ((candidates '())
        (category-handlers (make-hash-table :test 'eq)))

    ;; Define handlers for each type
    (puthash 'buffer
             (lambda ()
               (mapcar (lambda (buffer)
                         (cons (format "%s Buffer: %s"
                                       (universal-launcher--get-icon 'buffer)
                                       (buffer-name buffer))
                               (list 'buffer buffer)))
                       (buffer-list)))
             category-handlers)

    (puthash 'running
             (lambda ()
               (mapcar (lambda (app)
                         (cons (format "%s Running: %s"
                                       (universal-launcher--get-icon 'running)
                                       (car app))
                               (list 'running (cdr app))))
                       (universal-launcher--get-running-applications)))
             category-handlers)

    (puthash 'file
             (lambda ()
               (mapcar (lambda (file)
                         (cons (format "%s File: %s"
                                       (universal-launcher--get-icon 'file)
                                       (file-name-nondirectory file))
                               (list 'file file)))
                       recentf-list))
             category-handlers)

    (puthash 'app
             (lambda ()
               (mapcar (lambda (app)
                         (cons (format "%s %s"
                                       (universal-launcher--get-icon 'app)
                                       (car app))
                               (list 'app (cdr app))))
                       (universal-launcher--get-applications)))
             category-handlers)

    (puthash 'flatpak
             (lambda ()
               (mapcar (lambda (app)
                         (cons (format "%s Flatpak: %s"
                                       (universal-launcher--get-icon 'flatpak)
                                       (car app))
                               (list 'app (cdr app))))
                       (universal-launcher--get-flatpak-applications)))
             category-handlers)

    (puthash 'bookmark
             (lambda ()
               (mapcar (lambda (bookmark)
                         (cons (format "%s Bookmark: %s"
                                       (universal-launcher--get-icon 'bookmark)
                                       (car bookmark))
                               (list 'bookmark (cdr bookmark))))
                       (universal-launcher--parse-org-bookmarks
                        (expand-file-name "~/org/bookmarks.org"))))
             category-handlers)

    (puthash 'firefox-action
             (lambda ()
               (mapcar (lambda (action)
                         (cons (format "%s Firefox: %s"
                                       (universal-launcher--get-icon 'firefox)
                                       (car action))
                               (list 'firefox-action (cdr action))))
                       (universal-launcher--get-firefox-actions)))
             category-handlers)

    (puthash 'command
             (lambda ()
               (mapcar (lambda (cmd)
                         (cons (format "%s Command %s"
                                       (universal-launcher--get-icon 'command)
                                       cmd)
                               (list 'command cmd)))
                       (universal-launcher--get-system-commands)))
             category-handlers)

    ;; Process categories
    (dolist (category universal-launcher--categories)
      (let* ((cat-name (plist-get category :name))
             (cat-icon (gethash cat-name universal-launcher--icon-cache))
             (types (plist-get category :types))
             (section-items '()))

        (dolist (type types)
          (when-let ((handler (gethash type category-handlers)))
            (setq section-items (append section-items (funcall handler)))))

        (when section-items
          (push (cons (format "%s  %s " cat-icon cat-name) 'separator) candidates)
          (dolist (item section-items)
            (push (cons (concat "   " (car item)) (cdr item)) candidates)))))

    (nreverse candidates)))

(defun universal-launcher--update-candidates (&optional force)
  "Update cached candidates if needed or FORCE is non-nil."
  (when (or force
            (> (- (float-time) universal-launcher--last-update)
               universal-launcher--update-interval))
    (setq universal-launcher--all-candidates (universal-launcher--grouped-candidates))
    (setq universal-launcher--last-update (float-time))))

(defun universal-launcher--get-icon (type)
  "Get cached icon for TYPE instantly."
  (gethash type universal-launcher--icon-cache ""))

(defun universal-launcher--get-running-applications ()
  "Get list of currently running applications."
  (let ((apps '()))
    (with-temp-buffer
      (when (= 0 (call-process "wmctrl" nil t nil "-l"))
        (goto-char (point-min))
        (while (re-search-forward "^\\(0x[0-9a-f]+\\)\\s-+\\S-+\\s-+\\S-+\\s-+\\(.+\\)$" nil t)
          (let ((window-id (match-string-no-properties 1))
                (app-name (match-string-no-properties 2)))
            (unless (string-match-p "\\(Desktop\\|Dock\\|Emacs\\)" app-name)
              (push (cons app-name (list window-id app-name)) apps))))))
    apps))

(defun universal-launcher--get-applications ()
  "Get list of system applications from .desktop files."
  (let ((apps '())
        (dirs '("/usr/share/applications/"
                "/usr/local/share/applications/"
                "~/.local/share/applications/"
                "/var/lib/flatpak/exports/share/applications/"
                "~/.local/share/flatpak/exports/share/applications/")))
    (dolist (dir dirs)
      (when (file-directory-p (expand-file-name dir))
        (dolist (file (directory-files (expand-file-name dir) t "\\.desktop$"))
          (with-temp-buffer
            (insert-file-contents file)
            (when (re-search-forward "^Name=\\(.+\\)$" nil t)
              (let ((name (match-string 1))
                    exec-line)
                (goto-char (point-min))
                (when (re-search-forward "^Exec=\\(.+\\)$" nil t)
                  (setq exec-line (match-string 1))
                  (push (cons name (replace-regexp-in-string "%[FfUu]" "" exec-line))
                        apps))))))))
    apps))

(defun universal-launcher--get-flatpak-applications ()
  "Get list of installed Flatpak applications."
  (let ((apps '()))
    (with-temp-buffer
      (when (= 0 (call-process "flatpak" nil t nil "list" "--app" "--columns=name,application"))
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 (parts (split-string line "\t"))
                 (name (nth 0 parts))
                 (app-id (nth 1 parts)))
            (when (and name app-id)
              (push (cons (format "%s (Flatpak)" name)
                          (concat "flatpak run " app-id))
                    apps)))
          (forward-line 1))))
    apps))

(defun universal-launcher--get-system-commands ()
  "Get system commands from PATH."
  (let ((commands '()))
    (dolist (dir (parse-colon-path (getenv "PATH")))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t))
          (when (and (file-executable-p file)
                     (not (file-directory-p file))
                     (not (backup-file-name-p file)))
            (push (file-name-nondirectory file) commands)))))
    (cl-remove-duplicates commands :test #'string=)))

(defun universal-launcher--get-firefox-actions ()
  "Get list of Firefox actions."
  (let ((actions '()))
    (when (= 0 (call-process "pgrep" nil nil nil "-x" "firefox"))
      (push (cons "Focus Firefox window" '(focus-window)) actions)
      (push (cons "Open new tab" '(new-tab)) actions)
      (let ((common-sites '(("Google" . "https://www.google.com")
                            ("GitHub" . "https://github.com")
                            ("YouTube" . "https://www.youtube.com")
                            ("Wikipedia" . "https://en.wikipedia.org"))))
        (dolist (site common-sites)
          (push (cons (concat "Open " (car site))
                      (list 'open-url (cdr site)))
                actions))))
    actions))

(defun universal-launcher--parse-org-bookmarks (file)
  "Parse bookmarks from an org FILE."
  (let ((bookmarks '()))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "\\[\\[\\(https?:[^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]" nil t)
          (let* ((url (match-string 1))
                 (desc (match-string 2)))
            (when (or (null desc) (string= desc ""))
              (setq desc url))
            (push (cons desc url) bookmarks)))))
    (nreverse bookmarks)))

(defun universal-launcher--focus-running-application (app-info)
  "Focus running application using APP-INFO."
  (let ((window-id (car app-info))
        (app-name (cadr app-info)))
    (condition-case nil
        (call-process "wmctrl" nil nil nil "-i" "-a" window-id)
      (error
       (call-process "wmctrl" nil nil nil "-a" app-name)))))

(defun universal-launcher--run-application (exec-string)
  "Run application with EXEC-STRING."
  (let* ((exec-parts (split-string exec-string))
         (cmd (car exec-parts))
         (proc (apply #'start-process cmd nil exec-parts)))
    (run-with-timer 0.5 nil
                    (lambda () (call-process "wmctrl" nil nil nil "-a" cmd)))))

(defun universal-launcher--handle-firefox-action (action)
  "Handle firefox ACTION."
  (pcase (car action)
    ('focus-window
     (call-process "wmctrl" nil nil nil "-a" "Firefox"))
    ('new-tab
     (call-process "firefox" nil nil nil "--new-tab" "about:newtab"))
    ('open-url
     (let ((url (cadr action)))
       (call-process "firefox" nil nil nil "--new-tab" url)))))

(defun universal-launcher--handle-bookmark (url)
  "Open URL in the default browser."
  (browse-url url))

(defun universal-launcher--run-command (command)
  "Run COMMAND."
  (start-process command nil command))

(defun universal-launcher-popup ()
  "Ultra-fast launcher popup."
  (interactive)

  ;; Initialize if needed
  (unless universal-launcher--initialized
    (universal-launcher--initialize))

  ;; Simply make the pre-created frame visible
  (make-frame-visible universal-launcher--frame)
  (select-frame-set-input-focus universal-launcher--frame)

  (let ((minibuffer-setup-hook
         (cons (lambda ()
                 (setq-local completion-ignore-case t)
                 (setq-local resize-mini-windows nil))
               minibuffer-setup-hook)))
    (unwind-protect
        (let* ((selection (completing-read "Launch: "
                                           (mapcar #'car universal-launcher--all-candidates)
                                           nil t))
               (candidate (cdr (assoc selection universal-launcher--all-candidates))))
          (when (and candidate (not (eq candidate 'separator)))
            (let ((type (car candidate))
                  (item (cadr candidate)))
              (pcase type
                ('buffer (switch-to-buffer item))
                ('running (universal-launcher--focus-running-application item))
                ('app (universal-launcher--run-application item))
                ('firefox-action (universal-launcher--handle-firefox-action item))
                ('bookmark (universal-launcher--handle-bookmark item))
                ('file (find-file item))
                ('command (universal-launcher--run-command item))))))

      ;; Just hide the frame instead of deleting it
      (make-frame-invisible universal-launcher--frame))))

(provide 'universal-launcher)
;;; universal-launcher.el ends here
