;;; universal-launcher.el --- Universal launcher for applications, files, buffers, and browser tabs

;;; Commentary:
;; This provides a unified interface for launching applications, switching buffers,
;; opening files, and navigating to Firefox tabs.

;;; Code:

(require 'all-the-icons)
(require 'json)

(defun universal-launcher ()
  "Universal launcher for applications, files, buffers, and browser tabs."
  (interactive)
  (let* ((candidates (append
                      ;; Emacs buffers
                      (mapcar (lambda (buffer)
                                (cons (format "%s Buffer: %s"
                                              (all-the-icons-icon-for-mode
                                               (with-current-buffer buffer major-mode))
                                              (buffer-name buffer))
                                      (list 'buffer buffer)))
                              (buffer-list))

                      ;; Running applications
                      (mapcar (lambda (app)
                                (cons (format "%s Running: %s"
                                              (all-the-icons-octicon "device-desktop")
                                              (car app))
                                      (list 'running (cdr app))))
                              (universal-launcher--get-running-applications))

                      ;; Regular applications
                      (mapcar (lambda (app)
                                (cons (format "%s App: %s"
                                              (all-the-icons-octicon "rocket")
                                              (car app))
                                      (list 'app (cdr app))))
                              (universal-launcher--get-applications))

                      ;; Flatpak applications
                      (mapcar (lambda (app)
                                (cons (format "%s App: %s"
                                              (all-the-icons-octicon "package")
                                              (car app))
                                      (list 'app (cdr app))))
                              (universal-launcher--get-flatpak-applications))

                      ;; Firefox options
                      (mapcar (lambda (tab)
                                (cons (format "%s Firefox: %s"
                                              (all-the-icons-faicon "firefox")
                                              (car tab))
                                      (list 'firefox-action (cdr tab))))
                              (universal-launcher--get-firefox-actions))

                      ;; Bookmarks from Org file
                      (universal-launcher--get-bookmarks)

                      ;; Recent files
                      (mapcar (lambda (file)
                                (cons (format "%s File: %s"
                                              (all-the-icons-icon-for-file file)
                                              (file-name-nondirectory file))
                                      (list 'file file)))
                              recentf-list)

                      ;; Commands (executable files in PATH)
                      (mapcar (lambda (cmd)
                                (cons (format "%s Run: %s"
                                              (all-the-icons-octicon "terminal")
                                              cmd)
                                      (list 'command cmd)))
                              (universal-launcher--get-commands))))

         ;; Use completing-read (works with ido, ivy, helm, etc.)
         (selection (completing-read "Launch: "
                                     (mapcar #'car candidates)
                                     nil t)))

    ;; Find the selected candidate
    (let* ((candidate (cdr (assoc selection candidates)))
           (type (car candidate))
           (item (cadr candidate)))

      ;; Take action based on type
      (pcase type
        ('buffer (switch-to-buffer item))
        ('running (universal-launcher--focus-running-application item))
        ('app (universal-launcher--run-application item))
        ('firefox-action (universal-launcher--handle-firefox-action item))
        ('bookmark (universal-launcher--handle-bookmark item))
        ('file (find-file item))
        ('command (universal-launcher--run-command item))))))

;; Helper function to get applications
(defun universal-launcher--get-applications ()
  "Get list of desktop applications."
  (let ((apps '()))
    (dolist (dir '("~/.local/share/applications" "/usr/share/applications"))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.desktop$"))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (when (re-search-forward "^\\[Desktop Entry\\]" nil t)
              (let ((name nil)
                    (exec nil))
                (when (re-search-forward "^Name=\\(.*\\)$" nil t)
                  (setq name (match-string 1)))
                (when (re-search-forward "^Exec=\\(.*\\)$" nil t)
                  (setq exec (match-string 1)))
                (when (and name exec)
                  (push (cons name exec) apps))))))))
    apps))

;; Helper function to get commands in PATH
(defun universal-launcher--get-commands ()
  "Get list of executable commands in PATH."
  (let ((path-dirs (split-string (getenv "PATH") ":"))
        (commands '()))
    (dolist (dir path-dirs)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t))
          (when (and (file-executable-p file)
                     (not (file-directory-p file)))
            (push (file-name-nondirectory file) commands)))))
    (delete-dups commands)))

;; Function to get list of running applications
(defun universal-launcher--get-running-applications ()
  "Get list of currently running applications."
  (let ((apps '()))
    ;; Try multiple methods to detect running applications

    ;; Method 1: Using wmctrl
    (with-temp-buffer
      (when (= 0 (call-process "wmctrl" nil t nil "-l"))
        (goto-char (point-min))
        (while (re-search-forward "^\\(0x[0-9a-f]+\\)\\s-+\\S-+\\s-+\\S-+\\s-+\\(.+\\)$" nil t)
          (let ((window-id (match-string-no-properties 1))
                (app-name (match-string-no-properties 2)))
            ;; Filter out some window manager entries
            (unless (string-match-p "\\(Desktop\\|Dock\\|Emacs\\)" app-name)
              (push (cons app-name (list window-id app-name)) apps))))))

    ;; Method 2: Using GNOME's specific commands for running applications
    (with-temp-buffer
      (when (= 0 (call-process "gsettings" nil t nil "get" "org.gnome.desktop.wm.preferences" "button-layout"))
        ;; This suggests we're in a GNOME environment
        (with-temp-buffer
          (call-process "ps" nil t nil "-e")
          (goto-char (point-min))
          ;; Look for common GNOME applications
          (let ((gnome-apps '("gnome-terminal" "nautilus" "gedit" "eog" "evince"
                              "gnome-system-monitor" "gnome-control-center"
                              "gnome-calculator" "gnome-calendar" "shotcut"
                              "dbeaver" "code" "gimp" "inkscape")))
            (dolist (app gnome-apps)
              (goto-char (point-min))
              (when (search-forward app nil t)
                (let ((window-id (concat "gnome-" app))
                      (app-name (capitalize app)))
                  (push (cons app-name (list window-id app-name)) apps))))))))

    ;; Method 3: Using xdotool as a fallback
    (when (null apps)
      (with-temp-buffer
        (when (= 0 (call-process "xdotool" nil t nil "search" "--class" ".*" "getwindowname"))
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((window-id (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
                   (app-name nil))
              (forward-line)
              (when (not (eobp))
                (setq app-name (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
                (unless (string-match-p "\\(Desktop\\|Dock\\|Emacs\\)" app-name)
                  (push (cons app-name (list window-id app-name)) apps))
                (forward-line)))))))

    ;; Remove duplicates based on application name
    (let ((unique-apps '())
          (seen-names '()))
      (dolist (app apps)
        (let ((name (car app)))
          (unless (member name seen-names)
            (push name seen-names)
            (push app unique-apps))))
      (nreverse unique-apps))))

;; Function to focus running application
(defun universal-launcher--focus-running-application (app-info)
  "Focus running application using APP-INFO."
  (let ((window-id (car app-info))
        (app-name (cadr app-info)))
    (message "Focusing application: %s (ID: %s)" app-name window-id)
    ;; Try multiple focusing methods
    (cond
     ;; If it's a GNOME app (identified by the prefix)
     ((string-prefix-p "gnome-" window-id)
      (let ((app-cmd (substring window-id 6)))
        (message "GNOME app detected: %s" app-cmd)
        (call-process "wmctrl" nil nil nil "-a" app-name)))

     ;; Try by window ID with wmctrl
     ((condition-case nil
          (progn
            (call-process "wmctrl" nil nil nil "-i" "-a" window-id)
            t)
        (error nil))
      (message "Focused with wmctrl by ID"))

     ;; Try by app name with wmctrl
     ((condition-case nil
          (progn
            (call-process "wmctrl" nil nil nil "-a" app-name)
            t)
        (error nil))
      (message "Focused with wmctrl by name"))

     ;; Try xdotool by window ID
     ((condition-case nil
          (progn
            (call-process "xdotool" nil nil nil "windowactivate" window-id)
            t)
        (error nil))
      (message "Focused with xdotool by ID"))

     ;; Try xdotool by app name
     ((condition-case nil
          (progn
            (call-process "xdotool" nil nil nil "search" "--name" app-name "windowactivate")
            t)
        (error nil))
      (message "Focused with xdotool by name"))

     ;; Last resort - try to relaunch the app with its command name
     (t
      (let ((cmd (downcase app-name)))
        (message "Attempting to run: %s" cmd)
        (start-process cmd nil cmd))))))

;; Helper function to run an application
(defun universal-launcher--run-application (exec-string)
  "Run application with EXEC-STRING and bring it to focus."
  (let* ((exec-parts (split-string exec-string))
         (cmd (car exec-parts))
         (proc (apply #'start-process cmd nil exec-parts)))
    ;; Give the process a moment to start
    (run-with-timer 0.5 nil
                    (lambda ()
                      ;; Try to focus the window using window manager tools
                      (condition-case nil
                          (call-process "wmctrl" nil nil nil "-a" cmd)
                        (error
                         (condition-case nil
                             (call-process "xdotool" nil nil nil "search" "--name" cmd "windowactivate")
                           (error nil))))))))

(defun universal-launcher--run-command (command)
  "Run COMMAND and bring it to focus."
  (let ((proc (start-process command nil command)))
    ;; Give the process a moment to start
    (run-with-timer 0.5 nil
                    (lambda ()
                      ;; Try to focus the window using window manager tools
                      (condition-case nil
                          (call-process "wmctrl" nil nil nil "-a" command)
                        (error
                         (condition-case nil
                             (call-process "xdotool" nil nil nil "search" "--name" command "windowactivate")
                           (error nil))))))))

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

;; Firefox actions without using remote debugging
(defun universal-launcher--get-firefox-actions ()
  "Get list of Firefox actions."
  (let ((actions '()))
    ;; Check if Firefox is running
    (when (= 0 (call-process "pgrep" nil nil nil "-x" "firefox"))
      ;; If Firefox is running, offer to focus its window
      (push (cons "Focus Firefox window" '(focus-window)) actions)
      (push (cons "Open new tab" '(new-tab)) actions)

      ;; Common websites
      (let ((common-sites '(("Google" . "https://www.google.com")
                            ("GitHub" . "https://github.com")
                            ("YouTube" . "https://www.youtube.com")
                            ("Wikipedia" . "https://en.wikipedia.org"))))
        (dolist (site common-sites)
          (push (cons (concat "Open " (car site))
                      (list 'open-url (cdr site)))
                actions))))
    actions))

(defun universal-launcher--handle-firefox-action (action)
  "Handle firefox ACTION."
  (pcase (car action)
    ('focus-window
     (condition-case nil
         (call-process "wmctrl" nil nil nil "-a" "Firefox")
       (error
        (call-process "xdotool" nil nil nil "search" "--class" "firefox" "windowactivate")))
     (message "Focused Firefox window"))

    ('new-tab
     (call-process "firefox" nil nil nil "--new-tab" "about:newtab")
     (message "Opened new Firefox tab"))

    ('open-url
     (let ((url (cadr action)))
       (call-process "firefox" nil nil nil "--new-tab" url)
       (message "Opened %s in Firefox" url)))))

;; Function to parse org bookmarks file
(defun universal-launcher--parse-org-bookmarks (file)
  "Parse bookmarks from an org FILE."
  (let ((bookmarks '()))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Look for links in the format - [[url][description]] or [[url][]] or [[url]]
        (while (re-search-forward "\\[\\[\\(https?:[^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]" nil t)
          (let* ((url (match-string 1))
                 (desc (match-string 2)))
            ;; If description is empty or nil, use URL as description
            (when (or (null desc) (string= desc ""))
              (setq desc url))
            (push (cons desc url) bookmarks)))))
    (nreverse bookmarks)))

;; Function to get bookmarks for the launcher
(defun universal-launcher--get-bookmarks ()
  "Get bookmarks from the user's org bookmarks file."
  (let ((bookmarks-file (expand-file-name "~/org/bookmarks.org")))
    (mapcar (lambda (bookmark)
              (cons (format "%s Bookmark: %s"
                            (all-the-icons-faicon "bookmark")
                            (car bookmark))
                    (list 'bookmark (cdr bookmark))))
            (universal-launcher--parse-org-bookmarks bookmarks-file))))

;; Handler for bookmark actions
(defun universal-launcher--handle-bookmark (url)
  "Open URL in the default browser."
  (browse-url url)
  (message "Opened bookmark: %s" url))

(provide 'universal-launcher)
;;; universal-launcher.el ends here
