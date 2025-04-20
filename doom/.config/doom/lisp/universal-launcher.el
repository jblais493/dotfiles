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

                      ;; Applications
                      (mapcar (lambda (app)
                                (cons (format "%s App: %s"
                                              (all-the-icons-faicon "desktop")
                                              (car app))
                                      (list 'app (cdr app))))
                              (universal-launcher--get-applications))

                      ;; Browser tabs
                      (mapcar (lambda (tab)
                                (cons (format "%s Tab: %s"
                                              (all-the-icons-faicon "firefox")
                                              (car tab))
                                      (list 'tab (cdr tab))))
                              (universal-launcher--get-browser-tabs))

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
        ('app (universal-launcher--run-application item))
        ('tab (universal-launcher--focus-browser-tab item))
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

;; Helper function to run an application
(defun universal-launcher--run-application (exec-string)
  "Run application with EXEC-STRING."
  (let ((cmd (car (split-string exec-string))))
    (start-process cmd nil cmd)))

;; Helper function to run a command
(defun universal-launcher--run-command (command)
  "Run COMMAND."
  (start-process command nil command))

;; Firefox remote debugging configuration
(defcustom universal-launcher-firefox-debug-port 6000
  "Port used for Firefox remote debugging."
  :type 'integer
  :group 'universal-launcher)

;; Function to get browser tabs using Firefox remote debugging
(defun universal-launcher--get-browser-tabs ()
  "Get list of Firefox tabs using Remote Debugging Protocol."
  (condition-case err
      (let ((tabs '())
            (json-string (with-temp-buffer
                           (message "Attempting to connect to Firefox on port %d..."
                                    universal-launcher-firefox-debug-port)
                           (call-process "curl" nil t nil "-s" "-v"
                                         (format "http://localhost:%d/json/list"
                                                 universal-launcher-firefox-debug-port))
                           (let ((response (buffer-string)))
                             (message "Firefox response: %s"
                                      (if (> (length response) 200)
                                          (concat (substring response 0 200) "...")
                                        response))
                             response))))
        (if (string-empty-p json-string)
            (progn
              (message "No response from Firefox. Make sure it's running with --remote-debugging-port=%d"
                       universal-launcher-firefox-debug-port)
              '())
          (condition-case parse-err
              (let ((tab-data (json-parse-string json-string :object-type 'alist)))
                (message "Successfully parsed JSON with %d entries" (length tab-data))
                (dolist (tab tab-data)
                  (let* ((id (or (alist-get 'id tab)
                                 (alist-get 'webSocketDebuggerUrl tab)))
                         (title (alist-get 'title tab))
                         (url (alist-get 'url tab))
                         (display-title (if (and title (> (length title) 50))
                                            (concat (substring title 0 47) "...")
                                          (or title "(No title)"))))
                    (when (and id url)
                      (push (cons display-title (list id url)) tabs))))
                tabs)
            (error
             (message "Error parsing Firefox tabs JSON: %s" (error-message-string parse-err))
             '()))))
    (error
     (message "Error connecting to Firefox: %s" (error-message-string err))
     '())))

;; Function to focus browser tab using Firefox remote debugging
(defun universal-launcher--focus-browser-tab (tab-info)
  "Focus browser tab with TAB-INFO using Remote Debugging Protocol."
  (let ((tab-id (car tab-info))
        (url (cadr tab-info)))
    (condition-case err
        (let ((result (shell-command-to-string
                       (format "curl -s -X POST http://localhost:%d/json/activate/%s"
                               universal-launcher-firefox-debug-port tab-id))))
          (if (string-match-p "\"wasActivated\":true" result)
              (message "Switched to tab: %s" url)
            (message "Failed to switch tabs. Response: %s" result)))
      (error
       (message "Error activating tab: %s" (error-message-string err))))))

(provide 'universal-launcher)
;;; universal-launcher.el ends here
