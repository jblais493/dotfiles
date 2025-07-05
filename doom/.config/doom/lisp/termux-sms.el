;;; termux-sms.el --- SMS integration via Termux SSH with notifications

(require 'org)
(require 'json)
(require 'notifications) ; For desktop notifications

(defgroup termux-sms nil
  "SMS integration via Termux SSH."
  :group 'communication)

(defcustom termux-sms-contacts-file "~/org/contacts.org"
  "Path to org file containing contacts."
  :type 'file
  :group 'termux-sms)

(defcustom termux-sms-phone-ip "192.168.1.84"
  "IP address of your phone."
  :type 'string
  :group 'termux-sms)

(defcustom termux-sms-ssh-port "8022"
  "SSH port for Termux."
  :type 'string
  :group 'termux-sms)

(defcustom termux-sms-ssh-user "u0_a123"
  "SSH username for Termux."
  :type 'string
  :group 'termux-sms)

(defcustom termux-sms-ssh-key "~/.ssh/phone"
  "SSH key path for Termux."
  :type 'file
  :group 'termux-sms)

(defcustom termux-sms-notification-enabled t
  "Enable desktop notifications for new SMS."
  :type 'boolean
  :group 'termux-sms)

(defcustom termux-sms-check-interval 30
  "Seconds between checking for new messages."
  :type 'integer
  :group 'termux-sms)

(defvar termux-sms-contacts-cache nil)
(defvar termux-sms-monitor-timer nil)
(defvar termux-sms-last-message-count 0)
(defvar termux-sms-org-buffer "*SMS Composer*")

;; Core SMS functions (same as before)
(defun termux-sms-ssh-command (command)
  "Execute SSH command on phone via Termux."
  (let ((ssh-cmd (format "ssh -i %s -p %s %s@%s \"%s\""
                         (expand-file-name termux-sms-ssh-key)
                         termux-sms-ssh-port
                         termux-sms-ssh-user
                         termux-sms-phone-ip
                         command)))
    (string-trim (shell-command-to-string ssh-cmd))))

(defun termux-sms-send (phone message)
  "Send SMS via Termux."
  (let ((result (termux-sms-ssh-command
                 (format "termux-sms-send -n \"%s\" \"%s\"" phone message))))
    (if (string-match-p "error\\|Error\\|failed" result)
        (message "Error sending SMS: %s" result)
      (message "SMS sent to %s" phone)
      result)))

(defun termux-sms-get-all-messages ()
  "Get all SMS messages from phone."
  (let ((json-str (termux-sms-ssh-command "termux-sms-list")))
    (when (not (string-empty-p json-str))
      (condition-case err
          (json-read-from-string json-str)
        (error (message "Error parsing SMS JSON: %s" err) nil)))))

(defun termux-sms-get-unread-messages ()
  "Get unread SMS messages."
  (let ((messages (termux-sms-get-all-messages)))
    (seq-filter (lambda (msg)
                  (eq (alist-get 'read msg) :false))
                messages)))

;; Contact management
(defun termux-sms-parse-contacts ()
  "Parse contacts from org file."
  (with-temp-buffer
    (insert-file-contents termux-sms-contacts-file)
    (org-mode)
    (let ((contacts '()))
      (org-map-entries
       (lambda ()
         (let* ((heading (nth 4 (org-heading-components)))
                (phone (org-entry-get (point) "PHONE"))
                (email (org-entry-get (point) "EMAIL")))
           (when phone
             (push (list heading phone email) contacts)))))
      (setq termux-sms-contacts-cache contacts)
      contacts)))

(defun termux-sms-find-contact-name (phone-number)
  "Find contact name for phone number."
  (unless termux-sms-contacts-cache
    (termux-sms-parse-contacts))
  (let ((contact (seq-find (lambda (contact)
                             (string= (nth 1 contact) phone-number))
                           termux-sms-contacts-cache)))
    (when contact (nth 0 contact))))

;; Notification system
(defun termux-sms-notify (title message)
  "Send desktop notification."
  (when termux-sms-notification-enabled
    (if (fboundp 'notifications-notify)
        (notifications-notify :title title :body message :urgency 'normal)
      (message "%s: %s" title message))))

(defun termux-sms-check-for-new-messages ()
  "Check for new messages and notify."
  (let* ((all-messages (termux-sms-get-all-messages))
         (current-count (length all-messages))
         (unread-messages (termux-sms-get-unread-messages)))

    ;; Check for new messages
    (when (> current-count termux-sms-last-message-count)
      (let ((new-count (- current-count termux-sms-last-message-count)))
        (termux-sms-notify "New SMS"
                           (format "%d new message%s"
                                   new-count
                                   (if (> new-count 1) "s" "")))
        (setq termux-sms-last-message-count current-count)))

    ;; Notify about unread messages
    (when unread-messages
      (dolist (msg unread-messages)
        (let* ((number (alist-get 'number msg))
               (body (alist-get 'body msg))
               (contact-name (termux-sms-find-contact-name number)))
          (termux-sms-notify (format "SMS from %s" (or contact-name number))
                             (substring body 0 (min 100 (length body)))))))))

;;;###autoload
(defun termux-sms-start-monitoring ()
  "Start monitoring for new SMS messages."
  (interactive)
  (when termux-sms-monitor-timer
    (cancel-timer termux-sms-monitor-timer))
  (setq termux-sms-last-message-count (length (termux-sms-get-all-messages)))
  (setq termux-sms-monitor-timer
        (run-with-timer termux-sms-check-interval
                        termux-sms-check-interval
                        'termux-sms-check-for-new-messages))
  (message "SMS monitoring started (checking every %d seconds)" termux-sms-check-interval))

;;;###autoload
(defun termux-sms-stop-monitoring ()
  "Stop SMS monitoring."
  (interactive)
  (when termux-sms-monitor-timer
    (cancel-timer termux-sms-monitor-timer)
    (setq termux-sms-monitor-timer nil)
    (message "SMS monitoring stopped")))

;; Org-mode buffer composer
(defvar termux-sms-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'termux-sms-send-from-org)
    (define-key map (kbd "C-c C-k") 'termux-sms-cancel-org)
    (define-key map (kbd "C-c C-s") 'termux-sms-select-contact-org)
    (define-key map (kbd "C-c C-r") 'termux-sms-reply-to-latest)
    map)
  "Keymap for SMS org-mode composer.")

(define-derived-mode termux-sms-org-mode org-mode "SMS-Org"
  "Major mode for composing SMS messages in org-mode format."
  (setq header-line-format
        "C-c C-c: Send | C-c C-k: Cancel | C-c C-s: Select Contact | C-c C-r: Reply to Latest"))

;;;###autoload
(defun termux-sms-compose-org ()
  "Open org-mode SMS composition buffer."
  (interactive)
  (let ((buffer (get-buffer-create termux-sms-org-buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (termux-sms-org-mode)
      (insert "#+TITLE: SMS Composer\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d %H:%M") "\n\n")
      (insert "* SMS Message\n")
      (insert ":PROPERTIES:\n")
      (insert ":TO: \n")
      (insert ":CONTACT: \n")
      (insert ":END:\n\n")
      (insert "Write your message here...\n\n")
      (insert "** Instructions\n")
      (insert "- Fill in TO: property with phone number\n")
      (insert "- Use C-c C-s to select from contacts\n")
      (insert "- Write message under 'SMS Message' heading\n")
      (insert "- Send with C-c C-c\n")
      (goto-char (point-min))
      (re-search-forward "Write your message here..." nil t)
      (replace-match "")
      (backward-char))
    (pop-to-buffer buffer)))

(defun termux-sms-select-contact-org ()
  "Select contact and fill org properties."
  (interactive)
  (unless termux-sms-contacts-cache
    (termux-sms-parse-contacts))
  (let* ((contacts termux-sms-contacts-cache)
         (names (mapcar #'car contacts))
         (selected (completing-read "Select contact: " names nil t))
         (contact (assoc selected contacts)))
    (when contact
      (let ((name (nth 0 contact))
            (phone (nth 1 contact)))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward ":TO: " nil t)
            (delete-region (point) (line-end-position))
            (insert phone))
          (when (re-search-forward ":CONTACT: " nil t)
            (delete-region (point) (line-end-position))
            (insert name)))
        (message "Selected contact: %s (%s)" name phone)))))

(defun termux-sms-extract-message-from-org ()
  "Extract message content from org buffer."
  (save-excursion
    (goto-char (point-min))
    (let* ((phone (progn (re-search-forward ":TO: \\(.*\\)" nil t)
                         (match-string 1)))
           (contact (progn (re-search-forward ":CONTACT: \\(.*\\)" nil t)
                           (match-string 1)))
           (message-start (progn (re-search-forward "^\\* SMS Message" nil t)
                                 (forward-line 1)
                                 (while (looking-at "^:")
                                   (forward-line 1))
                                 (point)))
           (message-end (or (save-excursion
                              (re-search-forward "^\\*\\* Instructions" nil t)
                              (line-beginning-position))
                            (point-max)))
           (message (string-trim (buffer-substring message-start message-end))))
      (list phone contact message))))

(defun termux-sms-send-from-org ()
  "Send SMS from org buffer."
  (interactive)
  (let* ((data (termux-sms-extract-message-from-org))
         (phone (nth 0 data))
         (contact (nth 1 data))
         (message (nth 2 data)))
    (if (and phone (not (string-empty-p phone))
             message (not (string-empty-p message)))
        (progn
          (termux-sms-send phone message)
          (message "SMS sent to %s" (or contact phone))
          (kill-buffer))
      (message "Missing phone number or message content"))))

(defun termux-sms-cancel-org ()
  "Cancel SMS composition."
  (interactive)
  (when (yes-or-no-p "Cancel SMS composition? ")
    (kill-buffer)))

(defun termux-sms-reply-to-latest ()
  "Reply to the latest unread message."
  (interactive)
  (let ((unread (termux-sms-get-unread-messages)))
    (if unread
        (let* ((latest (car unread))
               (phone (alist-get 'number latest))
               (contact-name (termux-sms-find-contact-name phone)))
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward ":TO: " nil t)
              (delete-region (point) (line-end-position))
              (insert phone))
            (when (re-search-forward ":CONTACT: " nil t)
              (delete-region (point) (line-end-position))
              (insert (or contact-name "Unknown"))))
          (message "Replying to %s (%s)" (or contact-name "Unknown") phone))
      (message "No unread messages to reply to"))))

;; Quick functions
;;;###autoload
(defun termux-sms-quick-reply ()
  "Quick reply to most recent unread message."
  (interactive)
  (let ((unread (termux-sms-get-unread-messages)))
    (if unread
        (let* ((latest (car unread))
               (phone (alist-get 'number latest))
               (body (alist-get 'body latest))
               (contact-name (termux-sms-find-contact-name phone))
               (reply (read-string (format "Reply to %s (%s): "
                                           (or contact-name "Unknown")
                                           (substring body 0 (min 50 (length body)))))))
          (when (not (string-empty-p reply))
            (termux-sms-send phone reply)))
      (message "No unread messages"))))

;;;###autoload
(defun termux-sms-check-unread ()
  "Check and display unread messages."
  (interactive)
  (let ((unread (termux-sms-get-unread-messages)))
    (if unread
        (with-current-buffer (get-buffer-create "*SMS Unread*")
          (erase-buffer)
          (insert "=== UNREAD SMS MESSAGES ===\n\n")
          (dolist (msg unread)
            (let ((number (alist-get 'number msg))
                  (body (alist-get 'body msg))
                  (date (alist-get 'date msg))
                  (contact-name (termux-sms-find-contact-name number)))
              (insert (format "From: %s (%s)\n"
                              (or contact-name "Unknown") number))
              (insert (format "Date: %s\n" date))
              (insert (format "Message: %s\n" body))
              (insert "Press 'r' to reply\n")
              (insert "---\n\n")))
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)))
      (message "No unread messages"))))

;;;###autoload
(defun termux-sms-setup ()
  "Set up SMS integration and start monitoring."
  (interactive)
  (unless (file-exists-p termux-sms-contacts-file)
    (error "Contacts file not found: %s" termux-sms-contacts-file))
  (termux-sms-parse-contacts)
  (termux-sms-start-monitoring)
  (message "SMS integration ready! Use M-x termux-sms-compose-org to compose messages"))

(provide 'termux-sms)
