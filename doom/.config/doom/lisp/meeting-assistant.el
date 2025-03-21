;;; meeting-assistant.el --- Manage and prepare for meetings -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides functionality to manage meetings, prepare for them,
;; set up reminders, and create notes across different meeting platforms.
;; It supports Zoom, Microsoft Teams, Google Meet, Jitsi, and others.

;;; Code:

(require 'org)
(require 'browse-url)

(defgroup meeting-assistant nil
  "Customization options for meeting-assistant."
  :group 'applications)

(defcustom meeting-notes-directory "~/org/meetings/"
  "Directory where meeting notes are stored."
  :type 'directory
  :group 'meeting-assistant)

(defcustom meeting-reminder-times '(15 5 1)
  "List of times (in minutes) before meetings to send reminders."
  :type '(repeat integer)
  :group 'meeting-assistant)

;; Core functions for meeting link handling
(defun meeting-extract-link-from-properties (properties)
  "Extract meeting link from properties, checking multiple possible property names."
  (or (cdr (assoc "ZOOM_LINK" properties))
      (cdr (assoc "TEAMS_LINK" properties))
      (cdr (assoc "MEET_LINK" properties))
      (cdr (assoc "JITSI_LINK" properties))
      (cdr (assoc "MEETING_LINK" properties))
      (cdr (assoc "URL" properties))
      (cdr (assoc "LINK" properties))))

(defun meeting-detect-platform (link)
  "Detect meeting platform based on the URL."
  (cond
   ((null link) "Unknown")
   ((string-match-p "zoom\\.us" link) "Zoom")
   ((string-match-p "teams\\.microsoft" link) "Microsoft Teams")
   ((string-match-p "meet\\.google" link) "Google Meet")
   ((string-match-p "jitsi" link) "Jitsi")
   ((string-match-p "webex" link) "Webex")
   ((string-match-p "bluejeans" link) "BlueJeans")
   (t "Other")))

(defun meeting-open-link (link)
  "Open meeting link with appropriate action based on platform."
  (when link
    (browse-url link)))

;; Reminder system
(defun meeting-set-reminder (meeting minutes-before)
  "Set a reminder for MEETING that will trigger MINUTES-BEFORE the meeting starts."
  (let* ((meeting-time (date-to-time (plist-get meeting :time)))
         (reminder-time (time-subtract meeting-time (seconds-to-time (* 60 minutes-before))))
         (title (plist-get meeting :title))
         (meeting-link (plist-get meeting :meeting-link))
         (platform (plist-get meeting :platform)))
    (run-at-time reminder-time nil
                 (lambda ()
                   (let ((notif-message (format "%s meeting '%s' starting in %d minutes"
                                                platform title minutes-before)))
                     ;; Display a notification that stays on screen
                     (when (fboundp 'notifications-notify)
                       (notifications-notify :title "Meeting Reminder"
                                             :body notif-message
                                             :timeout 60000))
                     (message notif-message)
                     (when (y-or-n-p "Join now? ")
                       (meeting-open-link meeting-link)))))))

(defun meeting-set-all-reminders (meeting)
  "Set multiple reminders for a meeting at different intervals."
  (dolist (time meeting-reminder-times)
    (meeting-set-reminder meeting time)))

;; Calendar integration
(defun meeting-add-to-calendar (meeting)
  "Add meeting to external calendar using ical format."
  (let* ((title (plist-get meeting :title))
         (start-time (date-to-time (plist-get meeting :time)))
         (duration (plist-get meeting :duration))
         (end-time (time-add start-time (seconds-to-time (* 60 duration))))
         (link (plist-get meeting :meeting-link))
         (platform (plist-get meeting :platform))
         (participants (plist-get meeting :participants))
         (uid (format "%s-%s" (format-time-string "%Y%m%dT%H%M%S" start-time)
                      (replace-regexp-in-string "[^a-zA-Z0-9]" "-" title)))
         (ical-file (expand-file-name (format "%s.ics" uid) temporary-file-directory)))

    (with-temp-file ical-file
      (insert "BEGIN:VCALENDAR\n")
      (insert "VERSION:2.0\n")
      (insert "PRODID:-//Emacs//Meeting Assistant//EN\n")
      (insert "BEGIN:VEVENT\n")
      (insert (format "UID:%s\n" uid))
      (insert (format "SUMMARY:%s\n" title))
      (insert (format "DTSTART:%s\n"
                      (format-time-string "%Y%m%dT%H%M%S" start-time)))
      (insert (format "DTEND:%s\n"
                      (format-time-string "%Y%m%dT%H%M%S" end-time)))
      (insert (format "DESCRIPTION:Platform: %s\\nParticipants: %s\\n"
                      platform participants))
      (when link
        (insert (format "URL:%s\n" link)))
      (insert "END:VEVENT\n")
      (insert "END:VCALENDAR\n"))

    (if (fboundp 'browse-url-file)
        (browse-url-file ical-file)
      (browse-url (concat "file://" ical-file)))))

;; Meeting history and past notes
(defun meeting-view-history ()
  "View meeting history and previous notes."
  (interactive)
  (let* ((meetings-dir (expand-file-name meeting-notes-directory))
         (files (when (file-exists-p meetings-dir)
                  (directory-files meetings-dir t "\\.org$")))
         (buffer (get-buffer-create "*Meeting History*")))

    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)

      (insert "* Meeting History\n\n")

      (if (not files)
          (insert "No meeting notes found.\n")
        (setq files (sort files (lambda (a b)
                                  (let ((time-a (nth 5 (file-attributes a)))
                                        (time-b (nth 5 (file-attributes b))))
                                    (time-less-p time-b time-a)))))
        (dolist (file files)
          (let ((file-name (file-name-nondirectory file))
                (mod-time (format-time-string "%Y-%m-%d"
                                              (nth 5 (file-attributes file)))))
            (insert-button (format "%s (%s)"
                                   (file-name-sans-extension file-name)
                                   mod-time)
                           'action (lambda (_) (find-file file)))
            (insert "\n")))))

    (switch-to-buffer buffer)))

;; Meeting material preparation
(defun meeting-prepare-materials (meeting)
  "Prepare materials for a meeting by searching relevant files."
  (let* ((title (plist-get meeting :title))
         (participants (plist-get meeting :participants))
         (participant-names (split-string participants "," t "[ \t]+"))
         (search-terms (cons title participant-names))
         (buffer (get-buffer-create "*Meeting Materials*")))

    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)

      (insert (format "* Materials for: %s\n\n" title))

      (dolist (term search-terms)
        (when (and term (not (string-empty-p term)))
          (insert (format "** Results for \"%s\"\n\n" term))

          ;; Search in org files
          (when (fboundp 'org-agenda-files)
            (dolist (file (org-agenda-files))
              (when (file-exists-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (while (search-forward term nil t)
                    (let* ((context-start (max (point-min) (- (point) 100)))
                           (context-end (min (point-max) (+ (point) 100)))
                           (context (buffer-substring context-start context-end)))
                      (with-current-buffer buffer
                        (insert (format "- Found in %s:\n" (file-name-nondirectory file)))
                        (insert (format "  %s\n\n"
                                        (replace-regexp-in-string "\n" " " context)))))))))))))

    (switch-to-buffer buffer)))

;; Meeting note taking
(defun meeting-create-notes (meeting)
  "Create a structured note file for the meeting."
  (let* ((title (plist-get meeting :title))
         (sanitized-title (replace-regexp-in-string
                           "[^a-zA-Z0-9]" "-" title))
         (date-str (format-time-string "%Y-%m-%d"))
         (file-name (format "%s/%s-%s.org"
                            meeting-notes-directory
                            date-str sanitized-title))
         (meeting-buffer (progn
                           (unless (file-exists-p meeting-notes-directory)
                             (make-directory meeting-notes-directory t))
                           (find-file-noselect file-name))))
    (with-current-buffer meeting-buffer
      (erase-buffer)  ;; Clear any existing content
      (insert (format "#+TITLE: %s\n" title))
      (insert (format "#+DATE: %s\n" date-str))
      (insert "#+STARTUP: overview\n\n")
      (insert (format "* Meeting: %s\n" title))
      (insert (format "- Date: %s\n" date-str))
      (insert (format "- Platform: %s\n"
                      (plist-get meeting :platform)))
      (insert (format "- Participants: %s\n\n"
                      (plist-get meeting :participants)))
      (insert "* Agenda\n\n")
      (insert "* Notes\n\n")
      (insert "* Action Items\n\n")
      (insert "* Decisions\n\n")
      (insert "* Follow-up\n\n")
      (save-buffer))
    (switch-to-buffer meeting-buffer)))

;; Meeting extraction from org files
;; Meeting extraction from org files
(defun org-meeting-extract-upcoming ()
  "Extract upcoming meetings from org agenda files."
  (let ((meetings '()))
    ;; This is a simplified approach - would need customization based on your setup
    (when (fboundp 'org-agenda-files)
      (dolist (file (org-agenda-files))
        (when (file-exists-p file)
          (with-current-buffer (find-file-noselect file)
            (org-map-entries
             (lambda ()
               (let* ((heading (org-get-heading t t t t))
                      (scheduled-time (org-get-scheduled-time (point)))
                      (deadline-time (org-get-deadline-time (point)))
                      (time (or scheduled-time deadline-time))
                      (tags (org-get-tags))
                      (properties (org-entry-properties))
                      (meeting-link (meeting-extract-link-from-properties properties))
                      (platform (meeting-detect-platform meeting-link))
                      (duration-prop (cdr (assoc "DURATION" properties)))
                      (duration (if duration-prop
                                    (string-to-number duration-prop)
                                  60))
                      (participants-prop (cdr (assoc "PARTICIPANTS" properties)))
                      (participants (or participants-prop "No participants specified")))
                 (when (and time
                            (or (member "MEETING" tags)
                                (string-match-p "meeting\\|call\\|conference" heading))
                            (time-less-p (current-time) time))
                   (push (list :title heading
                               :time (format-time-string "%Y-%m-%d %H:%M" time)
                               :duration duration
                               :participants participants
                               :meeting-link meeting-link
                               :platform platform)
                         meetings))))))))

      ;; Sort meetings by time
      (sort meetings
            (lambda (a b)
              (string< (plist-get a :time) (plist-get b :time)))))))

;; Main entry point
(defun meeting-assistant ()
  "Prepare for upcoming meetings by gathering relevant information."
  (interactive)
  (let* ((meetings (org-meeting-extract-upcoming))
         (buffer-name "*Meeting Assistant*")
         (buffer (get-buffer-create buffer-name)))

    ;; Create a new frame
    (let ((frame (make-frame `((name . "Meeting Assistant")
                               (width . 100)
                               (height . 40)
                               (minibuffer . t)))))
      (select-frame frame)

      ;; Populate the buffer
      (with-current-buffer buffer
        (erase-buffer)
        (when (fboundp 'org-mode)
          (org-mode))
        (insert "#+TITLE: Meeting Assistant\n\n")

        (if (null meetings)
            (progn
              (insert "No upcoming meetings found in your calendar.\n\n")
              (when (fboundp 'insert-button)
                (insert-button "View Meeting History"
                               'action (lambda (_)
                                         (when (fboundp 'meeting-view-history)
                                           (meeting-view-history))))
                (insert "\n")))

          (let ((next-meeting (car meetings)))
            ;; Show next meeting
            (insert "* Next Meeting\n\n")
            (insert (format "Title: %s\n" (plist-get next-meeting :title)))
            (insert (format "Time: %s\n" (plist-get next-meeting :time)))
            (insert (format "Platform: %s\n" (plist-get next-meeting :platform)))
            (insert (format "Duration: %d minutes\n" (plist-get next-meeting :duration)))
            (insert (format "Participants: %s\n" (plist-get next-meeting :participants)))
            (when (plist-get next-meeting :meeting-link)
              (insert (format "Link: %s\n" (plist-get next-meeting :meeting-link))))
            (insert "\n")

            ;; Main action buttons
            (insert "* Actions\n\n")
            (insert-button (format "Join %s Meeting" (plist-get next-meeting :platform))
                           'action (lambda (_)
                                     (let ((meeting-link (plist-get next-meeting :meeting-link)))
                                       (if meeting-link
                                           (meeting-open-link meeting-link)
                                         (message "No meeting link available")))))
            (insert "  ")
            (insert-button "Take Meeting Notes"
                           'action (lambda (_) (meeting-create-notes next-meeting)))
            (insert "  ")
            (insert-button "Send Reminder"
                           'action (lambda (_)
                                     (let ((participants (plist-get next-meeting :participants))
                                           (platform (plist-get next-meeting :platform))
                                           (meeting-link (plist-get next-meeting :meeting-link)))
                                       (compose-mail)
                                       (message-goto-to)
                                       (insert participants)
                                       (message-goto-subject)
                                       (insert (format "Reminder: %s" (plist-get next-meeting :title)))
                                       (message-goto-body)
                                       (insert (format "Just a friendly reminder about our meeting:\n\n%s\n\nTime: %s\nPlatform: %s\n"
                                                       (plist-get next-meeting :title)
                                                       (plist-get next-meeting :time)
                                                       platform))
                                       (when meeting-link
                                         (insert (format "Link: %s\n" meeting-link)))))))
          (insert "  ")
          (insert-button "Add to Calendar"
                         'action (lambda (_) (meeting-add-to-calendar next-meeting)))
          (insert "  ")
          (insert-button "Prepare Materials"
                         'action (lambda (_) (meeting-prepare-materials next-meeting)))
          (insert "  ")
          (insert-button "View Meeting History"
                         'action (lambda (_) (meeting-view-history)))

          ;; Gather relevant materials
          (insert "\n\n* Relevant Materials\n\n")

          ;; Search for related notes
          (let ((search-term (plist-get next-meeting :title))
                (related-notes nil))
            (with-temp-buffer
              (if (fboundp 'org-search-view)
                  (progn
                    (org-search-view nil search-term)
                    (setq related-notes (buffer-string)))
                (insert "Org search not available")))

            (if related-notes
                (progn
                  (insert "** Related Notes\n")
                  (insert related-notes)
                  (insert "\n"))
              (insert "No related notes found for this meeting.\n")))

          ;; List other upcoming meetings
          (when (cdr meetings)
            (insert "\n* Other Upcoming Meetings\n\n")
            (dolist (meeting (cdr meetings))
              (insert-button (format "%s (%s, %s)"
                                     (plist-get meeting :title)
                                     (plist-get meeting :time)
                                     (plist-get meeting :platform))
                             'action (lambda (_)
                                       (let ((buffer (get-buffer-create "*Meeting Details*")))
                                         (with-current-buffer buffer
                                           (erase-buffer)
                                           (org-mode)
                                           (insert (format "* %s\n\n" (plist-get meeting :title)))
                                           (insert (format "Time: %s\n" (plist-get meeting :time)))
                                           (insert (format "Platform: %s\n" (plist-get meeting :platform)))
                                           (insert (format "Duration: %d minutes\n" (plist-get meeting :duration)))
                                           (insert (format "Participants: %s\n" (plist-get meeting :participants)))
                                           (when (plist-get meeting :meeting-link)
                                             (insert (format "Link: %s\n\n" (plist-get meeting :meeting-link))))

                                           (insert "** Actions\n\n")
                                           (insert-button "Join Meeting"
                                                          'action (lambda (_)
                                                                    (meeting-open-link (plist-get meeting :meeting-link))))
                                           (insert "  ")
                                           (insert-button "Take Notes"
                                                          'action (lambda (_)
                                                                    (meeting-create-notes meeting)))
                                           (insert "  ")
                                           (insert-button "Add to Calendar"
                                                          'action (lambda (_)
                                                                    (meeting-add-to-calendar meeting))))
                                         (switch-to-buffer buffer))))
              (insert "\n")))))))

  (switch-to-buffer buffer))

;; Command to quickly join the next meeting
(defun meeting-quick-join ()
  "Quickly join the next scheduled meeting without showing the full interface."
  (interactive)
  (let ((meetings (org-meeting-extract-upcoming)))
    (if (null meetings)
        (message "No upcoming meetings found.")
      (let* ((next-meeting (car meetings))
             (meeting-link (plist-get next-meeting :meeting-link))
             (title (plist-get next-meeting :title))
             (platform (plist-get next-meeting :platform)))
        (if meeting-link
            (progn
              (message "Joining %s meeting: %s" platform title)
              (meeting-open-link meeting-link))
          (message "No meeting link available for %s" title))))))

;; Define mode for the meeting assistant
(define-minor-mode meeting-assistant-mode
  "Toggle Meeting Assistant mode.
When enabled, shows upcoming meetings and provides tools to prepare for them."
  :lighter " Meeting"
  :global t
  (if meeting-assistant-mode
      (progn
        (meeting-assistant)
        (message "Meeting Assistant mode enabled"))
    (message "Meeting Assistant mode disabled")))

(provide 'meeting-assistant)
;;; meeting-assistant.el ends here
