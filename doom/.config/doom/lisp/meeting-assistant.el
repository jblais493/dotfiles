;;; ../../dotfiles/doom/.config/doom/lisp/meeting-assistant.el -*- lexical-binding: t; -*-

(defun meeting-assistant ()
  "Prepare for upcoming meetings by gathering relevant information."
  (interactive)
  (let* ((meetings (org-meeting-extract-upcoming))
         (buffer (get-buffer-create "*Meeting Assistant*"))
         (frame (make-frame `((name . "Meeting Assistant")
                              (width . 100)
                              (height . 35)
                              (minibuffer . t)))))
    (select-frame frame)

    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)

      (if (null meetings)
          (insert "No upcoming meetings found in your calendar.\n")
        (let ((next-meeting (car meetings)))
          ;; Show next meeting
          (insert "* Next Meeting\n\n")
          (insert (format "Title: %s\n" (plist-get next-meeting :title)))
          (insert (format "Time: %s\n" (plist-get next-meeting :time)))
          (insert (format "Duration: %d minutes\n" (plist-get next-meeting :duration)))
          (insert (format "Participants: %s\n\n" (plist-get next-meeting :participants)))

          ;; Gather relevant materials
          (insert "* Relevant Materials\n\n")

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

          ;; Add action buttons
          (insert "* Actions\n\n")
          (insert-button "Start Zoom Meeting"
                         'action (lambda (_)
                                   (let ((zoom-link (plist-get next-meeting :zoom-link)))
                                     (if zoom-link
                                         (browse-url zoom-link)
                                       (browse-url "https://zoom.us/join")))))
          (insert "  ")
          (insert-button "Take Meeting Notes"
                         'action (lambda (_)
                                   (let ((meeting-buffer
                                          (find-file-noselect
                                           (format "~/org/meetings/%s.org"
                                                   (replace-regexp-in-string
                                                    "[^a-zA-Z0-9]" "-"
                                                    (plist-get next-meeting :title))))))
                                     (with-current-buffer meeting-buffer
                                       (insert (format "* Meeting: %s\n"
                                                       (plist-get next-meeting :title)))
                                       (insert (format "Date: %s\n"
                                                       (format-time-string "%Y-%m-%d")))
                                       (insert "Participants: \n\n")
                                       (insert "** Agenda\n\n")
                                       (insert "** Notes\n\n")
                                       (insert "** Action Items\n\n")
                                       (save-buffer))
                                     (switch-to-buffer meeting-buffer))))
          (insert "  ")
          (insert-button "Send Reminder"
                         'action (lambda (_)
                                   (let ((participants (plist-get next-meeting :participants)))
                                     (compose-mail)
                                     (message-goto-to)
                                     (insert participants)
                                     (message-goto-subject)
                                     (insert (format "Reminder: %s"
                                                     (plist-get next-meeting :title)))
                                     (message-goto-body)
                                     (insert (format "Just a friendly reminder about our meeting:\n\n%s\n\nTime: %s\n"
                                                     (plist-get next-meeting :title)
                                                     (plist-get next-meeting :time))))))

          ;; List other upcoming meetings
          (when (cdr meetings)
            (insert "\n\n* Other Upcoming Meetings\n\n")
            (dolist (meeting (cdr meetings))
              (insert (format "- %s (%s)\n"
                              (plist-get meeting :title)
                              (plist-get meeting :time))))))))

    (switch-to-buffer buffer)))

(defun org-meeting-extract-upcoming ()
  "Extract upcoming meetings from org agenda files."
  (let ((meetings '()))
    ;; This is a simplified approach - would need customization based on your setup
    (when (fboundp 'org-agenda-files)
      (dolist (file (org-agenda-files))
        (with-current-buffer (find-file-noselect file)
          (org-map-entries
           (lambda ()
             (let* ((heading (org-get-heading t t t t))
                    (scheduled-time (org-get-scheduled-time (point)))
                    (deadline-time (org-get-deadline-time (point)))
                    (time (or scheduled-time deadline-time))
                    (tags (org-get-tags))
                    (properties (org-entry-properties))
                    (zoom-link (cdr (assoc "ZOOM_LINK" properties)))
                    (duration-prop (cdr (assoc "DURATION" properties)))
                    (duration (if duration-prop
                                  (string-to-number duration-prop)
                                60))
                    (participants-prop (cdr (assoc "PARTICIPANTS" properties)))
                    (participants (or participants-prop "No participants specified")))
               (when (and time
                          (member "MEETING" tags)
                          (time-less-p (current-time) time))
                 (push (list :title heading
                             :time (format-time-string "%Y-%m-%d %H:%M" time)
                             :duration duration
                             :participants participants
                             :zoom-link zoom-link)
                       meetings)))))))

      ;; Sort meetings by time
      (sort meetings
            (lambda (a b)
              (string< (plist-get a :time) (plist-get b :time))))))
