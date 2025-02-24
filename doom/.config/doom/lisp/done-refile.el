;;; ../../dotfiles/doom/.config/doom/lisp/done-refile.el -*- lexical-binding: t; -*-

(defun my/move-to-done-org ()
  "Move the current org heading to done.org under today's date."
  (interactive)
  (let* ((done-file (expand-file-name "~/org/done.org"))
         (today-heading (format-time-string "* %Y-%m-%d %A"))
         ;; Get the heading components
         (heading-components (org-heading-components))
         (level (nth 0 heading-components))
         (todo-state (nth 2 heading-components))
         (priority (nth 3 heading-components))
         (heading-text (nth 4 heading-components))
         ;; Construct the full heading text including DONE state if it's a task
         (full-heading (concat
                        (if todo-state
                            (if (string= todo-state "DONE")
                                "DONE "
                              "DONE ")
                          "")
                        (if priority (format "[#%s] " priority) "")
                        heading-text))
         ;; Get the entire subtree content
         (subtree-content (buffer-substring-no-properties
                           (line-beginning-position)
                           (save-excursion (org-end-of-subtree t) (point))))
         ;; Extract just the content without the heading line
         (content-body (progn
                         (string-match "\n\\(.*\\)" subtree-content)
                         (or (match-string 1 subtree-content) "")))
         ;; Create the entry to add to done.org
         (entry (format "** %s\n   CLOSED: %s\n%s"
                        full-heading
                        (format-time-string "[%Y-%m-%d %a %H:%M]")
                        content-body)))

    ;; Write to done.org
    (with-temp-buffer
      (when (file-exists-p done-file)
        (insert-file-contents done-file))
      (goto-char (point-min))

      ;; Find or create today's heading
      (if (search-forward today-heading nil t)
          (forward-line 1)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert today-heading "\n"))

      ;; Insert our entry
      (insert entry)

      ;; Write back to file
      (write-region (point-min) (point-max) done-file nil 'quiet))

    ;; Delete the original subtree
    (org-cut-subtree)

    (message "Task moved to done.org under %s" today-heading)))

;; Bind to a convenient key
(global-set-key (kbd "C-c d") 'my/move-to-done-org)

(provide 'done-refile)
