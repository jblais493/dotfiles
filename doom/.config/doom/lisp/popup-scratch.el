;;; ../../dotfiles/doom/.config/doom/lisp/popup-scratch.el -*- lexical-binding: t; -*-


(defun popup-scratch-for-web ()
  "Create a popup frame with a scratch buffer optimized for Wayland environments."
  (interactive)
  ;; Create our scratch buffer
  (let ((buffer (get-buffer-create "*web-compose*")))
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (insert "# Compose your text below, then press C-c C-c when done.\n")
      (insert "# Text will be copied to clipboard, ready to paste.\n\n")

      ;; Define our finish function
      (fset 'web-compose-finish
            (lambda ()
              (interactive)
              ;; Extract content (skipping header comments)
              (let ((content (buffer-substring-no-properties
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line 3)
                                (point))
                              (point-max))))

                ;; Copy to Emacs clipboard (this is fast and non-blocking)
                (kill-new content)

                ;; Also try wl-copy WITHOUT --foreground flag (non-blocking)
                (when (executable-find "wl-copy")
                  (start-process "wl-copy" nil "wl-copy" content))

                ;; Message and setup to close frame
                (message "Text copied to clipboard! Closing frame...")

                ;; Store frame to close
                (let ((frame-to-close (selected-frame)))
                  ;; Close frame after a short delay
                  (run-with-timer 0.5 nil
                                  (lambda ()
                                    (delete-frame frame-to-close)))))))

      ;; Bind our function directly to the local map
      (local-set-key (kbd "C-c C-c") 'web-compose-finish)

      ;; Set up mode line
      (setq mode-line-format
            (list "-- WEB COMPOSE (WAYLAND) -- Press C-c C-c when done "))))

  ;; Create the frame
  (let ((frame (make-frame `((name . "Web Compose")
                             (width . 80)
                             (height . 30)
                             (minibuffer . nil)
                             (vertical-scroll-bars . nil)
                             (menu-bar-lines . 0)
                             (tool-bar-lines . 0)))))

    ;; Set up the frame
    (select-frame frame)
    (switch-to-buffer "*web-compose*")

    ;; Position cursor after comments
    (goto-char (point-min))
    (forward-line 3)

    ;; Center the frame using a timer
    (run-with-timer 0.2 nil
                    (lambda ()
                      (let* ((display-width (display-pixel-width))
                             (display-height (display-pixel-height))
                             (frame-width (frame-pixel-width))
                             (frame-height (frame-pixel-height))
                             (left-pos (max 0 (/ (- display-width frame-width) 2)))
                             (top-pos (max 0 (/ (- display-height frame-height) 2))))
                        (set-frame-position frame left-pos top-pos))))))
