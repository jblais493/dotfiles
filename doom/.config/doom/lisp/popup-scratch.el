;;; ../../dotfiles/doom/.config/doom/lisp/popup-scratch.el -*- lexical-binding: t; -*-


(defun popup-scratch-for-web ()
  "Create a popup frame with a scratch buffer for web text editing.
Designed specifically for GNOME Wayland with Doom Emacs spell checking."
  (interactive)
  (let ((buffer (get-buffer-create "*web-compose*")))
    (with-current-buffer buffer
      (erase-buffer)
      ;; Use text mode
      (text-mode)

      ;; For Doom Emacs, enable appropriate spell checking mode
      ;; This should enable the z= binding for spell suggestions
      (when (fboundp 'spell-checking-enable)
        (spell-checking-enable))

      ;; Add header with instructions
      (insert "# Compose your text below, then press C-c C-c when done.\n")
      (insert "# Use z= on misspelled words to see correction suggestions.\n")
      (insert "# Text will be copied to clipboard for pasting.\n\n")

      ;; Define our finish function
      (fset 'web-compose-finish
            (lambda ()
              (interactive)
              ;; Extract content (skipping header comments)
              (let ((content (buffer-substring-no-properties
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line 4)
                                (point))
                              (point-max))))

                ;; Copy to Emacs clipboard
                (kill-new content)

                ;; For Wayland - use wl-copy which is the most compatible
                (when (executable-find "wl-copy")
                  (call-process "wl-copy" nil nil nil content))

                ;; Message user about next steps
                (message "Text copied to clipboard! Ready to paste with Ctrl+V")

                ;; Store frame to close
                (let ((frame-to-close (selected-frame)))
                  ;; Close frame after a short delay
                  (run-with-timer 0.5 nil
                                  (lambda ()
                                    (delete-frame frame-to-close)))))))

      ;; Bind our function to the local map
      (local-set-key (kbd "C-c C-c") 'web-compose-finish)

      ;; Set up mode line to indicate Doom/Evil spell check is available
      (setq mode-line-format
            (list "-- WEB COMPOSE (WAYLAND) -- Use z= for spelling -- C-c C-c when done ")))

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
      (forward-line 4)

      ;; Center the frame using a timer
      (run-with-timer 0.2 nil
                      (lambda ()
                        (let* ((display-width (display-pixel-width))
                               (display-height (display-pixel-height))
                               (frame-width (frame-pixel-width))
                               (frame-height (frame-pixel-height))
                               (left-pos (max 0 (/ (- display-width frame-width) 2)))
                               (top-pos (max 0 (/ (- display-height frame-height) 2))))
                          (set-frame-position frame left-pos top-pos)))))))
