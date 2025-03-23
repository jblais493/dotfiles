;;; my-twitter.el --- Twitter integration for POSSE system -*- lexical-binding: t; -*-

;;; Commentary:
;; Twitter integration that allows posting tweets with optional media attachments.

;;; Code:

(require 'auth-source)

(defvar my-twitter-script-path "~/.config/scripts/posse-twitter.py"
  "Path to the Twitter posting Python script.")

(defun my-post-tweet ()
  "Compose and post a tweet using credentials from auth-source."
  (interactive)

  ;; First, check if the script exists
  (unless (file-exists-p my-twitter-script-path)
    (error "Twitter script not found at %s" my-twitter-script-path))

  ;; Create a buffer for composing the tweet
  (let ((buf (get-buffer-create "*Tweet Composer*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "# Compose your tweet below (280 chars max):\n\n")
      (insert "You can leave this empty for media-only tweets.")
      (org-mode)
      (goto-char (point-max))

      ;; Local variables for this buffer
      (setq-local media-path nil)

      ;; Custom keymap for this buffer
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (my-send-tweet-from-buffer)))
      (local-set-key (kbd "C-c C-a")
                     (lambda ()
                       (interactive)
                       (my-select-media-for-tweet)))
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (when (y-or-n-p "Cancel this tweet? ")
                         (kill-buffer)
                         (message "Tweet canceled.")))))

    ;; Switch to the buffer
    (switch-to-buffer buf)
    (message "Compose your tweet. C-c C-c to send, C-c C-a to attach media, C-c C-k to cancel.")))

(defun my-select-media-for-tweet ()
  "Select media to attach to the tweet."
  (interactive)
  ;; Use standard file selection dialog
  (let ((file (expand-file-name (read-file-name "Select media file: " nil nil t))))
    ;; Verify the file exists and is a supported media type
    (if (and file
             (file-exists-p file)
             (string-match-p "\\(?:png\\|jpg\\|jpeg\\|gif\\|mp4\\)$" file))
        (with-current-buffer "*Tweet Composer*"
          (setq-local media-path file)
          (message "Media selected: %s" (file-name-nondirectory file)))
      (message "Error: Selected file does not exist or is not a supported media type."))))

(defun my-send-tweet-from-buffer ()
  "Send the tweet composed in the current buffer."
  (interactive)
  (let* ((content (buffer-substring-no-properties
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line 2)
                     (point))
                   (point-max)))
         (tweet-text (string-trim content))
         (media (buffer-local-value 'media-path (current-buffer))))

    ;; Check if we have either text or media (or both)
    (cond
     ((and (string-empty-p tweet-text) (not media))
      (message "Tweet must contain either text or media (or both)."))
     ((and (not (string-empty-p tweet-text)) (> (length tweet-text) 280))
      (message "Tweet exceeds 280 characters (%d). Please shorten it."
               (length tweet-text)))
     ((and media (not (file-exists-p media)))
      (message "Selected media file does not exist: %s" media))
     (t
      ;; Get credentials from auth-source
      (let* ((consumer-key-entry (car (auth-source-search :host "api.twitter.com" :user "TwitterAPI" :max 1)))
             (consumer-secret-entry (car (auth-source-search :host "api.twitter.com.consumer" :user "TwitterAPI" :max 1)))
             (access-token-entry (car (auth-source-search :host "api.twitter.com.token" :user "TwitterAPI" :max 1)))
             (access-token-secret-entry (car (auth-source-search :host "api.twitter.com.secret" :user "TwitterAPI" :max 1)))

             ;; Get the secret functions
             (consumer-key-fn (plist-get consumer-key-entry :secret))
             (consumer-secret-fn (plist-get consumer-secret-entry :secret))
             (access-token-fn (plist-get access-token-entry :secret))
             (access-token-secret-fn (plist-get access-token-secret-entry :secret))

             ;; Get the actual values
             (consumer-key (when consumer-key-fn (funcall consumer-key-fn)))
             (consumer-secret (when consumer-secret-fn (funcall consumer-secret-fn)))
             (access-token (when access-token-fn (funcall access-token-fn)))
             (access-token-secret (when access-token-secret-fn (funcall access-token-secret-fn)))

             ;; Clean the keys (remove any whitespace, quotes, etc.)
             (ck (when consumer-key (string-trim consumer-key)))
             (cs (when consumer-secret (string-trim consumer-secret)))
             (at (when access-token (string-trim access-token)))
             (ats (when access-token-secret (string-trim access-token-secret)))

             ;; Only create a temp text file if we have text content
             (temp-file (when (not (string-empty-p tweet-text))
                          (let ((tf (make-temp-file "tweet-" nil ".txt")))
                            (with-temp-file tf
                              (insert tweet-text))
                            tf))))

        ;; Check if we have all credentials
        (if (and ck cs at ats)
            (progn
              ;; Create a Python script that handles both text and media-only tweets
              (let* ((script-file (make-temp-file "twitter-script-" nil ".py")))
                (with-temp-file script-file
                  (insert "
import tempfile
import subprocess
import tweepy
import sys
from typing import Optional

# Twitter API credentials
CONSUMER_KEY = '" ck "'
CONSUMER_SECRET = '" cs "'
ACCESS_TOKEN = '" at "'
ACCESS_TOKEN_SECRET = '" ats "'

class TwitterClient:
    def __init__(self):
        # For Twitter API v2, we only need one client instance
        self.client = tweepy.Client(
            consumer_key=CONSUMER_KEY,
            consumer_secret=CONSUMER_SECRET,
            access_token=ACCESS_TOKEN,
            access_token_secret=ACCESS_TOKEN_SECRET
        )
        # Set up auth for media uploads
        auth = tweepy.OAuthHandler(CONSUMER_KEY, CONSUMER_SECRET)
        auth.set_access_token(ACCESS_TOKEN, ACCESS_TOKEN_SECRET)
        self.api = tweepy.API(auth)

    def upload_media(self, photo_path: str) -> int:
        media = self.api.media_upload(filename=photo_path)
        return media.media_id

    def create_tweet(self, text: str = None, media_ids: Optional[list] = None) -> None:
        # Handle empty text for media-only tweets
        if not text or text.strip() == '':
            text = None
        self.client.create_tweet(text=text, media_ids=media_ids)

def main():
    try:
        twitter = TwitterClient()

        # Parse arguments
        has_text = False
        has_media = False
        tweet_text = None
        media_ids = None

        # Check for text
        if len(sys.argv) > 1 and sys.argv[1] != 'no_text':
            with open(sys.argv[1], 'r') as f:
                tweet_text = f.read().strip()
                if tweet_text:
                    has_text = True

        # Check for media
        if len(sys.argv) > 2:
            media_path = sys.argv[2]
            media_ids = [twitter.upload_media(media_path)]
            has_media = True
            print(f\"Media attached: {media_path}\")

        # Post tweet
        twitter.create_tweet(text=tweet_text, media_ids=media_ids)

        # Success message
        if has_text and has_media:
            print(f\"Tweet with media posted successfully: {tweet_text[:30]}...\")
        elif has_text:
            print(f\"Text-only tweet posted successfully: {tweet_text[:30]}...\")
        elif has_media:
            print(\"Media-only tweet posted successfully!\")

    except tweepy.errors.TweepyException as e:
        print(f\"Twitter API error: {e}\")
        sys.exit(1)
    except Exception as e:
        print(f\"An unexpected error occurred: {e}\")
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == '__main__':
    main()
"))

                ;; Execute the script with appropriate arguments
                (let* ((text-arg (if (and temp-file (not (string-empty-p tweet-text)))
                                     (shell-quote-argument temp-file)
                                   "no_text"))
                       (media-arg (when media
                                    (shell-quote-argument media)))
                       (command (if media
                                    (format "python %s %s %s"
                                            script-file
                                            text-arg
                                            media-arg)
                                  (format "python %s %s"
                                          script-file
                                          text-arg))))

                  ;; Run the command and get output
                  (let ((result (shell-command-to-string command)))

                    ;; Clean up temporary files
                    (when temp-file (delete-file temp-file))
                    (delete-file script-file)

                    ;; Show result
                    (if (string-match-p "successfully" result)
                        (progn
                          (message "Tweet sent successfully!")
                          (kill-buffer))
                      (message "Error sending tweet: %s" result))))))
          (message "Could not retrieve all required credentials from auth-source. Check your ~/.authinfo.gpg file.")))))))

;; Bind it to a key
(map! :leader
      (:prefix ("t" . "Tweet")
       :desc "Post tweet" "t" #'my-post-tweet))

(provide 'my-twitter)
;;; my-twitter.el ends here
