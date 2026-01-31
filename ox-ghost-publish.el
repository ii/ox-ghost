;;; ox-ghost-publish.el --- Publish org-mode to Ghost CMS -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ii.coop
;; Author: ii.coop
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (ox-ghost "0.8.0"))
;; Keywords: org, ghost, lexical, publishing

;;; Commentary:

;; Provides a complete workflow for publishing org-mode files to Ghost CMS.
;; Separates concerns into: Generate → Enrich → Preview → Publish
;;
;; Part of the ox-ghost package.

;;; Code:

(require 'org)
(require 'json)
(require 'url)
(require 'cl-lib)

;;;; Configuration

(defgroup ghost-publish nil
  "Publish org-mode to Ghost CMS."
  :group 'org
  :prefix "ghost-publish-")

(defcustom ghost-publish-url nil
  "Ghost site URL (e.g., \"https://your-site.ghost.io\").
Must be set before publishing."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "URL"))
  :group 'ghost-publish)

(defcustom ghost-publish-script nil
  "Path to ghost.js CLI script.
If nil, looks for ghost.js in the ox-ghost directory."
  :type '(choice (const :tag "Auto-detect" nil)
                 (file :tag "Custom path"))
  :group 'ghost-publish)

(defcustom ghost-publish-temp-dir nil
  "Temporary directory for generated files. If nil, uses `make-temp-file'."
  :type '(choice (const nil) string)
  :group 'ghost-publish)

(defconst ghost-publish--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where ghost-publish is installed.")

;;;; Utility Functions

(defun ghost-publish--find-script ()
  "Find ghost.js script path.
Checks `ghost-publish-script', then the package directory."
  (or ghost-publish-script
      (let ((local (expand-file-name "ghost.js" ghost-publish--directory)))
        (when (file-exists-p local) local))
      (error "ghost.js not found. Set `ghost-publish-script'")))

(defun ghost-publish--script-dir ()
  "Get directory containing ghost.js."
  (file-name-directory (ghost-publish--find-script)))

(defun ghost-publish--temp-dir ()
  "Get or create temporary directory."
  (or ghost-publish-temp-dir
      (make-temp-file "ghost-" t)))

(defun ghost-publish--run-command (command)
  "Run COMMAND and return output as string."
  (string-trim (shell-command-to-string command)))

(defun ghost-publish--upload-file (file)
  "Upload FILE to Ghost, return URL."
  (let ((output (ghost-publish--run-command
                 (format "cd %s && node ghost.js upload %s 2>&1 | grep -o 'https://[^ ]*'"
                         (ghost-publish--script-dir)
                         (shell-quote-argument file)))))
    (if (string-prefix-p "https://" output)
        output
      (error "Upload failed: %s" output))))

;;;; Phase 1: Generation Functions

(defun ghost-tts (text &optional name)
  "Generate TTS audio from TEXT, upload to Ghost.
Optional NAME for the file (default: tts-TIMESTAMP).
Returns org AUDIO block syntax."
  (let* ((tmpdir (ghost-publish--temp-dir))
         (name (or name (format "tts-%s" (format-time-string "%s"))))
         (wav (expand-file-name (concat name ".wav") tmpdir))
         (mp3 (expand-file-name (concat name ".mp3") tmpdir)))
    ;; Generate speech
    (ghost-publish--run-command
     (format "espeak-ng %s -w %s 2>/dev/null"
             (shell-quote-argument text)
             (shell-quote-argument wav)))
    ;; Convert to MP3
    (ghost-publish--run-command
     (format "ffmpeg -i %s -c:a libmp3lame -q:a 2 %s -y 2>/dev/null"
             (shell-quote-argument wav)
             (shell-quote-argument mp3)))
    ;; Upload and return org syntax
    (let ((url (ghost-publish--upload-file mp3)))
      ;; Cleanup
      (delete-file wav)
      (delete-file mp3)
      (format "#+BEGIN_AUDIO :src %s\n%s\n#+END_AUDIO" url text))))

(defun ghost-image (text &optional color1 color2 name)
  "Generate gradient image with TEXT overlay, upload to Ghost.
COLOR1 and COLOR2 are gradient colors (default: purple gradient).
Optional NAME for the file.
Returns org image link syntax."
  (let* ((tmpdir (ghost-publish--temp-dir))
         (color1 (or color1 "#667eea"))
         (color2 (or color2 "#764ba2"))
         (name (or name (format "img-%s" (format-time-string "%s"))))
         (png (expand-file-name (concat name ".png") tmpdir)))
    ;; Generate image
    (ghost-publish--run-command
     (format "magick -size 800x400 -define gradient:angle=135 'gradient:%s-%s' -font Helvetica-Bold -pointsize 42 -fill white -gravity center -annotate +0+0 %s %s 2>/dev/null"
             color1 color2
             (shell-quote-argument text)
             (shell-quote-argument png)))
    ;; Upload and return org syntax
    (let ((url (ghost-publish--upload-file png)))
      ;; Cleanup
      (delete-file png)
      (format "[[%s][%s]]" url text))))

(defun ghost-video (text &optional bgcolor name)
  "Generate video with TEXT overlay and TTS narration, upload to Ghost.
BGCOLOR is background color (default: dark).
Optional NAME for the file.
Returns org VIDEO block syntax."
  (let* ((tmpdir (ghost-publish--temp-dir))
         (bgcolor (or bgcolor "#2d3436"))
         (name (or name (format "vid-%s" (format-time-string "%s"))))
         (wav (expand-file-name "audio.wav" tmpdir))
         (mp4 (expand-file-name (concat name ".mp4") tmpdir)))
    ;; Generate speech
    (ghost-publish--run-command
     (format "espeak-ng %s -w %s 2>/dev/null"
             (shell-quote-argument text)
             (shell-quote-argument wav)))
    ;; Generate video with audio
    (ghost-publish--run-command
     (format "ffmpeg -f lavfi -i 'color=c=%s:s=640x360:d=10' -i %s -vf \"drawtext=text='%s':fontsize=36:fontcolor=white:x=(w-text_w)/2:y=(h-text_h)/2\" -c:v libx264 -c:a aac -pix_fmt yuv420p -shortest %s -y 2>/dev/null"
             bgcolor
             (shell-quote-argument wav)
             (ghost-publish--escape-ffmpeg-text text)
             (shell-quote-argument mp4)))
    ;; Upload and return org syntax
    (let ((url (ghost-publish--upload-file mp4)))
      ;; Cleanup
      (delete-file wav)
      (delete-file mp4)
      (format "#+BEGIN_VIDEO :src %s\n%s\n#+END_VIDEO" url text))))

(defun ghost-publish--escape-ffmpeg-text (text)
  "Escape TEXT for ffmpeg drawtext filter."
  (replace-regexp-in-string "'" "'\\''" text))

(defun ghost-upload (file)
  "Upload FILE to Ghost, return URL."
  (interactive "fFile to upload: ")
  (let ((url (ghost-publish--upload-file file)))
    (when (called-interactively-p 'any)
      (message "Uploaded: %s" url)
      (kill-new url))
    url))

;;;; Phase 2: Enrichment Functions

(defun ghost-publish--probe-media (url)
  "Probe media at URL for metadata. Returns plist."
  (let* ((output (ghost-publish--run-command
                  (format "ffprobe -v error -select_streams v:0 -show_entries stream=width,height,duration -show_entries format=duration -of json %s 2>/dev/null"
                          (shell-quote-argument url))))
         (json (condition-case nil
                   (json-read-from-string output)
                 (error nil))))
    (when json
      (let* ((streams (cdr (assoc 'streams json)))
             (stream (and streams (aref streams 0)))
             (format-info (cdr (assoc 'format json)))
             (width (cdr (assoc 'width stream)))
             (height (cdr (assoc 'height stream)))
             (duration (or (cdr (assoc 'duration stream))
                           (cdr (assoc 'duration format-info)))))
        (list :width width
              :height height
              :duration (and duration (string-to-number duration)))))))

(defun ghost-publish--probe-audio (url)
  "Probe audio at URL for metadata. Returns plist."
  (let* ((output (ghost-publish--run-command
                  (format "ffprobe -v error -show_entries format=duration -of json %s 2>/dev/null"
                          (shell-quote-argument url))))
         (json (condition-case nil
                   (json-read-from-string output)
                 (error nil))))
    (when json
      (let* ((format-info (cdr (assoc 'format json)))
             (duration (cdr (assoc 'duration format-info))))
        (list :duration (and duration (string-to-number duration)))))))

(defun ghost-publish--get-mimetype (url)
  "Determine MIME type from URL extension."
  (let ((ext (file-name-extension url)))
    (cond
     ((member ext '("mp4" "m4v")) "video/mp4")
     ((string= ext "webm") "video/webm")
     ((string= ext "ogv") "video/ogg")
     ((string= ext "mp3") "audio/mpeg")
     ((string= ext "ogg") "audio/ogg")
     ((string= ext "wav") "audio/wav")
     ((string= ext "m4a") "audio/mp4")
     (t nil))))

(defun ghost-publish--youtube-embed-html (url)
  "Generate iframe HTML for YouTube URL."
  (when (string-match "youtube\\.com/watch\\?v=\\([^&]+\\)" url)
    (let ((video-id (match-string 1 url)))
      (format "<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/%s\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen></iframe>"
              video-id))))

(defun ghost-publish--vimeo-embed-html (url)
  "Generate iframe HTML for Vimeo URL."
  (when (string-match "vimeo\\.com/\\([0-9]+\\)" url)
    (let ((video-id (match-string 1 url)))
      (format "<iframe src=\"https://player.vimeo.com/video/%s\" width=\"560\" height=\"315\" frameborder=\"0\" allow=\"autoplay; fullscreen; picture-in-picture\" allowfullscreen></iframe>"
              video-id))))

(defun ghost-publish--fetch-og-tags (url)
  "Fetch Open Graph metadata from URL. Returns plist."
  (let* ((output (ghost-publish--run-command
                  (format "curl -sL %s | grep -oE '<meta property=\"og:[^\"]+\" content=\"[^\"]*\"' | head -10"
                          (shell-quote-argument url))))
         (result nil))
    (dolist (line (split-string output "\n" t))
      (when (string-match "og:\\([^\"]+\\)\" content=\"\\([^\"]*\\)\"" line)
        (let ((prop (match-string 1 line))
              (val (match-string 2 line)))
          (cond
           ((string= prop "title") (setq result (plist-put result :title val)))
           ((string= prop "description") (setq result (plist-put result :description val)))
           ((string= prop "image") (setq result (plist-put result :thumbnail val)))))))
    result))

(defun ghost-enrich-at-point ()
  "Enrich the special block at point with metadata."
  (interactive)
  (save-excursion
    (let* ((element (org-element-at-point))
           (type (org-element-type element)))
      (when (eq type 'special-block)
        (let* ((block-type (org-element-property :type element))
               (begin (org-element-property :begin element))
               (end (org-element-property :end element)))
          (cond
           ((string= block-type "VIDEO")
            (ghost-publish--enrich-video-block begin end))
           ((string= block-type "AUDIO")
            (ghost-publish--enrich-audio-block begin end))
           ((string= block-type "EMBED")
            (ghost-publish--enrich-embed-block begin end))
           ((string= block-type "BOOKMARK")
            (ghost-publish--enrich-bookmark-block begin end))
           (t (message "No enrichment available for %s blocks" block-type))))))))

(defun ghost-publish--strip-attributes (attrs)
  "Remove ATTRS (list of attribute names) from current line.
Point should be on the line to modify."
  (let ((line-beg (line-beginning-position))
        (line-end (line-end-position)))
    (dolist (attr attrs)
      (save-excursion
        (goto-char line-beg)
        ;; Match :attr followed by value - handle quoted strings with escapes
        (while (re-search-forward
                (format " %s \\(?:\"\\(?:[^\"\\\\]\\|\\\\.\\)*\"\\|[^[:space:]]+\\)"
                        (regexp-quote attr))
                line-end t)
          (replace-match "")
          ;; Update line-end after deletion
          (setq line-end (line-end-position)))))))

(defun ghost-publish--enrich-video-block (begin end)
  "Enrich VIDEO block between BEGIN and END."
  (goto-char begin)
  (when (re-search-forward "^#\\+BEGIN_VIDEO\\s-+:src\\s-+\\(https?://[^ \n]+\\)" end t)
    (let* ((url (match-string 1))
           (meta (ghost-publish--probe-media url))
           (mimetype (ghost-publish--get-mimetype url)))
      ;; Remove existing metadata attributes before adding new ones
      (ghost-publish--strip-attributes '(":width" ":height" ":duration" ":mimeType"))
      ;; Add new metadata at end of line
      (goto-char (line-end-position))
      (when (plist-get meta :width)
        (insert (format " :width %d" (plist-get meta :width))))
      (when (plist-get meta :height)
        (insert (format " :height %d" (plist-get meta :height))))
      (when (plist-get meta :duration)
        (insert (format " :duration %.1f" (plist-get meta :duration))))
      (when mimetype
        (insert (format " :mimeType %s" mimetype)))
      (message "Enriched VIDEO block: %dx%d, %.1fs"
               (or (plist-get meta :width) 0)
               (or (plist-get meta :height) 0)
               (or (plist-get meta :duration) 0)))))

(defun ghost-publish--enrich-audio-block (begin end)
  "Enrich AUDIO block between BEGIN and END."
  (goto-char begin)
  (when (re-search-forward "^#\\+BEGIN_AUDIO\\s-+:src\\s-+\\(https?://[^ \n]+\\)" end t)
    (let* ((url (match-string 1))
           (meta (ghost-publish--probe-audio url))
           (mimetype (ghost-publish--get-mimetype url)))
      ;; Remove existing metadata attributes before adding new ones
      (ghost-publish--strip-attributes '(":duration" ":mimeType"))
      ;; Add new metadata at end of line
      (goto-char (line-end-position))
      (when (plist-get meta :duration)
        (insert (format " :duration %.1f" (plist-get meta :duration))))
      (when mimetype
        (insert (format " :mimeType %s" mimetype)))
      (message "Enriched AUDIO block: %.1fs" (or (plist-get meta :duration) 0)))))

(defun ghost-publish--enrich-embed-block (begin end)
  "Enrich EMBED block between BEGIN and END."
  (goto-char begin)
  (when (re-search-forward "^#\\+BEGIN_EMBED\\s-+:url\\s-+\\(https?://[^ \n]+\\)" end t)
    (let* ((url (match-string 1))
           (html (or (ghost-publish--youtube-embed-html url)
                     (ghost-publish--vimeo-embed-html url))))
      (when html
        ;; Remove existing :html attributes before adding new one
        (ghost-publish--strip-attributes '(":html"))
        ;; Add new html at end of line
        (goto-char (line-end-position))
        (insert (format " :html %S" html))
        (message "Enriched EMBED block with iframe HTML")))))

(defun ghost-publish--enrich-bookmark-block (begin end)
  "Enrich BOOKMARK block between BEGIN and END."
  (goto-char begin)
  (when (re-search-forward "^#\\+BEGIN_BOOKMARK\\s-+:url\\s-+\\(https?://[^ \n]+\\)" end t)
    (let* ((url (match-string 1))
           (og (ghost-publish--fetch-og-tags url)))
      ;; Remove existing metadata attributes before adding new ones
      (ghost-publish--strip-attributes '(":title" ":description" ":thumbnail" ":icon"))
      ;; Add new metadata at end of line
      (goto-char (line-end-position))
      (when (plist-get og :title)
        (insert (format " :title %S" (plist-get og :title))))
      (when (plist-get og :description)
        (insert (format " :description %S" (plist-get og :description))))
      (when (plist-get og :thumbnail)
        (insert (format " :thumbnail %S" (plist-get og :thumbnail))))
      (message "Enriched BOOKMARK: %s" (or (plist-get og :title) url)))))

(defun ghost-enrich-buffer ()
  "Enrich all VIDEO, AUDIO, EMBED, and BOOKMARK blocks in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^#\\+BEGIN_\\(VIDEO\\|AUDIO\\|EMBED\\|BOOKMARK\\)" nil t)
        (ghost-enrich-at-point)
        (setq count (1+ count)))
      (message "Enriched %d blocks" count))))

(defun ghost-enrich-region (start end)
  "Enrich all blocks in region from START to END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (ghost-enrich-buffer))))

;;;; Phase 3: Preview Functions

(defun ghost-preview ()
  "Export current buffer to HTML and open in browser."
  (interactive)
  (let* ((org-file (buffer-file-name))
         (json-file (concat (file-name-sans-extension org-file) ".json"))
         (html-file (concat (file-name-sans-extension org-file) ".html")))
    ;; Export to Lexical JSON
    (require 'ox-ghost)
    (org-lexical-export-to-file)
    ;; Convert to HTML (using ghost renderer or simple conversion)
    (ghost-publish--json-to-html json-file html-file)
    ;; Open in browser
    (browse-url (concat "file://" html-file))
    (message "Preview opened in browser")))

(defun ghost-publish--json-to-html (json-file html-file)
  "Convert Lexical JSON-FILE to HTML-FILE for preview."
  (let ((json-content (with-temp-buffer
                        (insert-file-contents json-file)
                        (buffer-string))))
    (with-temp-file html-file
      (insert "<!DOCTYPE html>\n<html>\n<head>\n")
      (insert "<meta charset=\"utf-8\">\n")
      (insert "<title>Ghost Preview</title>\n")
      (insert "<style>\n")
      (insert "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }\n")
      (insert "pre { background: #f4f4f4; padding: 1em; overflow-x: auto; }\n")
      (insert ".callout { padding: 1em; border-radius: 4px; margin: 1em 0; }\n")
      (insert ".callout.blue { background: #e3f2fd; }\n")
      (insert ".callout.yellow { background: #fff8e1; }\n")
      (insert ".callout.green { background: #e8f5e9; }\n")
      (insert ".callout.purple { background: #f3e5f5; }\n")
      (insert ".callout.grey { background: #f5f5f5; }\n")
      (insert "video, audio { max-width: 100%; }\n")
      (insert "img { max-width: 100%; height: auto; }\n")
      (insert "</style>\n")
      (insert "</head>\n<body>\n")
      (insert "<h1>Preview</h1>\n")
      (insert "<p><em>Note: This is a simplified preview. Actual Ghost rendering may differ.</em></p>\n")
      (insert "<hr>\n")
      ;; Simple extraction of content (could be enhanced)
      (insert "<pre>")
      (insert (ghost-publish--escape-html json-content))
      (insert "</pre>\n")
      (insert "</body>\n</html>\n"))))

(defun ghost-publish--escape-html (string)
  "Escape HTML special characters in STRING."
  (replace-regexp-in-string
   ">" "&gt;"
   (replace-regexp-in-string
    "<" "&lt;"
    (replace-regexp-in-string "&" "&amp;" string))))

(defun ghost-preview-json ()
  "Export to Lexical JSON and display in buffer."
  (interactive)
  (require 'ox-ghost)
  (let* ((json-file (org-lexical-export-to-file)))
    (find-file-other-window json-file)
    (json-pretty-print-buffer)
    (message "Lexical JSON: %s" json-file)))

;;;; Phase 4: Publish Functions

(defun ghost-publish (&optional title)
  "Publish current buffer to Ghost as a new draft post.
TITLE defaults to #+TITLE value.
After successful publish, syncs Ghost metadata back to org headers."
  (interactive)
  (let* ((org-file (buffer-file-name))
         (title (or title
                    (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward "^#\\+TITLE:\\s-*\\(.+\\)$" nil t)
                        (match-string 1)))
                    (file-name-base org-file)))
         (json-file (concat (file-name-sans-extension org-file) ".json"))
         (metadata-file (concat (file-name-sans-extension org-file) ".metadata.json")))
    ;; Export to Lexical JSON
    (require 'ox-ghost)
    (org-lexical-export-to-file)
    ;; Publish via ghost.js and capture full JSON response
    (let* ((temp-response (make-temp-file "ghost-response-" nil ".json"))
           (cmd (format "cd %s && node ghost.js post get-create-response %s %s > %s 2>&1"
                        (ghost-publish--script-dir)
                        (shell-quote-argument title)
                        (shell-quote-argument json-file)
                        (shell-quote-argument temp-response)))
           ;; Fall back to simple create if get-create-response doesn't exist
           (output (ghost-publish--run-command
                    (format "cd %s && node ghost.js post create %s %s 2>&1"
                            (ghost-publish--script-dir)
                            (shell-quote-argument title)
                            (shell-quote-argument json-file)))))
      (if (string-match "https://[^ \n]+" output)
          (let ((url (match-string 0 output)))
            ;; Try to pull metadata from Ghost for the newly created post
            ;; Extract slug from URL: https://www.ii.coop/slug-here/
            (when (string-match "https://[^/]+/\\([^/]+\\)/?$" url)
              (let ((slug (match-string 1 url)))
                (condition-case err
                    (progn
                      ;; Fetch full post data and write to metadata file
                      (let ((post-json (ghost-publish--run-command
                                        (format "cd %s && node ghost.js post get %s 2>&1"
                                                (ghost-publish--script-dir)
                                                (shell-quote-argument slug)))))
                        (with-temp-file metadata-file
                          (insert post-json))
                        ;; Sync metadata to org headers
                        (ox-ghost-sync-metadata-to-org metadata-file)
                        (save-buffer)
                        (message "Published and synced: %s" url)))
                  (error
                   (message "Published: %s (metadata sync failed: %s)" url (error-message-string err))))))
            (kill-new url)
            url)
        (error "Publish failed: %s" output)))))

(defun ghost-update (&optional slug-or-id)
  "Update existing Ghost post.
If SLUG-OR-ID is nil, auto-detects from #+GHOST_ID: or #+GHOST_SLUG: in buffer.
After successful update, syncs Ghost metadata back to org headers."
  (interactive)
  (let* ((org-file (buffer-file-name))
         (json-file (concat (file-name-sans-extension org-file) ".json"))
         (metadata-file (concat (file-name-sans-extension org-file) ".metadata.json"))
         ;; Auto-detect identifier from org headers if not provided
         (identifier (or slug-or-id
                         (ox-ghost--get-header "GHOST_ID")
                         (ox-ghost--get-header "GHOST_SLUG")
                         (read-string "Post slug or ID: "))))
    (when (string-empty-p identifier)
      (user-error "No post identifier. Publish first or provide slug/ID"))
    ;; Export to Lexical JSON
    (require 'ox-ghost)
    (org-lexical-export-to-file)
    ;; Update via ghost.js
    (let ((output (ghost-publish--run-command
                   (format "cd %s && node ghost.js post update %s %s 2>&1"
                           (ghost-publish--script-dir)
                           (shell-quote-argument identifier)
                           (shell-quote-argument json-file)))))
      (if (string-match "https://[^ \n]+" output)
          (let ((url (match-string 0 output)))
            ;; Fetch updated metadata and sync back
            (condition-case err
                (progn
                  (let ((post-json (ghost-publish--run-command
                                    (format "cd %s && node ghost.js post get %s 2>&1"
                                            (ghost-publish--script-dir)
                                            (shell-quote-argument identifier)))))
                    (with-temp-file metadata-file
                      (insert post-json))
                    (ox-ghost-sync-metadata-to-org metadata-file)
                    (save-buffer)
                    (message "Updated and synced: %s" url)))
              (error
               (message "Updated: %s (metadata sync failed: %s)" url (error-message-string err))))
            (kill-new url)
            url)
        (error "Update failed: %s" output)))))

(defun ghost-publish-and-open ()
  "Publish current buffer and open in browser."
  (interactive)
  (let ((url (ghost-publish)))
    (browse-url url)))

;;;; Org-babel Integration

;; These functions can be called from org files using #+CALL:

(defun org-babel-execute:ghost-tts (body params)
  "Execute ghost-tts with BODY as text and PARAMS."
  (let ((text (or (cdr (assq :text params)) body))
        (name (cdr (assq :name params))))
    (ghost-tts text name)))

(defun org-babel-execute:ghost-image (body params)
  "Execute ghost-image with BODY as text and PARAMS."
  (let ((text (or (cdr (assq :text params)) body))
        (color1 (cdr (assq :color1 params)))
        (color2 (cdr (assq :color2 params)))
        (name (cdr (assq :name params))))
    (ghost-image text color1 color2 name)))

(defun org-babel-execute:ghost-video (body params)
  "Execute ghost-video with BODY as text and PARAMS."
  (let ((text (or (cdr (assq :text params)) body))
        (bgcolor (cdr (assq :bgcolor params)))
        (name (cdr (assq :name params))))
    (ghost-video text bgcolor name)))

;;;; Convenience Functions

(defun ghost-enrich-export-publish ()
  "Full workflow: enrich, export to JSON, publish to Ghost."
  (interactive)
  (message "Step 1/3: Enriching media blocks...")
  (ghost-enrich-buffer)
  (save-buffer)
  (message "Step 2/3: Exporting to Lexical JSON...")
  (require 'ox-ghost)
  (let ((json-file (org-lexical-export-to-file)))
    (message "Step 3/3: Publishing to Ghost...")
    (let ((url (ghost-publish)))
      (message "Published: %s" url)
      url)))

(defun ghost-enrich-and-export ()
  "Enrich media blocks and export to Lexical JSON."
  (interactive)
  (message "Enriching media blocks...")
  (ghost-enrich-buffer)
  (save-buffer)
  (message "Exporting to Lexical JSON...")
  (require 'ox-ghost)
  (let ((json-file (org-lexical-export-to-file)))
    (message "Exported: %s" json-file)
    json-file))

(defun ghost-execute-and-enrich ()
  "Execute all babel blocks then enrich media blocks."
  (interactive)
  (message "Executing babel blocks...")
  (org-babel-execute-buffer)
  (save-buffer)
  (message "Enriching media blocks...")
  (ghost-enrich-buffer)
  (save-buffer)
  (message "Done - ready to export with M-x ghost-preview or M-x ghost-publish"))

;;;; Phase 5: Metadata Sync Functions

(defun ox-ghost--get-header (key)
  "Get value of #+KEY: header from current buffer.
Returns nil if not found."
  (save-excursion
    (goto-char (point-min))
    (let ((header-re (format "^#\\+%s:\\s-*\\(.*\\)$" (regexp-quote key))))
      (when (re-search-forward header-re nil t)
        (let ((val (string-trim (match-string 1))))
          (unless (string-empty-p val) val))))))

(defun ox-ghost--update-header (key value)
  "Update or insert #+KEY: VALUE in current buffer's header section.
If VALUE is nil or empty, removes the header line."
  (save-excursion
    (goto-char (point-min))
    (let ((header-re (format "^#\\+%s:.*$" (regexp-quote key))))
      (if (re-search-forward header-re nil t)
          ;; Found existing header - update or remove
          (if (and value (not (string-empty-p (if (stringp value) value (format "%s" value)))))
              (replace-match (format "#+%s: %s" key value))
            ;; Remove the line including newline
            (beginning-of-line)
            (delete-region (point) (min (1+ (line-end-position)) (point-max))))
        ;; Not found - insert if value is non-nil
        (when (and value (not (string-empty-p (if (stringp value) value (format "%s" value)))))
          (ox-ghost--insert-header key value))))))

(defun ox-ghost--insert-header (key value)
  "Insert #+KEY: VALUE in appropriate location in header.
Inserts after last #+GHOST_* line, or after #+TITLE if no ghost headers exist."
  (save-excursion
    (goto-char (point-min))
    ;; Find last #+GHOST_* header
    (let ((last-ghost-pos nil)
          (title-pos nil))
      ;; Search for all GHOST_ headers
      (while (re-search-forward "^#\\+GHOST_[A-Z_]+:.*$" nil t)
        (setq last-ghost-pos (point)))
      ;; If no ghost headers, find #+TITLE
      (unless last-ghost-pos
        (goto-char (point-min))
        (when (re-search-forward "^#\\+TITLE:.*$" nil t)
          (setq title-pos (point))))
      ;; Insert at appropriate position
      (cond
       (last-ghost-pos
        (goto-char last-ghost-pos)
        (end-of-line)
        (insert (format "\n#+%s: %s" key value)))
       (title-pos
        (goto-char title-pos)
        (end-of-line)
        (insert (format "\n#+%s: %s" key value)))
       (t
        ;; No headers found, insert at beginning
        (goto-char (point-min))
        (insert (format "#+%s: %s\n" key value)))))))

(defun ox-ghost-sync-metadata-to-org (metadata-file &optional org-buffer)
  "Read METADATA-FILE (.metadata.json) and update org buffer headers.
If ORG-BUFFER is nil, updates current buffer.
Returns t on success, nil on failure."
  (interactive
   (list (read-file-name "Metadata file: " nil nil t
                         (concat (file-name-sans-extension (buffer-file-name)) ".metadata.json"))))
  (unless (file-exists-p metadata-file)
    (message "Metadata file not found: %s" metadata-file)
    (cl-return-from ox-ghost-sync-metadata-to-org nil))
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (metadata (condition-case err
                       (json-read-file metadata-file)
                     (error
                      (message "Failed to parse metadata file: %s" (error-message-string err))
                      nil)))
         ;; Map JSON keys to org header names
         (mappings '((id . "GHOST_ID")
                     (uuid . "GHOST_UUID")
                     (slug . "GHOST_SLUG")
                     (url . "GHOST_URL")
                     (status . "GHOST_STATUS")
                     (visibility . "GHOST_VISIBILITY")
                     (created_at . "GHOST_CREATED_AT")
                     (updated_at . "GHOST_UPDATED_AT")
                     (published_at . "GHOST_PUBLISHED_AT")
                     (feature_image . "GHOST_IMAGE")
                     (custom_excerpt . "GHOST_EXCERPT"))))
    (when metadata
      (with-current-buffer (or org-buffer (current-buffer))
        (dolist (mapping mappings)
          (let* ((json-key (car mapping))
                 (header-name (cdr mapping))
                 (value (alist-get json-key metadata)))
            ;; Handle different value types
            (when value
              (let ((str-value (cond
                                ((stringp value) value)
                                ((numberp value) (number-to-string value))
                                ((eq value t) "true")
                                ((eq value :json-false) "false")
                                (t (format "%s" value)))))
                (ox-ghost--update-header header-name str-value)))))
        (message "Synced metadata from %s" (file-name-nondirectory metadata-file))
        t))))

(defun ghost-pull-metadata ()
  "Fetch current post metadata from Ghost and update org headers.
Uses GHOST_ID or GHOST_SLUG from current buffer to identify the post."
  (interactive)
  (let ((id (ox-ghost--get-header "GHOST_ID"))
        (slug (ox-ghost--get-header "GHOST_SLUG")))
    (unless (or id slug)
      (user-error "No GHOST_ID or GHOST_SLUG found in buffer. Publish first"))
    (let* ((identifier (or id slug))
           (script-dir (ghost-publish--script-dir))
           (temp-file (make-temp-file "ghost-metadata-" nil ".json"))
           (cmd (format "cd %s && node ghost.js post get %s 2>&1"
                        script-dir
                        (shell-quote-argument identifier)))
           (output (ghost-publish--run-command cmd)))
      ;; Write output to temp file (it should be JSON)
      (condition-case err
          (progn
            ;; Parse and re-encode to validate JSON
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (parsed (json-read-from-string output)))
              ;; Write to temp file
              (with-temp-file temp-file
                (insert (json-encode parsed)))
              ;; Sync to org
              (ox-ghost-sync-metadata-to-org temp-file)
              (save-buffer)
              (delete-file temp-file)
              (message "Pulled metadata for %s" identifier)))
        (error
         (when (file-exists-p temp-file)
           (delete-file temp-file))
         (user-error "Failed to fetch metadata: %s" (error-message-string err)))))))

(defun ghost-status ()
  "Show sync status comparing local org headers to Ghost.
Displays diff of local metadata vs Ghost's current state."
  (interactive)
  (let ((id (ox-ghost--get-header "GHOST_ID"))
        (slug (ox-ghost--get-header "GHOST_SLUG")))
    (unless (or id slug)
      (user-error "No GHOST_ID or GHOST_SLUG found. Publish first"))
    (let* ((identifier (or id slug))
           (script-dir (ghost-publish--script-dir))
           (cmd (format "cd %s && node ghost.js post get %s 2>&1"
                        script-dir
                        (shell-quote-argument identifier)))
           (output (ghost-publish--run-command cmd))
           (local-headers '(("GHOST_ID" . id)
                            ("GHOST_SLUG" . slug)
                            ("GHOST_STATUS" . status)
                            ("GHOST_URL" . url)
                            ("GHOST_UPDATED_AT" . updated_at)
                            ("GHOST_PUBLISHED_AT" . published_at))))
      (condition-case err
          (let* ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol)
                 (ghost-data (json-read-from-string output)))
            (with-current-buffer (get-buffer-create "*Ghost Sync Status*")
              (erase-buffer)
              (insert "Ghost Sync Status\n")
              (insert "=================\n\n")
              (insert (format "Post: %s\n\n" (or (alist-get 'title ghost-data) identifier)))
              (insert (format "%-20s %-30s %-30s\n" "Field" "Local (org)" "Remote (Ghost)"))
              (insert (make-string 80 ?-) "\n")
              (dolist (pair local-headers)
                (let* ((header (car pair))
                       (json-key (cdr pair))
                       (local-val (ox-ghost--get-header header))
                       (remote-val (alist-get json-key ghost-data))
                       (remote-str (if remote-val (format "%s" remote-val) "(none)"))
                       (local-str (or local-val "(none)"))
                       (diff-marker (if (equal local-str remote-str) " " "*")))
                  (insert (format "%s %-19s %-30s %-30s\n"
                                  diff-marker header
                                  (truncate-string-to-width local-str 29)
                                  (truncate-string-to-width remote-str 29)))))
              (insert "\n* = differs from Ghost\n")
              (goto-char (point-min))
              (display-buffer (current-buffer))))
        (error
         (user-error "Failed to fetch status: %s" (error-message-string err)))))))

(provide 'ox-ghost-publish)
;;; ox-ghost-publish.el ends here
