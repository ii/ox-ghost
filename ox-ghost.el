;;; ox-ghost.el --- Export org-mode to Ghost Lexical JSON -*- lexical-binding: t; -*-

;; Author: ii.coop
;; Version: 0.8.0
;; Package-Requires: ((emacs "27.1") (org "9.5"))
;; Keywords: org, ghost, lexical, export

;;; Commentary:

;; Export org-mode documents to Ghost's Lexical JSON format.
;;
;; Strategy: Transcoders return JSON strings wrapped with markers.
;; The template transcoder collects and assembles the final JSON.
;;
;; Usage:
;;   M-x org-lexical-export-as-json    - Export to JSON buffer
;;   M-x org-lexical-export-to-file    - Export to JSON file
;;   M-x org-lexical-export-for-ghost  - Export JSON + metadata.json for Ghost
;;   M-x org-lexical-export-to-html    - Export to HTML (via Node.js renderer)
;;   M-x org-lexical-validate          - Validate JSON file
;;   M-x org-lexical-show-metadata     - Preview Ghost metadata
;;
;; Keybindings (via C-c C-e x):
;;   x - Export to JSON buffer
;;   j - Export to JSON file
;;   g - Export for Ghost (JSON + metadata)
;;   h - Export to HTML file
;;   o - Export to HTML and open in browser
;;   v - Validate existing JSON file
;;   m - Show metadata preview
;;
;; Requirements for HTML export/validation:
;;   - Node.js installed
;;   - npm install @tryghost/kg-lexical-html-renderer @tryghost/kg-default-nodes
;;
;; Node Categories (matching Ghost's kg-default-nodes):
;;   - Text/Inline: text, link, linebreak
;;   - Block Elements: paragraph, heading, list/listitem, quote, aside
;;   - Code: codeblock
;;   - Media Cards: image, gallery, video, audio, file
;;   - Content Cards: callout, toggle, button, header, bookmark, embed
;;   - Email Cards: email, email-cta
;;   - Membership Cards: signup, product, paywall
;;   - Integration Cards: transistor
;;   - Raw Content: html, markdown
;;   - Structural: horizontalrule

;;; Code:

(require 'org)
(require 'ox)
(require 'ox-html)
(require 'json)
(require 'cl-lib)

;;; ============================================================================
;;; Customization
;;; ============================================================================

(defgroup ox-ghost nil
  "Options for exporting Org files to Ghost Lexical JSON."
  :tag "Org Lexical"
  :group 'org-export)

(defcustom ox-ghost-renderer-path nil
  "Path to ox-ghost-render.js.
If nil, searches in the same directory as `ox-ghost.el'."
  :type '(choice (const :tag "Auto-detect" nil)
                 (file :tag "Custom path"))
  :group 'ox-ghost)

(defcustom ox-ghost-node-command
  (or (executable-find "node") "node")
  "Command to run Node.js.
Defaults to the result of `executable-find' or \"node\"."
  :type 'string
  :group 'ox-ghost)

(defconst ox-ghost--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where ox-ghost is installed.
Captured at load time to find the renderer script.")

(defcustom ox-ghost-ghost-defaults nil
  "Default Ghost metadata for posts/pages.
Alist of (PROPERTY . VALUE) pairs. Can be set in .dir-locals.el.
Example: ((type . \"post\") (status . \"draft\") (visibility . \"public\"))"
  :type '(alist :key-type symbol :value-type string)
  :group 'ox-ghost)

(defcustom ox-ghost-ghost-config nil
  "Path to ghost-config.json for resolving tier/newsletter names to IDs.
If nil, metadata will use names instead of IDs."
  :type '(choice (const :tag "None" nil)
                 (file :tag "Config file path"))
  :group 'ox-ghost)

;;; ============================================================================
;;; Constants
;;; ============================================================================

(defconst ox-ghost--node-marker "‹LEXNODE›"
  "Marker to separate JSON node strings.")

;;; Format flags (bitmask) - matches Lexical TextNode format
(defconst ox-ghost--format-bold 1)
(defconst ox-ghost--format-italic 2)
(defconst ox-ghost--format-strikethrough 4)
(defconst ox-ghost--format-underline 8)
(defconst ox-ghost--format-code 16)
(defconst ox-ghost--format-subscript 32)
(defconst ox-ghost--format-superscript 64)
(defconst ox-ghost--format-highlight 128)

;;; Image detection
(defconst ox-ghost--image-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "svg" "avif")
  "File extensions recognized as images.")

(defconst ox-ghost--image-hosting-domains
  '("images.unsplash.com" "i.imgur.com" "imgur.com"
    "pbs.twimg.com" "cdn.pixabay.com" "images.pexels.com"
    "upload.wikimedia.org" "i.redd.it" "preview.redd.it")
  "Known image hosting domains that may serve images without extensions.")

;;; ============================================================================
;;; Backend Definition
;;; ============================================================================

(org-export-define-backend 'ghost
  '((headline . ox-ghost--headline)
    (section . ox-ghost--section)
    (paragraph . ox-ghost--paragraph)
    (plain-text . ox-ghost--plain-text)
    (bold . ox-ghost--bold)
    (italic . ox-ghost--italic)
    (underline . ox-ghost--underline)
    (strike-through . ox-ghost--strike-through)
    (subscript . ox-ghost--subscript)
    (superscript . ox-ghost--superscript)
    (export-snippet . ox-ghost--export-snippet)
    (code . ox-ghost--code)
    (verbatim . ox-ghost--verbatim)
    (link . ox-ghost--link)
    (plain-list . ox-ghost--plain-list)
    (item . ox-ghost--item)
    (quote-block . ox-ghost--quote-block)
    (src-block . ox-ghost--src-block)
    (example-block . ox-ghost--example-block)
    (fixed-width . ox-ghost--fixed-width)
    (export-block . ox-ghost--export-block)
    (special-block . ox-ghost--special-block)
    (horizontal-rule . ox-ghost--horizontal-rule)
    (table . ox-ghost--table)
    (template . ox-ghost--template)
    (inner-template . ox-ghost--inner-template)
    (line-break . ox-ghost--line-break)
    (entity . ox-ghost--entity))
  :menu-entry
  '(?x "Export to Ghost (Lexical)"
       ((?x "As JSON buffer" org-lexical-export-as-json)
        (?j "To JSON file" org-lexical-export-to-file)
        (?g "For Ghost (JSON + metadata)" org-lexical-export-for-ghost)
        (?h "To HTML file" org-lexical-export-to-html)
        (?o "To HTML and open" org-lexical-export-to-html-and-open)
        (?v "Validate JSON" org-lexical-validate)
        (?m "Show metadata" org-lexical-show-metadata))))

;;; ============================================================================
;;; Core Utilities
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; JSON Node Construction
;;; ----------------------------------------------------------------------------

(defun ox-ghost--node (type &rest props)
  "Create a JSON node string of TYPE with PROPS."
  (let ((node `((type . ,type) (version . 1) ,@props)))
    (concat ox-ghost--node-marker (json-encode node))))

(defun ox-ghost--text-node (text &optional format-val)
  "Create text node JSON with TEXT and optional FORMAT-VAL bitmask."
  (ox-ghost--node "text"
    `(text . ,text)
    `(format . ,(or format-val 0))
    '(style . "")
    '(detail . 0)
    '(mode . "normal")))

(defun ox-ghost--text-node-alist (text &optional format-val)
  "Return text node as alist (not JSON string) for embedding in parent nodes."
  `((type . "text")
    (version . 1)
    (text . ,text)
    (format . ,(or format-val 0))
    (style . "")
    (detail . 0)
    (mode . "normal")))

;;; ----------------------------------------------------------------------------
;;; Content Parsing
;;; ----------------------------------------------------------------------------

(defun ox-ghost--parse-children (content)
  "Parse CONTENT string into vector of child nodes.
Handles post-blank spaces that appear between JSON nodes."
  (if (or (null content) (string-empty-p content))
      []
    (let* ((parts (split-string content ox-ghost--node-marker t))
           (nodes '())
           (pending-space nil))
      (dolist (part parts)
        ;; Check for trailing whitespace (post-blank from org-export)
        (let* ((trimmed (string-trim-right part))
               (trailing (when (< (length trimmed) (length part))
                           (substring part (length trimmed))))
               (node (condition-case nil
                         (json-read-from-string trimmed)
                       (error nil))))
          ;; If we have pending space from previous node, prepend to current text
          (when (and pending-space node (string= (cdr (assoc 'type node)) "text"))
            (let ((txt (cdr (assoc 'text node))))
              (setcdr (assoc 'text node) (concat pending-space txt))))
          (setq pending-space nil)
          ;; Save this node
          (when node (push node nodes))
          ;; Save trailing space for next node
          (when trailing (setq pending-space trailing))))
      ;; If trailing space remains, append to last text node
      (when (and pending-space nodes (string= (cdr (assoc 'type (car nodes))) "text"))
        (let ((txt (cdr (assoc 'text (car nodes)))))
          (setcdr (assoc 'text (car nodes)) (concat txt pending-space))))
      (vconcat (nreverse nodes)))))

(defun ox-ghost--parse-block-nodes (content)
  "Parse CONTENT into list of block-level nodes."
  (if (or (null content) (string-empty-p content))
      '()
    (let* ((parts (split-string content ox-ghost--node-marker t))
           (nodes (mapcar (lambda (part)
                            (condition-case nil
                                (json-read-from-string part)
                              (error nil)))
                          parts)))
      (delq nil nodes))))

(defun ox-ghost--extract-text (content)
  "Extract plain text from CONTENT for card bodies."
  (if (or (null content) (string-empty-p content))
      ""
    (let* ((nodes (ox-ghost--parse-block-nodes content))
           (texts (mapcar (lambda (node)
                            (let ((type (cdr (assoc 'type node))))
                              (cond
                               ((string= type "text")
                                (cdr (assoc 'text node)))
                               ((member type '("paragraph" "heading" "quote"))
                                (mapconcat (lambda (child)
                                             (if (string= (cdr (assoc 'type child)) "text")
                                                 (cdr (assoc 'text child))
                                               ""))
                                           (append (cdr (assoc 'children node)) nil)
                                           ""))
                               ;; Handle codeblock nodes (from EXAMPLE blocks)
                               ((string= type "codeblock")
                                (cdr (assoc 'code node)))
                               (t ""))))
                          nodes)))
      (string-trim (mapconcat #'identity texts "\n")))))

(defun ox-ghost--parse-params (params)
  "Parse block PARAMS string into plist.
Handles URLs with colons and multi-word values properly."
  (when (and params (not (string-empty-p params)))
    (let ((result nil)
          (str (string-trim params))
          (pos 0))
      ;; Match :key value patterns where value can be:
      ;; - A quoted string: \"...\"
      ;; - A URL: https://... (up to next whitespace+colon-word)
      ;; - A single word
      (while (string-match ":\\([a-zA-Z_]+\\)\\s-+\\(\"[^\"]*\"\\|[^[:space:]]+\\(?:[[:space:]]+[^:[:space:]][^[:space:]]*\\)*\\)" str pos)
        (let* ((key (intern (concat ":" (match-string 1 str))))
               (val-raw (match-string 2 str))
               ;; Strip quotes if present
               (val (if (and (string-prefix-p "\"" val-raw)
                             (string-suffix-p "\"" val-raw))
                        (substring val-raw 1 -1)
                      (string-trim val-raw))))
          (setq result (plist-put result key val))
          (setq pos (match-end 0))))
      result)))

(defun ox-ghost--strip-quotes (str)
  "Strip surrounding double quotes from STR if present."
  (if (and str (stringp str)
           (> (length str) 1)
           (string-prefix-p "\"" str)
           (string-suffix-p "\"" str))
      (substring str 1 -1)
    str))

(defun ox-ghost--render-to-html (element)
  "Render ELEMENT's contents to HTML for toggle/callout bodies.
Uses org's HTML exporter to convert nested content to HTML.
Post-processes code blocks to use Prism.js format for consistent styling."
  (let* ((beg (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element)))
    (if (and beg end (< beg end))
        (let* ((content-str (buffer-substring-no-properties beg end))
               (html (string-trim
                      (org-export-string-as content-str 'html t
                                            '(:with-toc nil
                                              :section-numbers nil
                                              :with-smart-quotes nil)))))
          ;; Convert org's code block format to Prism.js format
          ;; From: <div class="org-src-container"><pre class="src src-LANG">...</pre></div>
          ;; To: <pre><code class="language-LANG">...</code></pre>
          (setq html (replace-regexp-in-string
                      "<div class=\"org-src-container\">\n?<pre class=\"src src-\\([^\"]+\\)\">"
                      "<pre><code class=\"language-\\1\">"
                      html))
          (setq html (replace-regexp-in-string
                      "</pre>\n?</div>"
                      "</code></pre>"
                      html))
          html)
      "")))

(defun ox-ghost--escape-html (str)
  "Escape HTML special characters in STR for safe embedding."
  (when str
    (replace-regexp-in-string
     "&" "&amp;"
     (replace-regexp-in-string
      "<" "&lt;"
      (replace-regexp-in-string
       ">" "&gt;"
       (replace-regexp-in-string "\"" "&quot;" str))))))

;;; ============================================================================
;;; Text Nodes (Inline)
;;; ============================================================================

(defun ox-ghost--plain-text (text info)
  "Transcode plain TEXT, preserving surrounding whitespace."
  ;; Collapse internal whitespace/newlines but preserve leading/trailing
  (let* ((collapsed (replace-regexp-in-string "[ \t]*\n[ \t]*" " " text))
         (clean (replace-regexp-in-string "[ \t]+" " " collapsed)))
    (if (string-empty-p clean)
        ""
      (ox-ghost--text-node clean))))

(defun ox-ghost--apply-format (contents flag)
  "Apply format FLAG to text nodes in CONTENTS."
  (if (or (null contents) (string-empty-p contents))
      ""
    (let* ((parts (split-string contents ox-ghost--node-marker t))
           (updated (mapcar
                     (lambda (part)
                       (condition-case nil
                           (let* ((node (json-read-from-string part))
                                  (type (cdr (assoc 'type node))))
                             (if (string= type "text")
                                 (let ((fmt (or (cdr (assoc 'format node)) 0)))
                                   (setcdr (assoc 'format node) (logior fmt flag))
                                   (concat ox-ghost--node-marker (json-encode node)))
                               (concat ox-ghost--node-marker part)))
                         (error (concat ox-ghost--node-marker part))))
                     parts)))
      (mapconcat #'identity updated ""))))

;;; ----------------------------------------------------------------------------
;;; Text Formatting
;;; ----------------------------------------------------------------------------

(defun ox-ghost--bold (bold contents info)
  "Transcode BOLD."
  (ox-ghost--apply-format contents ox-ghost--format-bold))

(defun ox-ghost--italic (italic contents info)
  "Transcode ITALIC."
  (ox-ghost--apply-format contents ox-ghost--format-italic))

(defun ox-ghost--underline (underline contents info)
  "Transcode UNDERLINE."
  (ox-ghost--apply-format contents ox-ghost--format-underline))

(defun ox-ghost--strike-through (strike contents info)
  "Transcode STRIKE-THROUGH."
  (ox-ghost--apply-format contents ox-ghost--format-strikethrough))

(defun ox-ghost--subscript (subscript contents info)
  "Transcode SUBSCRIPT.
Org syntax: text_{subscript} or text_subscript"
  (ox-ghost--apply-format contents ox-ghost--format-subscript))

(defun ox-ghost--superscript (superscript contents info)
  "Transcode SUPERSCRIPT.
Org syntax: text^{superscript} or text^superscript"
  (ox-ghost--apply-format contents ox-ghost--format-superscript))

(defun ox-ghost--code (code contents info)
  "Transcode inline CODE."
  (ox-ghost--text-node (org-element-property :value code) ox-ghost--format-code))

(defun ox-ghost--verbatim (verbatim contents info)
  "Transcode VERBATIM."
  (ox-ghost--text-node (org-element-property :value verbatim) ox-ghost--format-code))

(defun ox-ghost--export-snippet (export-snippet contents info)
  "Transcode EXPORT-SNIPPET.
Supports @@ghost:highlight:text@@ for highlighted text.
Syntax: @@ghost:FORMAT:text@@ where FORMAT is 'highlight'."
  (let* ((backend (org-element-property :back-end export-snippet))
         (value (org-element-property :value export-snippet)))
    (when (string= backend "ghost")
      (cond
       ;; @@ghost:highlight:text@@ - highlighted text
       ((string-prefix-p "highlight:" value)
        (ox-ghost--text-node (substring value 10) ox-ghost--format-highlight))
       ;; @@ghost:raw JSON@@ - pass through raw JSON
       (t (concat ox-ghost--node-marker value))))))

;;; ----------------------------------------------------------------------------
;;; Links
;;; ----------------------------------------------------------------------------

(defun ox-ghost--image-url-p (url)
  "Check if URL points to an image.
Checks both file extension (ignoring query params) and known image hosts."
  (when url
    (let* ((url-no-query (car (split-string url "?" t)))
           (url-no-fragment (car (split-string url-no-query "#" t)))
           (ext (downcase (or (file-name-extension url-no-fragment) "")))
           (host (when (string-match "^\\(?:https?://\\)?\\([^/]+\\)" url)
                   (match-string 1 url))))
      (or
       ;; Has image extension
       (member ext ox-ghost--image-extensions)
       ;; Is known image hosting domain
       (member host ox-ghost--image-hosting-domains)))))

(defun ox-ghost--link (link contents info)
  "Transcode LINK."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         ;; For images: only use desc from actual contents (alt text)
         ;; For links: fall back to path for display text
         (desc-from-contents (when contents (ox-ghost--extract-text contents)))
         (full-url (cond
                    ((member type '("http" "https")) (concat type ":" path))
                    ((string= type "file") path)
                    (t raw-link))))
    (cond
     ;; Images (file:// local paths or http/https URLs)
     ((and (or (string= type "file")
               (member type '("http" "https")))
           (ox-ghost--image-url-p full-url))
      ;; For images, use explicit description as alt, otherwise empty
      (ox-ghost--card-image full-url (or desc-from-contents "") link))
     ;; Regular links
     (t
      (ox-ghost--node "link"
        `(url . ,full-url)
        `(children . ,(vector (ox-ghost--text-node-alist
                               (or desc-from-contents full-url))))
        '(direction . "ltr")
        '(format . "")
        '(indent . 0)
        '(rel . "noopener"))))))

;;; ============================================================================
;;; Block Elements
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Document Structure
;;; ----------------------------------------------------------------------------

(defun ox-ghost--template (contents info)
  "Wrap CONTENTS in Lexical root structure."
  (let* ((nodes (ox-ghost--parse-block-nodes contents))
         (root `((root . ((type . "root")
                          (version . 1)
                          (children . ,(vconcat nodes))
                          (direction . "ltr")
                          (format . "")
                          (indent . 0))))))
    (json-encode root)))

(defun ox-ghost--inner-template (contents info)
  "Return CONTENTS."
  (or contents ""))

(defun ox-ghost--section (section contents info)
  "Transcode SECTION - pass through."
  (or contents ""))

;;; ----------------------------------------------------------------------------
;;; Paragraph Node Constructor
;;; ----------------------------------------------------------------------------

(defun ox-ghost--para-node (children-json)
  "Create paragraph node JSON with CHILDREN-JSON."
  (let ((children (ox-ghost--parse-children children-json)))
    (ox-ghost--node "paragraph"
      `(children . ,children)
      '(direction . "ltr")
      '(format . "")
      '(indent . 0)
      '(textFormat . 0)
      '(textStyle . ""))))

;;; ----------------------------------------------------------------------------
;;; Heading Node Constructor
;;; ----------------------------------------------------------------------------

(defun ox-ghost--heading-node (level children-json)
  "Create heading node JSON at LEVEL with CHILDREN-JSON."
  (let ((children (ox-ghost--parse-children children-json)))
    (ox-ghost--node "heading"
      `(tag . ,(format "h%d" (min level 6)))
      `(children . ,children)
      '(direction . "ltr")
      '(format . "")
      '(indent . 0))))

;;; ----------------------------------------------------------------------------
;;; Headline & Paragraph Transcoders
;;; ----------------------------------------------------------------------------

(defun ox-ghost--headline (headline contents info)
  "Transcode HEADLINE."
  (let* ((level (1+ (org-element-property :level headline)))
         (title (org-element-property :raw-value headline))
         (heading (ox-ghost--heading-node level (ox-ghost--text-node title))))
    (concat heading (or contents ""))))

(defun ox-ghost--paragraph (paragraph contents info)
  "Transcode PARAGRAPH.
If the paragraph contains only an image (block-level card), return
the image directly without paragraph wrapping. Ghost expects images
to be root children, not wrapped in paragraphs."
  (if (and contents (not (string-empty-p (string-trim contents))))
      (let* ((parent-type (org-element-type (org-element-property :parent paragraph)))
             (children (ox-ghost--parse-block-nodes contents))
             ;; Filter out whitespace-only text nodes
             (significant-children
              (seq-filter
               (lambda (n)
                 (let ((type (cdr (assoc 'type n))))
                   (not (and (string= type "text")
                             (string-blank-p (cdr (assoc 'text n)))))))
               children)))
        (cond
         ;; Don't wrap if inside item (list handles it)
         ((eq parent-type 'item) contents)
         ;; Single image child - return unwrapped (image is block-level)
         ((and (= (length significant-children) 1)
               (string= (cdr (assoc 'type (car significant-children))) "image"))
          (concat ox-ghost--node-marker
                  (json-encode (car significant-children))))
         ;; Normal paragraph
         (t (ox-ghost--para-node contents))))
    ""))

;;; ----------------------------------------------------------------------------
;;; Lists
;;; ----------------------------------------------------------------------------

(defun ox-ghost--list-depth (element)
  "Compute nesting depth of list ELEMENT by counting parent lists.
Returns 0 for top-level list items, 1 for second-level, etc.
We start at -1 because the item's immediate parent list doesn't count
toward its indent level (Ghost expects top-level items to have indent=0)."
  (let ((depth -1)
        (parent (org-element-property :parent element)))
    (while parent
      (when (eq (org-element-type parent) 'plain-list)
        (setq depth (1+ depth)))
      (setq parent (org-element-property :parent parent)))
    (max 0 depth)))

(defun ox-ghost--plain-list (plain-list contents info)
  "Transcode PLAIN-LIST to nested Lexical list structure."
  (let* ((list-type (if (eq (org-element-property :type plain-list) 'ordered)
                        "number" "bullet"))
         (items (ox-ghost--parse-block-nodes contents)))
    (ox-ghost--node "list"
      `(listType . ,list-type)
      `(children . ,(vconcat items))
      '(direction . "ltr")
      '(format . "")
      '(indent . 0)
      '(start . 1)
      `(tag . ,(if (string= list-type "number") "ol" "ul")))))

(defun ox-ghost--item (item contents info)
  "Transcode list ITEM.
When an item contains both text and a nested list, split them into
separate sibling listitems. Ghost's renderer will combine them properly.
Set indent based on nesting depth for proper nested list rendering."
  (let* ((checkbox (org-element-property :checkbox item))
         (counter (or (org-element-property :counter item) 1))
         (depth (ox-ghost--list-depth item))
         (children (ox-ghost--parse-block-nodes contents))
         ;; Separate text/link nodes from list nodes
         (text-nodes (seq-filter
                      (lambda (n)
                        (let ((type (cdr (assoc 'type n))))
                          (not (string= type "list"))))
                      children))
         (list-nodes (seq-filter
                      (lambda (n)
                        (string= (cdr (assoc 'type n)) "list"))
                      children))
         (checked-val (cond ((eq checkbox 'on) t)
                            ((eq checkbox 'off) :json-false)
                            (t nil)))
         (make-listitem
          (lambda (child-nodes)
            (ox-ghost--node "listitem"
              `(children . ,(vconcat child-nodes))
              '(direction . "ltr")
              '(format . "")
              `(indent . ,depth)
              `(value . ,counter)
              `(checked . ,checked-val)))))
    (cond
     ;; Both text and nested list - split into two listitems
     ((and text-nodes list-nodes)
      (concat (funcall make-listitem text-nodes)
              (funcall make-listitem list-nodes)))
     ;; Only text or only list
     (t (funcall make-listitem children)))))

;;; ----------------------------------------------------------------------------
;;; Quote & Aside
;;; ----------------------------------------------------------------------------

(defun ox-ghost--quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK."
  (let ((children (ox-ghost--parse-children contents)))
    (ox-ghost--node "quote"
      `(children . ,children)
      '(direction . "ltr")
      '(format . "")
      '(indent . 0))))

(defun ox-ghost--card-aside (contents)
  "Create aside card from CONTENTS."
  (let ((children (ox-ghost--parse-children contents)))
    (ox-ghost--node "aside"
      `(children . ,children)
      '(direction . "ltr")
      '(format . "")
      '(indent . 0))))

;;; ----------------------------------------------------------------------------
;;; Line Break & Entity
;;; ----------------------------------------------------------------------------

(defun ox-ghost--line-break (lb contents info)
  "Transcode LINE-BREAK to Lexical linebreak node.
In Lexical, linebreaks are only valid in paragraph and quote blocks
that are NOT inside containers that prohibit linebreaks (like items).
In other contexts, emit a space instead."
  (let* ((parent (org-element-property :parent lb))
         (invalid-context nil)
         (valid-context nil))
    ;; Walk up to check context - need to be in paragraph/quote
    ;; but NOT inside item, headline, link, or aside
    (while (and parent (not invalid-context))
      (let ((type (org-element-type parent)))
        (cond
         ;; These block-level contexts prohibit linebreaks
         ((memq type '(item headline link))
          (setq invalid-context t))
         ;; special-block ASIDE prohibits linebreaks
         ((and (eq type 'special-block)
               (string= (upcase (org-element-property :type parent)) "ASIDE"))
          (setq invalid-context t))
         ;; Valid top-level containers
         ((memq type '(paragraph quote-block))
          (unless invalid-context
            (setq valid-context t)))
         ;; Stop at section/root level
         ((memq type '(section org-data))
          (setq parent nil)))
        (when parent
          (setq parent (org-element-property :parent parent)))))
    (if (and valid-context (not invalid-context))
        (ox-ghost--node "linebreak")
      ;; In invalid context, emit a space
      (ox-ghost--text-node " "))))

(defun ox-ghost--entity (entity contents info)
  "Transcode ENTITY."
  (ox-ghost--text-node (org-element-property :utf-8 entity)))

;;; ============================================================================
;;; Code Blocks
;;; ============================================================================

(defun ox-ghost--src-block (src-block contents info)
  "Transcode SRC-BLOCK."
  (let ((code (org-element-property :value src-block))
        (lang (or (org-element-property :language src-block) "")))
    (ox-ghost--node "codeblock"
      `(code . ,code)
      `(language . ,lang))))

(defun ox-ghost--example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK."
  (let ((code (org-element-property :value example-block)))
    (ox-ghost--node "codeblock"
      `(code . ,code)
      '(language . "text"))))

(defun ox-ghost--fixed-width (fixed-width contents info)
  "Transcode FIXED-WIDTH (lines starting with :)."
  (let ((code (org-element-property :value fixed-width)))
    (ox-ghost--node "codeblock"
      `(code . ,code)
      '(language . "text"))))

;;; ============================================================================
;;; Media Cards
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Image Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-image (src desc link)
  "Create image card from SRC, DESC, and LINK element."
  (let* ((alt-text (or desc ""))
         (parent (org-element-property :parent link))
         (attrs (when parent (org-export-read-attribute :attr_lexical parent)))
         (width (plist-get attrs :width))
         (height (plist-get attrs :height)))
    (ox-ghost--node "image"
      `(src . ,src)
      `(alt . ,alt-text)
      `(caption . ,(ox-ghost--strip-quotes (or (plist-get attrs :caption) "")))
      `(title . ,(ox-ghost--strip-quotes (or (plist-get attrs :title) "")))
      `(cardWidth . ,(or (plist-get attrs :cardWidth) "regular"))
      `(width . ,(when width (string-to-number width)))
      `(height . ,(when height (string-to-number height)))
      `(href . ,(or (plist-get attrs :href) "")))))

;;; ----------------------------------------------------------------------------
;;; Gallery Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--parse-gallery-image (src)
  "Parse gallery image SRC into image object with fileName, width, height.
Extracts dimensions from URL query params (w=, h=) and fileName from path."
  (let* ((trimmed (string-trim src))
         ;; Split URL at ? to get path and query
         (parts (split-string trimmed "?" t))
         (path (car parts))
         (query (cadr parts))
         ;; Extract fileName from path (last segment)
         (path-parts (split-string path "/" t))
         (file-name (or (car (last path-parts)) "image"))
         ;; Parse query params for width and height
         (width nil)
         (height nil))
    (when query
      (let ((params (split-string query "&" t)))
        (dolist (param params)
          (let ((kv (split-string param "=" t)))
            (when (= (length kv) 2)
              (cond
               ((member (car kv) '("w" "width"))
                (setq width (string-to-number (cadr kv))))
               ((member (car kv) '("h" "height"))
                (setq height (string-to-number (cadr kv))))))))))
    `((src . ,trimmed)
      (fileName . ,file-name)
      (width . ,(or width 800))
      (height . ,(or height 600))
      (caption . "")
      (alt . ""))))

(defun ox-ghost--card-gallery (attr contents)
  "Create gallery card from ATTR and CONTENTS.
Images are arranged in rows of up to 3 images each."
  (let* ((images-raw (split-string (or (plist-get attr :images) "") "," t "[ \t\n]+"))
         (images-parsed (mapcar #'ox-ghost--parse-gallery-image images-raw))
         ;; Add row index to each image (3 images per row)
         (images (cl-loop for img in images-parsed
                          for idx from 0
                          collect (append img `((row . ,(/ idx 3))))))
         (caption (or (plist-get attr :caption) "")))
    (ox-ghost--node "gallery"
      `(images . ,(vconcat images))
      `(caption . ,caption))))

;;; ----------------------------------------------------------------------------
;;; Video Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-video (attr text)
  "Create video card from ATTR and TEXT (caption)."
  (let ((width (plist-get attr :width))
        (height (plist-get attr :height))
        (duration (plist-get attr :duration))
        (thumb-width (plist-get attr :thumbnailWidth))
        (thumb-height (plist-get attr :thumbnailHeight)))
    (ox-ghost--node "video"
      `(src . ,(or (plist-get attr :src) ""))
      `(caption . ,(or text ""))
      `(fileName . ,(or (plist-get attr :fileName) ""))
      `(mimeType . ,(or (plist-get attr :mimeType) ""))
      `(width . ,(when width (if (stringp width) (string-to-number width) width)))
      `(height . ,(when height (if (stringp height) (string-to-number height) height)))
      `(duration . ,(if duration (if (stringp duration) (string-to-number duration) duration) 0))
      `(thumbnailSrc . ,(or (plist-get attr :thumbnailSrc) ""))
      `(customThumbnailSrc . ,(or (plist-get attr :customThumbnailSrc) ""))
      `(thumbnailWidth . ,(when thumb-width (if (stringp thumb-width) (string-to-number thumb-width) thumb-width)))
      `(thumbnailHeight . ,(when thumb-height (if (stringp thumb-height) (string-to-number thumb-height) thumb-height)))
      `(cardWidth . ,(or (plist-get attr :cardWidth) "regular"))
      `(loop . :json-false))))

;;; ----------------------------------------------------------------------------
;;; Audio Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-audio (attr text)
  "Create audio card from ATTR and TEXT (title)."
  (let ((duration (plist-get attr :duration)))
    (ox-ghost--node "audio"
      `(src . ,(or (plist-get attr :src) ""))
      `(title . ,(or text ""))
      `(duration . ,(if duration (if (stringp duration) (string-to-number duration) duration) 0))
      `(thumbnailSrc . ,(or (plist-get attr :thumbnailSrc) ""))
      `(mimeType . ,(or (plist-get attr :mimeType) "")))))

;;; ----------------------------------------------------------------------------
;;; File Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-file (attr text)
  "Create file card from ATTR and TEXT (title)."
  (ox-ghost--node "file"
    `(src . ,(or (plist-get attr :src) ""))
    `(title . ,text)
    `(caption . ,(or (plist-get attr :caption) ""))
    `(fileName . ,(or (plist-get attr :fileName) ""))
    `(fileSize . ,(plist-get attr :fileSize))))

;;; ============================================================================
;;; Content Cards
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Callout Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-callout (attr text)
  "Create callout card from ATTR and TEXT."
  (ox-ghost--node "callout"
    `(calloutEmoji . ,(or (plist-get attr :emoji) "💡"))
    `(calloutText . ,text)
    `(backgroundColor . ,(or (plist-get attr :color) "blue"))))

;;; ----------------------------------------------------------------------------
;;; Toggle Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-toggle (attr special-block)
  "Create toggle card from ATTR and SPECIAL-BLOCK element.
Toggle content supports HTML - renders inner content as HTML."
  (let ((html-content (ox-ghost--render-to-html special-block)))
    (ox-ghost--node "toggle"
      `(heading . ,(or (plist-get attr :heading) ""))
      `(content . ,html-content))))

;;; ----------------------------------------------------------------------------
;;; Button Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-button (attr text)
  "Create button card from ATTR and TEXT."
  (ox-ghost--node "button"
    `(buttonText . ,text)
    `(buttonUrl . ,(or (plist-get attr :url) ""))
    `(alignment . ,(or (plist-get attr :alignment) "center"))))

;;; ----------------------------------------------------------------------------
;;; Header Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-header (attr text)
  "Create header card from ATTR and TEXT.
Supports: size (small/medium/large), style (image/light/dark/accent),
layout (split/full for v2), backgroundImageSrc, subheader, buttonText, buttonUrl."
  (let* ((style-val (or (plist-get attr :style) "dark"))
         (layout-val (plist-get attr :layout))
         ;; Use v2 for split layout
         (is-v2 (or layout-val (string= style-val "split")))
         (node-props
          `((size . ,(or (plist-get attr :size) "small"))
            (header . ,text))))
    ;; Version and layout/style handling
    (if is-v2
        (progn
          (push '(version . 2) node-props)
          (push `(layout . ,(or layout-val "split")) node-props)
          ;; v2 needs these defaults
          (push `(backgroundColor . ,(or (plist-get attr :backgroundColor) "")) node-props)
          (push `(backgroundSize . "cover") node-props)
          (push `(swapped . ,(if (plist-get attr :swapped) t :json-false)) node-props))
      (push `(style . ,style-val) node-props))
    ;; Optional subheader
    (when (plist-get attr :subheader)
      (push `(subheader . ,(plist-get attr :subheader)) node-props))
    ;; Background image
    (when (plist-get attr :backgroundImageSrc)
      (push `(backgroundImageSrc . ,(plist-get attr :backgroundImageSrc)) node-props))
    ;; Button properties
    (when (plist-get attr :buttonText)
      (push `(buttonEnabled . t) node-props)
      (push `(buttonText . ,(plist-get attr :buttonText)) node-props)
      (push `(buttonUrl . ,(or (plist-get attr :buttonUrl) "")) node-props))
    (apply #'ox-ghost--node "header" node-props)))

;;; ----------------------------------------------------------------------------
;;; Bookmark Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-bookmark (attr text)
  "Create bookmark card from ATTR and TEXT (title fallback)."
  (let ((metadata `((title . ,(or (plist-get attr :title) text)))))
    (when (plist-get attr :description)
      (push `(description . ,(plist-get attr :description)) metadata))
    (when (plist-get attr :thumbnail)
      (push `(thumbnail . ,(plist-get attr :thumbnail)) metadata))
    (when (plist-get attr :icon)
      (push `(icon . ,(plist-get attr :icon)) metadata))
    (ox-ghost--node "bookmark"
      `(url . ,(or (plist-get attr :url) ""))
      `(metadata . ,metadata))))

;;; ----------------------------------------------------------------------------
;;; Embed Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-embed (attr text)
  "Create embed card from ATTR and TEXT (title)."
  (ox-ghost--node "embed"
    `(url . ,(or (plist-get attr :url) ""))
    `(embedType . "url")
    `(metadata . ((title . ,text)))
    `(html . ,(ox-ghost--strip-quotes (or (plist-get attr :html) "")))))

;;; ----------------------------------------------------------------------------
;;; Call-to-Action Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-cta (attr text)
  "Create call-to-action card from ATTR and TEXT."
  (let ((node-props
         `((layout . ,(or (plist-get attr :layout) "minimal"))
           (textValue . ,text)
           (showButton . t)
           (buttonText . ,(or (plist-get attr :buttonText) "Learn more"))
           (buttonUrl . ,(or (plist-get attr :url) ""))
           ;; Hide sponsor label by default unless explicitly requested
           (hasSponsorLabel . ,(if (plist-get attr :sponsor) t :json-false)))))
    ;; Optional color properties
    (when (plist-get attr :buttonColor)
      (push `(buttonColor . ,(plist-get attr :buttonColor)) node-props))
    (when (plist-get attr :buttonTextColor)
      (push `(buttonTextColor . ,(plist-get attr :buttonTextColor)) node-props))
    (when (plist-get attr :backgroundColor)
      (push `(backgroundColor . ,(plist-get attr :backgroundColor)) node-props))
    (when (plist-get attr :alignment)
      (push `(alignment . ,(plist-get attr :alignment)) node-props))
    (apply #'ox-ghost--node "call-to-action" node-props)))

;;; ============================================================================
;;; Email Cards
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Email Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-email (text)
  "Create email card from TEXT (HTML content)."
  (ox-ghost--node "email"
    `(html . ,text)))

;;; ----------------------------------------------------------------------------
;;; Email-CTA Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-email-cta (attr text)
  "Create email-cta card from ATTR and TEXT (HTML content)."
  (ox-ghost--node "email-cta"
    `(html . ,text)
    `(alignment . ,(or (plist-get attr :alignment) "left"))
    `(buttonText . ,(or (plist-get attr :buttonText) ""))
    `(buttonUrl . ,(or (plist-get attr :buttonUrl) ""))
    `(segment . ,(or (plist-get attr :segment) "status:free"))
    `(showButton . ,(if (plist-get attr :buttonText) t :json-false))
    `(showDividers . ,(if (plist-get attr :dividers)
                          (if (member (plist-get attr :dividers) '("true" "t" "yes"))
                              t :json-false)
                        t))))

;;; ============================================================================
;;; Membership Cards
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Signup Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-signup (attr)
  "Create signup card from ATTR."
  (ox-ghost--node "signup"
    `(layout . ,(or (plist-get attr :layout) "regular"))
    `(buttonText . ,(or (plist-get attr :buttonText) "Subscribe"))))

;;; ----------------------------------------------------------------------------
;;; Product Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-product (attr text)
  "Create product card from ATTR and TEXT (title)."
  (ox-ghost--node "product"
    `(productTitle . ,text)
    `(productDescription . "")
    `(productUrl . ,(or (plist-get attr :url) ""))
    `(productButtonEnabled . ,(if (plist-get attr :buttonText) t :json-false))
    `(productButton . ,(or (plist-get attr :buttonText) ""))))

;;; ----------------------------------------------------------------------------
;;; Paywall Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-paywall ()
  "Create paywall card - marks content below as members-only."
  (ox-ghost--node "paywall"))

;;; ============================================================================
;;; Integration Cards
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Transistor Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-transistor (attr)
  "Create transistor podcast card from ATTR."
  (ox-ghost--node "transistor"
    `(episodeUrl . ,(or (plist-get attr :url) ""))))

;;; ============================================================================
;;; Raw Content Cards
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; HTML Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-html (html)
  "Create HTML card from raw HTML content."
  (ox-ghost--node "html" `(html . ,html)))

;;; ----------------------------------------------------------------------------
;;; Markdown Card
;;; ----------------------------------------------------------------------------

(defun ox-ghost--card-markdown (text)
  "Create markdown card from TEXT."
  (ox-ghost--node "markdown"
    `(markdown . ,text)))

;;; ============================================================================
;;; REPL Block (Special Handling)
;;; ============================================================================

(defun ox-ghost--repl-block (attr contents info)
  "Render REPL block with style from ATTR.
Styles:
  simple  - pass through contents (default)
  labeled - add 'Output:' label before results
  callout - wrap output in a callout
  toggle  - code in toggle heading, output in content
  aside   - wrap all in aside block"
  (let* ((style (or (plist-get attr :style) "simple"))
         (label (or (plist-get attr :label) "Output"))
         (children (ox-ghost--parse-block-nodes contents))
         (is-code-p (lambda (n)
                      (and (string= (cdr (assoc 'type n)) "codeblock")
                           (not (string= (cdr (assoc 'language n)) "text")))))
         (is-output-p (lambda (n)
                        (or (not (string= (cdr (assoc 'type n)) "codeblock"))
                            (string= (cdr (assoc 'language n)) "text"))))
         (code-nodes (seq-filter is-code-p children))
         (output-nodes (seq-filter is-output-p children))
         (first-code (car code-nodes))
         (code-lang (when first-code (cdr (assoc 'language first-code))))
         (encode-nodes (lambda (nodes)
                         (mapconcat (lambda (n)
                                      (concat ox-ghost--node-marker (json-encode n)))
                                    nodes ""))))
    (pcase style
      ("simple" contents)
      ("labeled"
       (concat
        (funcall encode-nodes code-nodes)
        (ox-ghost--para-node (ox-ghost--text-node (concat label ":")))
        (funcall encode-nodes output-nodes)))
      ("callout"
       (let ((output-text (mapconcat (lambda (n) (or (cdr (assoc 'code n)) ""))
                                     output-nodes "\n")))
         (concat
          (funcall encode-nodes code-nodes)
          (ox-ghost--node "callout"
            `(calloutEmoji . ,(or (plist-get attr :emoji) "📤"))
            `(calloutText . ,output-text)
            `(backgroundColor . ,(or (plist-get attr :color) "grey"))))))
      ("toggle"
       (let* ((heading (or (plist-get attr :heading)
                           (format "Code (%s)" (or code-lang "source"))))
              (code-text (mapconcat (lambda (n) (or (cdr (assoc 'code n)) ""))
                                    code-nodes "\n")))
         (concat
          (ox-ghost--node "toggle" `(heading . ,heading) `(content . ,code-text))
          (funcall encode-nodes output-nodes))))
      ("aside"
       (ox-ghost--node "aside"
         `(children . ,(vconcat children))
         '(direction . "ltr")
         '(format . "")
         '(indent . 0)))
      (_ contents))))

;;; ============================================================================
;;; Structural Elements
;;; ============================================================================

(defun ox-ghost--horizontal-rule (hr contents info)
  "Transcode HORIZONTAL-RULE."
  (ox-ghost--node "horizontalrule"))

(defun ox-ghost--table (table contents info)
  "Transcode TABLE to HTML node.
Handles errors gracefully by returning a placeholder instead of crashing."
  (condition-case err
      (let* ((table-str (buffer-substring-no-properties
                         (org-element-property :begin table)
                         (org-element-property :end table)))
             (html (org-export-string-as table-str 'html t '(:with-toc nil))))
        (if (and html (stringp html) (not (string-empty-p (string-trim html))))
            (ox-ghost--card-html (string-trim html))
          ;; Empty result - return placeholder
          (ox-ghost--card-html
           (format "<!-- Table export returned empty result -->\n<pre>%s</pre>"
                   (ox-ghost--escape-html table-str)))))
    (error
     ;; On error, log a warning and return a visible placeholder
     (message "ox-ghost: Table export failed: %S" err)
     (let ((table-str (ignore-errors
                        (buffer-substring-no-properties
                         (org-element-property :begin table)
                         (org-element-property :end table)))))
       (ox-ghost--card-html
        (format "<!-- Table export error: %s -->\n<pre>%s</pre>"
                (error-message-string err)
                (ox-ghost--escape-html (or table-str "[table content unavailable]"))))))))

;;; ============================================================================
;;; Export Block Transcoder
;;; ============================================================================

(defun ox-ghost--export-block (export-block contents info)
  "Transcode EXPORT-BLOCK."
  (let ((type (upcase (org-element-property :type export-block)))
        (value (org-element-property :value export-block)))
    (cond
     ((string= type "HTML")
      (ox-ghost--card-html value))
     ((string= type "LEXICAL")
      (concat ox-ghost--node-marker value))
     (t ""))))

;;; ============================================================================
;;; Special Block Dispatcher
;;; ============================================================================

(defun ox-ghost--special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK by dispatching to appropriate card constructor."
  (let* ((type (upcase (org-element-property :type special-block)))
         (params (org-element-property :parameters special-block))
         (attr (ox-ghost--parse-params params))
         (text (ox-ghost--extract-text contents)))
    (pcase type
      ;; Content Cards
      ("CALLOUT"    (ox-ghost--card-callout attr text))
      ("TOGGLE"     (ox-ghost--card-toggle attr special-block))
      ("BUTTON"     (ox-ghost--card-button attr text))
      ("HEADER"     (ox-ghost--card-header attr text))
      ("BOOKMARK"   (ox-ghost--card-bookmark attr text))
      ("EMBED"      (ox-ghost--card-embed attr text))
      ("CTA"        (ox-ghost--card-cta attr text))
      ;; Block Elements
      ("ASIDE"      (ox-ghost--card-aside contents))
      ;; Media Cards
      ("GALLERY"    (ox-ghost--card-gallery attr contents))
      ("VIDEO"      (ox-ghost--card-video attr text))
      ("AUDIO"      (ox-ghost--card-audio attr text))
      ("FILE"       (ox-ghost--card-file attr text))
      ;; Email Cards
      ("EMAIL"      (ox-ghost--card-email text))
      ("EMAIL-CTA"  (ox-ghost--card-email-cta attr text))
      ;; Membership Cards
      ("SIGNUP"     (ox-ghost--card-signup attr))
      ("PRODUCT"    (ox-ghost--card-product attr text))
      ("PAYWALL"    (ox-ghost--card-paywall))
      ;; Integration Cards
      ("TRANSISTOR" (ox-ghost--card-transistor attr))
      ;; Raw Content
      ("MARKDOWN"   (ox-ghost--card-markdown text))
      ;; Special Handling
      ("REPL"       (ox-ghost--repl-block attr contents info))
      ;; Unknown - wrap in HTML comment
      (_            (ox-ghost--card-html
                     (format "<!-- Unknown: %s -->\n%s" type text))))))

;;; ============================================================================
;;; Ghost Metadata Extraction
;;; ============================================================================

(defun ox-ghost--get-ghost-property (key)
  "Get GHOST_KEY property from current org buffer.
Returns nil if not found."
  (save-excursion
    (goto-char (point-min))
    (let ((prop-re (format "^#\\+GHOST_%s:\\s-*\\(.+\\)$" (upcase key))))
      (when (re-search-forward prop-re nil t)
        (string-trim (match-string 1))))))

(defun ox-ghost--parse-comma-list (str)
  "Parse comma-separated STR into list of trimmed strings."
  (when str
    (mapcar #'string-trim (split-string str "," t "\\s-*"))))

(defun ox-ghost--get-org-keyword (key)
  "Get #+KEY value from current org buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((prop-re (format "^#\\+%s:\\s-*\\(.+\\)$" (upcase key))))
      (when (re-search-forward prop-re nil t)
        (string-trim (match-string 1))))))

(defun ox-ghost--slugify (title)
  "Convert TITLE to a URL-friendly slug."
  (let ((slug (downcase title)))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "^-+\\|-+$" "" slug))
    slug))

(defun ox-ghost-extract-metadata ()
  "Extract Ghost metadata from current org buffer.
Merges org properties with `ox-ghost-ghost-defaults'."
  (let* ((defaults ox-ghost-ghost-defaults)
         (title (or (ox-ghost--get-org-keyword "title") "Untitled"))
         (metadata
          `((title . ,title)
            (slug . ,(or (ox-ghost--get-ghost-property "slug")
                         (alist-get 'slug defaults)
                         (ox-ghost--slugify title)))
            (type . ,(or (ox-ghost--get-ghost-property "type")
                         (alist-get 'type defaults)
                         "post"))
            (status . ,(or (ox-ghost--get-ghost-property "status")
                           (alist-get 'status defaults)
                           "draft"))
            (visibility . ,(or (ox-ghost--get-ghost-property "visibility")
                               (alist-get 'visibility defaults)
                               "public"))
            (featured . ,(let ((val (ox-ghost--get-ghost-property "featured")))
                           (and val (member val '("t" "true" "yes" "1")))))
            (tags . ,(or (ox-ghost--parse-comma-list
                          (ox-ghost--get-ghost-property "tags"))
                         (alist-get 'tags defaults)))
            (tiers . ,(ox-ghost--parse-comma-list
                       (ox-ghost--get-ghost-property "tiers")))
            (author . ,(or (ox-ghost--get-org-keyword "author")
                           (alist-get 'author defaults)))
            (date . ,(ox-ghost--get-org-keyword "date"))
            (excerpt . ,(or (ox-ghost--get-ghost-property "excerpt")
                            (ox-ghost--get-org-keyword "description")))
            (feature_image . ,(ox-ghost--get-ghost-property "image"))
            (feature_image_alt . ,(ox-ghost--get-ghost-property "image_alt"))
            (newsletter . ,(or (ox-ghost--get-ghost-property "newsletter")
                               (alist-get 'newsletter defaults)))
            (email_subject . ,(ox-ghost--get-ghost-property "email_subject"))
            (email_segment . ,(ox-ghost--get-ghost-property "email_segment"))
            (canonical_url . ,(ox-ghost--get-ghost-property "canonical"))
            (meta_title . ,(ox-ghost--get-ghost-property "meta_title"))
            (meta_description . ,(ox-ghost--get-ghost-property "meta_description"))
            (og_title . ,(ox-ghost--get-ghost-property "og_title"))
            (og_description . ,(ox-ghost--get-ghost-property "og_description"))
            (og_image . ,(ox-ghost--get-ghost-property "og_image"))
            (twitter_title . ,(ox-ghost--get-ghost-property "twitter_title"))
            (twitter_description . ,(ox-ghost--get-ghost-property "twitter_description"))
            (twitter_image . ,(ox-ghost--get-ghost-property "twitter_image"))
            (codeinjection_head . ,(ox-ghost--get-ghost-property "head"))
            (codeinjection_foot . ,(ox-ghost--get-ghost-property "foot")))))
    ;; Remove nil values
    (cl-remove-if (lambda (pair) (null (cdr pair))) metadata)))

(defun ox-ghost-write-metadata (metadata file)
  "Write METADATA alist to FILE as JSON."
  (with-temp-file file
    (insert (json-encode metadata))
    ;; Pretty print
    (json-pretty-print-buffer)))

;;; ============================================================================
;;; Public API
;;; ============================================================================

;;;###autoload
(defun org-lexical-export-as-json (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to Ghost Lexical JSON buffer.
The buffer is pretty-printed and uses `json-mode' if available."
  (interactive)
  (org-export-to-buffer 'ghost "*Org Lexical Export*"
    async subtreep visible-only body-only ext-plist
    (lambda ()
      ;; Pretty-print the JSON
      (let ((json-encoding-pretty-print t)
            (json-encoding-default-indentation "  "))
        (condition-case nil
            (let ((obj (json-read-from-string (buffer-string))))
              (erase-buffer)
              (insert (json-encode obj)))
          (error nil)))  ; Leave as-is if parsing fails
      ;; Enable JSON mode for syntax highlighting
      (cond
       ((fboundp 'json-mode) (json-mode))
       ((fboundp 'js-json-mode) (js-json-mode))
       ((fboundp 'javascript-mode) (javascript-mode))
       (t (fundamental-mode)))
      (goto-char (point-min)))))

;;;###autoload
(defun org-lexical-export-to-file (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to Ghost Lexical JSON file."
  (interactive)
  (let ((outfile (org-export-output-file-name ".json")))
    (org-export-to-file 'ghost outfile
      async subtreep visible-only body-only ext-plist)))

;;; ----------------------------------------------------------------------------
;;; Node.js Renderer Integration
;;; ----------------------------------------------------------------------------

(defun ox-ghost--renderer-path ()
  "Return path to ox-ghost-render.js.
Uses `ox-ghost-renderer-path' if set, otherwise the package directory."
  (or ox-ghost-renderer-path
      (expand-file-name "ox-ghost-render.js" ox-ghost--directory)))

(defun ox-ghost--ensure-dependencies ()
  "Ensure Node.js dependencies are installed.
Prompts to run npm install if node_modules is missing."
  (let* ((renderer (ox-ghost--renderer-path))
         (dir (file-name-directory renderer))
         (node-modules (expand-file-name "node_modules" dir))
         (package-json (expand-file-name "package.json" dir)))
    ;; Check renderer exists
    (unless (file-exists-p renderer)
      (error "Renderer not found: %s" renderer))
    ;; Check package.json exists
    (unless (file-exists-p package-json)
      (error "package.json not found in %s" dir))
    ;; Check node_modules exists, offer to install
    (unless (file-directory-p node-modules)
      (if (yes-or-no-p
           (format "Node.js dependencies not installed.\nRun `npm install` in %s? " dir))
          (let ((default-directory dir))
            (message "Installing dependencies...")
            (let ((output (shell-command-to-string "npm install 2>&1")))
              (if (file-directory-p node-modules)
                  (message "Dependencies installed successfully.")
                (error "npm install failed:\n%s" output))))
        (error "Cannot render without dependencies.\nRun: cd %s && npm install" dir)))))

(defun ox-ghost--run-renderer (json-file &optional html-file quiet)
  "Run the Node.js renderer on JSON-FILE.
If HTML-FILE is provided, generate HTML output.
If QUIET is non-nil, only return stats.
Automatically prompts to install dependencies if missing."
  (ox-ghost--ensure-dependencies)
  (let* ((renderer (ox-ghost--renderer-path))
         (args (list json-file))
         (cmd nil))
    (when html-file
      (setq args (append args (list "--html" html-file))))
    (when quiet
      (setq args (append args (list "--quiet"))))
    (setq cmd (mapconcat #'shell-quote-argument
                         (cons ox-ghost-node-command (cons renderer args))
                         " "))
    (shell-command-to-string cmd)))

;;;###autoload
(defun org-lexical-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to HTML via Ghost Lexical JSON.
First exports to JSON, then renders to HTML using Ghost's renderer.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed to the exporter."
  (interactive)
  (ignore async) ; Always run synchronously for the render step
  (let* ((json-file (org-export-output-file-name ".json"))
         (html-file (org-export-output-file-name ".html")))
    ;; Export to JSON first (synchronously)
    (org-export-to-file 'ghost json-file
      nil subtreep visible-only body-only ext-plist)
    ;; Then render to HTML
    (let ((output (ox-ghost--run-renderer json-file html-file)))
      (message "Exported: %s\n%s" html-file (string-trim output)))
    html-file))

;;;###autoload
(defun org-lexical-export-to-html-and-open (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to HTML and open in browser.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed to the exporter."
  (interactive)
  (let ((html-file (org-lexical-export-to-html async subtreep visible-only body-only ext-plist)))
    (browse-url-of-file html-file)))

;;;###autoload
(defun org-lexical-validate (&optional async subtreep visible-only body-only ext-plist)
  "Validate Ghost Lexical JSON using Ghost's renderer.
When called interactively, prompts for a JSON file.
When called from export menu, validates the default output file.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are ignored."
  (interactive)
  (let* ((default-file (org-export-output-file-name ".json"))
         (json-file (if (called-interactively-p 'any)
                        (read-file-name "JSON file to validate: "
                                        nil default-file t default-file)
                      default-file))
         (output (ox-ghost--run-renderer json-file nil nil)))
    (ignore async subtreep visible-only body-only ext-plist)
    (with-current-buffer (get-buffer-create "*Lexical Validation*")
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun org-lexical-export-for-ghost (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to Ghost Lexical JSON and metadata.json for Ghost.
Creates both <name>.json (content) and <name>-metadata.json (Ghost properties).
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed to the exporter."
  (interactive)
  (ignore async)
  (let* ((base-name (file-name-sans-extension (buffer-file-name)))
         (json-file (concat base-name ".json"))
         (meta-file (concat base-name "-metadata.json"))
         (metadata (ox-ghost-extract-metadata)))
    ;; Export Lexical JSON
    (org-export-to-file 'ghost json-file
      nil subtreep visible-only body-only ext-plist)
    ;; Write metadata
    (ox-ghost-write-metadata metadata meta-file)
    (message "Exported:\n  Content: %s\n  Metadata: %s\n  Slug: %s"
             (file-name-nondirectory json-file)
             (file-name-nondirectory meta-file)
             (alist-get 'slug metadata))
    (list json-file meta-file)))

;;;###autoload
(defun org-lexical-show-metadata ()
  "Display extracted Ghost metadata for current buffer."
  (interactive)
  (let ((metadata (ox-ghost-extract-metadata)))
    (with-current-buffer (get-buffer-create "*Ghost Metadata*")
      (erase-buffer)
      (insert (json-encode metadata))
      (json-pretty-print-buffer)
      (goto-char (point-min))
      (when (fboundp 'json-mode) (json-mode))
      (display-buffer (current-buffer)))))

(provide 'ox-ghost)
;;; ox-ghost.el ends here
