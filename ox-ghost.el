;;; ox-ghost.el --- Export org-mode to Ghost Ghost Lexical JSON -*- lexical-binding: t; -*-

;; Author: ii.coop
;; Version: 0.7.0
;; Package-Requires: ((emacs "27.1") (org "9.5"))
;; Keywords: org, ghost, lexical, export

;;; Commentary:

;; Export org-mode documents to Ghost's Ghost Lexical JSON format.
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

;;; Code:

(require 'org)
(require 'ox)
(require 'ox-html)
(require 'json)
(require 'cl-lib)

;;; Customization

(defgroup ox-ghost nil
  "Options for exporting Org files to Ghost Ghost Lexical JSON."
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

;;; Constants

(defconst ox-ghost--node-marker "‹LEXNODE›"
  "Marker to separate JSON node strings.")

;;; Format flags (bitmask)
(defconst ox-ghost--format-bold 1)
(defconst ox-ghost--format-italic 2)
(defconst ox-ghost--format-strikethrough 4)
(defconst ox-ghost--format-underline 8)
(defconst ox-ghost--format-code 16)

;; Common emoji shortcodes used in Ghost callouts
(defconst ox-ghost--emoji-map
  '(("bulb" . "💡")
    ("light_bulb" . "💡")
    ("warning" . "⚠️")
    ("exclamation" . "❗")
    ("question" . "❓")
    ("info" . "ℹ️")
    ("information_source" . "ℹ️")
    ("white_check_mark" . "✅")
    ("check" . "✅")
    ("x" . "❌")
    ("cross_mark" . "❌")
    ("star" . "⭐")
    ("fire" . "🔥")
    ("rocket" . "🚀")
    ("tada" . "🎉")
    ("sparkles" . "✨")
    ("heart" . "❤️")
    ("thumbsup" . "👍")
    ("thumbsdown" . "👎")
    ("eyes" . "👀")
    ("memo" . "📝")
    ("pencil" . "✏️")
    ("book" . "📖")
    ("link" . "🔗")
    ("lock" . "🔒")
    ("key" . "🔑")
    ("gear" . "⚙️")
    ("wrench" . "🔧")
    ("hammer" . "🔨")
    ("zap" . "⚡")
    ("boom" . "💥")
    ("bug" . "🐛")
    ("robot" . "🤖")
    ("unicorn" . "🦄")
    ("speech_balloon" . "💬")
    ("thought_balloon" . "💭")
    ("email" . "📧")
    ("package" . "📦")
    ("gift" . "🎁")
    ("trophy" . "🏆")
    ("medal" . "🏅")
    ("target" . "🎯")
    ("dart" . "🎯")
    ("alarm_clock" . "⏰")
    ("hourglass" . "⏳")
    ("calendar" . "📅")
    ("art" . "🎨")
    ("palette" . "🎨")
    ("paintbrush" . "🖌️")
    ("computer" . "💻")
    ("desktop" . "🖥️")
    ("keyboard" . "⌨️")
    ("mouse" . "🖱️")
    ("cloud" . "☁️")
    ("sun" . "☀️")
    ("moon" . "🌙")
    ("earth" . "🌍")
    ("globe" . "🌐"))
  "Map of emoji shortcodes to actual emoji characters.")

(defun ox-ghost--emoji (name)
  "Convert emoji NAME (shortcode or emoji) to actual emoji.
If NAME is already an emoji, return it unchanged.
If NAME is a shortcode, look it up in `ox-ghost--emoji-map'."
  (if (or (null name) (string-empty-p name))
      "💡"  ; default
    (or (cdr (assoc name ox-ghost--emoji-map))
        name)))  ; return as-is if not found (might already be emoji)

;;; Define the backend

(org-export-define-backend 'ghost
  '((headline . ox-ghost--headline)
    (section . ox-ghost--section)
    (paragraph . ox-ghost--paragraph)
    (plain-text . ox-ghost--plain-text)
    (bold . ox-ghost--bold)
    (italic . ox-ghost--italic)
    (underline . ox-ghost--underline)
    (strike-through . ox-ghost--strike-through)
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
  '(?x "Export to Ghost (Ghost)"
       ((?x "As JSON buffer" org-lexical-export-as-json)
        (?j "To JSON file" org-lexical-export-to-file)
        (?g "For Ghost (JSON + metadata)" org-lexical-export-for-ghost)
        (?h "To HTML file" org-lexical-export-to-html)
        (?o "To HTML and open" org-lexical-export-to-html-and-open)
        (?v "Validate JSON" org-lexical-validate)
        (?m "Show metadata" org-lexical-show-metadata))))

;;; Node constructors - return JSON strings

(defun ox-ghost--node (type &rest props)
  "Create a JSON node string of TYPE with PROPS."
  (let ((node `((type . ,type) (version . 1) ,@props)))
    (concat ox-ghost--node-marker (json-encode node))))

(defun ox-ghost--text-node (text &optional format-val)
  "Create text node JSON."
  (ox-ghost--node "text"
    `(text . ,text)
    `(format . ,(or format-val 0))
    '(style . "")
    '(detail . 0)
    '(mode . "normal")))

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

(defun ox-ghost--heading-node (level children-json)
  "Create heading node JSON."
  (let ((children (ox-ghost--parse-children children-json)))
    (ox-ghost--node "heading"
      `(tag . ,(format "h%d" (min level 6)))
      `(children . ,children)
      '(direction . "ltr")
      '(format . "")
      '(indent . 0))))

;;; Helpers

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

(defun ox-ghost--gallery-node (attr contents)
  "Create gallery node from ATTR and CONTENTS (images extracted from links)."
  (let* ((images-raw (split-string (or (plist-get attr :images) "") "," t "[ \t\n]+"))
         (images (mapcar (lambda (src)
                           `((src . ,(string-trim src))
                             (caption . "")
                             (alt . "")))
                         images-raw)))
    (ox-ghost--node "gallery"
      `(images . ,(vconcat images))
      `(caption . ""))))

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
            `(calloutEmoji . ,(ox-ghost--emoji (or (plist-get attr :emoji) "📤")))
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

(defun ox-ghost--image-node (src desc link)
  "Create image node from SRC, DESC, and LINK element."
  (let* ((alt-text (or desc ""))
         (parent (org-element-property :parent link))
         (attrs (when parent (org-export-read-attribute :attr_lexical parent))))
    (ox-ghost--node "image"
      `(src . ,src)
      `(alt . ,alt-text)
      `(caption . "")
      `(title . "")
      `(cardWidth . ,(or (plist-get attrs :cardWidth) "regular"))
      `(width . nil)
      `(height . nil)
      `(href . ""))))

;;; Transcoders

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

(defun ox-ghost--headline (headline contents info)
  "Transcode HEADLINE."
  (let* ((level (1+ (org-element-property :level headline)))
         (title (org-element-property :raw-value headline))
         (heading (ox-ghost--heading-node level (ox-ghost--text-node title))))
    (concat heading (or contents ""))))

(defun ox-ghost--paragraph (paragraph contents info)
  "Transcode PARAGRAPH."
  (if (and contents (not (string-empty-p (string-trim contents))))
      (let ((parent-type (org-element-type (org-element-property :parent paragraph))))
        ;; Don't wrap if inside item (list handles it)
        (if (eq parent-type 'item)
            contents
          (ox-ghost--para-node contents)))
    ""))

(defun ox-ghost--plain-text (text info)
  "Transcode plain TEXT, preserving surrounding whitespace."
  ;; Collapse internal whitespace/newlines but preserve leading/trailing
  (let* ((collapsed (replace-regexp-in-string "[ \t]*\n[ \t]*" " " text))
         (clean (replace-regexp-in-string "[ \t]+" " " collapsed)))
    (if (string-empty-p clean)
        ""
      (ox-ghost--text-node clean))))

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

(defun ox-ghost--code (code contents info)
  "Transcode inline CODE."
  (ox-ghost--text-node (org-element-property :value code) ox-ghost--format-code))

(defun ox-ghost--verbatim (verbatim contents info)
  "Transcode VERBATIM."
  (ox-ghost--text-node (org-element-property :value verbatim) ox-ghost--format-code))

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

(defun ox-ghost--link (link contents info)
  "Transcode LINK."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link))
         (desc (if contents (ox-ghost--extract-text contents) path)))
    (cond
     ;; Images (file:// local paths)
     ((and (string= type "file")
           (member (downcase (or (file-name-extension path) ""))
                   '("png" "jpg" "jpeg" "gif" "webp" "svg")))
      (ox-ghost--image-node path desc link))
     ;; Images (http/https URLs)
     ((and (member type '("http" "https"))
           (member (downcase (or (file-name-extension path) ""))
                   '("png" "jpg" "jpeg" "gif" "webp" "svg")))
      (ox-ghost--image-node (concat type ":" path) desc link))
     ;; Regular links
     (t
      (let ((url (cond
                  ((member type '("http" "https")) (concat type ":" path))
                  ((string= type "file") path)
                  (t raw-link)))
            (link-text (or desc path)))
        (ox-ghost--node "link"
          `(url . ,url)
          `(children . ,(vector (ox-ghost--text-node-alist link-text)))
          '(direction . "ltr")
          '(format . "")
          '(indent . 0)
          '(rel . "noopener")))))))

(defun ox-ghost--text-node-alist (text &optional format-val)
  "Return text node as alist (not JSON string)."
  `((type . "text")
    (version . 1)
    (text . ,text)
    (format . ,(or format-val 0))
    (style . "")
    (detail . 0)
    (mode . "normal")))

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
  "Transcode list ITEM. Nested lists stay as children."
  (let* ((checkbox (org-element-property :checkbox item))
         (counter (or (org-element-property :counter item) 1))
         (children (ox-ghost--parse-block-nodes contents)))
    (ox-ghost--node "listitem"
      `(children . ,(vconcat children))
      '(direction . "ltr")
      '(format . "")
      '(indent . 0)
      `(value . ,counter)
      `(checked . ,(cond ((eq checkbox 'on) t)
                         ((eq checkbox 'off) :json-false)
                         (t nil))))))

(defun ox-ghost--quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK."
  (let ((children (ox-ghost--parse-children contents)))
    (ox-ghost--node "quote"
      `(children . ,children)
      '(direction . "ltr")
      '(format . "")
      '(indent . 0))))

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

(defun ox-ghost--export-block (export-block contents info)
  "Transcode EXPORT-BLOCK."
  (let ((type (upcase (org-element-property :type export-block)))
        (value (org-element-property :value export-block)))
    (cond
     ((string= type "HTML")
      (ox-ghost--node "html" `(html . ,value)))
     ((string= type "LEXICAL")
      (concat ox-ghost--node-marker value))
     (t ""))))

(defun ox-ghost--special-block (special-block contents info)
  "Transcode SPECIAL-BLOCK."
  (let* ((type (upcase (org-element-property :type special-block)))
         (params (org-element-property :parameters special-block))
         (attr (ox-ghost--parse-params params))
         (text (ox-ghost--extract-text contents)))
    (cond
     ((string= type "CALLOUT")
      (ox-ghost--node "callout"
        `(calloutEmoji . ,(ox-ghost--emoji (or (plist-get attr :emoji) "bulb")))
        `(calloutText . ,text)
        `(backgroundColor . ,(or (plist-get attr :color) "blue"))))
     ((string= type "TOGGLE")
      ;; Toggle content supports HTML - render inner content as HTML
      (let ((html-content (ox-ghost--render-to-html special-block)))
        (ox-ghost--node "toggle"
          `(heading . ,(or (plist-get attr :heading) ""))
          `(content . ,html-content))))
     ((string= type "ASIDE")
      (let ((children (ox-ghost--parse-children contents)))
        (ox-ghost--node "aside"
          `(children . ,children)
          '(direction . "ltr")
          '(format . "")
          '(indent . 0))))
     ((string= type "BUTTON")
      (ox-ghost--node "button"
        `(buttonText . ,text)
        `(buttonUrl . ,(or (plist-get attr :url) ""))
        `(alignment . ,(or (plist-get attr :alignment) "center"))))
     ((string= type "HEADER")
      (ox-ghost--node "header"
        `(size . ,(or (plist-get attr :size) "small"))
        `(header . ,text)))
     ((string= type "SIGNUP")
      (ox-ghost--node "signup"
        `(layout . ,(or (plist-get attr :layout) "regular"))
        `(buttonText . ,(or (plist-get attr :buttonText) "Subscribe"))))
     ((string= type "CTA")
      (ox-ghost--node "call-to-action"
        `(layout . ,(or (plist-get attr :layout) "minimal"))
        `(buttonText . ,(or (plist-get attr :buttonText) "Learn more"))
        `(buttonUrl . ,(or (plist-get attr :url) ""))
        `(textValue . ,text)))
     ((string= type "BOOKMARK")
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
     ((string= type "EMAIL")
      (ox-ghost--node "email"
        `(html . ,text)))
     ((string= type "PAYWALL")
      ;; Paywall is just a marker - content below is members-only
      (ox-ghost--node "paywall"))
     ((string= type "TRANSISTOR")
      (ox-ghost--node "transistor"
        `(episodeUrl . ,(or (plist-get attr :url) ""))))
     ((string= type "VIDEO")
      (ox-ghost--node "video"
        `(src . ,(or (plist-get attr :src) ""))
        `(caption . ,(or text ""))
        `(width . ,(plist-get attr :width))
        `(height . ,(plist-get attr :height))
        `(duration . ,(plist-get attr :duration))
        `(mimeType . ,(or (plist-get attr :mimeType) ""))
        `(thumbnailSrc . ,(or (plist-get attr :thumbnailSrc) ""))
        `(customThumbnail . ,(or (plist-get attr :customThumbnail) ""))
        `(loop . :json-false)))
     ((string= type "AUDIO")
      (ox-ghost--node "audio"
        `(src . ,(or (plist-get attr :src) ""))
        `(title . ,(or text ""))
        `(duration . ,(plist-get attr :duration))
        `(mimeType . ,(or (plist-get attr :mimeType) ""))))
     ((string= type "EMBED")
      ;; Include html directly in node creation (can't modify string afterward)
      (ox-ghost--node "embed"
        `(url . ,(or (plist-get attr :url) ""))
        `(embedType . "url")
        `(metadata . ((title . ,text)))
        `(html . ,(or (plist-get attr :html) ""))))
     ((string= type "GALLERY")
      (ox-ghost--gallery-node attr contents))
     ((string= type "FILE")
      (ox-ghost--node "file"
        `(src . ,(or (plist-get attr :src) ""))
        `(title . ,text)
        `(caption . "")
        `(fileName . ,(or (plist-get attr :fileName) ""))
        `(fileSize . nil)))
     ((string= type "PRODUCT")
      (ox-ghost--node "product"
        `(productTitle . ,text)
        `(productDescription . "")
        `(productUrl . ,(or (plist-get attr :url) ""))
        `(productButtonEnabled . ,(if (plist-get attr :buttonText) t :json-false))
        `(productButton . ,(or (plist-get attr :buttonText) ""))))
     ;; REPL block: wrap source + output with configurable style
     ;; Styles: simple (default), labeled, callout, toggle, aside
     ((string= type "REPL")
      (ox-ghost--repl-block attr contents info))
     (t
      (ox-ghost--node "html"
        `(html . ,(format "<!-- Unknown: %s -->\n%s" type text)))))))

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
            (ox-ghost--node "html" `(html . ,(string-trim html)))
          ;; Empty result - return placeholder
          (ox-ghost--node "html"
            `(html . ,(format "<!-- Table export returned empty result -->\n<pre>%s</pre>"
                              (ox-ghost--escape-html table-str))))))
    (error
     ;; On error, log a warning and return a visible placeholder
     (message "ox-ghost: Table export failed: %S" err)
     (let ((table-str (ignore-errors
                        (buffer-substring-no-properties
                         (org-element-property :begin table)
                         (org-element-property :end table)))))
       (ox-ghost--node "html"
         `(html . ,(format "<!-- Table export error: %s -->\n<pre>%s</pre>"
                           (error-message-string err)
                           (ox-ghost--escape-html (or table-str "[table content unavailable]")))))))))

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

(defun ox-ghost--line-break (lb contents info)
  "Transcode LINE-BREAK to Lexical linebreak node.
In Lexical, linebreaks are their own node type, not \\n in text."
  (ox-ghost--node "linebreak"))

(defun ox-ghost--entity (entity contents info)
  "Transcode ENTITY."
  (ox-ghost--text-node (org-element-property :utf-8 entity)))

;;; Ghost Metadata Extraction

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

;;; Public API

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

;;; Node.js Renderer Integration

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
    ;; Export Ghost Lexical JSON
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
