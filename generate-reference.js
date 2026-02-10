#!/usr/bin/env node
/**
 * Generate Ghost Node Reference document from schema
 *
 * Focus: Pure Ghost/Koenig/Lexical node documentation
 * Structure: Hierarchical from root, showing containment
 */

const fs = require('fs');
const path = require('path');

// Run extract-node-schema to get all schemas
const { execSync } = require('child_process');
const schemaJson = execSync('node extract-node-schema.js --all', { cwd: __dirname, encoding: 'utf8' });
const allSchemas = JSON.parse(schemaJson);

// Format properties for display
function formatProps(schema) {
  if (!schema || !schema.properties || schema.properties.length === 0) {
    return null;
  }
  return schema.properties.map(p => {
    let desc = `=${p.name}=`;
    if (p.values) {
      desc += ` (${p.values.join(', ')})`;
    } else if (p.default !== undefined && p.default !== '' && p.default !== null) {
      if (Array.isArray(p.default)) {
        desc += ` (default: [])`;
      } else {
        desc += ` (default: ${JSON.stringify(p.default)})`;
      }
    }
    return desc;
  }).join(', ');
}

// Generate org content
let org = `#+TITLE: Ghost Node Reference
#+SUBTITLE: Koenig Editor Node Types and Structure
#+OPTIONS: toc:nil num:nil

* Inheritance Hierarchy

Ghost's Koenig editor stores content as a tree of Lexical nodes. Nodes inherit from base classes that determine their capabilities.

#+BEGIN_SRC text
LexicalNode (base)
├── ElementNode (containers - have children, format, direction, indent)
│   ├── RootNode ─────────── root
│   ├── ParagraphNode ────── paragraph
│   ├── HeadingNode ──────── heading (extended-heading)
│   ├── QuoteNode ────────── quote (extended-quote)
│   ├── AsideNode ────────── aside
│   ├── ListNode ─────────── list
│   ├── ListItemNode ─────── listitem
│   └── LinkNode ─────────── link
│
├── TextNode (leaf - has text, format bitmask)
│   └── ExtendedTextNode ─── text (extended-text)
│
├── LineBreakNode (leaf - no properties)
│   └── ─────────────────── linebreak
│
└── DecoratorNode (self-contained blocks - no children)
    └── KoenigDecoratorNode
        ├── [Text & Callouts] callout, toggle, codeblock
        ├── [Media] image, gallery, video, audio, file
        ├── [Embeds] embed, bookmark, html, markdown
        ├── [Actions] button, call-to-action, signup, product
        ├── [Email] email, email-cta
        ├── [Markers] horizontalrule, paywall
        └── [Integration] transistor
#+END_SRC

** Node Count by Category

| Category        | Count | Description                          |
|-----------------+-------+--------------------------------------|
| ElementNode     |     8 | Container nodes with children        |
| TextNode        |     1 | Text content leaf                    |
| LineBreakNode   |     1 | Line break leaf                      |
| DecoratorNode   |    21 | Self-contained card blocks           |
| Editor-Only     |     4 | Removed before save                  |
| *Total*         |    35 |                                      |

** Node Count by Role

| Role            | Count | Nodes                                |
|-----------------+-------+--------------------------------------|
| Document        |     1 | root                                 |
| Block           |     5 | paragraph, heading, quote, aside, list |
| List Structure  |     1 | listitem                             |
| Inline          |     3 | text, link, linebreak                |
| Cards           |    21 | image, callout, codeblock, etc.      |
| Editor-Only     |     4 | at-link, at-link-search, tk, zwnj    |

** Inherited Properties by Base Class

*** ElementNode Properties

All ElementNode children inherit these properties:

| Property  | Type   | Default | Values                                   |
|-----------+--------+---------+------------------------------------------|
| type      | string | —       | Node type identifier                     |
| children  | array  | []      | Child nodes                              |
| format    | string | ""      | "", "left", "center", "right", "justify" |
| direction | string | "ltr"   | "ltr", "rtl"                             |
| indent    | number | 0       | 0, 1, 2, ... (indentation level)         |

*** TextNode Properties

| Property | Type   | Default  | Description                    |
|----------+--------+----------+--------------------------------|
| type     | string | —        | "extended-text" or "text"      |
| text     | string | ""       | The text content               |
| format   | number | 0        | Bitmask (1=bold, 2=italic...) |
| mode     | string | "normal" | "normal", "token", "segmented" |
| style    | string | ""       | Inline CSS styles              |

*** DecoratorNode Properties

DecoratorNodes have only =type= and =version= as common properties. All other properties are node-specific (see Card Nodes section).

| Property | Type   | Default | Description          |
|----------+--------+---------+----------------------|
| type     | string | —       | Node type identifier |
| version  | number | 1       | Schema version       |

* Containment Structure

This tree shows what each node type can contain.

#+BEGIN_SRC text
root (ElementNode)
│
├─► paragraph (ElementNode)
│   ├─► text (TextNode) ◄── leaf
│   ├─► link (ElementNode)
│   │   └─► text ◄── leaf
│   └─► linebreak (LineBreakNode) ◄── leaf
│
├─► heading (ElementNode)
│   ├─► text ◄── leaf
│   └─► link
│       └─► text ◄── leaf
│
├─► quote (ElementNode)
│   ├─► text ◄── leaf
│   ├─► link
│   │   └─► text ◄── leaf
│   └─► linebreak ◄── leaf
│
├─► aside (ElementNode)
│   ├─► text ◄── leaf
│   └─► link
│       └─► text ◄── leaf
│
├─► list (ElementNode)
│   └─► listitem (ElementNode)
│       ├─► text ◄── leaf
│       ├─► link
│       │   └─► text ◄── leaf
│       └─► list ◄── recursive nesting
│           └─► listitem...
│
└─► [any DecoratorNode] ◄── leaf (no children)
    image, callout, codeblock, embed, bookmark,
    html, markdown, button, signup, etc.
#+END_SRC

** Containment Rules Table

| Parent Node | Can Contain                    | Cannot Contain              |
|-------------+--------------------------------+-----------------------------|
| root        | paragraph, heading, quote,     | text, link, linebreak       |
|             | aside, list, all cards         | (no inline nodes directly)  |
| paragraph   | text, link, linebreak          | blocks, cards               |
| heading     | text, link                     | linebreak, blocks, cards    |
| quote       | text, link, linebreak          | blocks, cards               |
| aside       | text, link                     | linebreak, blocks, cards    |
| list        | listitem only                  | anything else               |
| listitem    | text, link, list (nested)      | linebreak, cards            |
| link        | text only                      | link (no nesting), linebreak|
| text        | nothing (leaf)                 | —                           |
| linebreak   | nothing (leaf)                 | —                           |
| cards       | nothing (leaf)                 | —                           |

** Leaf Nodes (Terminal - No Children)

Leaf nodes terminate the tree. They cannot contain any children.

| Leaf Type     | Base Class      | Has Content Property | Notes                        |
|---------------+-----------------+----------------------+------------------------------|
| text          | TextNode        | yes (=text=)         | All visible text lives here  |
| linebreak     | LineBreakNode   | no                   | Just renders =<br>=          |
| horizontalrule| DecoratorNode   | no                   | Just renders =<hr>=          |
| paywall       | DecoratorNode   | no                   | Just renders =<!--members-->= |
| image         | DecoratorNode   | yes (=src=, =alt=)   | Self-contained media         |
| video         | DecoratorNode   | yes (=src=)          | Self-contained media         |
| audio         | DecoratorNode   | yes (=src=)          | Self-contained media         |
| file          | DecoratorNode   | yes (=src=)          | Self-contained media         |
| gallery       | DecoratorNode   | yes (=images[]=)     | Self-contained media         |
| embed         | DecoratorNode   | yes (=url=, =html=)  | External embed               |
| bookmark      | DecoratorNode   | yes (=url=)          | Link preview                 |
| codeblock     | DecoratorNode   | yes (=code=)         | Code content                 |
| html          | DecoratorNode   | yes (=html=)         | Raw HTML                     |
| markdown      | DecoratorNode   | yes (=markdown=)     | Markdown source              |
| callout       | DecoratorNode   | yes (=calloutText=)  | HTML in property             |
| toggle        | DecoratorNode   | yes (=heading=, =content=) | HTML in properties     |
| button        | DecoratorNode   | yes (=buttonText=)   | Simple text                  |
| signup        | DecoratorNode   | yes (=header=)       | HTML in property             |
| product       | DecoratorNode   | yes (=productTitle=) | HTML in property             |
| call-to-action| DecoratorNode   | yes (=textValue=)    | HTML in property             |
| email         | DecoratorNode   | yes (=html=)         | Raw HTML                     |
| email-cta     | DecoratorNode   | yes (=html=)         | HTML in property             |
| transistor    | DecoratorNode   | no                   | Just embed URL               |

** Container Nodes (Have Children)

Container nodes can hold child nodes. Only ElementNodes are containers.

| Container   | Max Depth | Children Types         | Termination               |
|-------------+-----------+------------------------+---------------------------|
| root        | 1         | blocks, cards          | → blocks/cards (leaves)   |
| paragraph   | 2         | text, link, linebreak  | → text (leaf)             |
| heading     | 2         | text, link             | → text (leaf)             |
| quote       | 2         | text, link, linebreak  | → text (leaf)             |
| aside       | 2         | text, link             | → text (leaf)             |
| list        | ∞         | listitem               | → recursive or text       |
| listitem    | ∞         | text, link, list       | → text (leaf) or recurse  |
| link        | 1         | text only              | → text (leaf)             |

** Linebreak Restrictions

=linebreak= is only valid in certain containers:

| Container | linebreak Allowed? | Reason                              |
|-----------+--------------------+-------------------------------------|
| paragraph | YES                | Multi-line paragraphs               |
| quote     | YES                | Multi-line quotes                   |
| heading   | NO                 | Headings are single-line            |
| aside     | NO                 | Asides are single-line              |
| listitem  | NO                 | List items are single-line          |
| link      | NO                 | Links wrap text only                |

** Key Structural Rules

1. *Two-level structure*: root → blocks → inlines (cards are special blocks)
2. *Only =list= allows deep nesting* via recursive listitem → list
3. *=link= is an inline container* (wraps text, lives inside blocks)
4. *No inline cards* - all DecoratorNodes are block-level
5. *=linebreak= is restricted* - allowed in paragraph/quote only
6. *Cards are always leaves* - they may have HTML in properties but no Lexical children
7. *All text content ends in =text= nodes* - the only node with visible character content
8. *HTML-in-properties* - cards store rich content as HTML strings, not as child nodes

* Document Structure

** root

The document root. Every Ghost post/page has exactly one root node.

*Can contain:* Block nodes and card nodes (not inline nodes directly)

*Inherits:* ElementNode (format, direction, indent)

Children appear in document order. The root cannot be nested.

#+BEGIN_SRC json
{
  "root": {
    "children": [...],
    "type": "root",
    "direction": "ltr",
    "format": "",
    "indent": 0
  }
}
#+END_SRC

* Block Nodes

Block nodes are top-level content containers. They appear as direct children of root. All block nodes inherit ElementNode properties (format, direction, indent).

** paragraph

Basic text block. The fundamental content container.

*Can contain:* text, link, linebreak

*Inherits:* ElementNode (format, direction, indent)

#+BEGIN_SRC json
{
  "type": "paragraph",
  "children": [
    {"type": "text", "text": "Hello world", "format": 0}
  ],
  "direction": "ltr",
  "format": "",
  "indent": 0
}
#+END_SRC

** heading

Section heading. Levels h1-h6.

*Can contain:* text, link

*Inherits:* ElementNode (format, direction, indent)

#+BEGIN_CALLOUT :emoji "📋" :color grey
*Properties:* =tag= (h1, h2, h3, h4, h5, h6)

[[https://github.com/facebook/lexical/blob/main/packages/lexical-rich-text/src/index.ts][Source →]]
#+END_CALLOUT

#+BEGIN_SRC json
{
  "type": "heading",
  "tag": "h2",
  "children": [
    {"type": "text", "text": "Section Title", "format": 0}
  ],
  "direction": "ltr",
  "format": "",
  "indent": 0
}
#+END_SRC

** quote

Standard blockquote. Rendered as =<blockquote>=.

*Can contain:* text, link, linebreak

*Inherits:* ElementNode (format, direction, indent)

#+BEGIN_SRC json
{
  "type": "quote",
  "children": [
    {"type": "text", "text": "To be or not to be", "format": 2}
  ],
  "direction": "ltr",
  "format": "",
  "indent": 0
}
#+END_SRC

** aside

Pull quote / emphasized blockquote. Rendered as =<blockquote class="kg-blockquote-alt">=.

Visually distinct from standard quote - larger text, different styling.

*Can contain:* text, link (no nested blocks)

*Inherits:* ElementNode (format, direction, indent)

#+BEGIN_CALLOUT :emoji "📋" :color grey
*Properties:* Node-specific: none

[[https://github.com/TryGhost/Koenig/blob/main/packages/kg-default-nodes/lib/nodes/aside/AsideNode.js][Source →]]
#+END_CALLOUT

** list

Ordered or unordered list.

*Can contain:* listitem only

*Inherits:* ElementNode (format, direction, indent)

#+BEGIN_CALLOUT :emoji "📋" :color grey
*Properties:* =listType= (bullet, number, check), =start= (default: 1), =tag= (ul, ol)

[[https://github.com/facebook/lexical/blob/main/packages/lexical-list/src/LexicalListNode.ts][Source →]]
#+END_CALLOUT

#+BEGIN_SRC json
{
  "type": "list",
  "listType": "bullet",
  "tag": "ul",
  "direction": "ltr",
  "format": "",
  "indent": 0,
  "children": [
    {"type": "listitem", "children": [...]}
  ]
}
#+END_SRC

** listitem

Item within a list. Can nest lists for hierarchy.

*Can contain:* text, link, list (for nesting)

*Inherits:* ElementNode (format, direction, indent)

#+BEGIN_CALLOUT :emoji "📋" :color grey
*Properties:* =value= (number for ordered lists), =checked= (true/false/null for checklists)

*⚠️ Not rendered:* =checked=, =value= — Ghost's HTML renderer ignores these properties

[[https://github.com/facebook/lexical/blob/main/packages/lexical-list/src/LexicalListItemNode.ts][Node →]] [[https://github.com/TryGhost/Koenig/blob/main/packages/kg-lexical-html-renderer/lib/transformers/element/list.ts][Renderer →]]
#+END_CALLOUT

#+BEGIN_SRC json
{
  "type": "listitem",
  "value": 1,
  "checked": false,
  "direction": "ltr",
  "format": "",
  "indent": 0,
  "children": [
    {"type": "text", "text": "Checkbox item", "format": 0}
  ]
}
#+END_SRC

* Inline Nodes

Inline nodes appear inside block nodes. They cannot be direct children of root.

** text

Text content with formatting. The leaf node for all text.

*Can contain:* Nothing (leaf node)

#+BEGIN_CALLOUT :emoji "📋" :color grey
*Properties:* =text= (string), =format= (bitmask), =mode= (normal, token, segmented), =style= (CSS)

[[https://github.com/facebook/lexical/blob/main/packages/lexical/src/nodes/LexicalTextNode.ts][Source →]]
#+END_CALLOUT

#+BEGIN_SRC json
{"type": "text", "text": "bold text", "format": 1, "mode": "normal"}
#+END_SRC

*** Format Bitmask

| Bit | Value | Format        |
|-----+-------+---------------|
|   0 |     1 | Bold          |
|   1 |     2 | Italic        |
|   2 |     4 | Strikethrough |
|   3 |     8 | Underline     |
|   4 |    16 | Code          |
|   5 |    32 | Subscript     |
|   6 |    64 | Superscript   |
|   7 |   128 | Highlight     |

Combine with OR: bold + italic = 3, bold + underline = 9

** link

Hyperlink wrapper.

*Can contain:* text

*Inherits:* ElementNode (format, direction, indent)

#+BEGIN_CALLOUT :emoji "📋" :color grey
*Properties:* =url=, =rel=, =target=, =title=

[[https://github.com/facebook/lexical/blob/main/packages/lexical-link/src/index.ts][Source →]]
#+END_CALLOUT

#+BEGIN_SRC json
{
  "type": "link",
  "url": "https://ghost.org",
  "rel": "noopener",
  "target": "_blank",
  "direction": "ltr",
  "format": "",
  "indent": 0,
  "children": [
    {"type": "text", "text": "Ghost", "format": 0}
  ]
}
#+END_SRC

** linebreak

Soft line break (not a new paragraph).

*Can contain:* Nothing (leaf node)

#+BEGIN_SRC json
{"type": "linebreak"}
#+END_SRC

* Card Nodes

Cards are Ghost's rich content blocks extending DecoratorNode. They're self-contained - they don't contain children or inherit ElementNode properties (format, direction, indent). Instead, each card has its own specific properties. Cards may contain HTML content in string properties.

`;

// Card nodes organized by purpose
const cardCategories = {
  'Text & Callouts': {
    nodes: ['callout', 'toggle', 'codeblock'],
    descriptions: {
      'callout': 'Highlighted box with emoji and colored background.',
      'toggle': 'Collapsible accordion. Heading visible, content expands on click.',
      'codeblock': 'Syntax-highlighted code with language and optional caption.'
    }
  },
  'Media': {
    nodes: ['image', 'gallery', 'video', 'audio', 'file'],
    descriptions: {
      'image': 'Single image with caption, alt, title. Width: regular/wide/full.',
      'gallery': 'Multiple images with automatic layout.',
      'video': 'Video player with thumbnail. Width: regular/wide/full. Loop option.',
      'audio': 'Audio player with title and thumbnail.',
      'file': 'File download card with name, size, caption.'
    }
  },
  'Embeds & Links': {
    nodes: ['embed', 'bookmark', 'html', 'markdown'],
    descriptions: {
      'embed': 'External embed (YouTube, Twitter, etc). Stores URL and embed HTML.',
      'bookmark': 'Rich link preview with metadata (title, description, thumbnail).',
      'html': 'Raw HTML block. Rendered without processing.',
      'markdown': 'Markdown content. Converted to HTML on render.'
    }
  },
  'Actions': {
    nodes: ['button', 'call-to-action', 'signup', 'product'],
    descriptions: {
      'button': 'CTA button with text, URL, alignment.',
      'call-to-action': 'Rich promotional block. Layout: minimal/immersive/branded.',
      'signup': 'Email subscription form. Layout: regular/wide/split.',
      'product': 'Product card with image, rating, buy button.'
    }
  },
  'Email-Specific': {
    nodes: ['email', 'email-cta'],
    descriptions: {
      'email': 'Content only shown in email newsletters, hidden on web.',
      'email-cta': 'Email CTA with member segment targeting.'
    }
  },
  'Specialty': {
    nodes: ['transistor'],
    descriptions: {
      'transistor': 'Transistor.fm podcast player embed.'
    }
  }
};

for (const [category, info] of Object.entries(cardCategories)) {
  org += `** ${category}\n\n`;

  for (const nodeType of info.nodes) {
    const schema = allSchemas[nodeType];
    const desc = info.descriptions[nodeType];

    org += `*** ${nodeType}\n\n`;
    if (desc) org += `${desc}\n\n`;

    const props = formatProps(schema);
    if (props) {
      org += `#+BEGIN_CALLOUT :emoji "📋" :color grey\n`;
      org += `*Properties:* ${props}\n\n`;
      if (schema.rendererIgnoredProps && schema.rendererIgnoredProps.length > 0) {
        org += `*⚠️ Not rendered:* ${schema.rendererIgnoredProps.map(p => `=${p}=`).join(', ')}\n\n`;
      }
      if (schema.github) org += `[[${schema.github}][Node →]] `;
      if (schema.rendererGithub) org += `[[${schema.rendererGithub}][Renderer →]]\n`;
      else if (schema.github) org += `\n`;
      org += `#+END_CALLOUT\n`;
    }

    org += '\n';
  }
}

// Markers
org += `* Markers

Markers are special nodes with no content properties.

** horizontalrule

Visual divider. Rendered as =<hr>=.

*Properties:* None

#+BEGIN_CALLOUT :emoji "📋" :color grey
[[https://github.com/TryGhost/Koenig/blob/main/packages/kg-default-nodes/lib/nodes/horizontalrule/HorizontalRuleNode.js][Source →]]
#+END_CALLOUT

#+BEGIN_SRC json
{"type": "horizontalrule"}
#+END_SRC

** paywall

Member content gate. Everything after this node requires membership.

*Properties:* None

#+BEGIN_CALLOUT :emoji "📋" :color grey
[[https://github.com/TryGhost/Koenig/blob/main/packages/kg-default-nodes/lib/nodes/paywall/PaywallNode.js][Source →]]
#+END_CALLOUT

#+BEGIN_SRC json
{"type": "paywall"}
#+END_SRC

* Complete Attribute Reference

** ElementNode Base Attributes

These attributes appear on ALL ElementNode types (root, paragraph, heading, quote, aside, list, listitem, link):

| Attribute | Type   | Required | Default | Description                          |
|-----------+--------+----------+---------+--------------------------------------|
| type      | string | yes      | —       | Node type identifier                 |
| children  | array  | yes      | []      | Array of child nodes                 |
| format    | string | no       | ""      | Text alignment (see values below)    |
| direction | string | no       | "ltr"   | Text direction: "ltr" or "rtl"       |
| indent    | number | no       | 0       | Indentation level (0, 1, 2, ...)     |

*** format Values (ElementNode)

| Value     | Effect                              |
|-----------+-------------------------------------|
| ""        | Default/inherit from parent         |
| "left"    | Left-aligned                        |
| "center"  | Centered                            |
| "right"   | Right-aligned                       |
| "justify" | Justified text                      |
| "start"   | Start-aligned (respects direction)  |
| "end"     | End-aligned (respects direction)    |

** TextNode Attributes

| Attribute | Type   | Required | Default  | Description                      |
|-----------+--------+----------+----------+----------------------------------|
| type      | string | yes      | —        | "extended-text" or "text"        |
| text      | string | yes      | ""       | The actual text content          |
| format    | number | no       | 0        | Bitmask for inline formatting    |
| mode      | string | no       | "normal" | Text mode                        |
| style     | string | no       | ""       | Inline CSS styles                |
| detail    | number | no       | 0        | Internal detail flags            |

*** format Bitmask (TextNode)

| Bit | Value | Format        | Org Equivalent |
|-----+-------+---------------+----------------|
|   0 |     1 | Bold          | *bold*         |
|   1 |     2 | Italic        | /italic/       |
|   2 |     4 | Strikethrough | +strike+       |
|   3 |     8 | Underline     | _underline_    |
|   4 |    16 | Code          | =code=         |
|   5 |    32 | Subscript     | _{sub}         |
|   6 |    64 | Superscript   | ^{super}       |
|   7 |   128 | Highlight     | (no default)   |

Combine with bitwise OR: bold + italic = 3, bold + code = 17

*** mode Values (TextNode)

| Value       | Description                                    |
|-------------+------------------------------------------------|
| "normal"    | Standard editable text                         |
| "token"     | Immutable token (e.g., @mentions)              |
| "segmented" | Segmented for IME input                        |

** Node-Specific Attributes (ElementNodes)

These attributes are specific to each node type, in addition to the inherited ElementNode base attributes.

*** heading

/Inherits: type, children, format, direction, indent/

| Attribute | Type   | Inherited | Values                             |
|-----------+--------+-----------+------------------------------------|
| type      | string | yes       | "heading" or "extended-heading"    |
| children  | array  | yes       | text, link                         |
| format    | string | yes       | "", "left", "center", "right"      |
| direction | string | yes       | "ltr", "rtl"                       |
| indent    | number | yes       | 0, 1, 2, ...                       |
| tag       | string | *NO*      | "h1", "h2", "h3", "h4", "h5", "h6" |

*** list

/Inherits: type, children, format, direction, indent/

| Attribute | Type   | Inherited | Values                            |
|-----------+--------+-----------+-----------------------------------|
| type      | string | yes       | "list"                            |
| children  | array  | yes       | listitem only                     |
| format    | string | yes       | "", "left", "center", "right"     |
| direction | string | yes       | "ltr", "rtl"                      |
| indent    | number | yes       | 0, 1, 2, ...                      |
| listType  | string | *NO*      | "bullet", "number", "check"       |
| tag       | string | *NO*      | "ul", "ol"                        |
| start     | number | *NO*      | Starting number (default: 1)      |

*** listitem

/Inherits: type, children, format, direction, indent/

| Attribute | Type    | Inherited | Values / Description              |
|-----------+---------+-----------+-----------------------------------|
| type      | string  | yes       | "listitem"                        |
| children  | array   | yes       | text, link, list (for nesting)    |
| format    | string  | yes       | "", "left", "center", "right"     |
| direction | string  | yes       | "ltr", "rtl"                      |
| indent    | number  | yes       | 0, 1, 2, ...                      |
| value     | number  | *NO*      | Item number (ordered lists)       |
| checked   | boolean | *NO*      | Checkbox state (checklists)       |

*** link

/Inherits: type, children, format, direction, indent/

| Attribute | Type   | Inherited | Values / Description              |
|-----------+--------+-----------+-----------------------------------|
| type      | string | yes       | "link"                            |
| children  | array  | yes       | text only                         |
| format    | string | yes       | "", "left", "center", "right"     |
| direction | string | yes       | "ltr", "rtl"                      |
| indent    | number | yes       | 0, 1, 2, ...                      |
| url       | string | *NO*      | Link destination URL              |
| rel       | string | *NO*      | Link rel attribute                |
| target    | string | *NO*      | Link target (_blank, etc.)        |
| title     | string | *NO*      | Link title attribute              |

*** paragraph, quote, aside

/Inherits: type, children, format, direction, indent/

These nodes have NO node-specific attributes - they use only the inherited ElementNode base attributes.

| Attribute | Type   | Inherited | Values / Description              |
|-----------+--------+-----------+-----------------------------------|
| type      | string | yes       | "paragraph", "quote", or "aside"  |
| children  | array  | yes       | text, link, linebreak             |
| format    | string | yes       | "", "left", "center", "right"     |
| direction | string | yes       | "ltr", "rtl"                      |
| indent    | number | yes       | 0, 1, 2, ...                      |

*** root

/Inherits: type, children, format, direction, indent/

| Attribute | Type   | Inherited | Values / Description              |
|-----------+--------+-----------+-----------------------------------|
| type      | string | yes       | "root"                            |
| children  | array  | yes       | blocks and cards (not inlines)    |
| format    | string | yes       | "", "left", "center", "right"     |
| direction | string | yes       | "ltr", "rtl"                      |
| indent    | number | yes       | 0 (always)                        |

** Common Card Attributes

*** backgroundColor

| Value    | Appearance                        | Used By                     |
|----------+-----------------------------------+-----------------------------|
| "white"  | Transparent with border           | callout, header, signup     |
| "grey"   | Light grey background             | callout, header, signup, CTA|
| "blue"   | Blue background                   | callout, header, signup, CTA|
| "green"  | Green background                  | callout, header, signup, CTA|
| "yellow" | Yellow background                 | callout, header, signup, CTA|
| "red"    | Red background                    | callout, header, signup, CTA|
| "pink"   | Pink background                   | callout, header, signup, CTA|
| "purple" | Purple background                 | callout, header, signup, CTA|
| "accent" | Site's configured brand color     | callout, header, signup, CTA|

*** alignment (Cards)

| Value    | Effect                            | Used By                     |
|----------+-----------------------------------+-----------------------------|
| "left"   | Left-aligned content              | button, CTA, header, signup |
| "center" | Centered content                  | button, CTA, header, signup |
| "right"  | Right-aligned content             | button, CTA, header, signup |

*** cardWidth

| Value     | Effect                           | Used By                     |
|-----------+----------------------------------+-----------------------------|
| "regular" | Within content column            | image, video, gallery       |
| "wide"    | Breaks out of column             | image, video, gallery       |
| "full"    | Edge to edge                     | image, video, gallery       |

*** layout

| Value       | Effect                          | Used By                     |
|-------------+---------------------------------+-----------------------------|
| "minimal"   | Compact layout                  | CTA, signup                 |
| "immersive" | Full-width with background      | CTA                         |
| "wide"      | Wide layout                     | signup                      |
| "split"     | Side-by-side image/content      | signup, header              |
| "full"      | Full-width                      | header                      |

*** segment (email targeting)

| Value          | Audience                       |
|----------------+--------------------------------|
| "status:free"  | Free members only              |
| "status:-free" | Paid members only              |
| ""             | All subscribers                |

** Visibility Property (Member Gating)

Some cards support visibility gating. Cards with =hasVisibility: true=:
- html
- call-to-action
- transistor (members-only by default)

#+BEGIN_SRC json
{
  "visibility": {
    "web": {
      "nonMember": true,
      "memberSegment": "status:free,status:-free"
    },
    "email": {
      "memberSegment": "status:free"
    }
  }
}
#+END_SRC

* Editor-Only Nodes

These exist in the editor but are removed before save/render.

| Node           | Purpose                                    |
|----------------+--------------------------------------------|
| at-link        | Placeholder while typing @ for link search |
| at-link-search | Search dropdown for @ links                |
| tk             | "To come" marker for incomplete content    |
| zwnj           | Zero-width non-joiner for cursor position  |

* Resources

- [[https://github.com/TryGhost/Koenig/tree/main/packages/kg-default-nodes][kg-default-nodes]] — Node definitions
- [[https://github.com/TryGhost/Koenig/tree/main/packages/kg-lexical-html-renderer][kg-lexical-html-renderer]] — HTML rendering
- [[https://lexical.dev/docs/concepts/nodes][Lexical Nodes]] — Framework docs
- [[https://ghost.org/docs/admin-api/][Ghost Admin API]] — Post creation API
`;

// Output
const outputFile = process.argv[2] || 'docs/ghost-node-reference.org';
fs.writeFileSync(path.join(__dirname, outputFile), org);
console.log(`Generated: ${outputFile}`);
