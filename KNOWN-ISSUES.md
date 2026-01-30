# ox-ghost Known Issues

Issues with Ghost's Lexical renderer that affect ox-ghost exports.

## 1. Nested Lists Don't Render

**Status:** Ghost renderer bug  
**Package:** `@tryghost/kg-lexical-html-renderer`  
**File:** `build/transformers/element/list.js:26`

### Problem

Nested lists with text before them don't render:

```org
- Parent item
  - Child item    ← Lost!
```

### Root Cause

```javascript
// line 26 - only checks FIRST child
if ($isListNode(listChildren[0])) {
    output.push(exportList(listChildren[0], ...));
}
```

If text comes before the nested list, `listChildren[0]` is text, not a list, so nested content is silently dropped.

### Workaround

Use flat lists or raw HTML block.

---

## 2. Checkboxes Don't Render

**Status:** Ghost renderer limitation  
**Package:** `@tryghost/kg-lexical-html-renderer`

### Problem

Org checkbox syntax exports correctly to Lexical JSON:

```org
- [ ] Unchecked → checked: false
- [X] Checked   → checked: true
- [-] Partial   → checked: null
```

But Ghost renders plain `<li>` without checkbox styling.

### Root Cause

Ghost's list renderer ignores the `checked` property. No CSS or HTML is generated for checkboxes.

### Workaround

Use callouts or emoji prefixes:
```org
- ☐ Unchecked
- ☑ Checked
```

---

## Upstream

These should be reported to:
- https://github.com/TryGhost/Koenig/issues
- https://github.com/TryGhost/Ghost/issues
