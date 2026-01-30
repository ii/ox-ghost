#!/usr/bin/env node
// ox-lexical-render.js - Render and validate Lexical JSON using Ghost's renderer
//
// Part of ox-lexical: https://github.com/ii-coop/ox-lexical
//
// Usage:
//   ox-lexical-render input.json              # Validate JSON
//   ox-lexical-render input.json --html out   # Render to HTML
//   ox-lexical-render input.org --html out    # Export org → JSON → HTML
//
// Requires: npm install @tryghost/kg-lexical-html-renderer @tryghost/kg-default-nodes

const fs = require('fs');
const path = require('path');

async function main() {
  const args = process.argv.slice(2);

  if (args.length < 1 || args.includes('--help') || args.includes('-h')) {
    console.log(`
ox-lexical-render - Render and validate Lexical JSON using Ghost's renderer

Usage:
  ox-lexical-render input.json [options]
  ox-lexical-render input.org [options]    # exports org first, then renders

Options:
  --html FILE    Render to HTML and save to FILE
  --quiet        Only output JSON stats (for scripting)
  --help         Show this help

Examples:
  ox-lexical-render export.json                    # Validate only
  ox-lexical-render export.json --html preview.html
  ox-lexical-render doc.org --html preview.html    # Full pipeline
  ox-lexical-render export.json --quiet            # JSON stats for scripts

Exit codes:
  0  Success (valid Lexical JSON)
  1  Error (invalid JSON or render failure)
`);
    process.exit(0);
  }

  const quiet = args.includes('--quiet');
  const htmlIndex = args.indexOf('--html');
  const outputFile = htmlIndex !== -1 ? args[htmlIndex + 1] : null;

  let inputFile = args.find(a => !a.startsWith('--') && a !== outputFile);

  // If input is .org file, export it first
  if (inputFile && inputFile.endsWith('.org')) {
    const jsonFile = inputFile.replace(/\.org$/, '.json');
    const scriptDir = __dirname;
    const exportScript = path.join(scriptDir, 'org-to-lexical.sh');

    if (!quiet) console.log(`Exporting ${inputFile} to JSON...`);

    const { execSync } = require('child_process');
    try {
      execSync(`"${exportScript}" "${inputFile}" "${jsonFile}"`, {
        stdio: quiet ? 'pipe' : 'inherit'
      });
      inputFile = jsonFile;
    } catch (err) {
      console.error('Export failed:', err.message);
      process.exit(1);
    }
  }

  // Load the Lexical JSON
  let lexicalJson;
  try {
    const content = fs.readFileSync(inputFile, 'utf-8');
    lexicalJson = JSON.parse(content);
  } catch (err) {
    console.error(`Error reading ${inputFile}:`, err.message);
    process.exit(1);
  }

  // Validate structure
  if (!lexicalJson.root) {
    console.error('Invalid Lexical JSON: missing root');
    process.exit(1);
  }
  if (!lexicalJson.root.children) {
    console.error('Invalid Lexical JSON: missing root.children');
    process.exit(1);
  }

  const nodeCount = lexicalJson.root.children.length;

  if (!quiet) {
    console.log(`\nLoaded: ${inputFile}`);
    console.log(`  Nodes: ${nodeCount}`);
  }

  // Count node types
  const typeCounts = {};
  lexicalJson.root.children.forEach(node => {
    typeCounts[node.type] = (typeCounts[node.type] || 0) + 1;
  });

  if (!quiet) {
    console.log(`  Types: ${Object.keys(typeCounts).length}`);
    Object.entries(typeCounts)
      .sort((a, b) => b[1] - a[1])
      .forEach(([type, count]) => {
        console.log(`    ${type}: ${count}`);
      });
  }

  // Render with Ghost's renderer
  try {
    const Renderer = require('@tryghost/kg-lexical-html-renderer');
    const { DEFAULT_NODES } = require('@tryghost/kg-default-nodes');

    if (!quiet) console.log(`\nRendering with Ghost renderer (${DEFAULT_NODES.length} node types)...`);

    const renderer = new Renderer({ nodes: DEFAULT_NODES });
    const html = await renderer.render(JSON.stringify(lexicalJson));

    const stats = {
      length: html.length,
      paragraphs: (html.match(/<p>/g) || []).length,
      headings: (html.match(/<h[1-6]/g) || []).length,
      codeBlocks: (html.match(/<pre>/g) || []).length,
      callouts: (html.match(/kg-callout/g) || []).length,
      toggles: (html.match(/kg-toggle/g) || []).length,
      images: (html.match(/<img/g) || []).length,
    };

    if (!quiet) {
      console.log(`\n  Output: ${stats.length.toLocaleString()} chars`);
      console.log(`  Paragraphs: ${stats.paragraphs}`);
      console.log(`  Headings: ${stats.headings}`);
      console.log(`  Code blocks: ${stats.codeBlocks}`);
      console.log(`  Callouts: ${stats.callouts}`);
      console.log(`  Toggles: ${stats.toggles}`);
      if (stats.images > 0) console.log(`  Images: ${stats.images}`);
    }

    if (outputFile) {
      const fullHtml = wrapHtml(html);
      fs.writeFileSync(outputFile, fullHtml);
      if (!quiet) console.log(`\nSaved: ${outputFile}`);
    }

    if (!quiet) console.log('\n✓ Validation passed');

    // Output JSON stats for scripting
    if (quiet) {
      console.log(JSON.stringify({ nodes: nodeCount, types: typeCounts, ...stats }));
    }

  } catch (err) {
    console.error('\n✗ Render failed:', err.message);
    if (!quiet && err.stack) {
      console.error(err.stack.split('\n').slice(1, 5).join('\n'));
    }
    process.exit(1);
  }
}

function wrapHtml(content) {
  return `<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Lexical Preview</title>
  <style>
    :root {
      --font-sans: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      --font-mono: "SF Mono", Consolas, "Liberation Mono", Menlo, monospace;
      --color-primary: #667eea;
      --color-border: #e5e7eb;
      --color-bg-code: #f5f5f5;
    }
    * { box-sizing: border-box; }
    body {
      max-width: 720px;
      margin: 0 auto;
      padding: 2rem 1rem;
      font-family: var(--font-sans);
      line-height: 1.7;
      color: #1a1a1a;
    }
    h1, h2, h3, h4, h5, h6 {
      margin-top: 2rem;
      margin-bottom: 1rem;
      line-height: 1.3;
    }
    h2 { border-bottom: 1px solid var(--color-border); padding-bottom: 0.5rem; }
    p { margin: 1rem 0; }

    /* Code */
    pre {
      background: var(--color-bg-code);
      padding: 1rem;
      overflow-x: auto;
      border-radius: 6px;
      font-family: var(--font-mono);
      font-size: 0.9rem;
      line-height: 1.5;
    }
    code {
      background: var(--color-bg-code);
      padding: 0.2rem 0.4rem;
      border-radius: 3px;
      font-family: var(--font-mono);
      font-size: 0.9em;
    }
    pre code { background: none; padding: 0; }

    /* Callouts */
    .kg-callout-card {
      padding: 1.25rem;
      margin: 1.5rem 0;
      border-radius: 6px;
      display: flex;
      gap: 0.75rem;
      align-items: flex-start;
    }
    .kg-callout-card-blue { background: #dbeafe; }
    .kg-callout-card-yellow { background: #fef3c7; }
    .kg-callout-card-green { background: #d1fae5; }
    .kg-callout-card-red { background: #fee2e2; }
    .kg-callout-card-grey { background: #f3f4f6; }
    .kg-callout-card-purple { background: #ede9fe; }
    .kg-callout-emoji { font-size: 1.25rem; flex-shrink: 0; }
    .kg-callout-text { flex: 1; }

    /* Toggles */
    .kg-toggle-card {
      border: 1px solid var(--color-border);
      border-radius: 6px;
      margin: 1.5rem 0;
      overflow: hidden;
    }
    .kg-toggle-heading-container {
      padding: 1rem;
      background: #fafafa;
      cursor: pointer;
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }
    .kg-toggle-heading-container::before {
      content: "▶";
      font-size: 0.75rem;
      transition: transform 0.2s;
    }
    .kg-toggle-card[data-open] .kg-toggle-heading-container::before {
      transform: rotate(90deg);
    }
    .kg-toggle-content {
      padding: 1rem;
      border-top: 1px solid var(--color-border);
    }

    /* Buttons */
    .kg-button-card { text-align: center; margin: 1.5rem 0; }
    .kg-btn {
      display: inline-block;
      padding: 0.75rem 1.5rem;
      background: var(--color-primary);
      color: white;
      text-decoration: none;
      border-radius: 6px;
      font-weight: 500;
    }
    .kg-btn:hover { opacity: 0.9; }

    /* Quotes */
    blockquote {
      border-left: 4px solid var(--color-primary);
      padding-left: 1rem;
      margin: 1.5rem 0;
      font-style: italic;
      color: #4b5563;
    }

    /* Horizontal rules */
    hr {
      border: none;
      border-top: 1px solid var(--color-border);
      margin: 2rem 0;
    }

    /* Lists */
    ul, ol { padding-left: 1.5rem; }
    li { margin: 0.5rem 0; }

    /* Images */
    figure { margin: 1.5rem 0; }
    img { max-width: 100%; height: auto; border-radius: 6px; }
    figcaption {
      text-align: center;
      font-size: 0.9rem;
      color: #6b7280;
      margin-top: 0.5rem;
    }

    /* Cards */
    .kg-card { margin: 1.5rem 0; }

    /* Header cards */
    .kg-header-card {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 2rem;
      border-radius: 6px;
      text-align: center;
      margin: 1.5rem 0;
    }

    /* Aside */
    aside, .kg-aside {
      color: #6b7280;
      font-size: 0.95rem;
      padding-left: 1rem;
      border-left: 2px solid var(--color-border);
      margin: 1.5rem 0;
    }
  </style>
</head>
<body>
${content}
<script>
  // Toggle functionality
  document.querySelectorAll('.kg-toggle-heading-container').forEach(el => {
    el.addEventListener('click', () => {
      el.closest('.kg-toggle-card').toggleAttribute('data-open');
    });
  });
</script>
</body>
</html>`;
}

main();
