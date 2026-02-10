#!/usr/bin/env node
/**
 * Extract Ghost Koenig node schemas from upstream source
 *
 * Scans kg-default-nodes to extract properties for each node type.
 * Also identifies Lexical core nodes that Ghost supports.
 * Analyzes renderers to find unused properties (patch opportunities).
 *
 * Usage:
 *   node extract-node-schema.js                       # Output JSON to stdout
 *   node extract-node-schema.js --json schema.json   # Save JSON to file
 *   node extract-node-schema.js --org                # Output org-mode callouts
 *   node extract-node-schema.js --lookup callout     # Lookup specific node
 *   node extract-node-schema.js --all                # Include Lexical core nodes
 *   node extract-node-schema.js --renderer-report    # Analyze renderer usage
 *   node extract-node-schema.js --renderer-detail X  # Deep analysis of node X
 *   node extract-node-schema.js --list               # List all node types
 *   node extract-node-schema.js --stats              # Show statistics
 *
 * Renderer Analysis:
 *   --renderer-report   Summary of all nodes showing which properties are
 *                       rendered vs ignored (patch opportunities)
 *   --renderer-detail   Line-by-line analysis of a specific node's renderer
 *                       with GitHub links to exact line numbers
 */

const fs = require('fs');
const path = require('path');

// Path to Koenig nodes source
const KOENIG_NODES_PATH = path.join(__dirname,
  '../upstream-src/TryGhost/Koenig/packages/kg-default-nodes/lib/nodes');

const KOENIG_LIB_PATH = path.join(__dirname,
  '../upstream-src/TryGhost/Koenig/packages/kg-default-nodes/lib');

const RENDERER_PATH = path.join(__dirname,
  '../upstream-src/TryGhost/Koenig/packages/kg-lexical-html-renderer/lib');

// =============================================================================
// VERSION PINNING - Why this specific tag?
// =============================================================================
//
// We pin to the exact Koenig version used by Ghost Pro (hosted Ghost).
// This matches what most Ghost users run, ensuring our schema extraction
// and renderer analysis reflects real-world production behavior.
//
// Current target: Ghost 6.16.1 (Ghost Pro / hosted Ghost)
//   └─ uses @tryghost/kg-default-nodes@2.0.8
//   └─ uses @tryghost/kg-lexical-html-renderer@1.3.31
//   └─ both packages released from commit a36cc9d61789
//
// HOW TO UPDATE when Ghost upgrades:
//   1. Check hosted version:  curl -s https://your-site.ghost.io/ | grep generator
//   2. Find Koenig versions:  curl -sL "https://raw.githubusercontent.com/TryGhost/Ghost/vX.Y.Z/ghost/core/package.json" \
//                               | jq '.dependencies | to_entries[] | select(.key | contains("kg-"))'
//   3. Verify tag exists:     gh api repos/TryGhost/Koenig/git/refs/tags --jq '.[] | select(.ref | contains("2.0.8"))'
//   4. Update KOENIG_GIT_REF below
//
// To override at runtime: KOENIG_GIT_REF=main node extract-node-schema.js
//
const KOENIG_GIT_REF = process.env.KOENIG_GIT_REF || '@tryghost/kg-default-nodes@2.0.8';

const GITHUB_BASE = `https://github.com/TryGhost/Koenig/blob/${KOENIG_GIT_REF}/packages/kg-default-nodes/lib/nodes`;
const GITHUB_RENDERER_BASE = `https://github.com/TryGhost/Koenig/blob/${KOENIG_GIT_REF}/packages/kg-lexical-html-renderer/lib`;
const LEXICAL_GITHUB = 'https://github.com/facebook/lexical/blob/main/packages';
const RENDERER_GITHUB = `https://github.com/TryGhost/Koenig/blob/${KOENIG_GIT_REF}/packages/kg-lexical-html-renderer/lib/transformers/element`;

// Lexical core nodes that Ghost supports (from LexicalHTMLRenderer.ts)
const LEXICAL_CORE_NODES = {
  'paragraph': {
    nodeType: 'paragraph',
    category: 'lexical-core',
    description: 'Basic paragraph container for text content',
    source: '@lexical/core',
    github: `${LEXICAL_GITHUB}/lexical/src/nodes/LexicalParagraphNode.ts`,
    rendererGithub: `${RENDERER_GITHUB}/paragraph.ts`,
    canContain: ['text', 'linebreak', 'link'],
    properties: []
  },
  'text': {
    nodeType: 'text',
    category: 'lexical-core',
    description: 'Text content with formatting (bold, italic, underline, strikethrough, code, subscript, superscript)',
    source: '@lexical/core',
    github: `${LEXICAL_GITHUB}/lexical/src/nodes/LexicalTextNode.ts`,
    rendererGithub: null, // Text rendering is inline, handled by convert-to-html-string.ts
    formats: ['bold', 'italic', 'underline', 'strikethrough', 'code', 'subscript', 'superscript', 'highlight'],
    properties: []
  },
  'linebreak': {
    nodeType: 'linebreak',
    category: 'lexical-core',
    description: 'Line break within text (soft return)',
    source: '@lexical/core',
    github: `${LEXICAL_GITHUB}/lexical/src/nodes/LexicalLineBreakNode.ts`,
    rendererGithub: null, // Simple <br> output
    properties: []
  },
  'heading': {
    nodeType: 'heading',
    category: 'lexical-core',
    description: 'Heading levels h1-h6',
    source: '@lexical/rich-text',
    github: `${LEXICAL_GITHUB}/lexical-rich-text/src/index.ts`,
    rendererGithub: `${RENDERER_GITHUB}/heading.ts`,
    canContain: ['text', 'link'],
    properties: [{ name: 'tag', values: ['h1', 'h2', 'h3', 'h4', 'h5', 'h6'] }]
  },
  'quote': {
    nodeType: 'quote',
    category: 'lexical-core',
    description: 'Block quotation',
    source: '@lexical/rich-text',
    github: `${LEXICAL_GITHUB}/lexical-rich-text/src/index.ts`,
    rendererGithub: `${RENDERER_GITHUB}/blockquote.ts`,
    canContain: ['text', 'linebreak', 'link'],
    properties: []
  },
  'list': {
    nodeType: 'list',
    category: 'lexical-core',
    description: 'Ordered or unordered list container',
    source: '@lexical/list',
    github: `${LEXICAL_GITHUB}/lexical-list/src/LexicalListNode.ts`,
    rendererGithub: `${RENDERER_GITHUB}/list.ts`,
    canContain: ['listitem'],
    properties: [
      { name: 'listType', values: ['bullet', 'number', 'check'] },
      { name: 'start', default: 1 }
    ]
  },
  'listitem': {
    nodeType: 'listitem',
    category: 'lexical-core',
    description: 'List item within a list',
    source: '@lexical/list',
    github: `${LEXICAL_GITHUB}/lexical-list/src/LexicalListItemNode.ts`,
    rendererGithub: `${RENDERER_GITHUB}/list.ts`,
    rendererIgnoredProps: ['checked', 'value'],
    rendererNote: 'WARNING: checked and value properties not rendered - Ghost limitation',
    canContain: ['text', 'link', 'list'],
    properties: [
      { name: 'value', default: 1 },
      { name: 'checked', default: null }
    ]
  },
  'link': {
    nodeType: 'link',
    category: 'lexical-core',
    description: 'Hyperlink wrapping text content',
    source: '@lexical/link',
    github: `${LEXICAL_GITHUB}/lexical-link/src/index.ts`,
    rendererGithub: `${GITHUB_RENDERER_BASE}/utils/TextContent.ts`,
    rendererUsedProps: ['url', 'rel'],
    rendererIgnoredProps: ['target', 'title'],
    canContain: ['text'],
    properties: [
      { name: 'url', urlType: 'url' },
      { name: 'rel' },
      { name: 'target' },
      { name: 'title' }
    ]
  }
};

// Map ox-ghost block names to Ghost node types
const OX_GHOST_MAP = {
  'CALLOUT': 'callout',
  'TOGGLE': 'toggle',
  'ASIDE': 'aside',
  'BUTTON': 'button',
  'HEADER': 'header',
  'SIGNUP': 'signup',
  'CTA': 'call-to-action',
  'BOOKMARK': 'bookmark',
  'EMAIL': 'email',
  'PRODUCT': 'product',
  'AUDIO': 'audio',
  'VIDEO': 'video',
  'FILE': 'file',
  'GALLERY': 'gallery',
  'PAYWALL': 'paywall',
  'TRANSISTOR': 'transistor',
  'HTML': 'html',
  'EMBED': 'embed'
};

// Known valid values for certain properties (extracted from UI components)
const KNOWN_VALUES = {
  'backgroundColor': ['white', 'grey', 'blue', 'green', 'yellow', 'red', 'pink', 'purple', 'accent'],
  'alignment': ['left', 'center', 'right'],
  'layout': {
    'call-to-action': ['minimal', 'immersive', 'branded'],
    'signup': ['regular', 'wide', 'split'],
    'header': ['regular'] // header uses 'size' not 'layout'
  },
  'size': ['small', 'medium', 'large'],
  'cardWidth': ['regular', 'wide', 'full']
};

// Ghost node categories
const NODE_CATEGORIES = {
  'decorator': {
    description: 'Rich content blocks with configurable properties',
    nodes: ['audio', 'bookmark', 'button', 'call-to-action', 'callout', 'codeblock',
            'email', 'email-cta', 'embed', 'file', 'gallery', 'header', 'html',
            'image', 'markdown', 'product', 'signup', 'toggle', 'transistor', 'video']
  },
  'element': {
    description: 'Container nodes that hold child nodes',
    nodes: ['aside']
  },
  'marker': {
    description: 'Simple markers with no configurable properties',
    nodes: ['horizontalrule', 'paywall']
  },
  'editor-only': {
    description: 'Nodes that exist only in the editor, not serialized to output',
    nodes: ['at-link', 'at-link-search', 'tk', 'zwnj']
  },
  'extension': {
    description: 'Ghost extensions of Lexical core nodes',
    nodes: ['extended-text', 'extended-heading', 'extended-quote']
  }
};

// Extract properties from a node file
function extractNodeSchema(filePath) {
  const content = fs.readFileSync(filePath, 'utf8');
  const fileName = path.basename(filePath);
  const dirName = path.basename(path.dirname(filePath));

  // Determine if this is a file directly in nodes/ or in a subdirectory
  const isRootFile = dirName === 'nodes';

  // Extract nodeType from various patterns
  let nodeType;

  // Pattern 1: nodeType: 'name' (decorator nodes)
  const nodeTypeMatch = content.match(/nodeType:\s*['"]([^'"]+)['"]/);
  if (nodeTypeMatch) {
    nodeType = nodeTypeMatch[1];
  }

  // Pattern 2: static getType() { return 'name' } (element nodes)
  if (!nodeType) {
    const getTypeMatch = content.match(/static\s+getType\s*\(\s*\)\s*\{[\s\S]*?return\s*['"]([^'"]+)['"]/);
    if (getTypeMatch) {
      nodeType = getTypeMatch[1];
    }
  }

  // Pattern 3: infer from directory or file name
  if (!nodeType) {
    if (isRootFile) {
      // Extract from file name like "ExtendedTextNode.js" -> "extended-text"
      nodeType = fileName.replace(/Node\.js$/, '').replace(/([A-Z])/g, '-$1').toLowerCase().replace(/^-/, '');
    } else {
      nodeType = dirName;
    }
  }

  // Detect node base class
  let baseClass = 'unknown';
  if (content.includes('extends generateDecoratorNode')) {
    baseClass = 'DecoratorNode';
  } else if (content.includes('extends ElementNode')) {
    baseClass = 'ElementNode';
  } else if (content.includes('extends TextNode')) {
    baseClass = 'TextNode';
  } else if (content.includes('extends HeadingNode')) {
    baseClass = 'HeadingNode';
  } else if (content.includes('extends QuoteNode')) {
    baseClass = 'QuoteNode';
  }

  // Extract properties array - need to handle nested brackets like default: []
  // Use bracket-balanced extraction
  const properties = [];
  const propsStartMatch = content.match(/properties:\s*\[/);

  if (propsStartMatch) {
    const startIdx = propsStartMatch.index + propsStartMatch[0].length;
    let depth = 1; // We're inside the opening [
    let endIdx = startIdx;

    // Find the matching closing bracket
    for (let i = startIdx; i < content.length && depth > 0; i++) {
      const char = content[i];
      if (char === '[') depth++;
      else if (char === ']') depth--;
      endIdx = i;
    }

    const propertiesStr = content.slice(startIdx, endIdx);

    // Now parse individual property objects with balanced braces
    let objStart = -1;
    let braceDepth = 0;
    let bracketDepth = 0;
    let inString = false;
    let stringChar = '';

    for (let i = 0; i < propertiesStr.length; i++) {
      const char = propertiesStr[i];
      const prevChar = i > 0 ? propertiesStr[i - 1] : '';

      // Handle string detection
      if ((char === '"' || char === "'") && prevChar !== '\\') {
        if (!inString) {
          inString = true;
          stringChar = char;
        } else if (char === stringChar) {
          inString = false;
        }
        continue;
      }

      if (inString) continue;

      if (char === '{') {
        if (braceDepth === 0) objStart = i;
        braceDepth++;
      } else if (char === '}') {
        braceDepth--;
        if (braceDepth === 0 && objStart >= 0) {
          // Extract this property object
          const propStr = propertiesStr.slice(objStart + 1, i);
          const prop = parsePropertyString(propStr);
          if (prop && prop.name) {
            properties.push(prop);
          }
          objStart = -1;
        }
      } else if (char === '[') {
        bracketDepth++;
      } else if (char === ']') {
        bracketDepth--;
      }
    }
  }

  function parsePropertyString(propStr) {
    const prop = {};

    // Extract name
    const nameMatch = propStr.match(/name:\s*['"]([^'"]+)['"]/);
    if (nameMatch) prop.name = nameMatch[1];

    // Extract default value
    const defaultStartMatch = propStr.match(/default:\s*/);
    if (defaultStartMatch) {
      const afterDefault = propStr.slice(defaultStartMatch.index + defaultStartMatch[0].length);

      if (afterDefault.startsWith('[]')) {
        prop.default = [];
      } else if (afterDefault.startsWith('null')) {
        prop.default = null;
      } else if (afterDefault.startsWith('true')) {
        prop.default = true;
      } else if (afterDefault.startsWith('false')) {
        prop.default = false;
      } else if (afterDefault.match(/^-?\d+/)) {
        prop.default = parseInt(afterDefault.match(/^-?\d+/)[0]);
      } else if (afterDefault.startsWith("'")) {
        // Single-quoted string - handle escaped quotes
        const strMatch = afterDefault.match(/^'((?:[^'\\]|\\.)*)'/);
        if (strMatch) prop.default = strMatch[1].replace(/\\'/g, "'");
      } else if (afterDefault.startsWith('"')) {
        // Double-quoted string
        const strMatch = afterDefault.match(/^"((?:[^"\\]|\\.)*)"/);
        if (strMatch) prop.default = strMatch[1].replace(/\\"/g, '"');
      }
    }

    // Extract urlType if present
    const urlTypeMatch = propStr.match(/urlType:\s*['"]([^'"]+)['"]/);
    if (urlTypeMatch) prop.urlType = urlTypeMatch[1];

    // Extract wordCount if present
    if (propStr.includes('wordCount: true')) prop.wordCount = true;

    return prop;
  }

  // Check for hasVisibility
  const hasVisibility = content.includes('hasVisibility: true');

  // Build GitHub URL
  let relativePath, githubUrl, rendererPath, rendererGithub;
  if (isRootFile) {
    relativePath = fileName;
    githubUrl = `${GITHUB_BASE}/${fileName}`;
    // Root files don't have separate renderers
    rendererPath = null;
    rendererGithub = null;
  } else {
    relativePath = `${dirName}/${fileName}`;
    githubUrl = `${GITHUB_BASE}/${relativePath}`;

    // Check for renderer file (multiple naming conventions)
    const dirPath = path.join(KOENIG_NODES_PATH, dirName);
    const dirNameNoHyphens = dirName.replace(/-/g, '');

    // Try different renderer file patterns
    const rendererPatterns = [
      `${dirName}-renderer.js`,              // standard: image-renderer.js
      `${dirNameNoHyphens}-renderer.js`,     // no hyphens: calltoaction-renderer.js
      `renderers/${dirName}-renderer.js`,    // subdirectory: renderers/header-renderer.js
      `renderers/v2/${dirName}-renderer.js`, // versioned: renderers/v2/header-renderer.js
      `renderers/v1/${dirName}-renderer.js`, // versioned: renderers/v1/header-renderer.js
    ];

    for (const pattern of rendererPatterns) {
      const fullPath = path.join(dirPath, pattern);
      if (fs.existsSync(fullPath)) {
        rendererPath = `${dirName}/${pattern}`;
        rendererGithub = `${GITHUB_BASE}/${rendererPath}`;
        break;
      }
    }

    // Special case: check for renderers/ directory with any .js file
    if (!rendererPath) {
      const renderersDir = path.join(dirPath, 'renderers');
      if (fs.existsSync(renderersDir)) {
        const files = fs.readdirSync(renderersDir).filter(f => f.endsWith('.js'));
        if (files.length > 0) {
          rendererPath = `${dirName}/renderers/${files[0]}`;
          rendererGithub = `${GITHUB_BASE}/${rendererPath}`;
        }
      }
    }
  }

  return {
    nodeType,
    baseClass,
    file: relativePath,
    github: githubUrl,
    rendererFile: rendererPath,
    rendererGithub: rendererGithub,
    hasVisibility,
    properties
  };
}

// Scan all node directories
function extractAllSchemas(includeAll = true) {
  const schemas = {};

  const entries = fs.readdirSync(KOENIG_NODES_PATH, { withFileTypes: true });

  for (const entry of entries) {
    if (entry.isDirectory()) {
      const dirPath = path.join(KOENIG_NODES_PATH, entry.name);
      const files = fs.readdirSync(dirPath);

      // Find ALL Node.js files in the directory (some directories have multiple)
      const nodeFiles = files.filter(f => f.endsWith('Node.js') && !f.includes('test'));
      for (const nodeFile of nodeFiles) {
        const filePath = path.join(dirPath, nodeFile);
        const schema = extractNodeSchema(filePath);
        if (schema) {
          // Include all nodes, not just those with properties
          schemas[schema.nodeType] = schema;
        }
      }
    } else if (entry.name.endsWith('Node.js')) {
      // Handle files directly in nodes/ directory (Extended*, TK, etc.)
      const filePath = path.join(KOENIG_NODES_PATH, entry.name);
      const schema = extractNodeSchema(filePath);
      if (schema) {
        schemas[schema.nodeType] = schema;
      }
    }
  }

  return schemas;
}

// Find line numbers where a property is used in a file
function findPropertyLineNumbers(filePath, propName) {
  if (!fs.existsSync(filePath)) return [];

  const content = fs.readFileSync(filePath, 'utf8');
  const lines = content.split('\n');
  const matches = [];

  // Patterns to match property usage
  const patterns = [
    new RegExp(`node\\.${propName}\\b`, 'i'),
    new RegExp(`node\\.get${propName.charAt(0).toUpperCase() + propName.slice(1)}\\(`, 'i'),
    new RegExp(`\\b${propName}\\b.*=.*node`, 'i'),
    new RegExp(`{[^}]*\\b${propName}\\b[^}]*}\\s*=`, 'i'),
  ];

  lines.forEach((line, idx) => {
    for (const pattern of patterns) {
      if (pattern.test(line)) {
        matches.push({
          line: idx + 1,
          content: line.trim().substring(0, 80)
        });
        break;
      }
    }
  });

  return matches;
}

// Analyze which node properties are actually used in the renderer with line numbers
function analyzeRendererUsage(schema) {
  if (!schema.rendererFile) return schema;

  const rendererPath = path.join(KOENIG_NODES_PATH, schema.rendererFile);
  if (!fs.existsSync(rendererPath)) return schema;

  const rendererContent = fs.readFileSync(rendererPath, 'utf8');

  const usedInRenderer = [];
  const ignoredInRenderer = [];
  const propertyLines = {};

  for (const prop of schema.properties) {
    // Check if property name appears in renderer (node.propName or destructured)
    const propPattern = new RegExp(`(node\\.${prop.name}|\\b${prop.name}\\b.*=.*node|{[^}]*\\b${prop.name}\\b[^}]*}.*=.*node)`, 'g');
    const lineMatches = findPropertyLineNumbers(rendererPath, prop.name);

    if (propPattern.test(rendererContent) || lineMatches.length > 0) {
      usedInRenderer.push(prop.name);
      if (lineMatches.length > 0) {
        propertyLines[prop.name] = lineMatches;
      }
    } else {
      ignoredInRenderer.push(prop.name);
    }
  }

  schema.rendererUsedProps = usedInRenderer;
  schema.rendererIgnoredProps = ignoredInRenderer;
  schema.propertyLines = propertyLines;

  return schema;
}

// Enrich schema with valid values
function enrichSchema(schema) {
  for (const prop of schema.properties) {
    // Check for known values
    if (KNOWN_VALUES[prop.name]) {
      const values = KNOWN_VALUES[prop.name];
      if (Array.isArray(values)) {
        prop.values = values;
      } else if (values[schema.nodeType]) {
        prop.values = values[schema.nodeType];
      }
    }
  }

  // Add ox-ghost block name
  for (const [oxName, nodeType] of Object.entries(OX_GHOST_MAP)) {
    if (nodeType === schema.nodeType) {
      schema.oxBlock = oxName;
      break;
    }
  }

  // Analyze renderer usage
  analyzeRendererUsage(schema);

  return schema;
}

// Generate org-mode properties callout
function generateOrgCallout(schema) {
  const props = schema.properties.map(p => {
    let desc = `=${p.name}=`;
    if (p.values) {
      desc += ` (${p.values.join(', ')})`;
    } else if (p.default !== undefined && p.default !== '' && p.default !== null) {
      desc += ` (default: ${JSON.stringify(p.default)})`;
    }
    return desc;
  }).join(', ');

  return `#+BEGIN_CALLOUT :emoji "📋" :color grey
*Properties:* ${props}

[[${schema.github}][View source →]]
#+END_CALLOUT`;
}

// Generate detailed org-mode table
function generateOrgTable(schema) {
  let org = `| Property | Default | Valid Values |\n`;
  org += `|----------+---------+--------------|\n`;
  for (const p of schema.properties) {
    const defaultVal = p.default !== undefined && p.default !== '' ? String(p.default) : '-';
    const values = p.values ? p.values.join(', ') : (p.urlType ? 'URL' : '-');
    org += `| =${p.name}= | ${defaultVal} | ${values} |\n`;
  }
  return org;
}

// Generate markdown table
function generateMarkdownTable(schema) {
  let md = `| Property | Default | Type |\n|----------|---------|------|\n`;
  for (const p of schema.properties) {
    const defaultVal = p.default !== undefined ? JSON.stringify(p.default) : '-';
    const type = p.urlType || (p.wordCount ? 'text' : 'string');
    md += `| \`${p.name}\` | ${defaultVal} | ${type} |\n`;
  }
  md += `\n[Source](${schema.github})`;
  return md;
}

// Scan the kg-default-nodes.js to find all exported nodes
function extractAllNodeTypes() {
  const kgNodesFile = path.join(KOENIG_LIB_PATH, 'kg-default-nodes.js');
  const content = fs.readFileSync(kgNodesFile, 'utf8');

  const nodes = [];

  // Extract from DEFAULT_NODES array
  const defaultNodesMatch = content.match(/DEFAULT_NODES\s*=\s*\[([\s\S]*?)\]/);
  if (defaultNodesMatch) {
    const arrayContent = defaultNodesMatch[1];
    // Match node references like "textnode.ExtendedTextNode" or "codeblock.CodeBlockNode"
    const nodeRefs = arrayContent.match(/\w+\.\w+Node/g) || [];
    for (const ref of nodeRefs) {
      const parts = ref.split('.');
      if (parts.length === 2) {
        nodes.push({
          module: parts[0],
          className: parts[1]
        });
      }
    }
  }

  return nodes;
}

// Categorize a node based on its type and properties
function categorizeNode(schema) {
  const nodeType = schema.nodeType;

  // Check if it's a known category
  for (const [category, info] of Object.entries(NODE_CATEGORIES)) {
    if (info.nodes.includes(nodeType) || info.nodes.includes(schema.className)) {
      return category;
    }
  }

  // Infer category from properties
  if (schema.properties && schema.properties.length > 0) {
    return 'decorator';
  }

  return 'other';
}

// Get full schema including Lexical core nodes
function getAllSchemas(includeCore = false) {
  const ghostSchemas = extractAllSchemas();

  // Enrich ghost schemas
  for (const nodeType of Object.keys(ghostSchemas)) {
    ghostSchemas[nodeType] = enrichSchema(ghostSchemas[nodeType]);
    ghostSchemas[nodeType].category = categorizeNode(ghostSchemas[nodeType]);
  }

  if (!includeCore) {
    return ghostSchemas;
  }

  // Combine with Lexical core nodes
  return {
    ...LEXICAL_CORE_NODES,
    ...ghostSchemas
  };
}

// Generate summary statistics
function generateStats(schemas) {
  const stats = {
    total: Object.keys(schemas).length,
    byCategory: {}
  };

  for (const schema of Object.values(schemas)) {
    const cat = schema.category || 'other';
    stats.byCategory[cat] = (stats.byCategory[cat] || 0) + 1;
  }

  return stats;
}

// Main
function main() {
  const args = process.argv.slice(2);
  const includeCore = args.includes('--all');
  let schemas = getAllSchemas(includeCore);

  // Handle --stats
  if (args.includes('--stats')) {
    const stats = generateStats(schemas);
    console.log('Node Statistics:');
    console.log(`  Total: ${stats.total}`);
    console.log('  By category:');
    for (const [cat, count] of Object.entries(stats.byCategory).sort()) {
      console.log(`    ${cat}: ${count}`);
    }
    return;
  }

  // Handle --lookup for specific node
  const lookupIdx = args.indexOf('--lookup');
  if (lookupIdx !== -1) {
    const query = args[lookupIdx + 1]?.toLowerCase();
    if (!query) {
      console.error('Usage: --lookup <node-type-or-ox-block>');
      process.exit(1);
    }

    // Find by node type or ox-ghost block name
    let schema = schemas[query];
    if (!schema) {
      // Try ox-ghost block name
      const nodeType = OX_GHOST_MAP[query.toUpperCase()];
      if (nodeType) schema = schemas[nodeType];
    }

    // Also check Lexical core nodes
    if (!schema && LEXICAL_CORE_NODES[query]) {
      schema = LEXICAL_CORE_NODES[query];
    }

    if (!schema) {
      console.error(`Node not found: ${query}`);
      console.error(`Ghost nodes: ${Object.keys(extractAllSchemas()).join(', ')}`);
      console.error(`Lexical core: ${Object.keys(LEXICAL_CORE_NODES).join(', ')}`);
      console.error(`ox-ghost blocks: ${Object.keys(OX_GHOST_MAP).join(', ')}`);
      process.exit(1);
    }

    console.log(`\n=== ${schema.nodeType} ===`);
    if (schema.category) console.log(`Category: ${schema.category}`);
    if (schema.oxBlock) console.log(`ox-ghost block: #+BEGIN_${schema.oxBlock}`);
    if (schema.description) console.log(`Description: ${schema.description}`);
    if (schema.source) console.log(`Source: ${schema.source}`);
    console.log(`GitHub: ${schema.github}\n`);

    if (schema.canContain) {
      console.log(`Can contain: ${schema.canContain.join(', ')}`);
    }
    if (schema.formats) {
      console.log(`Text formats: ${schema.formats.join(', ')}`);
    }

    if (schema.properties && schema.properties.length > 0) {
      console.log('Properties:');
      for (const p of schema.properties) {
        let line = `  ${p.name}`;
        if (p.default !== undefined && p.default !== '') line += ` (default: ${JSON.stringify(p.default)})`;
        if (p.values) line += `\n    values: ${p.values.join(', ')}`;
        if (p.urlType) line += ` [URL]`;
        console.log(line);
      }
    }
    return;
  }

  if (args.includes('--org')) {
    // Output org-mode callouts for each node
    // Group by category if --all is specified
    if (includeCore) {
      console.log('* Lexical Core Nodes\n');
      console.log('These nodes are provided by Lexical and used by Ghost.\n');
      for (const [nodeType, schema] of Object.entries(LEXICAL_CORE_NODES)) {
        console.log(`** ${nodeType}\n`);
        if (schema.description) console.log(`${schema.description}\n`);
        if (schema.properties && schema.properties.length > 0) {
          console.log(generateOrgCallout(schema));
        } else {
          console.log(`#+BEGIN_CALLOUT :emoji "📋" :color grey\n*Source:* ${schema.source}\n\n[[${schema.github}][View source →]]\n#+END_CALLOUT`);
        }
      }
      console.log('\n* Ghost-Specific Nodes\n');
      console.log('These nodes are defined by Ghost\'s Koenig editor.\n');
    }

    const ghostSchemas = extractAllSchemas();
    for (const nodeType of Object.keys(ghostSchemas)) {
      ghostSchemas[nodeType] = enrichSchema(ghostSchemas[nodeType]);
    }

    for (const [nodeType, schema] of Object.entries(ghostSchemas).sort()) {
      const blockName = schema.oxBlock ? ` (=${schema.oxBlock}=)` : '';
      console.log(`** ${nodeType}${blockName}\n`);
      console.log(generateOrgCallout(schema));
    }
  } else if (args.includes('--org-table')) {
    // Output org-mode tables
    for (const [nodeType, schema] of Object.entries(schemas).sort()) {
      const blockName = schema.oxBlock ? ` (=${schema.oxBlock}=)` : '';
      console.log(`\n** ${nodeType}${blockName}\n`);
      console.log(generateOrgTable(schema));
      console.log(`\n[[${schema.github}][View source →]]\n`);
    }
  } else if (args.includes('--md')) {
    // Output markdown
    for (const [nodeType, schema] of Object.entries(schemas).sort()) {
      console.log(`\n### ${nodeType}\n`);
      console.log(generateMarkdownTable(schema));
    }
  } else if (args.includes('--list')) {
    // Simple list of all nodes
    console.log('Lexical Core Nodes:');
    for (const nodeType of Object.keys(LEXICAL_CORE_NODES)) {
      console.log(`  ${nodeType}`);
    }
    console.log('\nGhost Decorator Nodes:');
    const ghostSchemas = extractAllSchemas();
    for (const nodeType of Object.keys(ghostSchemas).sort()) {
      console.log(`  ${nodeType}`);
    }
  } else if (args.includes('--renderer-report')) {
    // Comprehensive renderer analysis report
    console.log('='.repeat(70));
    console.log('GHOST LEXICAL RENDERER ANALYSIS REPORT');
    console.log('Properties defined in nodes vs actually used in HTML renderers');
    console.log('='.repeat(70));

    const allSchemas = getAllSchemas(true);
    const patchOpportunities = [];
    const fullyRendered = [];
    const noRenderer = [];

    for (const [nodeType, schema] of Object.entries(allSchemas).sort()) {
      const props = schema.properties || [];
      if (props.length === 0) continue;

      const hasRenderer = schema.rendererGithub || schema.rendererFile;
      const ignored = schema.rendererIgnoredProps || [];
      const used = schema.rendererUsedProps || [];

      if (!hasRenderer) {
        noRenderer.push({ nodeType, props: props.map(p => p.name) });
      } else if (ignored.length > 0) {
        patchOpportunities.push({
          nodeType,
          ignored,
          used,
          renderer: schema.rendererGithub || schema.rendererFile
        });
      } else if (used.length > 0) {
        fullyRendered.push({ nodeType, used });
      }
    }

    // Report: Patch Opportunities (properties stored but not rendered)
    console.log('\n' + '─'.repeat(70));
    console.log('🔧 PATCH OPPORTUNITIES (properties stored but NOT rendered)');
    console.log('─'.repeat(70));

    if (patchOpportunities.length === 0) {
      console.log('  None found - all properties are rendered!');
    } else {
      for (const item of patchOpportunities) {
        console.log(`\n  ${item.nodeType}:`);
        console.log(`    Ignored: ${item.ignored.join(', ')}`);
        if (item.used.length > 0) {
          console.log(`    Used:    ${item.used.join(', ')}`);
        }
        console.log(`    Renderer: ${item.renderer}`);
      }
    }

    // Report: Fully Rendered
    console.log('\n' + '─'.repeat(70));
    console.log('✓ FULLY RENDERED (all properties used)');
    console.log('─'.repeat(70));

    for (const item of fullyRendered) {
      console.log(`  ${item.nodeType}: ${item.used.join(', ')}`);
    }

    // Report: No Renderer Analysis
    console.log('\n' + '─'.repeat(70));
    console.log('? NO RENDERER ANALYSIS (needs manual review)');
    console.log('─'.repeat(70));

    for (const item of noRenderer) {
      console.log(`  ${item.nodeType}: ${item.props.join(', ')}`);
    }

    // Summary
    console.log('\n' + '='.repeat(70));
    console.log('SUMMARY');
    console.log('='.repeat(70));
    console.log(`  Patch opportunities:  ${patchOpportunities.length} nodes with ignored properties`);
    console.log(`  Fully rendered:       ${fullyRendered.length} nodes (all props used)`);
    console.log(`  Needs manual review:  ${noRenderer.length} nodes (no renderer found)`);

    const totalIgnored = patchOpportunities.reduce((sum, i) => sum + i.ignored.length, 0);
    if (totalIgnored > 0) {
      console.log(`\n  Total ignored properties: ${totalIgnored}`);
      console.log('  These are stored in JSON but not rendered to HTML.');
      console.log('  Consider submitting PRs to Ghost/Koenig to render them.');
    }

    console.log(`\n  Koenig ref: ${KOENIG_GIT_REF}`);
    console.log('  Run --renderer-detail <node> for line-by-line analysis');

  } else if (args.includes('--renderer-detail')) {
    // Detailed renderer analysis for a specific node
    const detailIdx = args.indexOf('--renderer-detail');
    const nodeType = args[detailIdx + 1];

    if (!nodeType) {
      console.error('Usage: --renderer-detail <node-type>');
      console.error('Example: --renderer-detail link');
      process.exit(1);
    }

    const allSchemas = getAllSchemas(true);
    const schema = allSchemas[nodeType] || LEXICAL_CORE_NODES[nodeType];

    if (!schema) {
      console.error(`Node not found: ${nodeType}`);
      process.exit(1);
    }

    console.log('='.repeat(70));
    console.log(`RENDERER DETAIL: ${nodeType}`);
    console.log('='.repeat(70));

    // Node info
    console.log(`\nNode Definition:`);
    console.log(`  ${schema.github}`);

    if (schema.rendererGithub) {
      console.log(`\nRenderer:`);
      console.log(`  ${schema.rendererGithub}`);
    }

    // Get local renderer path for line analysis
    let localRendererPath = null;
    if (schema.rendererFile) {
      localRendererPath = path.join(KOENIG_NODES_PATH, schema.rendererFile);
    } else if (schema.rendererGithub && schema.rendererGithub.includes('TextContent.ts')) {
      localRendererPath = path.join(RENDERER_PATH, 'utils/TextContent.ts');
    } else if (schema.rendererGithub && schema.rendererGithub.includes('transformers/element')) {
      const match = schema.rendererGithub.match(/element\/([^#]+)/);
      if (match) localRendererPath = path.join(RENDERER_PATH, 'transformers/element', match[1]);
    }

    const props = schema.properties || [];
    if (props.length === 0) {
      console.log('\n  No properties defined');
      return;
    }

    console.log(`\n${'─'.repeat(70)}`);
    console.log('PROPERTY ANALYSIS');
    console.log('─'.repeat(70));

    for (const prop of props) {
      const isUsed = (schema.rendererUsedProps || []).includes(prop.name);
      const isIgnored = (schema.rendererIgnoredProps || []).includes(prop.name);
      const status = isUsed ? '✓' : (isIgnored ? '✗' : '?');

      console.log(`\n  ${status} ${prop.name}`);
      if (prop.default !== undefined) console.log(`    Default: ${JSON.stringify(prop.default)}`);
      if (prop.urlType) console.log(`    Type: URL`);

      // Find line numbers in renderer
      if (localRendererPath && fs.existsSync(localRendererPath)) {
        const lines = findPropertyLineNumbers(localRendererPath, prop.name);
        if (lines.length > 0) {
          console.log(`    Renderer usage:`);
          for (const match of lines) {
            const githubLine = schema.rendererGithub ? `${schema.rendererGithub}#L${match.line}` : `Line ${match.line}`;
            console.log(`      L${match.line}: ${match.content}`);
            console.log(`      → ${githubLine}`);
          }
        } else if (isIgnored) {
          console.log(`    ⚠️  NOT FOUND in renderer - patch needed!`);
        }
      }
    }

    // Show where to add missing properties
    const ignored = schema.rendererIgnoredProps || [];
    if (ignored.length > 0 && localRendererPath && fs.existsSync(localRendererPath)) {
      console.log(`\n${'─'.repeat(70)}`);
      console.log('PATCH LOCATIONS');
      console.log('─'.repeat(70));

      // Find a good insertion point (after a used property)
      const used = schema.rendererUsedProps || [];
      if (used.length > 0) {
        const lastUsed = used[used.length - 1];
        const lastUsedLines = findPropertyLineNumbers(localRendererPath, lastUsed);
        if (lastUsedLines.length > 0) {
          const lastLine = lastUsedLines[lastUsedLines.length - 1];
          console.log(`\n  Add missing properties after line ${lastLine.line}:`);
          console.log(`  ${schema.rendererGithub}#L${lastLine.line}`);
          console.log(`\n  Missing: ${ignored.join(', ')}`);
        }
      }
    }

  } else {
    // Output JSON
    const outputFile = args.find((a, i) => args[i-1] === '--json');
    const output = JSON.stringify(schemas, null, 2);

    if (outputFile) {
      fs.writeFileSync(outputFile, output);
      console.error(`Wrote ${Object.keys(schemas).length} node schemas to ${outputFile}`);
    } else {
      console.log(output);
    }
  }
}

main();
