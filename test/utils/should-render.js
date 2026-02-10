/**
 * should-render.js - JSON→HTML assertion helpers
 *
 * Provides assertion helpers for testing Lexical JSON to HTML rendering
 * using Ghost's actual kg-lexical-html-renderer.
 */

const { expect } = require('chai');

// Lazy load Ghost renderer to avoid startup cost if not needed
let Renderer = null;
let DEFAULT_NODES = null;

function getRenderer() {
  if (!Renderer) {
    Renderer = require('@tryghost/kg-lexical-html-renderer');
    DEFAULT_NODES = require('@tryghost/kg-default-nodes').DEFAULT_NODES;
  }
  return new Renderer({ nodes: DEFAULT_NODES });
}

/**
 * Render Lexical JSON to HTML
 * @param {Object} lexicalJson - Lexical JSON object
 * @returns {Promise<string>} Rendered HTML
 */
async function render(lexicalJson) {
  const renderer = getRenderer();
  return renderer.render(JSON.stringify(lexicalJson));
}

/**
 * Render a single node (wraps in minimal root structure)
 * @param {Object} node - Single Lexical node
 * @returns {Promise<string>} Rendered HTML
 */
async function renderNode(node) {
  const wrapped = {
    root: {
      type: 'root',
      version: 1,
      children: [node],
      direction: 'ltr',
      format: '',
      indent: 0
    }
  };
  return render(wrapped);
}

/**
 * Assert that Lexical JSON renders to expected HTML
 * @param {Object} options
 * @param {Object} options.input - Lexical JSON object
 * @param {string} options.output - Expected HTML output (exact match)
 */
async function shouldRender({ input, output }) {
  const html = await render(input);
  expect(html).to.equal(output);
}

/**
 * Assert that Lexical JSON renders to HTML containing expected content
 * @param {Object} options
 * @param {Object} options.input - Lexical JSON object
 * @param {string|Array<string>} options.contains - String(s) that should appear in output
 */
async function shouldRenderContaining({ input, contains }) {
  const html = await render(input);
  const checks = Array.isArray(contains) ? contains : [contains];
  checks.forEach(str => {
    expect(html, `Expected HTML to contain: ${str}`).to.include(str);
  });
  return html;
}

/**
 * Assert that Lexical JSON renders without error
 * @param {Object} input - Lexical JSON object
 * @returns {Promise<string>} Rendered HTML
 */
async function shouldRenderSuccessfully(input) {
  const html = await render(input);
  expect(html).to.be.a('string');
  return html;
}

/**
 * Assert that a single node renders to expected HTML
 * @param {Object} options
 * @param {Object} options.node - Single Lexical node
 * @param {string} options.output - Expected HTML output
 */
async function shouldRenderNode({ node, output }) {
  const html = await renderNode(node);
  expect(html).to.equal(output);
}

/**
 * Assert that a single node renders to HTML containing expected content
 * @param {Object} options
 * @param {Object} options.node - Single Lexical node
 * @param {string|Array<string>} options.contains - String(s) that should appear in output
 */
async function shouldRenderNodeContaining({ node, contains }) {
  const html = await renderNode(node);
  const checks = Array.isArray(contains) ? contains : [contains];
  checks.forEach(str => {
    expect(html, `Expected HTML to contain: ${str}`).to.include(str);
  });
  return html;
}

/**
 * Assert HTML contains specific element pattern
 * @param {string} html - HTML string
 * @param {string} pattern - Regex pattern to match
 * @param {string} message - Assertion message
 */
function assertHtmlContains(html, pattern, message) {
  const regex = new RegExp(pattern);
  expect(html, message || `Expected HTML to match: ${pattern}`).to.match(regex);
}

/**
 * Assert HTML contains specific tag with attributes
 * @param {string} html - HTML string
 * @param {string} tag - Tag name (e.g., 'p', 'div')
 * @param {Object} attrs - Expected attributes
 */
function assertHtmlHasTag(html, tag, attrs = {}) {
  // Build pattern from tag and attributes
  let pattern = `<${tag}`;
  for (const [key, value] of Object.entries(attrs)) {
    if (value === true) {
      pattern += `[^>]*${key}`;
    } else {
      pattern += `[^>]*${key}="${value}"`;
    }
  }
  pattern += '[^>]*>';
  assertHtmlContains(html, pattern, `Expected <${tag}> with attributes: ${JSON.stringify(attrs)}`);
}

/**
 * Build a minimal Lexical JSON document with given children
 * @param {Array} children - Array of child nodes
 * @returns {Object} Complete Lexical JSON structure
 */
function buildLexicalDoc(children) {
  return {
    root: {
      type: 'root',
      version: 1,
      children: children,
      direction: 'ltr',
      format: '',
      indent: 0
    }
  };
}

/**
 * Build a paragraph node with text
 * @param {string} text - Text content
 * @param {number} format - Text format bitmask
 * @returns {Object} Paragraph node
 */
function buildParagraph(text, format = 0) {
  return {
    type: 'paragraph',
    version: 1,
    children: [{
      type: 'text',
      version: 1,
      text: text,
      format: format,
      style: '',
      detail: 0,
      mode: 'normal'
    }],
    direction: 'ltr',
    format: '',
    indent: 0,
    textFormat: 0,
    textStyle: ''
  };
}

/**
 * Build a heading node with text
 * @param {number} level - Heading level (1-6)
 * @param {string} text - Text content
 * @returns {Object} Heading node
 */
function buildHeading(level, text) {
  return {
    type: 'heading',
    version: 1,
    tag: `h${level}`,
    children: [{
      type: 'text',
      version: 1,
      text: text,
      format: 0,
      style: '',
      detail: 0,
      mode: 'normal'
    }],
    direction: 'ltr',
    format: '',
    indent: 0
  };
}

module.exports = {
  render,
  renderNode,
  shouldRender,
  shouldRenderContaining,
  shouldRenderSuccessfully,
  shouldRenderNode,
  shouldRenderNodeContaining,
  assertHtmlContains,
  assertHtmlHasTag,
  buildLexicalDoc,
  buildParagraph,
  buildHeading
};
