/**
 * should-export.js - Org→JSON assertion helpers
 *
 * Provides assertion helpers for testing org-mode to Lexical JSON export.
 */

const { expect } = require('chai');
const { exportOrg, getRootChildren, getFirstNodeOfType, getAllNodesOfType } = require('./export-org');

/**
 * Assert that org content exports to expected JSON structure
 * @param {Object} options
 * @param {string} options.org - Org-mode content
 * @param {Object} options.expected - Expected JSON structure (partial match)
 */
function shouldExport({ org, expected }) {
  const result = exportOrg(org);
  expect(result).to.deep.include(expected);
}

/**
 * Assert that org content produces expected root children
 * @param {Object} options
 * @param {string} options.org - Org-mode content
 * @param {Array} options.children - Expected children array
 */
function shouldExportChildren({ org, children }) {
  const result = getRootChildren(org);
  expect(result).to.have.lengthOf(children.length);
  children.forEach((expected, i) => {
    expect(result[i]).to.deep.include(expected);
  });
}

/**
 * Assert that org content produces a node of specific type with expected properties
 * @param {Object} options
 * @param {string} options.org - Org-mode content
 * @param {string} options.nodeType - Expected node type
 * @param {Object} options.expected - Expected node properties
 */
function shouldExportNode({ org, nodeType, expected }) {
  const node = getFirstNodeOfType(org, nodeType);
  expect(node, `Expected node of type "${nodeType}"`).to.not.be.null;
  expect(node).to.deep.include(expected);
  return node;
}

/**
 * Assert that org content produces multiple nodes of a specific type
 * @param {Object} options
 * @param {string} options.org - Org-mode content
 * @param {string} options.nodeType - Expected node type
 * @param {Array} options.expectedNodes - Array of expected node properties
 */
function shouldExportNodes({ org, nodeType, expectedNodes }) {
  const nodes = getAllNodesOfType(org, nodeType);
  expect(nodes).to.have.lengthOf(expectedNodes.length);
  expectedNodes.forEach((expected, i) => {
    expect(nodes[i]).to.deep.include(expected);
  });
  return nodes;
}

/**
 * Assert paragraph contains text with specific format
 * @param {Object} options
 * @param {string} options.org - Org-mode content
 * @param {string} options.text - Expected text content
 * @param {number} options.format - Expected format bitmask (default 0)
 */
function shouldExportText({ org, text, format = 0 }) {
  const para = getFirstNodeOfType(org, 'paragraph');
  expect(para, 'Expected paragraph node').to.not.be.null;
  expect(para.children).to.be.an('array').with.lengthOf.at.least(1);

  const textNode = para.children.find(c => c.type === 'text' && c.text.includes(text));
  expect(textNode, `Expected text node containing "${text}"`).to.exist;
  expect(textNode.format).to.equal(format);
  return textNode;
}

/**
 * Assert text node has specific format flags set
 * @param {Object} textNode - Text node to check
 * @param {number} formatFlag - Format flag to check (should be set)
 */
function shouldHaveFormat(textNode, formatFlag) {
  expect(textNode.format & formatFlag, `Expected format flag ${formatFlag} to be set`).to.not.equal(0);
}

/**
 * Assert heading export
 * @param {Object} options
 * @param {string} options.org - Org-mode content
 * @param {string} options.tag - Expected heading tag (h1-h6)
 * @param {string} options.text - Expected heading text
 */
function shouldExportHeading({ org, tag, text }) {
  const heading = getFirstNodeOfType(org, 'heading');
  expect(heading, 'Expected heading node').to.not.be.null;
  expect(heading.tag).to.equal(tag);

  const textChild = heading.children?.find(c => c.type === 'text');
  expect(textChild?.text).to.include(text);
  return heading;
}

/**
 * Assert list export
 * @param {Object} options
 * @param {string} options.org - Org-mode content
 * @param {string} options.listType - Expected list type ('bullet' or 'number')
 * @param {number} options.itemCount - Expected number of items
 */
function shouldExportList({ org, listType, itemCount }) {
  const list = getFirstNodeOfType(org, 'list');
  expect(list, 'Expected list node').to.not.be.null;
  expect(list.listType).to.equal(listType);

  // Count listitem children
  const items = list.children?.filter(c => c.type === 'listitem') || [];
  expect(items).to.have.lengthOf(itemCount);
  return list;
}

/**
 * Assert code block export
 * @param {Object} options
 * @param {string} options.org - Org-mode content
 * @param {string} options.language - Expected language
 * @param {string} options.codeContains - Text that code should contain
 */
function shouldExportCodeBlock({ org, language, codeContains }) {
  const codeblock = getFirstNodeOfType(org, 'codeblock');
  expect(codeblock, 'Expected codeblock node').to.not.be.null;
  expect(codeblock.language).to.equal(language);
  if (codeContains) {
    expect(codeblock.code).to.include(codeContains);
  }
  return codeblock;
}

/**
 * Assert card node export (callout, toggle, button, etc.)
 * @param {Object} options
 * @param {string} options.org - Org-mode content
 * @param {string} options.cardType - Expected card type
 * @param {Object} options.expected - Expected card properties
 */
function shouldExportCard({ org, cardType, expected }) {
  const card = getFirstNodeOfType(org, cardType);
  expect(card, `Expected ${cardType} card`).to.not.be.null;
  expect(card).to.deep.include(expected);
  return card;
}

// Format constants matching ox-ghost.el
const FORMAT = {
  BOLD: 1,
  ITALIC: 2,
  STRIKETHROUGH: 4,
  UNDERLINE: 8,
  CODE: 16,
  SUBSCRIPT: 32,
  SUPERSCRIPT: 64,
  HIGHLIGHT: 128
};

module.exports = {
  shouldExport,
  shouldExportChildren,
  shouldExportNode,
  shouldExportNodes,
  shouldExportText,
  shouldHaveFormat,
  shouldExportHeading,
  shouldExportList,
  shouldExportCodeBlock,
  shouldExportCard,
  FORMAT
};
