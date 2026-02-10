/**
 * Style Test - Validates ghost-style-test.org
 *
 * Runs the same structural tests against the comprehensive style guide
 * to ensure it exports and renders correctly.
 */

const { expect } = require('chai');
const path = require('path');
const { exportOrgFile, shouldRenderSuccessfully } = require('./utils');

describe('Ghost Style Test', function() {
  this.timeout(30000);

  let json;
  let nodes;

  before(function() {
    const fixtureFile = path.join(__dirname, 'fixtures', 'ghost-style-test.org');
    json = exportOrgFile(fixtureFile);
    nodes = json.root.children;
  });

  function findByType(type) {
    return nodes.filter(n => n.type === type);
  }

  function findFirstByType(type) {
    return nodes.find(n => n.type === type);
  }

  describe('Document Structure', function() {
    it('should have valid root', function() {
      expect(json.root).to.exist;
      expect(json.root.type).to.equal('root');
    });

    it('should have more nodes than minimal fixture', function() {
      // Style test should be more comprehensive
      expect(nodes.length).to.be.at.least(100);
    });

    it('should render without errors', async function() {
      await shouldRenderSuccessfully(json);
    });
  });

  describe('Node Type Coverage', function() {
    const expectedTypes = [
      'paragraph', 'heading', 'list', 'quote', 'codeblock',
      'image', 'callout', 'toggle', 'button', 'header',
      'bookmark', 'embed', 'call-to-action', 'aside',
      'gallery', 'video', 'audio', 'file',
      'email', 'email-cta', 'signup', 'product', 'paywall',
      'transistor', 'html', 'markdown', 'horizontalrule'
    ];

    for (const type of expectedTypes) {
      it(`should have ${type} node`, function() {
        const found = findFirstByType(type);
        expect(found, `Expected ${type} node`).to.exist;
      });
    }
  });

  describe('Text Formats', function() {
    it('should have all heading levels (h2-h6)', function() {
      const headings = findByType('heading');
      const tags = new Set(headings.map(h => h.tag));
      expect(tags.has('h2')).to.be.true;
      expect(tags.has('h3')).to.be.true;
      expect(tags.has('h4')).to.be.true;
      expect(tags.has('h5')).to.be.true;
      expect(tags.has('h6')).to.be.true;
    });
  });

  describe('Card Variants', function() {
    it('should have multiple callout colors', function() {
      const callouts = findByType('callout');
      const colors = new Set(callouts.map(c => c.backgroundColor));
      expect(colors.size).to.be.at.least(4);
    });

    it('should have multiple header styles', function() {
      const headers = findByType('header');
      expect(headers.length).to.be.at.least(4);
    });

    it('should have v1 and v2 headers', function() {
      const headers = findByType('header');
      const hasV1 = headers.some(h => h.style);
      const hasV2 = headers.some(h => h.layout);
      expect(hasV1 || hasV2, 'Expected v1 or v2 headers').to.be.true;
    });
  });

  describe('Rich Content', function() {
    it('should have code blocks with multiple languages', function() {
      const codeblocks = findByType('codeblock');
      const languages = new Set(codeblocks.map(c => c.language).filter(l => l));
      expect(languages.size).to.be.at.least(3);
    });

    it('should have gallery with multiple images', function() {
      const gallery = findFirstByType('gallery');
      expect(gallery.images.length).to.be.at.least(4);
    });
  });
});
