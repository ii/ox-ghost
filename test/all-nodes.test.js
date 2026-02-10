/**
 * All Nodes Test - Comprehensive Single Export
 *
 * Exports one comprehensive org file once and runs all assertions
 * against the cached output. Tests are fixture-agnostic - they check
 * structure and types rather than specific content.
 *
 * E2E tests (optional): If GHOST_URL and GHOST_ADMIN_KEY are set,
 * also pushes a draft to Ghost for visual verification.
 */

const { expect } = require('chai');
const path = require('path');

// Load .env for Ghost E2E tests
require('dotenv').config({ path: path.join(__dirname, '..', '.env') });
const {
  exportOrgFile,
  FORMAT,
  render,
  shouldRenderSuccessfully,
  buildLexicalDoc,
  buildParagraph,
  buildHeading
} = require('./utils');

describe('All Nodes', function() {
  this.timeout(30000);

  let json;
  let nodes;

  // Export once before all tests
  before(function() {
    const fixtureFile = path.join(__dirname, 'fixtures', 'all-nodes.org');
    json = exportOrgFile(fixtureFile);
    nodes = json.root.children;
  });

  // Helpers
  function findByType(type) {
    return nodes.filter(n => n.type === type);
  }

  function findFirstByType(type) {
    return nodes.find(n => n.type === type);
  }

  function findTextInParagraph(searchText) {
    for (const para of findByType('paragraph')) {
      for (const child of para.children || []) {
        if (child.type === 'text' && child.text.includes(searchText)) {
          return child;
        }
      }
    }
    return null;
  }

  function findTextWithFormat(format) {
    for (const para of findByType('paragraph')) {
      for (const child of para.children || []) {
        if (child.type === 'text' && (child.format & format) === format) {
          return child;
        }
      }
    }
    return null;
  }

  // =========================================================================
  // Document Structure
  // =========================================================================

  describe('Document Structure', function() {
    it('should have valid root', function() {
      expect(json.root).to.exist;
      expect(json.root.type).to.equal('root');
      expect(json.root.version).to.equal(1);
      expect(json.root.direction).to.equal('ltr');
    });

    it('should have many children', function() {
      expect(nodes.length).to.be.at.least(30);
    });

    it('should render without errors', async function() {
      await shouldRenderSuccessfully(json);
    });

    it('should produce valid JSON', function() {
      const str = JSON.stringify(json);
      const reparsed = JSON.parse(str);
      expect(reparsed).to.deep.equal(json);
    });
  });

  // =========================================================================
  // Text Formats (Org→JSON)
  // =========================================================================

  describe('Text Formats', function() {
    it('should export bold (format=1)', function() {
      const text = findTextWithFormat(FORMAT.BOLD);
      expect(text).to.exist;
    });

    it('should export italic (format=2)', function() {
      const text = findTextWithFormat(FORMAT.ITALIC);
      expect(text).to.exist;
    });

    it('should export underline (format=8)', function() {
      const text = findTextWithFormat(FORMAT.UNDERLINE);
      expect(text).to.exist;
    });

    it('should export strikethrough (format=4)', function() {
      const text = findTextWithFormat(FORMAT.STRIKETHROUGH);
      expect(text).to.exist;
    });

    it('should export inline code (format=16)', function() {
      const text = findTextWithFormat(FORMAT.CODE);
      expect(text).to.exist;
    });

    it('should export subscript (format=32)', function() {
      const text = findTextWithFormat(FORMAT.SUBSCRIPT);
      expect(text).to.exist;
    });

    it('should export superscript (format=64)', function() {
      const text = findTextWithFormat(FORMAT.SUPERSCRIPT);
      expect(text).to.exist;
    });

    it('should export highlight (format=128)', function() {
      const text = findTextWithFormat(FORMAT.HIGHLIGHT);
      expect(text).to.exist;
    });

    it('should combine formats (bold+italic)', function() {
      const combined = FORMAT.BOLD | FORMAT.ITALIC;
      const text = findTextWithFormat(combined);
      expect(text).to.exist;
    });
  });

  // =========================================================================
  // Headings
  // =========================================================================

  describe('Headings', function() {
    it('should have h2 (* level)', function() {
      const h2 = findByType('heading').find(h => h.tag === 'h2');
      expect(h2).to.exist;
    });

    it('should have h3 (** level)', function() {
      const h3 = findByType('heading').find(h => h.tag === 'h3');
      expect(h3).to.exist;
    });

    it('should have h4 (*** level)', function() {
      const h4 = findByType('heading').find(h => h.tag === 'h4');
      expect(h4).to.exist;
    });

    it('should have h5 (**** level)', function() {
      const h5 = findByType('heading').find(h => h.tag === 'h5');
      expect(h5).to.exist;
    });

    it('should have h6 (***** level, capped)', function() {
      const h6 = findByType('heading').find(h => h.tag === 'h6');
      expect(h6).to.exist;
    });

    it('should have correct structure', function() {
      const heading = findFirstByType('heading');
      expect(heading.direction).to.equal('ltr');
      expect(heading.children).to.be.an('array');
    });
  });

  // =========================================================================
  // Paragraphs
  // =========================================================================

  describe('Paragraphs', function() {
    it('should have multiple paragraphs', function() {
      expect(findByType('paragraph').length).to.be.at.least(3);
    });

    it('should have correct structure', function() {
      const para = findFirstByType('paragraph');
      expect(para.direction).to.equal('ltr');
      expect(para.children).to.be.an('array');
    });
  });

  // =========================================================================
  // Lists
  // =========================================================================

  describe('Lists', function() {
    it('should have lists', function() {
      const lists = findByType('list');
      expect(lists.length).to.be.at.least(1);
    });

    it('should have nested lists', function() {
      const lists = findByType('list');
      let hasNested = false;
      for (const list of lists) {
        for (const item of list.children || []) {
          if (item.type === 'listitem') {
            for (const child of item.children || []) {
              if (child.type === 'list') {
                hasNested = true;
                break;
              }
            }
          }
        }
      }
      expect(hasNested, 'Expected nested list').to.be.true;
    });

    it('should have checkbox items', function() {
      const lists = findByType('list');
      let hasCheckbox = false;
      for (const list of lists) {
        for (const item of list.children || []) {
          if (item.checked !== undefined) {
            hasCheckbox = true;
            break;
          }
        }
      }
      expect(hasCheckbox, 'Expected checkbox item').to.be.true;
    });

    it('should have correct list structure', function() {
      const list = findFirstByType('list');
      expect(list.direction).to.equal('ltr');
      expect(list.listType).to.be.oneOf(['bullet', 'number', 'check']);
      expect(list.children).to.be.an('array');
    });
  });

  // =========================================================================
  // Quote Block
  // =========================================================================

  describe('Quote Block', function() {
    it('should have quote nodes', function() {
      const quotes = findByType('quote');
      expect(quotes.length).to.be.at.least(1);
    });

    it('should have content', function() {
      const quote = findFirstByType('quote');
      expect(quote.children).to.be.an('array');
      expect(quote.children.length).to.be.at.least(1);
    });
  });

  // =========================================================================
  // Code Blocks
  // =========================================================================

  describe('Code Blocks', function() {
    it('should have multiple codeblocks', function() {
      const codeblocks = findByType('codeblock');
      expect(codeblocks.length).to.be.at.least(2);
    });

    it('should have javascript codeblock', function() {
      const js = findByType('codeblock').find(c => c.language === 'javascript');
      expect(js).to.exist;
    });

    it('should have codeblock without language', function() {
      const noLang = findByType('codeblock').find(c => !c.language || c.language === '');
      expect(noLang).to.exist;
    });

    it('should have code content', function() {
      const codeblock = findFirstByType('codeblock');
      expect(codeblock.code).to.be.a('string');
      expect(codeblock.code.length).to.be.at.least(1);
    });
  });

  // =========================================================================
  // Links
  // =========================================================================

  describe('Links', function() {
    it('should have links in paragraphs', function() {
      let hasLink = false;
      for (const para of findByType('paragraph')) {
        for (const child of para.children || []) {
          if (child.type === 'link') {
            hasLink = true;
            break;
          }
        }
      }
      expect(hasLink, 'Expected link in paragraph').to.be.true;
    });

    it('should have correct link structure', function() {
      let link = null;
      for (const para of findByType('paragraph')) {
        link = (para.children || []).find(c => c.type === 'link');
        if (link) break;
      }
      expect(link).to.exist;
      expect(link.url).to.be.a('string');
      expect(link.url).to.match(/^https?:\/\//);
      expect(link.children).to.be.an('array');
    });
  });

  // =========================================================================
  // Images
  // =========================================================================

  describe('Images', function() {
    it('should have multiple images', function() {
      const images = findByType('image');
      expect(images.length).to.be.at.least(2);
    });

    it('should have image with alt text', function() {
      const img = findByType('image').find(i => i.alt && i.alt.length > 0);
      expect(img).to.exist;
    });

    it('should have correct image structure', function() {
      const img = findFirstByType('image');
      expect(img.src).to.be.a('string');
      expect(img.src).to.match(/^https?:\/\//);
    });
  });

  // =========================================================================
  // Content Cards
  // =========================================================================

  describe('Content Cards', function() {
    it('should have callout cards', function() {
      const callouts = findByType('callout');
      expect(callouts.length).to.be.at.least(1);
    });

    it('should have callout with backgroundColor', function() {
      const callout = findFirstByType('callout');
      expect(callout.backgroundColor).to.be.a('string');
    });

    it('should have toggle card', function() {
      const toggle = findFirstByType('toggle');
      expect(toggle).to.exist;
      expect(toggle.heading).to.be.a('string');
    });

    it('should have button card', function() {
      const buttons = findByType('button');
      expect(buttons.length).to.be.at.least(1);
      expect(buttons[0].buttonUrl).to.be.a('string');
    });

    it('should have header card', function() {
      const headers = findByType('header');
      expect(headers.length).to.be.at.least(1);
    });

    it('should have bookmark card', function() {
      const bookmark = findFirstByType('bookmark');
      expect(bookmark).to.exist;
      expect(bookmark.url).to.be.a('string');
    });

    it('should have embed card', function() {
      const embed = findFirstByType('embed');
      expect(embed).to.exist;
      expect(embed.url).to.be.a('string');
    });

    it('should have call-to-action card', function() {
      const cta = findFirstByType('call-to-action');
      expect(cta).to.exist;
      expect(cta.buttonUrl).to.be.a('string');
    });
  });

  // =========================================================================
  // Media Cards
  // =========================================================================

  describe('Media Cards', function() {
    it('should have aside card', function() {
      const aside = findFirstByType('aside');
      expect(aside).to.exist;
    });

    it('should have gallery with multiple images', function() {
      const gallery = findFirstByType('gallery');
      expect(gallery).to.exist;
      expect(gallery.images).to.be.an('array');
      expect(gallery.images.length).to.be.at.least(2);
    });

    it('should parse gallery image dimensions', function() {
      const gallery = findFirstByType('gallery');
      const img = gallery.images[0];
      expect(img.src).to.be.a('string');
    });

    it('should have video card', function() {
      const video = findFirstByType('video');
      expect(video).to.exist;
      expect(video.src).to.be.a('string');
    });

    it('should have audio card', function() {
      const audio = findFirstByType('audio');
      expect(audio).to.exist;
      expect(audio.src).to.be.a('string');
    });

    it('should have file card', function() {
      const file = findFirstByType('file');
      expect(file).to.exist;
      expect(file.src).to.be.a('string');
      expect(file.fileName).to.be.a('string');
    });
  });

  // =========================================================================
  // Email Cards
  // =========================================================================

  describe('Email Cards', function() {
    it('should have email card', function() {
      const email = findFirstByType('email');
      expect(email).to.exist;
      expect(email.html).to.be.a('string');
    });

    it('should have email-cta card', function() {
      const emailCta = findFirstByType('email-cta');
      expect(emailCta).to.exist;
      expect(emailCta.buttonText).to.be.a('string');
      expect(emailCta.buttonUrl).to.be.a('string');
    });
  });

  // =========================================================================
  // Membership Cards
  // =========================================================================

  describe('Membership Cards', function() {
    it('should have signup card', function() {
      const signup = findFirstByType('signup');
      expect(signup).to.exist;
      expect(signup.buttonText).to.be.a('string');
    });

    it('should have product card', function() {
      const product = findFirstByType('product');
      expect(product).to.exist;
    });

    it('should have paywall card', function() {
      const paywall = findFirstByType('paywall');
      expect(paywall).to.exist;
    });
  });

  // =========================================================================
  // Integration Cards
  // =========================================================================

  describe('Integration Cards', function() {
    it('should have transistor card', function() {
      const transistor = findFirstByType('transistor');
      expect(transistor).to.exist;
      expect(transistor.episodeUrl).to.include('transistor.fm');
    });
  });

  // =========================================================================
  // Raw Content Cards
  // =========================================================================

  describe('Raw Content Cards', function() {
    it('should have html card', function() {
      const html = findByType('html').find(h => h.html && h.html.includes('<'));
      expect(html).to.exist;
    });

    it('should have markdown card', function() {
      const markdown = findFirstByType('markdown');
      expect(markdown).to.exist;
      expect(markdown.markdown).to.be.a('string');
    });
  });

  // =========================================================================
  // Structural Elements
  // =========================================================================

  describe('Structural Elements', function() {
    it('should have horizontal rule', function() {
      const hr = findFirstByType('horizontalrule');
      expect(hr).to.exist;
      expect(hr.version).to.equal(1);
    });

    it('should have table as HTML', function() {
      const table = findByType('html').find(h => h.html && h.html.includes('<table'));
      expect(table).to.exist;
    });
  });

  // =========================================================================
  // Linebreaks
  // =========================================================================

  describe('Linebreaks', function() {
    it('should have linebreak in paragraph', function() {
      for (const para of findByType('paragraph')) {
        for (const child of para.children || []) {
          if (child.type === 'linebreak') return;
        }
      }
      expect.fail('Expected linebreak node');
    });
  });

  // =========================================================================
  // Special Characters & Unicode
  // =========================================================================

  describe('Special Characters', function() {
    it('should preserve unicode', function() {
      const text = findTextInParagraph('日本語');
      expect(text).to.exist;
    });

    it('should preserve emoji', function() {
      const text = findTextInParagraph('🎉');
      expect(text).to.exist;
    });
  });

  // =========================================================================
  // JSON→HTML Rendering Tests
  // =========================================================================

  describe('JSON→HTML Rendering', function() {

    it('should render bold as <strong>', async function() {
      const doc = buildLexicalDoc([buildParagraph('bold', FORMAT.BOLD)]);
      const html = await render(doc);
      expect(html).to.include('<strong>bold</strong>');
    });

    it('should render italic as <em>', async function() {
      const doc = buildLexicalDoc([buildParagraph('italic', FORMAT.ITALIC)]);
      const html = await render(doc);
      expect(html).to.include('<em>italic</em>');
    });

    it('should render underline as <u>', async function() {
      const doc = buildLexicalDoc([buildParagraph('underline', FORMAT.UNDERLINE)]);
      const html = await render(doc);
      expect(html).to.include('<u>underline</u>');
    });

    it('should render strikethrough as <s>', async function() {
      const doc = buildLexicalDoc([buildParagraph('strike', FORMAT.STRIKETHROUGH)]);
      const html = await render(doc);
      expect(html).to.include('<s>strike</s>');
    });

    it('should render code as <code>', async function() {
      const doc = buildLexicalDoc([buildParagraph('code', FORMAT.CODE)]);
      const html = await render(doc);
      expect(html).to.include('<code>code</code>');
    });

    it('should render subscript as <sub>', async function() {
      const doc = buildLexicalDoc([buildParagraph('2', FORMAT.SUBSCRIPT)]);
      const html = await render(doc);
      expect(html).to.include('<sub>2</sub>');
    });

    it('should render superscript as <sup>', async function() {
      const doc = buildLexicalDoc([buildParagraph('2', FORMAT.SUPERSCRIPT)]);
      const html = await render(doc);
      expect(html).to.include('<sup>2</sup>');
    });

    it('should render highlight as <mark>', async function() {
      const doc = buildLexicalDoc([buildParagraph('highlight', FORMAT.HIGHLIGHT)]);
      const html = await render(doc);
      expect(html).to.include('<mark>highlight</mark>');
    });

    it('should render headings with id', async function() {
      const doc = buildLexicalDoc([buildHeading(2, 'Test Heading')]);
      const html = await render(doc);
      expect(html).to.include('<h2');
      expect(html).to.include('id=');
    });

    it('should render paragraph as <p>', async function() {
      const doc = buildLexicalDoc([buildParagraph('text')]);
      const html = await render(doc);
      expect(html).to.include('<p>');
      expect(html).to.include('</p>');
    });

    it('should render codeblock with language class', async function() {
      const doc = buildLexicalDoc([{
        type: 'codeblock',
        version: 1,
        language: 'javascript',
        code: 'const x = 1;'
      }]);
      const html = await render(doc);
      expect(html).to.include('language-javascript');
    });

    it('should render callout card', async function() {
      const doc = buildLexicalDoc([{
        type: 'callout',
        version: 1,
        calloutText: 'Test callout',
        calloutEmoji: '💡',
        backgroundColor: 'blue'
      }]);
      const html = await render(doc);
      expect(html).to.include('kg-callout');
    });

    it('should render toggle card', async function() {
      const doc = buildLexicalDoc([{
        type: 'toggle',
        version: 1,
        heading: 'Toggle Title',
        content: 'Toggle content'
      }]);
      const html = await render(doc);
      expect(html).to.include('kg-toggle');
    });

    it('should render button card', async function() {
      const doc = buildLexicalDoc([{
        type: 'button',
        version: 1,
        buttonText: 'Click me',
        buttonUrl: 'https://example.com',
        alignment: 'center'
      }]);
      const html = await render(doc);
      expect(html).to.include('kg-button');
    });

    it('should render horizontal rule', async function() {
      const doc = buildLexicalDoc([{
        type: 'horizontalrule',
        version: 1
      }]);
      const html = await render(doc);
      expect(html).to.include('<hr');
    });

    it('should render blockquote', async function() {
      const doc = buildLexicalDoc([{
        type: 'quote',
        version: 1,
        children: [{
          type: 'paragraph',
          version: 1,
          direction: 'ltr',
          children: [{ type: 'text', version: 1, text: 'Quote text' }]
        }]
      }]);
      const html = await render(doc);
      expect(html).to.include('<blockquote');
    });

    it('should render image', async function() {
      const doc = buildLexicalDoc([{
        type: 'image',
        version: 1,
        src: 'https://example.com/image.jpg',
        alt: 'Test image'
      }]);
      const html = await render(doc);
      expect(html).to.include('<img');
      expect(html).to.include('src="https://example.com/image.jpg"');
    });
  });

  // =========================================================================
  // Ghost E2E Preview
  // =========================================================================

  describe('Ghost E2E Preview', function() {
    const ghostUrl = process.env.GHOST_URL;
    const ghostKey = process.env.GHOST_ADMIN_KEY;

    if (!ghostUrl || !ghostKey) {
      it.skip('should push draft to Ghost (GHOST_URL/GHOST_ADMIN_KEY not set)', function() {});
      return;
    }

    let postId = null;

    it('should push draft to Ghost and return preview URL', async function() {
      const GhostAdminAPI = require('@tryghost/admin-api');
      const api = new GhostAdminAPI({
        url: ghostUrl,
        key: ghostKey,
        version: 'v5.0'
      });

      const post = await api.posts.add({
        title: `ox-ghost Test ${Date.now()}`,
        lexical: JSON.stringify(json),
        status: 'draft',
        tags: ['test', 'deleteme']
      });

      postId = post.id;
      const previewUrl = `${ghostUrl}/p/${post.uuid}/`;

      console.log(`Post created: ${previewUrl}`);
      console.log(`
  ┌─────────────────────────────────────────────────────────────
  │ 🚀 Ghost API Response: 201 Created (success)
  │
  │ Post ID:  ${post.id}
  │ UUID:     ${post.uuid}
  │ Slug:     ${post.slug}
  │ Status:   ${post.status}
  │
  │ Preview: ${previewUrl}
  │
  │ The draft will be auto-deleted after tests complete.
  └─────────────────────────────────────────────────────────────`);

      expect(post.id).to.exist;
      expect(post.uuid).to.exist;
    });

    after(async function() {
      if (postId) {
        const GhostAdminAPI = require('@tryghost/admin-api');
        const api = new GhostAdminAPI({
          url: ghostUrl,
          key: ghostKey,
          version: 'v5.0'
        });
        try {
          await api.posts.delete({ id: postId });
          console.log(`\n  🗑 Deleted test draft: ${postId}\n`);
        } catch (e) {
          console.log(`\n  ⚠ Could not delete draft: ${e.message}\n`);
        }
      }
    });
  });
});
