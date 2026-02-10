#!/usr/bin/env node
require('dotenv').config({ quiet: true });
const GhostAdminAPI = require('@tryghost/admin-api');
const fs = require('fs');
const path = require('path');
const https = require('https');
const http = require('http');
const { URL } = require('url');

// Configuration - set via environment or .env file
const config = {
  url: process.env.GHOST_URL,
  key: process.env.GHOST_ADMIN_KEY, // Format: {id}:{secret}
  staffToken: process.env.GHOST_STAFF_TOKEN, // Staff Access Token for settings (Admin permissions)
  version: 'v5.98',
  postsDir: process.env.GHOST_POSTS_DIR || './posts'
};

// Card helpers - Ghost converts these HTML patterns to rich cards
const cards = {
  image: (src, alt = '', caption = '') => `
<figure class="kg-card kg-image-card${caption ? ' kg-card-hascaption' : ''}">
  <img src="${src}" class="kg-image" alt="${alt}" loading="lazy">
  ${caption ? `<figcaption>${caption}</figcaption>` : ''}
</figure>`,

  gallery: (images) => `
<figure class="kg-card kg-gallery-card kg-width-wide">
  <div class="kg-gallery-container">
    <div class="kg-gallery-row">
      ${images.map(img => `
        <div class="kg-gallery-image">
          <img src="${img.src}" alt="${img.alt || ''}" loading="lazy">
        </div>
      `).join('')}
    </div>
  </div>
</figure>`,

  bookmark: (url, title, description, icon, thumbnail, author, publisher) => `
<figure class="kg-card kg-bookmark-card">
  <a class="kg-bookmark-container" href="${url}">
    <div class="kg-bookmark-content">
      <div class="kg-bookmark-title">${title}</div>
      <div class="kg-bookmark-description">${description}</div>
      <div class="kg-bookmark-metadata">
        ${icon ? `<img class="kg-bookmark-icon" src="${icon}" alt="">` : ''}
        ${publisher ? `<span class="kg-bookmark-publisher">${publisher}</span>` : ''}
        ${author ? `<span class="kg-bookmark-author">${author}</span>` : ''}
      </div>
    </div>
    ${thumbnail ? `<div class="kg-bookmark-thumbnail"><img src="${thumbnail}" alt=""></div>` : ''}
  </a>
</figure>`,

  button: (text, url, alignment = 'center') => `
<div class="kg-card kg-button-card kg-align-${alignment}">
  <a href="${url}" class="kg-btn kg-btn-accent">${text}</a>
</div>`,

  callout: (text, emoji = '💡', color = 'blue') => `
<div class="kg-card kg-callout-card kg-callout-card-${color}">
  <div class="kg-callout-emoji">${emoji}</div>
  <div class="kg-callout-text">${text}</div>
</div>`,

  toggle: (heading, content) => `
<div class="kg-card kg-toggle-card" data-kg-toggle-state="close">
  <div class="kg-toggle-heading">
    <h4 class="kg-toggle-heading-text">${heading}</h4>
    <button class="kg-toggle-card-icon">
      <svg viewBox="0 0 24 24"><path d="M12 4l8 8h-16z"/></svg>
    </button>
  </div>
  <div class="kg-toggle-content">${content}</div>
</div>`,

  divider: () => `<hr class="kg-divider">`,

  embed: (html) => `
<!--kg-card-begin: html-->
${html}
<!--kg-card-end: html-->`,

  html: (html) => `
<!--kg-card-begin: html-->
${html}
<!--kg-card-end: html-->`,

  code: (code, language = '') => `
<pre><code class="language-${language}">${escapeHtml(code)}</code></pre>`,

  file: (url, filename, caption = '', size = '') => `
<div class="kg-card kg-file-card">
  <a class="kg-file-card-container" href="${url}" download="${filename}">
    <div class="kg-file-card-contents">
      <div class="kg-file-card-title">${filename}</div>
      ${caption ? `<div class="kg-file-card-caption">${caption}</div>` : ''}
      ${size ? `<div class="kg-file-card-metadata"><span class="kg-file-card-filesize">${size}</span></div>` : ''}
    </div>
    <div class="kg-file-card-icon">
      <svg viewBox="0 0 24 24"><path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"/></svg>
    </div>
  </a>
</div>`,

  product: (title, description, imageUrl, buttonText, buttonUrl, rating = 0) => `
<div class="kg-card kg-product-card">
  ${imageUrl ? `<div class="kg-product-card-image"><img src="${imageUrl}" alt="${title}"></div>` : ''}
  <div class="kg-product-card-content">
    <h4 class="kg-product-card-title">${title}</h4>
    ${rating ? `<div class="kg-product-card-rating">${'★'.repeat(rating)}${'☆'.repeat(5-rating)}</div>` : ''}
    <p class="kg-product-card-description">${description}</p>
    ${buttonText ? `<a href="${buttonUrl}" class="kg-product-card-button kg-btn kg-btn-accent">${buttonText}</a>` : ''}
  </div>
</div>`,

  header: (title, subtitle, backgroundImage, buttonText, buttonUrl, size = 'small') => `
<div class="kg-card kg-header-card kg-size-${size}" style="${backgroundImage ? `background-image: url(${backgroundImage})` : ''}">
  <h2 class="kg-header-card-header">${title}</h2>
  ${subtitle ? `<h3 class="kg-header-card-subheader">${subtitle}</h3>` : ''}
  ${buttonText ? `<a href="${buttonUrl}" class="kg-header-card-button kg-btn kg-btn-accent">${buttonText}</a>` : ''}
</div>`,

  video: (url, caption = '') => `
<figure class="kg-card kg-video-card">
  <video src="${url}" controls></video>
  ${caption ? `<figcaption>${caption}</figcaption>` : ''}
</figure>`,

  audio: (url, title = '', caption = '') => `
<div class="kg-card kg-audio-card">
  ${title ? `<div class="kg-audio-title">${title}</div>` : ''}
  <audio src="${url}" controls></audio>
  ${caption ? `<div class="kg-audio-caption">${caption}</div>` : ''}
</div>`
};

function escapeHtml(text) {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#039;');
}

function getApi() {
  if (!config.key) {
    console.error('Error: GHOST_ADMIN_KEY environment variable required');
    console.error('Get it from Ghost Admin → Settings → Integrations → Add custom integration');
    process.exit(1);
  }
  return new GhostAdminAPI({
    url: config.url,
    key: config.key,
    version: config.version
  });
}

// Download a file from URL to local path
async function downloadFile(url, destPath) {
  return new Promise((resolve, reject) => {
    const parsedUrl = new URL(url);
    const protocol = parsedUrl.protocol === 'https:' ? https : http;

    const dir = path.dirname(destPath);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }

    const file = fs.createWriteStream(destPath);
    protocol.get(url, (response) => {
      // Handle redirects
      if (response.statusCode >= 300 && response.statusCode < 400 && response.headers.location) {
        file.close();
        fs.unlinkSync(destPath);
        downloadFile(response.headers.location, destPath).then(resolve).catch(reject);
        return;
      }

      if (response.statusCode !== 200) {
        file.close();
        fs.unlinkSync(destPath);
        reject(new Error(`Failed to download ${url}: ${response.statusCode}`));
        return;
      }

      response.pipe(file);
      file.on('finish', () => {
        file.close();
        resolve(destPath);
      });
    }).on('error', (err) => {
      file.close();
      if (fs.existsSync(destPath)) fs.unlinkSync(destPath);
      reject(err);
    });
  });
}

// Upload a file to Ghost (images, media, files)
async function uploadFile(filePath, type = 'auto') {
  const api = getApi();
  const ext = path.extname(filePath).toLowerCase();

  // Determine type if auto
  if (type === 'auto') {
    if (['.jpg', '.jpeg', '.png', '.gif', '.svg', '.webp'].includes(ext)) {
      type = 'image';
    } else if (['.mp4', '.webm', '.ogv', '.mp3', '.ogg', '.wav', '.m4a'].includes(ext)) {
      type = 'media';
    } else {
      type = 'file';
    }
  }

  switch (type) {
    case 'image':
      return api.images.upload({ file: filePath });
    case 'media':
      // Ghost Admin API media endpoint
      return api.media.upload({ file: filePath });
    case 'file':
      return api.files.upload({ file: filePath });
    default:
      throw new Error(`Unknown upload type: ${type}`);
  }
}

// Extract all image URLs from HTML content
function extractImageUrls(html, featureImage) {
  const urls = new Set();

  // Feature image
  if (featureImage) {
    urls.add(featureImage);
  }

  // Images in content
  const imgRegex = /<img[^>]+src=["']([^"']+)["']/gi;
  let match;
  while ((match = imgRegex.exec(html)) !== null) {
    urls.add(match[1]);
  }

  // Background images
  const bgRegex = /url\(["']?([^"')]+)["']?\)/gi;
  while ((match = bgRegex.exec(html)) !== null) {
    urls.add(match[1]);
  }

  // Bookmark thumbnails and icons
  const srcRegex = /src=["']([^"']+)["']/gi;
  while ((match = srcRegex.exec(html)) !== null) {
    if (match[1].match(/\.(jpg|jpeg|png|gif|webp|svg)(\?|$)/i)) {
      urls.add(match[1]);
    }
  }

  return Array.from(urls);
}

// Generate a safe filename from URL
function urlToFilename(url) {
  try {
    const parsed = new URL(url);
    const pathname = parsed.pathname;
    const filename = path.basename(pathname);
    // Clean up filename
    return filename.replace(/[^a-zA-Z0-9._-]/g, '_');
  } catch {
    return url.replace(/[^a-zA-Z0-9._-]/g, '_').slice(-100);
  }
}

// Create post directory name
function postDirName(post) {
  const date = post.published_at || post.created_at || new Date().toISOString();
  const dateStr = date.slice(0, 10);
  const slug = post.slug || 'untitled';
  return `${dateStr}-${slug}`;
}

// Export all posts to local directory
async function exportPosts(options = {}) {
  const api = getApi();
  const postsDir = options.postsDir || config.postsDir;
  const downloadImages = options.downloadImages !== false;

  console.log(`Exporting posts to ${postsDir}...`);

  // Create posts directory
  if (!fs.existsSync(postsDir)) {
    fs.mkdirSync(postsDir, { recursive: true });
  }

  // Fetch all posts (published and drafts)
  let allPosts = [];
  let page = 1;
  const limit = 15;

  while (true) {
    const posts = await api.posts.browse({
      limit,
      page,
      include: 'tags,authors',
      formats: 'html,mobiledoc,lexical',
      filter: 'status:[published,draft,scheduled]'
    });

    allPosts = allPosts.concat(posts);
    console.log(`Fetched page ${page}: ${posts.length} posts`);

    if (posts.length < limit) break;
    page++;
  }

  console.log(`Total posts: ${allPosts.length}`);

  // Also fetch pages
  let allPages = [];
  page = 1;

  while (true) {
    try {
      const pages = await api.pages.browse({
        limit,
        page,
        include: 'tags,authors',
        formats: 'html,mobiledoc,lexical',
        filter: 'status:[published,draft,scheduled]'
      });

      allPages = allPages.concat(pages);
      console.log(`Fetched page ${page}: ${pages.length} pages`);

      if (pages.length < limit) break;
      page++;
    } catch (e) {
      console.log('No pages found or pages API not available');
      break;
    }
  }

  console.log(`Total pages: ${allPages.length}`);

  // Export each post
  const manifest = {
    exported_at: new Date().toISOString(),
    source_url: config.url,
    posts: [],
    pages: []
  };

  for (const post of allPosts) {
    const dirName = postDirName(post);
    const postDir = path.join(postsDir, dirName);

    console.log(`Exporting: ${post.title} -> ${dirName}`);

    if (!fs.existsSync(postDir)) {
      fs.mkdirSync(postDir, { recursive: true });
    }

    // Extract and download images
    const imageUrls = extractImageUrls(post.html || '', post.feature_image);
    const imageMap = {};

    if (downloadImages && imageUrls.length > 0) {
      const imagesDir = path.join(postDir, 'images');
      if (!fs.existsSync(imagesDir)) {
        fs.mkdirSync(imagesDir, { recursive: true });
      }

      for (const url of imageUrls) {
        try {
          const filename = urlToFilename(url);
          const localPath = path.join(imagesDir, filename);

          // Skip if already downloaded
          if (!fs.existsSync(localPath)) {
            await downloadFile(url, localPath);
            console.log(`  Downloaded: ${filename}`);
          }

          imageMap[url] = `images/${filename}`;
        } catch (err) {
          console.log(`  Failed to download ${url}: ${err.message}`);
        }
      }
    }

    // Create local HTML with relative image paths
    let localHtml = post.html || '';
    for (const [url, localPath] of Object.entries(imageMap)) {
      localHtml = localHtml.split(url).join(localPath);
    }

    // Save post metadata
    const metadata = {
      id: post.id,
      title: post.title,
      slug: post.slug,
      status: post.status,
      visibility: post.visibility,
      created_at: post.created_at,
      updated_at: post.updated_at,
      published_at: post.published_at,
      feature_image: post.feature_image,
      feature_image_local: imageMap[post.feature_image] || null,
      excerpt: post.excerpt,
      custom_excerpt: post.custom_excerpt,
      meta_title: post.meta_title,
      meta_description: post.meta_description,
      tags: (post.tags || []).map(t => ({ name: t.name, slug: t.slug })),
      authors: (post.authors || []).map(a => ({ name: a.name, slug: a.slug, email: a.email })),
      image_map: imageMap
    };

    fs.writeFileSync(
      path.join(postDir, 'metadata.json'),
      JSON.stringify(metadata, null, 2)
    );

    // Save HTML content
    fs.writeFileSync(path.join(postDir, 'content.html'), post.html || '');

    // Save local HTML (with relative image paths)
    fs.writeFileSync(path.join(postDir, 'content-local.html'), localHtml);

    // Save mobiledoc/lexical if available
    if (post.mobiledoc) {
      fs.writeFileSync(path.join(postDir, 'mobiledoc.json'), post.mobiledoc);
    }
    if (post.lexical) {
      fs.writeFileSync(path.join(postDir, 'lexical.json'), post.lexical);
    }

    manifest.posts.push({
      dir: dirName,
      title: post.title,
      slug: post.slug,
      status: post.status,
      published_at: post.published_at
    });
  }

  // Export pages similarly
  for (const page of allPages) {
    const dirName = `page-${page.slug}`;
    const pageDir = path.join(postsDir, dirName);

    console.log(`Exporting page: ${page.title} -> ${dirName}`);

    if (!fs.existsSync(pageDir)) {
      fs.mkdirSync(pageDir, { recursive: true });
    }

    const imageUrls = extractImageUrls(page.html || '', page.feature_image);
    const imageMap = {};

    if (downloadImages && imageUrls.length > 0) {
      const imagesDir = path.join(pageDir, 'images');
      if (!fs.existsSync(imagesDir)) {
        fs.mkdirSync(imagesDir, { recursive: true });
      }

      for (const url of imageUrls) {
        try {
          const filename = urlToFilename(url);
          const localPath = path.join(imagesDir, filename);

          if (!fs.existsSync(localPath)) {
            await downloadFile(url, localPath);
            console.log(`  Downloaded: ${filename}`);
          }

          imageMap[url] = `images/${filename}`;
        } catch (err) {
          console.log(`  Failed to download ${url}: ${err.message}`);
        }
      }
    }

    let localHtml = page.html || '';
    for (const [url, localPath] of Object.entries(imageMap)) {
      localHtml = localHtml.split(url).join(localPath);
    }

    const metadata = {
      id: page.id,
      title: page.title,
      slug: page.slug,
      status: page.status,
      visibility: page.visibility,
      created_at: page.created_at,
      updated_at: page.updated_at,
      published_at: page.published_at,
      feature_image: page.feature_image,
      feature_image_local: imageMap[page.feature_image] || null,
      tags: (page.tags || []).map(t => ({ name: t.name, slug: t.slug })),
      authors: (page.authors || []).map(a => ({ name: a.name, slug: a.slug })),
      image_map: imageMap
    };

    fs.writeFileSync(path.join(pageDir, 'metadata.json'), JSON.stringify(metadata, null, 2));
    fs.writeFileSync(path.join(pageDir, 'content.html'), page.html || '');
    fs.writeFileSync(path.join(pageDir, 'content-local.html'), localHtml);

    if (page.mobiledoc) {
      fs.writeFileSync(path.join(pageDir, 'mobiledoc.json'), page.mobiledoc);
    }
    if (page.lexical) {
      fs.writeFileSync(path.join(pageDir, 'lexical.json'), page.lexical);
    }

    manifest.pages.push({
      dir: dirName,
      title: page.title,
      slug: page.slug,
      status: page.status
    });
  }

  // Save manifest
  fs.writeFileSync(
    path.join(postsDir, 'manifest.json'),
    JSON.stringify(manifest, null, 2)
  );

  // Generate index.html for local preview
  const indexHtml = generateIndexHtml(manifest);
  fs.writeFileSync(path.join(postsDir, 'index.html'), indexHtml);

  console.log(`\nExport complete!`);
  console.log(`  Posts: ${manifest.posts.length}`);
  console.log(`  Pages: ${manifest.pages.length}`);
  console.log(`  Location: ${path.resolve(postsDir)}`);
  console.log(`\nOpen ${postsDir}/index.html in a browser to preview.`);

  return manifest;
}

// Generate an index.html for local preview
function generateIndexHtml(manifest) {
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Ghost Archive - ${manifest.source_url}</title>
  <style>
    * { box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      max-width: 900px;
      margin: 0 auto;
      padding: 2rem;
      background: #f5f5f5;
    }
    h1 { color: #333; }
    .meta { color: #666; font-size: 0.9rem; margin-bottom: 2rem; }
    .section { margin-top: 2rem; }
    .section h2 { border-bottom: 2px solid #333; padding-bottom: 0.5rem; }
    .post-list { list-style: none; padding: 0; }
    .post-item {
      background: white;
      margin: 1rem 0;
      padding: 1rem;
      border-radius: 8px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    }
    .post-title { font-size: 1.2rem; margin: 0 0 0.5rem; }
    .post-title a { color: #333; text-decoration: none; }
    .post-title a:hover { color: #666; }
    .post-meta { color: #888; font-size: 0.85rem; }
    .status {
      display: inline-block;
      padding: 0.2rem 0.5rem;
      border-radius: 4px;
      font-size: 0.75rem;
      font-weight: bold;
      text-transform: uppercase;
    }
    .status-published { background: #d4edda; color: #155724; }
    .status-draft { background: #fff3cd; color: #856404; }
    .status-scheduled { background: #cce5ff; color: #004085; }
  </style>
</head>
<body>
  <h1>Ghost Archive</h1>
  <div class="meta">
    <p>Source: <a href="${manifest.source_url}">${manifest.source_url}</a></p>
    <p>Exported: ${new Date(manifest.exported_at).toLocaleString()}</p>
  </div>

  <div class="section">
    <h2>Posts (${manifest.posts.length})</h2>
    <ul class="post-list">
      ${manifest.posts.map(p => `
        <li class="post-item">
          <h3 class="post-title"><a href="${p.dir}/content-local.html">${p.title}</a></h3>
          <div class="post-meta">
            <span class="status status-${p.status}">${p.status}</span>
            ${p.published_at ? `<span>Published: ${new Date(p.published_at).toLocaleDateString()}</span>` : ''}
            <span>Slug: ${p.slug}</span>
          </div>
        </li>
      `).join('')}
    </ul>
  </div>

  ${manifest.pages.length > 0 ? `
  <div class="section">
    <h2>Pages (${manifest.pages.length})</h2>
    <ul class="post-list">
      ${manifest.pages.map(p => `
        <li class="post-item">
          <h3 class="post-title"><a href="${p.dir}/content-local.html">${p.title}</a></h3>
          <div class="post-meta">
            <span class="status status-${p.status}">${p.status}</span>
            <span>Slug: ${p.slug}</span>
          </div>
        </li>
      `).join('')}
    </ul>
  </div>
  ` : ''}
</body>
</html>`;
}

// Import a post from local directory to Ghost
async function importPost(postDir, options = {}) {
  const api = getApi();

  const metadataPath = path.join(postDir, 'metadata.json');
  const contentPath = path.join(postDir, 'content.html');

  if (!fs.existsSync(metadataPath)) {
    throw new Error(`No metadata.json found in ${postDir}`);
  }

  const metadata = JSON.parse(fs.readFileSync(metadataPath, 'utf8'));
  const html = fs.existsSync(contentPath) ? fs.readFileSync(contentPath, 'utf8') : '';

  const post = {
    title: metadata.title,
    slug: options.newSlug || metadata.slug,
    html: html,
    status: options.status || 'draft', // Always import as draft unless specified
    tags: metadata.tags?.map(t => t.name) || [],
    feature_image: metadata.feature_image,
    excerpt: metadata.custom_excerpt || metadata.excerpt,
    meta_title: metadata.meta_title,
    meta_description: metadata.meta_description
  };

  try {
    const result = await api.posts.add(post, { source: 'html' });
    console.log(`Imported: ${result.title} -> ${result.url}`);
    return result;
  } catch (error) {
    console.error(`Error importing ${metadata.title}:`, error.message);
    throw error;
  }
}

// Import all posts from manifest
async function importAll(postsDir, options = {}) {
  const manifestPath = path.join(postsDir, 'manifest.json');

  if (!fs.existsSync(manifestPath)) {
    throw new Error(`No manifest.json found in ${postsDir}`);
  }

  const manifest = JSON.parse(fs.readFileSync(manifestPath, 'utf8'));

  console.log(`Importing ${manifest.posts.length} posts...`);

  for (const post of manifest.posts) {
    const postDir = path.join(postsDir, post.dir);
    try {
      await importPost(postDir, options);
    } catch (err) {
      console.error(`Failed to import ${post.title}: ${err.message}`);
    }
  }

  console.log('Import complete!');
}

async function createPost(options) {
  const api = getApi();

  // Support both HTML and Lexical content
  const isLexical = !!options.lexical;
  const post = {
    title: options.title,
    status: options.status || 'draft',
    tags: options.tags || [],
    feature_image: options.featureImage,
    excerpt: options.excerpt,
    slug: options.slug
  };

  // Optional backdating, canonical URL, and feature image alt
  if (options.published_at) post.published_at = options.published_at;
  if (options.canonical_url) post.canonical_url = options.canonical_url;
  if (options.feature_image_alt) post.feature_image_alt = options.feature_image_alt;

  // Add content field based on source type
  if (isLexical) {
    post.lexical = options.lexical;
  } else {
    post.html = options.html;
  }

  try {
    // Ghost auto-detects source when lexical field is provided
    // Only specify source for html content
    const apiOptions = isLexical ? {} : { source: 'html' };
    const result = await api.posts.add(post, apiOptions);
    console.log('Post created:', result.url);
    return result;
  } catch (error) {
    console.error('Error creating post:', error.message);
    throw error;
  }
}

async function updatePost(id, options) {
  const api = getApi();

  try {
    const current = await api.posts.read({ id });

    const post = {
      id,
      updated_at: current.updated_at,
      ...options
    };

    // Ghost auto-detects source when lexical field is provided
    // Only specify source for html content
    const apiOptions = options.lexical ? {} : { source: 'html' };
    const result = await api.posts.edit(post, apiOptions);
    console.log('Post updated:', result.url);
    return result;
  } catch (error) {
    console.error('Error updating post:', error.message);
    throw error;
  }
}

async function createPage(options) {
  const api = getApi();

  const isLexical = !!options.lexical;
  const page = {
    title: options.title,
    status: options.status || 'draft',
    tags: options.tags || [],
    feature_image: options.featureImage,
    custom_excerpt: options.custom_excerpt || options.excerpt,
    slug: options.slug
  };

  if (isLexical) {
    page.lexical = options.lexical;
  } else {
    page.html = options.html;
  }

  try {
    // Ghost auto-detects source when lexical field is provided
    // Only specify source for html content
    const apiOptions = isLexical ? {} : { source: 'html' };
    const result = await api.pages.add(page, apiOptions);
    console.log('Page created:', result.url);
    return result;
  } catch (error) {
    console.error('Error creating page:', error.message);
    throw error;
  }
}

async function updatePage(id, options) {
  const api = getApi();

  try {
    const current = await api.pages.read({ id });

    const page = {
      id,
      updated_at: current.updated_at,
      ...options
    };

    const apiOptions = options.lexical ? {} : { source: 'html' };
    const result = await api.pages.edit(page, apiOptions);
    console.log('Page updated:', result.url);
    return result;
  } catch (error) {
    console.error('Error updating page:', error.message);
    throw error;
  }
}

async function listPosts(options = {}) {
  const api = getApi();

  try {
    const posts = await api.posts.browse({
      limit: options.limit || 'all',
      filter: options.filter || 'status:[published,draft,scheduled]',
      include: 'tags,authors'
    });
    return posts;
  } catch (error) {
    console.error('Error listing posts:', error.message);
    throw error;
  }
}

async function listPages(options = {}) {
  const api = getApi();

  try {
    const pages = await api.pages.browse({
      limit: options.limit || 'all',
      filter: options.filter || 'status:[published,draft,scheduled]',
      include: 'tags,authors'
    });
    return pages;
  } catch (error) {
    console.error('Error listing pages:', error.message);
    throw error;
  }
}

async function getPost(idOrSlug) {
  const api = getApi();

  try {
    const post = await api.posts.read({ id: idOrSlug }, { formats: 'html,mobiledoc,lexical' });
    return post;
  } catch {
    // Try by slug
    const post = await api.posts.read({ slug: idOrSlug }, { formats: 'html,mobiledoc,lexical' });
    return post;
  }
}

async function getPage(idOrSlug) {
  const api = getApi();

  try {
    const page = await api.pages.read({ id: idOrSlug }, { formats: 'html,mobiledoc,lexical' });
    return page;
  } catch {
    // Try by slug
    const page = await api.pages.read({ slug: idOrSlug }, { formats: 'html,mobiledoc,lexical' });
    return page;
  }
}

// Helper to pull content (used by both post and page commands)
async function pullContent(item, outputDir, isPage = false) {
  const slug = item.slug;
  const date = item.published_at ? item.published_at.slice(0, 10) : item.created_at.slice(0, 10);
  const dirName = isPage ? `page-${slug}` : `${date}-${slug}`;
  const itemDir = path.join(outputDir, dirName);

  if (!fs.existsSync(itemDir)) fs.mkdirSync(itemDir, { recursive: true });

  const metadata = {
    id: item.id,
    title: item.title,
    slug: item.slug,
    status: item.status,
    published_at: item.published_at,
    updated_at: item.updated_at,
    feature_image: item.feature_image,
    custom_excerpt: item.custom_excerpt,
    tags: item.tags?.map(t => t.name) || [],
    page: isPage
  };
  fs.writeFileSync(path.join(itemDir, 'metadata.json'), JSON.stringify(metadata, null, 2));
  fs.writeFileSync(path.join(itemDir, 'content.html'), item.html || '');

  if (item.lexical) {
    fs.writeFileSync(path.join(itemDir, 'lexical.json'), item.lexical);
  }
  if (item.mobiledoc) {
    fs.writeFileSync(path.join(itemDir, 'mobiledoc.json'), item.mobiledoc);
  }

  console.log(`Pulled to: ${itemDir}/`);
  console.log('  - metadata.json');
  console.log('  - content.html');
  if (item.lexical) console.log('  - lexical.json');
  if (item.mobiledoc) console.log('  - mobiledoc.json');
}

// Fetch tiers and newsletters config from Ghost Admin API
async function fetchConfig() {
  const crypto = require('crypto');
  const [id, secret] = config.key.split(':');

  // Generate JWT token
  const header = Buffer.from(JSON.stringify({alg: 'HS256', typ: 'JWT', kid: id})).toString('base64url');
  const now = Math.floor(Date.now() / 1000);
  const payload = Buffer.from(JSON.stringify({iat: now, exp: now + 300, aud: '/admin/'})).toString('base64url');
  const signature = crypto.createHmac('sha256', Buffer.from(secret, 'hex')).update(header + '.' + payload).digest('base64url');
  const token = header + '.' + payload + '.' + signature;

  const fetchEndpoint = (endpoint) => new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.get(`${config.url}/ghost/api/admin/${endpoint}`, {
      headers: { 'Authorization': 'Ghost ' + token }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try { resolve(JSON.parse(data)); }
        catch (e) { reject(new Error('Invalid JSON: ' + data.substring(0, 200))); }
      });
    });
    req.on('error', reject);
  });

  const [tiersData, newslettersData] = await Promise.all([
    fetchEndpoint('tiers/'),
    fetchEndpoint('newsletters/')
  ]);

  // Create tier-newsletter mapping based on name matching
  const tierNewsletterMap = {};
  const tiers = tiersData.tiers.map(t => ({
    id: t.id,
    name: t.name,
    slug: t.slug,
    visibility: t.visibility,
    type: t.type,
    active: t.active,
    description: t.description
  }));

  const newsletters = newslettersData.newsletters.map(n => ({
    id: n.id,
    name: n.name,
    slug: n.slug,
    visibility: n.visibility,
    subscribe_on_signup: n.subscribe_on_signup,
    description: n.description
  }));

  // Auto-map tiers to newsletters by similar name
  tiers.forEach(tier => {
    const matchingNewsletter = newsletters.find(n =>
      n.name.toLowerCase().includes(tier.name.toLowerCase()) ||
      tier.name.toLowerCase().includes(n.name.toLowerCase()) ||
      n.slug === tier.slug
    );
    if (matchingNewsletter) {
      tierNewsletterMap[tier.slug] = matchingNewsletter.slug;
    }
  });

  return {
    fetchedAt: new Date().toISOString(),
    tiers,
    newsletters,
    tierNewsletterMap,
    // Private tiers/newsletters that should be hidden from public signup
    hiddenTiers: tiers.filter(t => t.visibility === 'none' || !t.active).map(t => t.slug),
    hiddenNewsletters: newsletters.filter(n => !n.subscribe_on_signup).map(n => n.slug)
  };
}

async function saveConfig(outputPath = './ghost-config.json') {
  const configData = await fetchConfig();
  fs.writeFileSync(outputPath, JSON.stringify(configData, null, 2));
  console.log(`Config saved to: ${outputPath}`);
  console.log(`\nTiers (${configData.tiers.length}):`);
  configData.tiers.forEach(t => console.log(`  - ${t.name} (${t.slug}) [${t.type}, ${t.visibility}] active:${t.active}`));
  console.log(`\nNewsletters (${configData.newsletters.length}):`);
  configData.newsletters.forEach(n => console.log(`  - ${n.name} (${n.slug}) [signup: ${n.subscribe_on_signup}]`));
  console.log(`\nTier → Newsletter Mapping:`);
  Object.entries(configData.tierNewsletterMap).forEach(([tier, newsletter]) =>
    console.log(`  - ${tier} → ${newsletter}`)
  );
  console.log(`\nHidden from signup:`);
  console.log(`  Tiers: ${configData.hiddenTiers.join(', ') || 'none'}`);
  console.log(`  Newsletters: ${configData.hiddenNewsletters.join(', ') || 'none'}`);
  return configData;
}

// Archive (deactivate) a tier by ID or slug
async function archiveTier(idOrSlug) {
  const crypto = require('crypto');
  const [id, secret] = config.key.split(':');

  // Generate JWT token
  const header = Buffer.from(JSON.stringify({alg: 'HS256', typ: 'JWT', kid: id})).toString('base64url');
  const now = Math.floor(Date.now() / 1000);
  const payload = Buffer.from(JSON.stringify({iat: now, exp: now + 300, aud: '/admin/'})).toString('base64url');
  const signature = crypto.createHmac('sha256', Buffer.from(secret, 'hex')).update(header + '.' + payload).digest('base64url');
  const token = header + '.' + payload + '.' + signature;

  // First, get the tier to find its ID and current data
  const configData = await fetchConfig();
  const tier = configData.tiers.find(t => t.id === idOrSlug || t.slug === idOrSlug);
  if (!tier) {
    throw new Error(`Tier not found: ${idOrSlug}`);
  }

  if (!tier.active) {
    console.log(`Tier "${tier.name}" is already archived`);
    return tier;
  }

  const body = JSON.stringify({
    tiers: [{ id: tier.id, active: false }]
  });

  return new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.request(`${config.url}/ghost/api/admin/tiers/${tier.id}/`, {
      method: 'PUT',
      headers: {
        'Authorization': 'Ghost ' + token,
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(body)
      }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          const json = JSON.parse(data);
          if (json.errors) {
            reject(new Error(json.errors[0]?.message || 'API Error'));
          } else {
            console.log(`Archived tier: ${tier.name} (${tier.slug})`);
            resolve(json.tiers?.[0] || json);
          }
        } catch (e) {
          reject(new Error('Invalid JSON: ' + data.substring(0, 200)));
        }
      });
    });
    req.on('error', reject);
    req.write(body);
    req.end();
  });
}

// Unarchive (reactivate) a tier by ID or slug
async function unarchiveTier(idOrSlug) {
  const crypto = require('crypto');
  const [id, secret] = config.key.split(':');

  const header = Buffer.from(JSON.stringify({alg: 'HS256', typ: 'JWT', kid: id})).toString('base64url');
  const now = Math.floor(Date.now() / 1000);
  const payload = Buffer.from(JSON.stringify({iat: now, exp: now + 300, aud: '/admin/'})).toString('base64url');
  const signature = crypto.createHmac('sha256', Buffer.from(secret, 'hex')).update(header + '.' + payload).digest('base64url');
  const token = header + '.' + payload + '.' + signature;

  const configData = await fetchConfig();
  const tier = configData.tiers.find(t => t.id === idOrSlug || t.slug === idOrSlug);
  if (!tier) {
    throw new Error(`Tier not found: ${idOrSlug}`);
  }

  if (tier.active) {
    console.log(`Tier "${tier.name}" is already active`);
    return tier;
  }

  const body = JSON.stringify({
    tiers: [{ id: tier.id, active: true }]
  });

  return new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.request(`${config.url}/ghost/api/admin/tiers/${tier.id}/`, {
      method: 'PUT',
      headers: {
        'Authorization': 'Ghost ' + token,
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(body)
      }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          const json = JSON.parse(data);
          if (json.errors) {
            reject(new Error(json.errors[0]?.message || 'API Error'));
          } else {
            console.log(`Unarchived tier: ${tier.name} (${tier.slug})`);
            resolve(json.tiers?.[0] || json);
          }
        } catch (e) {
          reject(new Error('Invalid JSON: ' + data.substring(0, 200)));
        }
      });
    });
    req.on('error', reject);
    req.write(body);
    req.end();
  });
}

// ============================================================
// Staff Management (Roles & Invites)
// ============================================================

// List available roles
async function listRoles() {
  const api = getApi();
  return new Promise((resolve, reject) => {
    const token = generateJWT();
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.get(`${config.url}/ghost/api/admin/roles/`, {
      headers: { 'Authorization': 'Ghost ' + token }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          const json = JSON.parse(data);
          if (json.errors) {
            reject(new Error(json.errors[0]?.message || 'API Error'));
          } else {
            resolve(json.roles || []);
          }
        } catch (e) {
          reject(new Error('Invalid JSON: ' + data.substring(0, 200)));
        }
      });
    });
    req.on('error', reject);
  });
}

// List current staff/users
async function listStaff() {
  const token = generateJWT();
  return new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.get(`${config.url}/ghost/api/admin/users/?include=roles`, {
      headers: { 'Authorization': 'Ghost ' + token }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          const json = JSON.parse(data);
          if (json.errors) {
            reject(new Error(json.errors[0]?.message || 'API Error'));
          } else {
            resolve(json.users || []);
          }
        } catch (e) {
          reject(new Error('Invalid JSON: ' + data.substring(0, 200)));
        }
      });
    });
    req.on('error', reject);
  });
}

// List pending invites
async function listInvites() {
  const token = generateJWT();
  return new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.get(`${config.url}/ghost/api/admin/invites/`, {
      headers: { 'Authorization': 'Ghost ' + token }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          const json = JSON.parse(data);
          if (json.errors) {
            reject(new Error(json.errors[0]?.message || 'API Error'));
          } else {
            resolve(json.invites || []);
          }
        } catch (e) {
          reject(new Error('Invalid JSON: ' + data.substring(0, 200)));
        }
      });
    });
    req.on('error', reject);
  });
}

// Send an invite to a new staff member
async function inviteUser(email, roleName) {
  // First, get role ID from name
  const roles = await listRoles();
  const role = roles.find(r => r.name.toLowerCase() === roleName.toLowerCase());
  if (!role) {
    const available = roles.map(r => r.name).join(', ');
    throw new Error(`Role "${roleName}" not found. Available: ${available}`);
  }

  const token = generateJWT();
  const body = JSON.stringify({
    invites: [{
      email: email,
      role_id: role.id
    }]
  });

  return new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.request(`${config.url}/ghost/api/admin/invites/`, {
      method: 'POST',
      headers: {
        'Authorization': 'Ghost ' + token,
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(body)
      }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          const json = JSON.parse(data);
          if (json.errors) {
            reject(new Error(json.errors[0]?.message || 'API Error'));
          } else {
            console.log(`✓ Invite sent to ${email} as ${role.name}`);
            resolve(json.invites?.[0] || json);
          }
        } catch (e) {
          reject(new Error('Invalid JSON: ' + data.substring(0, 200)));
        }
      });
    });
    req.on('error', reject);
    req.write(body);
    req.end();
  });
}

// Revoke a pending invite
async function revokeInvite(inviteId) {
  const token = generateJWT();
  return new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.request(`${config.url}/ghost/api/admin/invites/${inviteId}/`, {
      method: 'DELETE',
      headers: { 'Authorization': 'Ghost ' + token }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        if (res.statusCode === 204) {
          console.log(`✓ Invite revoked`);
          resolve(true);
        } else {
          try {
            const json = JSON.parse(data);
            reject(new Error(json.errors?.[0]?.message || 'Failed to revoke invite'));
          } catch (e) {
            reject(new Error(`HTTP ${res.statusCode}`));
          }
        }
      });
    });
    req.on('error', reject);
    req.end();
  });
}

// Generate JWT token for Ghost Admin API
// Prefers Staff Token (full permissions) over Admin API Key (limited on Ghost Pro)
function generateJWT(keyOverride) {
  const crypto = require('crypto');
  const key = keyOverride || config.staffToken || config.key;
  if (!key) {
    throw new Error('No API key available. Set GHOST_STAFF_TOKEN or GHOST_ADMIN_KEY');
  }
  const [id, secret] = key.split(':');
  const header = Buffer.from(JSON.stringify({alg: 'HS256', typ: 'JWT', kid: id})).toString('base64url');
  const now = Math.floor(Date.now() / 1000);
  const payload = Buffer.from(JSON.stringify({iat: now, exp: now + 300, aud: '/admin/'})).toString('base64url');
  const signature = crypto.createHmac('sha256', Buffer.from(secret, 'hex')).update(header + '.' + payload).digest('base64url');
  return header + '.' + payload + '.' + signature;
}

// Get code injection settings from Ghost Admin API
async function getCodeInjection() {
  const token = generateJWT();

  return new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.get(`${config.url}/ghost/api/admin/settings/`, {
      headers: { 'Authorization': 'Ghost ' + token }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          const json = JSON.parse(data);
          const settings = json.settings || [];
          const head = settings.find(s => s.key === 'codeinjection_head')?.value || '';
          const foot = settings.find(s => s.key === 'codeinjection_foot')?.value || '';
          resolve({ head, foot });
        } catch (e) {
          reject(new Error('Invalid JSON: ' + data.substring(0, 200)));
        }
      });
    });
    req.on('error', reject);
  });
}

// Set code injection settings via Ghost Admin API
// NOTE: On Ghost Pro, Admin API keys cannot write settings. Use Staff Access Token instead.
async function setCodeInjection(head, foot) {
  if (!config.staffToken && !config.key) {
    throw new Error('GHOST_STAFF_TOKEN required for setting code injection (Admin API keys lack permission on Ghost Pro)');
  }
  // Prefer staff token for write operations
  const token = generateJWT(config.staffToken || config.key);

  const body = JSON.stringify({
    settings: [
      ...(head !== undefined ? [{ key: 'codeinjection_head', value: head }] : []),
      ...(foot !== undefined ? [{ key: 'codeinjection_foot', value: foot }] : [])
    ]
  });

  return new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.request(`${config.url}/ghost/api/admin/settings/`, {
      method: 'PUT',
      headers: {
        'Authorization': 'Ghost ' + token,
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(body)
      }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          const json = JSON.parse(data);
          if (json.errors) {
            reject(new Error(json.errors[0]?.message || 'API Error'));
          } else {
            resolve(json);
          }
        } catch (e) {
          reject(new Error('Invalid JSON: ' + data.substring(0, 200)));
        }
      });
    });
    req.on('error', reject);
    req.write(body);
    req.end();
  });
}

// Update site footer code injection from a file
async function updateFooterFromFile(filePath) {
  const content = fs.readFileSync(filePath, 'utf8');
  console.log(`Reading footer code from: ${filePath}`);
  console.log(`Content length: ${content.length} characters`);
  await setCodeInjection(undefined, content);
  console.log('Footer code injection updated successfully!');
}

// Get routes.yaml from Ghost
async function getRoutes() {
  const token = generateJWT();
  return new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.get(`${config.url}/ghost/api/admin/settings/routes/yaml/`, {
      headers: { 'Authorization': 'Ghost ' + token }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => resolve(data));
    });
    req.on('error', reject);
  });
}

// Get redirects from Ghost
async function getRedirects() {
  const token = generateJWT();
  return new Promise((resolve, reject) => {
    const url = new URL(config.url);
    const reqModule = url.protocol === 'https:' ? https : http;
    const req = reqModule.get(`${config.url}/ghost/api/admin/redirects/download/`, {
      headers: { 'Authorization': 'Ghost ' + token }
    }, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        try {
          resolve(JSON.parse(data));
        } catch {
          resolve(data); // Return raw if not JSON
        }
      });
    });
    req.on('error', reject);
  });
}

// Sync all Ghost config to local directory for git tracking
async function syncAll(outputDir = '.') {
  console.log(`Syncing Ghost config to: ${outputDir}/`);

  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
  }

  const results = { success: [], failed: [] };

  // 1. Routes
  try {
    const routes = await getRoutes();
    fs.writeFileSync(path.join(outputDir, 'routes.yaml'), routes);
    results.success.push('routes.yaml');
  } catch (err) {
    results.failed.push({ file: 'routes.yaml', error: err.message });
  }

  // 2. Redirects
  try {
    const redirects = await getRedirects();
    fs.writeFileSync(path.join(outputDir, 'redirects.json'), JSON.stringify(redirects, null, 2));
    results.success.push('redirects.json');
  } catch (err) {
    results.failed.push({ file: 'redirects.json', error: err.message });
  }

  // 3. Code injection
  try {
    const { head, foot } = await getCodeInjection();
    if (head) {
      fs.writeFileSync(path.join(outputDir, 'site-header.html'), head);
      results.success.push('site-header.html');
    } else {
      results.success.push('site-header.html (empty)');
    }
    if (foot) {
      fs.writeFileSync(path.join(outputDir, 'site-footer.html'), foot);
      results.success.push('site-footer.html');
    } else {
      results.success.push('site-footer.html (empty)');
    }
  } catch (err) {
    results.failed.push({ file: 'code-injection', error: err.message });
  }

  // 4. Tiers & Newsletters config
  try {
    const configData = await fetchConfig();
    fs.writeFileSync(path.join(outputDir, 'ghost-config.json'), JSON.stringify(configData, null, 2));
    results.success.push('ghost-config.json');
  } catch (err) {
    results.failed.push({ file: 'ghost-config.json', error: err.message });
  }

  // 5. Site info
  try {
    const api = getApi();
    const site = await api.site.read();
    fs.writeFileSync(path.join(outputDir, 'site.json'), JSON.stringify(site, null, 2));
    results.success.push('site.json');
  } catch (err) {
    results.failed.push({ file: 'site.json', error: err.message });
  }

  // 6. All settings (raw)
  try {
    const token = generateJWT();
    const settings = await new Promise((resolve, reject) => {
      const url = new URL(config.url);
      const reqModule = url.protocol === 'https:' ? https : http;
      reqModule.get(`${config.url}/ghost/api/admin/settings/`, {
        headers: { 'Authorization': 'Ghost ' + token }
      }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => {
          try { resolve(JSON.parse(data)); }
          catch (e) { reject(new Error('Invalid JSON')); }
        });
      }).on('error', reject);
    });
    fs.writeFileSync(path.join(outputDir, 'settings.json'), JSON.stringify(settings, null, 2));
    results.success.push('settings.json');
  } catch (err) {
    results.failed.push({ file: 'settings.json', error: err.message });
  }

  // Summary
  console.log('\nSynced:');
  results.success.forEach(f => console.log(`  ✓ ${f}`));
  if (results.failed.length > 0) {
    console.log('\nFailed:');
    results.failed.forEach(f => console.log(`  ✗ ${f.file}: ${f.error}`));
  }

  // Write manifest
  const manifest = {
    synced_at: new Date().toISOString(),
    source: config.url,
    files: results.success,
    failed: results.failed
  };
  fs.writeFileSync(path.join(outputDir, '.ghost-sync.json'), JSON.stringify(manifest, null, 2));

  console.log(`\nManifest: ${outputDir}/.ghost-sync.json`);
  return results;
}

// CLI handlers for posts
function handlePostCommand(subCommand, args) {
  switch (subCommand) {
    case 'list':
      listPosts({ limit: args[0] || 20 }).then(posts => {
        posts.forEach(p => console.log(`${p.status.padEnd(9)} | ${p.slug.padEnd(40)} | ${p.title}`));
      });
      break;

    case 'get':
      if (!args[0]) {
        console.log('Usage: ghost.js post get <id-or-slug>');
        process.exit(1);
      }
      getPost(args[0]).then(post => {
        console.log(JSON.stringify(post, null, 2));
      });
      break;

    case 'create':
      if (args.length < 2) {
        console.log('Usage: ghost.js post create "Title" content.html [options]');
        console.log('       ghost.js post create "Title" content.json [options]  (Lexical format)');
        console.log('Options: --publish --slug --tags --published-at --canonical-url --feature-image --feature-image-alt');
        process.exit(1);
      }
      {
        const title = args[0];
        const contentFile = args[1];
        const publish = args.includes('--publish');
        const pubAtIdx = args.indexOf('--published-at');
        const publishedAt = pubAtIdx > -1 ? args[pubAtIdx + 1] : undefined;
        const canonIdx = args.indexOf('--canonical-url');
        const canonicalUrl = canonIdx > -1 ? args[canonIdx + 1] : undefined;
        const slugIdx = args.indexOf('--slug');
        const slug = slugIdx > -1 ? args[slugIdx + 1] : undefined;
        const tagsIdx = args.indexOf('--tags');
        const tags = tagsIdx > -1 ? args[tagsIdx + 1].split(',').map(t => t.trim()) : undefined;
        const featureIdx = args.indexOf('--feature-image');
        const featureImage = featureIdx > -1 ? args[featureIdx + 1] : undefined;
        const featureAltIdx = args.indexOf('--feature-image-alt');
        const featureImageAlt = featureAltIdx > -1 ? args[featureAltIdx + 1] : undefined;
        const content = fs.readFileSync(contentFile, 'utf8');
        const isLexicalFile = contentFile.endsWith('.json');
        const opts = {
          title,
          status: publish ? 'published' : 'draft',
          published_at: publishedAt,
          canonical_url: canonicalUrl,
          slug,
          tags,
          featureImage,
        };
        if (featureImageAlt) opts.feature_image_alt = featureImageAlt;
        if (isLexicalFile) {
          opts.lexical = content;
          console.log('Creating post with Lexical content...');
        } else {
          opts.html = content;
        }
        createPost(opts);
      }
      break;

    case 'update':
      if (args.length < 2) {
        console.log('Usage: ghost.js post update <id-or-slug> lexical.json [--title "New Title"]');
        console.log('       ghost.js post update <id-or-slug> content.html [--title "New Title"]');
        process.exit(1);
      }
      {
        const updateSlug = args[0];
        const updateFile = args[1];
        const titleIdx = args.indexOf('--title');
        const newTitle = titleIdx > -1 ? args[titleIdx + 1] : undefined;
        const featureIdx = args.indexOf('--feature-image');
        const featureImage = featureIdx > -1 ? args[featureIdx + 1] : undefined;
        const featureAltIdx = args.indexOf('--feature-image-alt');
        const featureImageAlt = featureAltIdx > -1 ? args[featureAltIdx + 1] : undefined;
        const pubAtIdx = args.indexOf('--published-at');
        const publishedAt = pubAtIdx > -1 ? args[pubAtIdx + 1] : undefined;
        const canonIdx = args.indexOf('--canonical-url');
        const canonicalUrl = canonIdx > -1 ? args[canonIdx + 1] : undefined;
        const statusIdx = args.indexOf('--status');
        const status = statusIdx > -1 ? args[statusIdx + 1] : undefined;
        getPost(updateSlug).then(post => {
          const content = fs.readFileSync(updateFile, 'utf8');
          const isLexical = updateFile.endsWith('.json');
          const updateOpts = isLexical ? { lexical: content } : { html: content };
          if (newTitle) updateOpts.title = newTitle;
          if (featureImage) updateOpts.feature_image = featureImage;
          if (featureImageAlt) updateOpts.feature_image_alt = featureImageAlt;
          if (publishedAt) updateOpts.published_at = publishedAt;
          if (canonicalUrl) updateOpts.canonical_url = canonicalUrl;
          if (status) updateOpts.status = status;
          console.log(`Updating post with ${isLexical ? 'Lexical' : 'HTML'} content...`);
          return updatePost(post.id, updateOpts);
        }).catch(err => {
          console.error('Error:', err.message);
          process.exit(1);
        });
      }
      break;

    case 'pull':
      if (!args[0]) {
        console.log('Usage: ghost.js post pull <id-or-slug> [output-dir]');
        process.exit(1);
      }
      {
        const pullSlug = args[0];
        const pullDir = args[1] || '.';
        getPost(pullSlug).then(post => pullContent(post, pullDir, false)).catch(err => {
          console.error('Error:', err.message);
          process.exit(1);
        });
      }
      break;

    default:
      console.log(`Ghost Post Commands

Usage:
  ghost.js post list [limit]                   List posts (default: 20)
  ghost.js post get <id-or-slug>               Get post details as JSON
  ghost.js post create "Title" file.html       Create post from HTML file
  ghost.js post create "Title" file.json       Create post from Lexical JSON
  ghost.js post update <slug> lexical.json     Update post with Lexical JSON
  ghost.js post update <slug> content.html     Update post with HTML
  ghost.js post pull <slug> [dir]              Pull post to local directory

Options:
  --publish                                    Publish immediately (create)
  --title "New Title"                          Change title (update)
  --slug "my-slug"                             Set URL slug (create)
  --tags "tag1,tag2"                           Set tags (create)
  --published-at "2017-01-04T00:00:00.000Z"   Backdate post (create/update)
  --canonical-url "https://..."                Set canonical URL (create/update)
  --feature-image "https://..."                Set feature image (create/update)
  --feature-image-alt "Description"            Set feature image alt text (create/update)
  --status "draft|published"                   Set post status (update)
`);
  }
}

// CLI handlers for pages
function handlePageCommand(subCommand, args) {
  switch (subCommand) {
    case 'list':
      listPages({ limit: args[0] || 20 }).then(pages => {
        pages.forEach(p => console.log(`${p.status.padEnd(9)} | ${p.slug.padEnd(40)} | ${p.title}`));
      });
      break;

    case 'get':
      if (!args[0]) {
        console.log('Usage: ghost.js page get <id-or-slug>');
        process.exit(1);
      }
      getPage(args[0]).then(page => {
        console.log(JSON.stringify(page, null, 2));
      });
      break;

    case 'create':
      if (args.length < 2) {
        console.log('Usage: ghost.js page create "Title" content.html [--publish] [--with-metadata]');
        console.log('       ghost.js page create "Title" content.json [--publish] [--with-metadata]');
        process.exit(1);
      }
      {
        const title = args[0];
        const contentFile = args[1];
        const publish = args.includes('--publish');
        const withMetadata = args.includes('--with-metadata');
        const content = fs.readFileSync(contentFile, 'utf8');
        const isLexicalFile = contentFile.endsWith('.json');
        const opts = { title, status: publish ? 'published' : 'draft' };
        if (isLexicalFile) {
          opts.lexical = content;
        } else {
          opts.html = content;
        }
        // Read companion metadata file if --with-metadata
        if (withMetadata) {
          const metaPath = contentFile.replace(/\.json$/, '-metadata.json').replace(/\.html$/, '-metadata.json');
          if (fs.existsSync(metaPath)) {
            const metadata = JSON.parse(fs.readFileSync(metaPath, 'utf8'));
            if (metadata.custom_excerpt) opts.custom_excerpt = metadata.custom_excerpt;
            if (metadata.tags) opts.tags = metadata.tags.map(t => typeof t === 'string' ? { name: t } : t);
            if (metadata.slug) opts.slug = metadata.slug;
            if (metadata.feature_image) opts.featureImage = metadata.feature_image;
            console.log(`Creating page with Lexical content + metadata from ${path.basename(metaPath)}...`);
          } else {
            console.log(`Creating page with ${isLexicalFile ? 'Lexical' : 'HTML'} content (no metadata file found)...`);
          }
        } else {
          console.log(`Creating page with ${isLexicalFile ? 'Lexical' : 'HTML'} content...`);
        }
        createPage(opts);
      }
      break;

    case 'update':
      if (args.length < 2) {
        console.log('Usage: ghost.js page update <id-or-slug> lexical.json [--title "New Title"] [--with-metadata]');
        console.log('       ghost.js page update <id-or-slug> content.html [--title "New Title"] [--with-metadata]');
        process.exit(1);
      }
      {
        const updateSlug = args[0];
        const updateFile = args[1];
        const titleIdx = args.indexOf('--title');
        const newTitle = titleIdx > -1 ? args[titleIdx + 1] : undefined;
        const withMetadata = args.includes('--with-metadata');
        getPage(updateSlug).then(page => {
          const content = fs.readFileSync(updateFile, 'utf8');
          const isLexical = updateFile.endsWith('.json');
          const updateOpts = isLexical ? { lexical: content } : { html: content };
          if (newTitle) updateOpts.title = newTitle;
          // Read companion metadata file if --with-metadata
          if (withMetadata) {
            const metaPath = updateFile.replace(/\.json$/, '-metadata.json').replace(/\.html$/, '-metadata.json');
            if (fs.existsSync(metaPath)) {
              const metadata = JSON.parse(fs.readFileSync(metaPath, 'utf8'));
              if (metadata.custom_excerpt) updateOpts.custom_excerpt = metadata.custom_excerpt;
              if (metadata.tags) updateOpts.tags = metadata.tags.map(t => typeof t === 'string' ? { name: t } : t);
              if (metadata.slug) updateOpts.slug = metadata.slug;
              if (metadata.feature_image) updateOpts.feature_image = metadata.feature_image;
            }
          }
          console.log(`Updating page with ${isLexical ? 'Lexical' : 'HTML'} content...`);
          return updatePage(page.id, updateOpts);
        }).catch(err => {
          console.error('Error:', err.message);
          process.exit(1);
        });
      }
      break;

    case 'pull':
      if (!args[0]) {
        console.log('Usage: ghost.js page pull <id-or-slug> [output-dir]');
        process.exit(1);
      }
      {
        const pullSlug = args[0];
        const pullDir = args[1] || '.';
        getPage(pullSlug).then(page => pullContent(page, pullDir, true)).catch(err => {
          console.error('Error:', err.message);
          process.exit(1);
        });
      }
      break;

    default:
      console.log(`Ghost Page Commands

Usage:
  ghost.js page list [limit]                   List pages (default: 20)
  ghost.js page get <id-or-slug>               Get page details as JSON
  ghost.js page create "Title" file.html       Create page from HTML file
  ghost.js page create "Title" file.json       Create page from Lexical JSON
  ghost.js page update <slug> lexical.json     Update page with Lexical JSON
  ghost.js page update <slug> content.html     Update page with HTML
  ghost.js page pull <slug> [dir]              Pull page to local directory

Options:
  --publish                                    Publish immediately (create)
  --title "New Title"                          Change title (update)
`);
  }
}

// CLI interface
if (require.main === module) {
  const args = process.argv.slice(2);
  const command = args[0];
  const subCommand = args[1];
  const restArgs = args.slice(2);

  switch (command) {
    case 'post':
      handlePostCommand(subCommand, restArgs);
      break;

    case 'page':
      handlePageCommand(subCommand, restArgs);
      break;

    case 'export':
      {
        const exportDir = args[1] || config.postsDir;
        const noImages = args.includes('--no-images');
        exportPosts({ postsDir: exportDir, downloadImages: !noImages });
      }
      break;

    case 'import':
      if (!args[1]) {
        console.log('Usage: ghost.js import <post-dir> [--publish]');
        console.log('       ghost.js import --all <posts-dir> [--publish]');
        process.exit(1);
      }
      if (args[1] === '--all') {
        const importDir = args[2] || config.postsDir;
        const importPublish = args.includes('--publish');
        importAll(importDir, { status: importPublish ? 'published' : 'draft' });
      } else {
        const postDir = args[1];
        const importPublish = args.includes('--publish');
        importPost(postDir, { status: importPublish ? 'published' : 'draft' });
      }
      break;

    case 'upload':
      {
        const filePath = args[1];
        if (!filePath) {
          console.log(`Upload files to Ghost

Usage: ghost.js upload <file>

Examples:
  ghost.js upload photo.jpg       # Upload image
  ghost.js upload video.mp4       # Upload video
  ghost.js upload audio.mp3       # Upload audio
  ghost.js upload document.pdf    # Upload file

Auto-detects file type and uses the appropriate Ghost API.
Returns the Ghost-hosted URL.`);
          process.exit(1);
        }
        if (!fs.existsSync(filePath)) {
          console.error(`File not found: ${filePath}`);
          process.exit(1);
        }
        uploadFile(filePath).then(result => {
          console.log(`Uploaded: ${result.url}`);
        }).catch(err => {
          console.error('Upload failed:', err.message);
          process.exit(1);
        });
      }
      break;

    case 'cards':
      console.log('Available card helpers:');
      Object.keys(cards).forEach(name => console.log(`  - ${name}`));
      break;

    case 'config':
      {
        const outputPath = args[1] || './ghost-config.json';
        saveConfig(outputPath).catch(err => {
          console.error('Error fetching config:', err.message);
          process.exit(1);
        });
      }
      break;

    case 'inject':
    case 'code':
      {
        const subCmd = args[1];
        if (subCmd === 'get') {
          getCodeInjection().then(({ head, foot }) => {
            console.log('=== Site Header ===');
            console.log(head || '(empty)');
            console.log('\n=== Site Footer ===');
            console.log(foot || '(empty)');
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'set-footer') {
          const filePath = args[2];
          if (!filePath) {
            console.log('Usage: ghost.js inject set-footer <file-path>');
            process.exit(1);
          }
          updateFooterFromFile(filePath).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'set-header') {
          const filePath = args[2];
          if (!filePath) {
            console.log('Usage: ghost.js inject set-header <file-path>');
            process.exit(1);
          }
          const content = fs.readFileSync(filePath, 'utf8');
          console.log(`Reading header code from: ${filePath}`);
          setCodeInjection(content, undefined).then(() => {
            console.log('Header code injection updated successfully!');
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else {
          console.log(`Code Injection Commands:
  ghost.js inject get                    Show current header/footer code
  ghost.js inject set-header <file>      Update site header from file
  ghost.js inject set-footer <file>      Update site footer from file`);
        }
      }
      break;

    case 'tier':
      {
        const subCmd = args[1];
        if (subCmd === 'list') {
          fetchConfig().then(configData => {
            console.log('Tiers:');
            configData.tiers.forEach(t => {
              const status = t.active ? 'active' : 'ARCHIVED';
              console.log(`  ${status.padEnd(8)} | ${t.slug.padEnd(20)} | ${t.name} [${t.type}]`);
            });
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'archive') {
          const tierSlug = args[2];
          if (!tierSlug) {
            console.log('Usage: ghost.js tier archive <slug>');
            process.exit(1);
          }
          archiveTier(tierSlug).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'unarchive') {
          const tierSlug = args[2];
          if (!tierSlug) {
            console.log('Usage: ghost.js tier unarchive <slug>');
            process.exit(1);
          }
          unarchiveTier(tierSlug).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'archive-all') {
          fetchConfig().then(async configData => {
            const paidTiers = configData.tiers.filter(t => t.type === 'paid' && t.active);
            console.log(`Archiving ${paidTiers.length} active paid tiers...`);
            for (const tier of paidTiers) {
              await archiveTier(tier.slug);
            }
            console.log('Done!');
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else {
          console.log(`Tier Commands:
  ghost.js tier list                     List all tiers with status
  ghost.js tier archive <slug>           Archive (deactivate) a tier
  ghost.js tier unarchive <slug>         Unarchive (reactivate) a tier
  ghost.js tier archive-all              Archive all active paid tiers`);
        }
      }
      break;

    case 'staff':
    case 'users':
      {
        const subCmd = args[1];
        if (subCmd === 'list' || !subCmd) {
          listStaff().then(users => {
            console.log('Staff Members:');
            users.forEach(u => {
              const roles = u.roles?.map(r => r.name).join(', ') || 'No role';
              const status = u.status || 'active';
              console.log(`  ${status.padEnd(8)} | ${u.email.padEnd(30)} | ${roles} | ${u.name || '(no name)'}`);
            });
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'roles') {
          listRoles().then(roles => {
            console.log('Available Roles:');
            roles.forEach(r => {
              console.log(`  ${r.name.padEnd(15)} | ${r.description || ''}`);
            });
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'invites') {
          listInvites().then(invites => {
            if (invites.length === 0) {
              console.log('No pending invites');
            } else {
              console.log('Pending Invites:');
              invites.forEach(inv => {
                console.log(`  ${inv.id} | ${inv.email.padEnd(30)} | ${inv.role_id} | expires: ${inv.expires}`);
              });
            }
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'invite') {
          const email = args[2];
          const role = args[3];
          if (!email || !role) {
            console.log('Usage: ghost.js staff invite <email> <role>');
            console.log('Roles: Contributor, Author, Editor, Administrator');
            process.exit(1);
          }
          inviteUser(email, role).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'revoke') {
          const inviteId = args[2];
          if (!inviteId) {
            console.log('Usage: ghost.js staff revoke <invite-id>');
            console.log('Use "ghost.js staff invites" to see pending invite IDs');
            process.exit(1);
          }
          revokeInvite(inviteId).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else {
          console.log(`Staff Commands:
  ghost.js staff                         List all staff members
  ghost.js staff list                    List all staff members
  ghost.js staff roles                   List available roles
  ghost.js staff invites                 List pending invites
  ghost.js staff invite <email> <role>   Send invite (Contributor, Author, Editor, Administrator)
  ghost.js staff revoke <invite-id>      Revoke pending invite`);
        }
      }
      break;

    case 'routes':
      {
        const subCmd = args[1];
        if (subCmd === 'get' || !subCmd) {
          getRoutes().then(routes => {
            console.log(routes);
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'pull') {
          const outFile = args[2] || 'routes.yaml';
          getRoutes().then(routes => {
            fs.writeFileSync(outFile, routes);
            console.log(`Routes saved to: ${outFile}`);
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else {
          console.log(`Routes Commands:
  ghost.js routes                         Show current routes.yaml
  ghost.js routes get                     Show current routes.yaml
  ghost.js routes pull [file]             Save to file (default: routes.yaml)

Note: Uploading routes requires browser/session auth (not supported via API token)`);
        }
      }
      break;

    case 'redirects':
      {
        const subCmd = args[1];
        if (subCmd === 'get' || !subCmd) {
          getRedirects().then(redirects => {
            console.log(JSON.stringify(redirects, null, 2));
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else if (subCmd === 'pull') {
          const outFile = args[2] || 'redirects.json';
          getRedirects().then(redirects => {
            fs.writeFileSync(outFile, JSON.stringify(redirects, null, 2));
            console.log(`Redirects saved to: ${outFile}`);
          }).catch(err => {
            console.error('Error:', err.message);
            process.exit(1);
          });
        } else {
          console.log(`Redirects Commands:
  ghost.js redirects                      Show current redirects
  ghost.js redirects get                  Show current redirects
  ghost.js redirects pull [file]          Save to file (default: redirects.json)

Note: Uploading redirects requires browser/session auth (not supported via API token)`);
        }
      }
      break;

    case 'sync':
      {
        const outputDir = args[1] || '.';
        syncAll(outputDir).catch(err => {
          console.error('Sync failed:', err.message);
          process.exit(1);
        });
      }
      break;

    default:
      console.log(`Ghost CLI

Usage:
  ghost.js sync [dir]                          Pull ALL config for git tracking
  ghost.js post <command>                      Manage posts
  ghost.js page <command>                      Manage pages
  ghost.js staff <command>                     Manage staff & invites
  ghost.js tier <command>                      Manage tiers (archive/unarchive)
  ghost.js routes [command]                    View/pull routes.yaml (read-only)
  ghost.js redirects [command]                 View/pull redirects (read-only)
  ghost.js export [dir]                        Export all posts and pages
  ghost.js import <dir>                        Import content from directory
  ghost.js config [output]                     Download tiers & newsletters config
  ghost.js inject <command>                    Manage code injection (header/footer)
  ghost.js upload <file>                       Upload image/video/audio/file to Ghost
  ghost.js cards                               List available card helpers

Post Commands:
  ghost.js post list [limit]                   List posts
  ghost.js post get <slug>                     Get post as JSON
  ghost.js post create "Title" file            Create new post
  ghost.js post update <slug> file             Update existing post
  ghost.js post pull <slug> [dir]              Pull post to local directory

Page Commands:
  ghost.js page list [limit]                   List pages
  ghost.js page get <slug>                     Get page as JSON
  ghost.js page create "Title" file            Create new page
  ghost.js page update <slug> file             Update existing page
  ghost.js page pull <slug> [dir]              Pull page to local directory

Staff Commands:
  ghost.js staff                               List all staff members
  ghost.js staff roles                         List available roles
  ghost.js staff invites                       List pending invites
  ghost.js staff invite <email> <role>         Send invite (Contributor, Author, Editor, Administrator)
  ghost.js staff revoke <invite-id>            Revoke pending invite

Round-trip Workflow:
  1. Pull:   ./ghost.js post pull my-slug ./drafts
  2. Edit:   Edit lexical.json in your editor
  3. Push:   ./ghost.js post update my-slug ./drafts/.../lexical.json

  Same workflow for pages:
  1. Pull:   ./ghost.js page pull about ./drafts
  2. Edit:   Edit lexical.json
  3. Push:   ./ghost.js page update about ./drafts/page-about/lexical.json

Environment:
  GHOST_URL         Ghost site URL (required, e.g., https://your-site.ghost.io)
  GHOST_ADMIN_KEY   Admin API key (required) - format: {id}:{secret}
  GHOST_STAFF_TOKEN Staff Access Token for settings (required on Ghost Pro)
                    Get from: Ghost Admin → Settings → Staff → Your Profile
  GHOST_POSTS_DIR   Default posts directory (default: ./posts)
`);
  }
}

module.exports = {
  createPost, updatePost, listPosts, getPost,
  createPage, updatePage, listPages, getPage,
  exportPosts, importPost, importAll,
  uploadFile,
  fetchConfig, saveConfig,
  archiveTier, unarchiveTier,
  listRoles, listStaff, listInvites, inviteUser, revokeInvite,
  getCodeInjection, setCodeInjection, updateFooterFromFile,
  getRoutes, getRedirects, syncAll,
  cards, config
};
