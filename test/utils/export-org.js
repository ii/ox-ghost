/**
 * export-org.js - Export org content to Lexical JSON via Emacs batch
 *
 * Provides utilities to export org-mode content to Ghost's Lexical JSON format
 * by invoking the ox-ghost export backend via Emacs in batch mode.
 */

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');
const os = require('os');

const OX_GHOST_DIR = path.resolve(__dirname, '..', '..');
const EXPORT_SCRIPT = path.join(OX_GHOST_DIR, 'ox-ghost-export.sh');

/**
 * Export org content string to Lexical JSON
 * @param {string} orgContent - Org-mode content to export
 * @param {Object} options - Export options
 * @param {boolean} options.keepTempFiles - Keep temp files for debugging
 * @returns {Object} Parsed Lexical JSON object
 */
function exportOrg(orgContent, options = {}) {
  const tmpDir = os.tmpdir();
  const timestamp = Date.now();
  const orgFile = path.join(tmpDir, `ox-ghost-test-${timestamp}.org`);
  const jsonFile = path.join(tmpDir, `ox-ghost-test-${timestamp}.json`);

  try {
    // Write org content to temp file
    fs.writeFileSync(orgFile, orgContent, 'utf-8');

    // Run export script
    execSync(`"${EXPORT_SCRIPT}" "${orgFile}" "${jsonFile}"`, {
      stdio: 'pipe',
      timeout: 30000,
      encoding: 'utf-8'
    });

    // Read and parse JSON output
    const jsonContent = fs.readFileSync(jsonFile, 'utf-8');
    return JSON.parse(jsonContent);
  } finally {
    // Cleanup temp files unless debugging
    if (!options.keepTempFiles) {
      try { fs.unlinkSync(orgFile); } catch (e) { /* ignore */ }
      try { fs.unlinkSync(jsonFile); } catch (e) { /* ignore */ }
    }
  }
}

/**
 * Export org file to Lexical JSON
 * @param {string} orgFilePath - Path to org file
 * @returns {Object} Parsed Lexical JSON object
 */
function exportOrgFile(orgFilePath) {
  const absolutePath = path.resolve(orgFilePath);
  const tmpDir = os.tmpdir();
  const timestamp = Date.now();
  const jsonFile = path.join(tmpDir, `ox-ghost-test-${timestamp}.json`);

  try {
    execSync(`"${EXPORT_SCRIPT}" "${absolutePath}" "${jsonFile}"`, {
      stdio: 'pipe',
      timeout: 30000,
      encoding: 'utf-8'
    });

    const jsonContent = fs.readFileSync(jsonFile, 'utf-8');
    return JSON.parse(jsonContent);
  } finally {
    try { fs.unlinkSync(jsonFile); } catch (e) { /* ignore */ }
  }
}

/**
 * Get root children from exported org content
 * @param {string} orgContent - Org-mode content
 * @returns {Array} Array of root-level nodes
 */
function getRootChildren(orgContent) {
  const json = exportOrg(orgContent);
  return json.root?.children || [];
}

/**
 * Get first node of a specific type from exported content
 * @param {string} orgContent - Org-mode content
 * @param {string} nodeType - Node type to find
 * @returns {Object|null} First matching node or null
 */
function getFirstNodeOfType(orgContent, nodeType) {
  const children = getRootChildren(orgContent);
  return children.find(n => n.type === nodeType) || null;
}

/**
 * Get all nodes of a specific type from exported content
 * @param {string} orgContent - Org-mode content
 * @param {string} nodeType - Node type to find
 * @returns {Array} Array of matching nodes
 */
function getAllNodesOfType(orgContent, nodeType) {
  const children = getRootChildren(orgContent);
  return children.filter(n => n.type === nodeType);
}

module.exports = {
  exportOrg,
  exportOrgFile,
  getRootChildren,
  getFirstNodeOfType,
  getAllNodesOfType,
  OX_GHOST_DIR,
  EXPORT_SCRIPT
};
