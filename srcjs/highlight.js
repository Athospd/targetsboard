// Colours R code in the board: a <pre class="language-r"> anywhere on the
// page, whenever it appears (Shiny replaces outputs as they update). Only
// highlight.js's core and its R grammar are bundled; the colours are in
// index.css, from the theme, so they follow dark mode.
import hljs from 'highlight.js/lib/core';
import r from 'highlight.js/lib/languages/r';

hljs.registerLanguage('r', r);
hljs.configure({ ignoreUnescapedHTML: true });

const SELECTOR = 'pre.language-r:not([data-highlighted])';

function highlightIn(root) {
  if (root.matches?.(SELECTOR)) hljs.highlightElement(root);
  root.querySelectorAll?.(SELECTOR).forEach((el) => hljs.highlightElement(el));
}

let pending = false;
const observer = new MutationObserver(() => {
  if (pending) return;
  pending = true;
  requestAnimationFrame(() => {
    pending = false;
    highlightIn(document.body);
  });
});

function start() {
  highlightIn(document.body);
  observer.observe(document.body, { childList: true, subtree: true });
}

if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
else start();

window.targetsboardHighlight = hljs;
