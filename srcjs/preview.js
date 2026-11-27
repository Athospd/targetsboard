// The value preview's PDFs, in a browser without a PDF viewer: RStudio's and
// Positron's viewers are Electron views with Chromium's viewer turned off,
// where a PDF's <iframe> stays blank. There, the PDF's first pages take the
// frame's place, as images the server draws with pdftools (the frame's
// data-pdf-pages and data-pdf-page-url); without pdftools, a note and a link
// to open the PDF in a browser. The frame comes without a src, in
// data-pdf-src: loaded where it can't show, a PDF could start a download.

const SELECTOR = 'iframe.tb-preview-pdf:not([data-checked])';
const PAGES_SHOWN = 20;

// navigator.pdfViewerEnabled is false where there's no viewer; browsers
// older than it have one.
function hasPdfViewer() {
  return navigator.pdfViewerEnabled !== false;
}

function caption(text) {
  const p = document.createElement('p');
  p.className = 'tb-table-caption';
  p.textContent = text;
  return p;
}

function openLink(src, text) {
  const a = document.createElement('a');
  a.href = src;
  a.target = '_blank';
  a.rel = 'noopener';
  a.textContent = text;
  return a;
}

function pagesView(frame) {
  const pages = parseInt(frame.dataset.pdfPages, 10);
  const url = frame.dataset.pdfPageUrl;
  const view = document.createElement('div');
  view.className = 'tb-preview-pdf-pages';
  if (!url || !(pages > 0)) {
    const note = document.createElement('p');
    note.className = 'tb-preview-note';
    note.append(
      "This viewer can't show PDFs. Install the pdftools package to see its pages here, or ",
      openLink(frame.dataset.pdfSrc, 'open it in a browser'),
      '.'
    );
    view.append(note);
    return view;
  }
  const shown = Math.min(pages, PAGES_SHOWN);
  const text = pages > shown ? `The first ${shown} of ${pages} pages. ` : '';
  const top = caption(text);
  top.append(openLink(frame.dataset.pdfSrc, 'Open the PDF'));
  view.append(top);
  for (let page = 1; page <= shown; page += 1) {
    const img = document.createElement('img');
    img.className = 'tb-preview-pdf-page';
    img.loading = 'lazy';
    img.alt = `${frame.title}, page ${page}`;
    img.src = `${url}&page=${page}`;
    view.append(img);
  }
  return view;
}

function check(frame) {
  frame.dataset.checked = 'true';
  if (hasPdfViewer()) frame.src = frame.dataset.pdfSrc;
  else frame.replaceWith(pagesView(frame));
}

function checkIn(root) {
  if (root.matches?.(SELECTOR)) check(root);
  root.querySelectorAll?.(SELECTOR).forEach(check);
}

const observer = new MutationObserver((records) => {
  records.forEach((record) => record.addedNodes.forEach(checkIn));
});

function start() {
  checkIn(document.body);
  observer.observe(document.body, { childList: true, subtree: true });
}

if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
else start();

// Nested full-screen cards (#276): the Value pane's card sits in the board's,
// and bslib expects one at a time. Every expanded card leaves on any Escape,
// and the first to leave unmarks the page, though the other is still
// expanded. Escape leaves the innermost only; a card that leaves while
// another stays keeps the page marked.
const FULL = '.bslib-card[data-full-screen="true"]';

document.addEventListener('keydown', (event) => {
  if (event.key !== 'Escape') return;
  // As bslib: Escape there closes an open select first.
  if (event.target.matches?.("select[open], input[aria-expanded='true']")) return;
  const open = Array.from(document.querySelectorAll(FULL));
  if (open.length < 2) return;
  const inner = open.find((card) => !card.querySelector(FULL));
  const exit = inner?.previousElementSibling?.querySelector('.bslib-full-screen-exit');
  if (!exit) return;
  event.stopImmediatePropagation(); // before bslib's own listeners
  exit.click();
}, true);

document.addEventListener('bslib.card', (event) => {
  if (event.detail?.fullScreen === false && document.querySelector(FULL)) {
    document.body.classList.add('bslib-has-full-screen');
  }
});
