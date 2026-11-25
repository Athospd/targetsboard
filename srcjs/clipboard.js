// Copy text to the clipboard, wherever the board is shown.
//
// The Clipboard API needs a secure page and, inside an iframe, the
// clipboard-write permission: IDE viewers (Positron's, RStudio's) show the
// board in an iframe without it, and refuse. There, the older route still
// works during a click: copy the selection of a hidden textarea.
// Resolves when copied, rejects when neither route could.

function copyBySelection(text) {
  const area = document.createElement('textarea');
  area.value = text;
  area.setAttribute('readonly', '');
  // Off screen, and not scrolled to.
  area.style.position = 'fixed';
  area.style.top = '-1000px';
  area.style.opacity = '0';
  document.body.appendChild(area);
  const focused = document.activeElement;
  area.select();
  let ok = false;
  try {
    ok = document.execCommand('copy');
  } catch (e) {
    ok = false;
  }
  area.remove();
  focused?.focus?.({ preventScroll: true });
  return ok;
}

export function copyText(text) {
  const fallback = () => (copyBySelection(text) ? Promise.resolve() : Promise.reject(new Error('Copy failed')));
  if (!navigator.clipboard?.writeText) return fallback();
  return navigator.clipboard.writeText(text).catch(fallback);
}
