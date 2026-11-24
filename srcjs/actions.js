// The board's buttons and links that tell the server something, or act in
// the page. R writes what they do as data attributes (R/actions.R), and
// these listeners, on the whole page, do it: so they work for any element
// Shiny adds later.
//
//   data-tb-input="id" data-tb-value="JSON", on a button or a link: a click
//     sends the value as input `id`, as an event (the same value twice is
//     sent twice).
//   data-tb-input="id", on <input type="number">: a change sends its value,
//     rounded, if it's within the input's min and max.
//   data-tb-input, data-tb-value and data-tb-output="id", on <details>:
//     opened the first time, it gets a Shiny output `id`, bound, and sends
//     the value, for the server to fill it.
//   data-tb-more="n", on a button: shows the next n hidden <details> of the
//     list just before it, and goes when none is left.
//   data-copy="text", on a button: copies the text. A .tb-copy-code button
//     shows a check for a moment (index.css); another says "Copied".
//   data-tb-enter="id", on a text input: Enter clicks button `id`, as in a
//     browser's address bar. The input's text goes first: Shiny sends a
//     text input's value only after a pause in typing.
//   data-tb-suggest="id", on a text input: a list under it of the <option>s
//     of <datalist id>. Focused, it shows them all; typing filters them.
//     Arrow keys and Enter, or a click, pick one, which then goes as Enter
//     does. A browser's own datalist shows only the options matching the
//     text already there: none, with a whole path in the field.

const FEEDBACK_MS = 1500;

function send(id, value) {
  window.Shiny?.setInputValue?.(id, value, { priority: 'event' });
}

// Says how a copy went, for a moment.
function copied(button, ok) {
  clearTimeout(button._tbCopied);
  if (button.classList.contains('tb-copy-code')) {
    const state = ok ? 'tb-copied' : 'tb-copy-failed';
    button.classList.remove('tb-copied', 'tb-copy-failed');
    button.classList.add(state);
    button.title = ok ? 'Copied' : 'Copy failed';
    button._tbCopied = setTimeout(() => {
      button.classList.remove(state);
      button.title = button.dataset.copy;
    }, FEEDBACK_MS);
    return;
  }
  button.dataset.tbLabel ??= button.textContent;
  button.textContent = ok ? 'Copied' : 'Copy failed';
  button._tbCopied = setTimeout(() => {
    button.textContent = button.dataset.tbLabel;
  }, FEEDBACK_MS);
}

// The clipboard needs a secure page, which localhost is.
function copy(button) {
  if (!navigator.clipboard) {
    copied(button, false);
    return;
  }
  navigator.clipboard.writeText(button.dataset.copy).then(
    () => copied(button, true),
    () => copied(button, false)
  );
}

function showMore(button) {
  const list = button.previousElementSibling;
  const hidden = list.querySelectorAll(':scope > details[hidden]');
  const n = Number(button.dataset.tbMore);
  hidden.forEach((details, i) => {
    if (i < n) details.hidden = false;
  });
  if (hidden.length <= n) button.remove();
}

document.addEventListener('click', (event) => {
  const el = event.target.closest('[data-copy], [data-tb-more], [data-tb-input]');
  if (!el || el.matches('input, details')) return;
  if (el.matches('a')) event.preventDefault();
  if (el.dataset.copy !== undefined) copy(el);
  else if (el.dataset.tbMore !== undefined) showMore(el);
  else send(el.dataset.tbInput, JSON.parse(el.dataset.tbValue));
});

document.addEventListener('change', (event) => {
  const input = event.target;
  if (!input.matches?.('input[type="number"][data-tb-input]')) return;
  const value = Math.round(Number(input.value));
  if (value >= Number(input.min) && value <= Number(input.max)) send(input.dataset.tbInput, value);
});

// toggle doesn't bubble: heard on its way down instead.
document.addEventListener('toggle', (event) => {
  const details = event.target;
  if (!details.matches?.('details[data-tb-output]') || !details.open) return;
  if (details.querySelector('.shiny-html-output')) return;
  const output = document.createElement('div');
  output.id = details.dataset.tbOutput;
  output.className = 'shiny-html-output tb-preview-file-body';
  details.appendChild(output);
  window.Shiny?.bindAll?.(details);
  send(details.dataset.tbInput, JSON.parse(details.dataset.tbValue));
}, true);

// The suggestions: one list at a time, for the focused input.
function suggestions(input) {
  const list = document.getElementById(input.dataset.tbSuggest);
  return list ? Array.from(list.options, (o) => o.value) : [];
}

function closeSuggestions(input) {
  input._tbSuggest?.remove();
  input._tbSuggest = null;
  input.setAttribute('aria-expanded', 'false');
}

// `filter`: only those containing the typed text, case aside.
function showSuggestions(input, filter) {
  const text = filter ? input.value.toLowerCase() : '';
  const paths = suggestions(input).filter((p) => p.toLowerCase().includes(text));
  closeSuggestions(input);
  if (paths.length === 0) return;
  const menu = document.createElement('ul');
  menu.className = 'tb-suggestions list-unstyled';
  menu.setAttribute('role', 'listbox');
  paths.forEach((path) => {
    const item = document.createElement('li');
    item.className = 'tb-suggestion';
    item.setAttribute('role', 'option');
    item.textContent = path;
    item.title = path; // the whole path, when it is cut short
    item.dataset.value = path;
    if (path === input.value) item.classList.add('tb-current');
    menu.appendChild(item);
  });
  // Keep the focus in the input; the click picks.
  menu.addEventListener('mousedown', (event) => event.preventDefault());
  menu.addEventListener('click', (event) => {
    const item = event.target.closest('.tb-suggestion');
    if (item) pick(input, item.dataset.value);
  });
  input.closest('.input-group')?.appendChild(menu) ?? input.after(menu);
  input._tbSuggest = menu;
  input.setAttribute('aria-expanded', 'true');
}

function pick(input, value) {
  input.value = value;
  closeSuggestions(input);
  window.Shiny?.setInputValue?.(input.id, value);
  if (input.dataset.tbEnter) document.getElementById(input.dataset.tbEnter)?.click();
}

function moveActive(menu, step) {
  const items = Array.from(menu.children);
  const at = items.findIndex((i) => i.classList.contains('tb-active'));
  const next = at < 0 ? (step > 0 ? 0 : items.length - 1) : (at + step + items.length) % items.length;
  items[at]?.classList.remove('tb-active');
  items[next].classList.add('tb-active');
  items[next].scrollIntoView({ block: 'nearest' });
}

const SUGGEST = 'input[data-tb-suggest]';
document.addEventListener('focusin', (event) => {
  if (event.target.matches?.(SUGGEST)) showSuggestions(event.target, false);
});
document.addEventListener('focusout', (event) => {
  if (event.target.matches?.(SUGGEST)) closeSuggestions(event.target);
});
document.addEventListener('input', (event) => {
  if (event.target.matches?.(SUGGEST)) showSuggestions(event.target, true);
});
// A click on the focused input opens a list closed by Escape or a pick.
document.addEventListener('click', (event) => {
  const input = event.target;
  if (input.matches?.(SUGGEST) && !input._tbSuggest) showSuggestions(input, false);
});
// Before the Enter listener below: Enter on a highlighted suggestion picks it.
document.addEventListener('keydown', (event) => {
  const input = event.target;
  if (!input.matches?.(SUGGEST)) return;
  const menu = input._tbSuggest;
  if (event.key === 'ArrowDown' || event.key === 'ArrowUp') {
    event.preventDefault();
    if (!menu) showSuggestions(input, false);
    if (input._tbSuggest) moveActive(input._tbSuggest, event.key === 'ArrowDown' ? 1 : -1);
  } else if (event.key === 'Escape') {
    closeSuggestions(input);
  } else if (event.key === 'Enter') {
    const active = menu?.querySelector('.tb-active');
    if (active) input.value = active.dataset.value;
    closeSuggestions(input);
  }
});

document.addEventListener('keydown', (event) => {
  const input = event.target;
  if (event.key !== 'Enter' || !input.matches?.('input[data-tb-enter]')) return;
  event.preventDefault();
  window.Shiny?.setInputValue?.(input.id, input.value);
  document.getElementById(input.dataset.tbEnter)?.click();
});

// The theme picker. The pick is remembered in this browser and restored on
// load, unless the targetsboard.theme option chose the theme. A dark-only
// theme forces dark mode and hides the switch; leaving it gives back the mode
// the page had before.
const THEME_KEY = 'targetsboard.theme';

// The dark mode button (R/mod_chrome.R) and bslib's switch, hidden, that it
// flips: the switch sets the page's mode and sends it to the server.
function modeButton() {
  return document.querySelector('[data-tb-mode-toggle]');
}

function darkSwitch() {
  const button = modeButton();
  return button && document.getElementById(button.dataset.tbModeToggle);
}

document.addEventListener('click', (event) => {
  const button = event.target.closest?.('[data-tb-mode-toggle]');
  const dark = button && darkSwitch();
  if (!dark) return;
  dark.setAttribute('mode', dark.getAttribute('mode') === 'dark' ? 'light' : 'dark');
});

function themeMode(darkOnly) {
  const button = modeButton();
  const dark = darkSwitch();
  if (!dark) return;
  if (darkOnly) {
    if (!button.hidden) button.dataset.tbMode = dark.getAttribute('mode') || 'light';
    dark.setAttribute('mode', 'dark');
    button.hidden = true;
  } else if (button.hidden) {
    button.hidden = false;
    dark.setAttribute('mode', button.dataset.tbMode || 'light');
    delete button.dataset.tbMode;
  }
}

function storage(fn) {
  try { return fn(window.localStorage); } catch (e) { return null; }
}

document.addEventListener('change', (event) => {
  const picker = event.target;
  if (!picker.matches?.('select[data-tb-theme]')) return;
  storage((s) => s.setItem(THEME_KEY, picker.value));
});

// A theme set while the page is open (session$setCurrentTheme()) appends
// its CSS to <head>, after ours. Some of our rules win by coming after
// Bootstrap's (index.css: the body's font, --bslib-spacer, the main area's
// padding), so ours go last again, in order, as on load.
const THEME_CSS = /(bootstrap(\.min)?|components|shiny-sass|bslib-component-css(\.min)?)\.css/;

function oursLast() {
  const sheets = [...document.head.querySelectorAll('link[rel="stylesheet"], style')];
  sheets
    .filter((el) => !(el.href && (el.href.includes('restyle=') || THEME_CSS.test(el.href))))
    .forEach((el) => document.head.appendChild(el));
}

// Shiny removes a theme's old CSS once the new has loaded, but two picks
// close together can leave the older one: its rules (a preset's canvas)
// would still apply. Once a theme's file loads, any older one of the same
// dependency (its folder) goes.
function restyleStamp(link) {
  return Number(new URL(link.href).searchParams.get('restyle')) || 0;
}

function folder(link) {
  const path = new URL(link.href).pathname;
  return path.slice(0, path.lastIndexOf('/'));
}

function dropStale(link) {
  document.head.querySelectorAll('link[rel="stylesheet"]').forEach((el) => {
    if (el !== link && folder(el) === folder(link) && restyleStamp(el) < restyleStamp(link)) el.remove();
  });
}

new MutationObserver((records) => {
  const added = records.flatMap((r) => [...r.addedNodes]).filter(
    (n) => n.tagName === 'LINK' && n.href.includes('restyle=')
  );
  if (!added.length) return;
  added.forEach((link) => link.addEventListener('load', () => dropStale(link), { once: true }));
  oursLast();
}).observe(document.head, { childList: true });

window.jQuery(document).on('shiny:connected', () => {
  window.Shiny.addCustomMessageHandler('tb-theme', (message) => themeMode(message.dark_only));
  const picker = document.querySelector('select[data-tb-theme="remember"]');
  const saved = storage((s) => s.getItem(THEME_KEY));
  if (!picker || !saved || saved === picker.value) return;
  if (![...picker.options].some((o) => o.value === saved)) return;
  picker.value = saved;
  window.jQuery(picker).trigger('change');
});
