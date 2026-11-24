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

document.addEventListener('keydown', (event) => {
  const input = event.target;
  if (event.key !== 'Enter' || !input.matches?.('input[data-tb-enter]')) return;
  event.preventDefault();
  window.Shiny?.setInputValue?.(input.id, input.value);
  document.getElementById(input.dataset.tbEnter)?.click();
});
