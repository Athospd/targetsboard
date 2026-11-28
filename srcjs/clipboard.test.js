import { afterEach, describe, expect, it, vi } from 'vitest';
import { copyText } from './clipboard';

// Just enough of a page for copyBySelection(): a textarea to select and
// document.execCommand('copy'), which says whether it copied.
const page = (copies) => {
  const area = { style: {}, setAttribute() {}, select() {}, remove: vi.fn() };
  vi.stubGlobal('document', {
    createElement: () => area,
    body: { appendChild() {} },
    activeElement: null,
    execCommand: vi.fn(() => {
      if (copies === 'throws') throw new Error('not allowed');
      return copies;
    }),
  });
  return area;
};

afterEach(() => vi.unstubAllGlobals());

describe('copyText', () => {
  it('uses the Clipboard API when there is one', async () => {
    const writeText = vi.fn(() => Promise.resolve());
    vi.stubGlobal('navigator', { clipboard: { writeText } });
    page(false);
    await expect(copyText('a')).resolves.toBeUndefined();
    expect(writeText).toHaveBeenCalledWith('a');
    expect(document.execCommand).not.toHaveBeenCalled();
  });

  it('falls back to selecting a textarea when the Clipboard API refuses', async () => {
    vi.stubGlobal('navigator', { clipboard: { writeText: () => Promise.reject(new Error('denied')) } });
    const area = page(true);
    await expect(copyText('b')).resolves.toBeUndefined();
    expect(area.value).toBe('b');
    expect(area.remove).toHaveBeenCalled(); // never left on the page
  });

  it('falls back when there is no Clipboard API, as in IDE viewers', async () => {
    vi.stubGlobal('navigator', {});
    page(true);
    await expect(copyText('c')).resolves.toBeUndefined();
  });

  it('rejects when neither route copies', async () => {
    vi.stubGlobal('navigator', {});
    page(false);
    await expect(copyText('d')).rejects.toThrow('Copy failed');
    page('throws');
    await expect(copyText('d')).rejects.toThrow('Copy failed');
  });
});
