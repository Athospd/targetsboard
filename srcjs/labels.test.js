// Small pure helpers: families, action labels, target kinds.
import { describe, expect, it } from 'vitest';
import { familyOf, foldFamilies, familyBoxes } from './FamilyBoxes';
import { groupLabel, actionLabel } from './NodeActions';
import { targetKind } from './TypeIcon';

describe('families', () => {
  const nodes = [
    { id: 'fit_a', type: 'uptodate', data: { family: 'fit', label: 'fit_a' } },
    { id: 'fit_b', type: 'errored', data: { family: 'fit', label: 'fit_b' } },
    { id: 'data', type: 'uptodate', data: {} },
    { id: 'report', type: 'outdated', data: {} },
  ];
  const edges = [
    { id: 'e1', source: 'data', target: 'fit_a' },
    { id: 'e2', source: 'data', target: 'fit_b' },
    { id: 'e3', source: 'fit_a', target: 'report' },
    { id: 'e4', source: 'fit_b', target: 'report' },
  ];

  it('knows a target family by name only', () => {
    expect(familyOf(nodes[0])).toBe('fit');
    expect(familyOf(nodes[2])).toBeNull();
    expect(familyOf({ data: { family: 3 } })).toBeNull();
  });

  it('leaves the graph alone with nothing folded', () => {
    expect(foldFamilies(nodes, edges, new Set())).toEqual({ nodes, edges });
  });

  it('folds a family into its first target, with its worst status and one edge each way', () => {
    const out = foldFamilies(nodes, edges, new Set(['fit']));
    const stand = out.nodes.find((n) => n.id === 'fit_a');
    expect(stand.type).toBe('errored');
    expect(stand.data.label).toBe('fit (2)');
    expect(stand.data.folded).toBe(2);
    expect(out.nodes.find((n) => n.id === 'fit_b').hidden).toBe(true);
    expect(out.edges.map((e) => `${e.source}->${e.target}`).sort()).toEqual(['data->fit_a', 'fit_a->report']);
  });

  it("draws a family's box around its shown, measured targets", () => {
    const placed = [
      { id: 'a', data: { family: 'f' }, position: { x: 0, y: 0 }, measured: { width: 100, height: 40 } },
      { id: 'b', data: { family: 'f' }, position: { x: 0, y: 60 }, measured: { width: 100, height: 40 } },
      { id: 'c', data: { family: 'f' }, position: { x: 900, y: 900 }, measured: { width: 100, height: 40 }, hidden: true },
      { id: 'd', data: {}, position: { x: 0, y: 0 }, measured: { width: 100, height: 40 } },
    ];
    expect(familyBoxes(placed)).toEqual([{ family: 'f', x0: 0, y0: 0, x1: 100, y1: 100, folded: 0 }]);
  });
});

describe('action labels', () => {
  const run = { id: 'run', label: 'Run {n} targets' };
  it('counts the targets of a group', () => {
    expect(groupLabel('Run {n} targets, {n}!', 3)).toBe('Run 3 targets, 3!');
    expect(actionLabel(run, ['a', 'b'], new Set())).toBe('Run 2 targets');
    expect(actionLabel({ id: 'run', label: 'Run' }, ['a'], new Set())).toBe('Run');
  });
  it('says Unqueue when every target is already queued', () => {
    expect(actionLabel(run, ['a', 'b'], new Set(['a', 'b']))).toBe('Unqueue');
    expect(actionLabel(run, ['a', 'b'], new Set(['a']))).toBe('Run 2 targets');
    expect(actionLabel({ id: 'invalidate', label: 'Invalidate' }, ['a'], new Set(['a']))).toBe('Invalidate');
  });
});

describe('targetKind', () => {
  it('puts a report and batched reps before a pattern', () => {
    expect(targetKind('pattern', 'quarto')).toBe('quarto');
    expect(targetKind('pattern', 'rep')).toBe('rep');
    expect(targetKind('pattern', null)).toBe('pattern');
  });
  it('names files, formats, functions and objects; a plain target has none', () => {
    expect(targetKind('stem', 'file')).toBe('file');
    expect(targetKind('stem', 'format:qs')).toBe('format');
    expect(targetKind('function', undefined)).toBe('function');
    expect(targetKind('object', undefined)).toBe('object');
    expect(targetKind('stem', undefined)).toBeNull();
  });
});
