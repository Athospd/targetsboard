import { describe, expect, it } from 'vitest';
import { layoutGraph, SIBLING_GAP } from './layout';

const size = { width: 100, height: 40 };
const node = (id, data = {}) => ({ id, data, measured: size });
const at = (out, id) => out.nodes.find((n) => n.id === id).position;

describe('layoutGraph', () => {
  // a -> c, b -> c
  const nodes = [node('a'), node('b'), node('c')];
  const edges = [{ source: 'a', target: 'c' }, { source: 'b', target: 'c' }];

  it('puts each target in the column of its level, left to right', () => {
    const out = layoutGraph(nodes, edges);
    expect(at(out, 'a').x).toBe(at(out, 'b').x);
    expect(at(out, 'c').x).toBeGreaterThan(at(out, 'a').x + size.width);
    expect(out.nodes[0].sourcePosition).toBe('right');
  });

  it('stacks a column with the sibling gap, centred on the axis', () => {
    const out = layoutGraph(nodes, edges);
    const [top, bottom] = [at(out, 'a').y, at(out, 'b').y].sort((p, q) => p - q);
    expect(bottom - top).toBe(size.height + SIBLING_GAP);
    expect(top + bottom + size.height).toBe(0); // centred
    expect(at(out, 'c').y).toBe(-size.height / 2);
  });

  it('runs top to bottom with TB', () => {
    const out = layoutGraph(nodes, edges, { direction: 'TB' });
    expect(at(out, 'c').y).toBeGreaterThan(at(out, 'a').y);
    expect(at(out, 'a').y).toBe(at(out, 'b').y);
    expect(out.nodes[0].targetPosition).toBe('top');
  });

  it("uses targets' own levels when every target has one", () => {
    const out = layoutGraph([node('a', { level: 3 }), node('b', { level: 1 })], []);
    expect(at(out, 'a').x).toBeGreaterThan(at(out, 'b').x);
  });

  it('gives a target not yet measured the size of one that is', () => {
    const out = layoutGraph([node('a'), { id: 'b', data: {} }], [{ source: 'a', target: 'b' }]);
    expect(at(out, 'b').y).toBe(-size.height / 2);
  });

  it('copes with nothing', () => {
    expect(layoutGraph(undefined, undefined)).toEqual({ nodes: [], edges: [] });
  });
});
