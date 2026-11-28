import { describe, expect, it } from 'vitest';
import {
  rNames, placeNewNodes, upstreamOf, downstreamOf, lineageOf, neighboursOf,
} from './graph';

// a -> b -> c -> d, and x -> c
const edges = [
  { source: 'a', target: 'b' },
  { source: 'b', target: 'c' },
  { source: 'c', target: 'd' },
  { source: 'x', target: 'c' },
];
const sorted = (set) => [...set].sort();

describe('walking the edges', () => {
  it('finds every level upstream and downstream, with the targets themselves', () => {
    expect(sorted(upstreamOf(['c'], edges))).toEqual(['a', 'b', 'c', 'x']);
    expect(sorted(downstreamOf(['b'], edges))).toEqual(['b', 'c', 'd']);
    expect(sorted(lineageOf(['b'], edges))).toEqual(['a', 'b', 'c', 'd']);
  });

  it('takes several targets at once, and one with no edges', () => {
    expect(sorted(downstreamOf(['a', 'x'], edges))).toEqual(['a', 'b', 'c', 'd', 'x']);
    expect(sorted(upstreamOf(['lonely'], edges))).toEqual(['lonely']);
  });

  it('stops at a cycle', () => {
    const loop = [{ source: 'p', target: 'q' }, { source: 'q', target: 'p' }];
    expect(sorted(downstreamOf(['p'], loop))).toEqual(['p', 'q']);
  });

  it('neighbours are the direct inputs and outputs only', () => {
    expect(sorted(neighboursOf(['c'], edges))).toEqual(['b', 'c', 'd', 'x']);
  });
});

describe('rNames', () => {
  it('writes one name as a string and several as c()', () => {
    expect(rNames(['model'])).toBe('"model"');
    expect(rNames(['a', 'b'])).toBe('c("a", "b")');
    expect(rNames(['say "hi"'])).toBe('"say \\"hi\\""');
  });
});

describe('placeNewNodes', () => {
  const size = { width: 100, height: 40 };
  const node = (id, position) => ({ id, data: {}, measured: size, position });

  it('leaves placed targets where the author put them', () => {
    const nodes = [node('a', { x: 500, y: 500 }), node('b', { x: 0, y: 0 })];
    const placed = placeNewNodes(nodes, [{ source: 'a', target: 'b' }], new Set(['b']), 'LR');
    expect(placed[0].position).toEqual({ x: 500, y: 500 });
  });

  it('puts a new target beside its placed neighbour, as a full layout would', () => {
    const nodes = [node('a', { x: 500, y: 500 }), node('b', { x: 0, y: 0 })];
    const b = placeNewNodes(nodes, [{ source: 'a', target: 'b' }], new Set(['b']), 'LR')[1];
    expect(b.position.x).toBeGreaterThan(500); // downstream: to the right
    expect(b.position.y).toBe(500);
    expect(b.sourcePosition).toBe('right');
  });

  it('moves a new target off a spot another target has taken', () => {
    // c is where b would go: b moves across the flow until it's free.
    const nodes = [node('a', { x: 0, y: 0 }), node('c', { x: 160, y: 0 }), node('b', { x: 0, y: 0 })];
    const out = placeNewNodes(nodes, [{ source: 'a', target: 'b' }], new Set(['b']), 'LR');
    const b = out.find((n) => n.id === 'b');
    expect(b.position.x).toBe(160);
    expect(b.position.y).toBeGreaterThanOrEqual(40);
  });
});
