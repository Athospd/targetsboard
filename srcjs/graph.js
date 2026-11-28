// The graph's logic, apart from React: target names as R code, placing new
// targets, and walking the edges. Tested in srcjs/graph.test.js.

import { layoutGraph, SIBLING_GAP } from './layout';

// Target names as R code: "model", or c("a", "b") for several.
export const rNames = (ids) => {
  const quoted = ids.map((id) => JSON.stringify(id));
  return quoted.length === 1 ? quoted[0] : `c(${quoted.join(', ')})`;
};

export const handlePositions = (direction) => ({
  sourcePosition: direction === 'LR' ? 'right' : 'bottom',
  targetPosition: direction === 'LR' ? 'left' : 'top',
});

// Place only the targets in `newIds` and leave the others where they are, so
// the author's picture of the graph survives edits to the pipeline. A new
// target goes where a full layout would put it relative to a neighbour that is
// already placed; without one, where the full layout would put it, shifted
// the way the placed targets are. A spot another target has taken (the
// author moved it there) moves the new one across the flow until it's free.
export const placeNewNodes = (nodes, edges, newIds, direction) => {
  const ideal = new Map(
    layoutGraph(nodes, edges, { direction }).nodes.map((n) => [n.id, n.position]),
  );
  const placed = nodes.filter((n) => !newIds.has(n.id));
  const byId = new Map(placed.map((n) => [n.id, n]));

  const shift = { x: 0, y: 0 };
  placed.forEach((n) => {
    shift.x += (n.position.x - ideal.get(n.id).x) / placed.length;
    shift.y += (n.position.y - ideal.get(n.id).y) / placed.length;
  });

  const anchors = new Map();
  edges.forEach((e) => {
    if (newIds.has(e.target) && byId.has(e.source) && !anchors.has(e.target))
      anchors.set(e.target, e.source);
    if (newIds.has(e.source) && byId.has(e.target) && !anchors.has(e.source))
      anchors.set(e.source, e.target);
  });

  const size = (n) => ({ w: n.measured?.width ?? 0, h: n.measured?.height ?? 0 });
  const taken = placed.map((n) => ({ ...n.position, ...size(n) }));
  const free = (box) =>
    !taken.some(
      (t) => box.x < t.x + t.w && t.x < box.x + box.w && box.y < t.y + t.h && t.y < box.y + box.h,
    );

  return nodes.map((node) => {
    if (!newIds.has(node.id)) return node;
    const own = ideal.get(node.id);
    const anchor = byId.get(anchors.get(node.id));
    const position = anchor
      ? {
          x: anchor.position.x + own.x - ideal.get(anchor.id).x,
          y: anchor.position.y + own.y - ideal.get(anchor.id).y,
        }
      : { x: own.x + shift.x, y: own.y + shift.y };
    const box = { ...position, ...size(node) };
    // Bounded: a spot is free past the targets, at most.
    for (let i = 0; i <= 2 * taken.length && !free(box); i++) {
      if (direction === 'LR') box.y += box.h + SIBLING_GAP;
      else box.x += box.w + SIBLING_GAP;
    }
    taken.push(box);
    return { ...node, position: { x: box.x, y: box.y }, ...handlePositions(direction) };
  });
};

// Targets `ids` with every target reached from them along the edges, from
// `from` to `to`: all levels, not only the direct neighbours.
export const reachable = (ids, edges, from, to) => {
  const next = new Map();
  edges.forEach((e) => {
    if (!next.has(e[from])) next.set(e[from], []);
    next.get(e[from]).push(e[to]);
  });
  const seen = new Set(ids);
  const stack = [...ids];
  while (stack.length) {
    (next.get(stack.pop()) ?? []).forEach((id) => {
      if (!seen.has(id)) {
        seen.add(id);
        stack.push(id);
      }
    });
  }
  return seen;
};
export const upstreamOf = (ids, edges) => reachable(ids, edges, 'target', 'source');
export const downstreamOf = (ids, edges) => reachable(ids, edges, 'source', 'target');
// Targets with everything upstream and downstream of them.
export const lineageOf = (ids, edges) => new Set([...upstreamOf(ids, edges), ...downstreamOf(ids, edges)]);
// Targets with their direct inputs and outputs, as tar_visnetwork() highlights.
export const neighboursOf = (ids, edges) => {
  const marked = new Set(ids);
  const near = new Set(ids);
  edges.forEach((e) => {
    if (marked.has(e.source)) near.add(e.target);
    if (marked.has(e.target)) near.add(e.source);
  });
  return near;
};
