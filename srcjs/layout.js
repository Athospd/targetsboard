// A layered layout for the pipeline graph, after the one tar_visnetwork()
// gets from vis.js: each target sits in the column of its level, and each
// column is stacked compactly and centred. Dagre spread dense pipelines (every
// target of a layer feeding every target of the next) into a wide diamond,
// because its coordinate step aligns targets with their median neighbours.

const LEVEL_GAP = 60;  // between columns, beyond the widest target
export const SIBLING_GAP = 20; // between targets of one column
const SWEEPS = 8;       // ordering passes to reduce edge crossings

// The level of each target: targets' own `level` when every target has one,
// else the longest path from a target with no dependencies.
const levelsOf = (nodes, edges) => {
  const given = new Map(nodes.map((n) => [n.id, Number(n.data?.level)]));
  if ([...given.values()].every((l) => Number.isFinite(l))) return given;

  const ids = new Set(nodes.map((n) => n.id));
  const parents = new Map(nodes.map((n) => [n.id, []]));
  edges.forEach((e) => {
    if (ids.has(e.source) && ids.has(e.target)) parents.get(e.target).push(e.source);
  });
  const level = new Map();
  const visiting = new Set();
  const visit = (id) => {
    if (level.has(id)) return level.get(id);
    if (visiting.has(id)) return 1; // a cycle: should not happen in a DAG
    visiting.add(id);
    const l = 1 + Math.max(0, ...parents.get(id).map(visit));
    visiting.delete(id);
    level.set(id, l);
    return l;
  };
  nodes.forEach((n) => visit(n.id));
  return level;
};

// Order the targets within each column to reduce edge crossings: alternately
// sort each column by the mean position of its neighbours in the columns
// before it, then after it (the barycenter heuristic).
const orderColumns = (columns, edges) => {
  const position = new Map();
  const renumber = () => columns.forEach((col) => col.forEach((id, i) => position.set(id, i / Math.max(1, col.length - 1))));
  renumber();

  const neighbours = new Map();
  const add = (a, b) => {
    if (!neighbours.has(a)) neighbours.set(a, []);
    neighbours.get(a).push(b);
  };
  edges.forEach((e) => { add(e.source, e.target); add(e.target, e.source); });

  const columnOf = new Map();
  columns.forEach((col, c) => col.forEach((id) => columnOf.set(id, c)));

  const sortColumn = (c, fromBefore) => {
    const score = new Map(columns[c].map((id) => {
      const near = (neighbours.get(id) ?? []).filter((n) =>
        fromBefore ? columnOf.get(n) < c : columnOf.get(n) > c);
      const mean = near.length
        ? near.reduce((sum, n) => sum + position.get(n), 0) / near.length
        : position.get(id); // no neighbour that side: keep its place
      return [id, mean];
    }));
    // Stable: ties keep their current order.
    columns[c] = columns[c]
      .map((id, i) => [id, i])
      .sort((a, b) => score.get(a[0]) - score.get(b[0]) || a[1] - b[1])
      .map(([id]) => id);
    columns[c].forEach((id, i) => position.set(id, i / Math.max(1, columns[c].length - 1)));
  };

  for (let sweep = 0; sweep < SWEEPS; sweep++) {
    if (sweep % 2 === 0) for (let c = 1; c < columns.length; c++) sortColumn(c, true);
    else for (let c = columns.length - 2; c >= 0; c--) sortColumn(c, false);
  }
  return columns;
};

// A tar_map() family's targets next to each other in their column, where its
// first one is, for its box (FamilyBoxes.js); the others keep their order.
const keepFamiliesTogether = (columns, byId) => columns.map((col) => {
  const family = (id) => {
    const f = byId.get(id)?.data?.family;
    return typeof f === 'string' ? f : null;
  };
  const first = new Map();
  col.forEach((id, i) => { const f = family(id); if (f && !first.has(f)) first.set(f, i); });
  return col
    .map((id, i) => [id, family(id) ? first.get(family(id)) : i, i])
    .sort((a, b) => a[1] - b[1] || a[2] - b[2])
    .map(([id]) => id);
});

// Lay out the whole graph. `direction` is 'LR' (columns left to right) or
// 'TB' (rows top to bottom). Nodes need their measured size; one not
// measured (off screen, when only visible targets are rendered) takes the
// size of one that is: all targets have the same width.
export const layoutGraph = (nodes, edges, { direction = 'LR' } = {}) => {
  nodes = Array.isArray(nodes) ? nodes : [];
  edges = Array.isArray(edges) ? edges : [];
  const horizontal = direction === 'LR';
  const known = nodes.find((n) => n.measured?.width && n.measured?.height)?.measured;
  const fallback = { w: known?.width ?? 0, h: known?.height ?? 0 };
  const size = (n) =>
    n.measured?.width && n.measured?.height ? { w: n.measured.width, h: n.measured.height } : fallback;

  const level = levelsOf(nodes, edges);
  const levels = [...new Set(level.values())].sort((a, b) => a - b);
  const byLevel = new Map(levels.map((l) => [l, []]));
  nodes.forEach((n) => byLevel.get(level.get(n.id)).push(n.id)); // keeps R's order to start
  const byId = new Map(nodes.map((n) => [n.id, n]));
  const columns = keepFamiliesTogether(orderColumns(levels.map((l) => byLevel.get(l)), edges), byId);

  // Along the flow: one step per column, as wide as the widest target.
  const step = Math.max(0, ...nodes.map((n) => (horizontal ? size(n).w : size(n).h))) + LEVEL_GAP;
  const position = new Map();
  columns.forEach((col, c) => {
    const extent = (id) => (horizontal ? size(byId.get(id)).h : size(byId.get(id)).w);
    const total = col.reduce((sum, id) => sum + extent(id), 0) + SIBLING_GAP * (col.length - 1);
    let across = -total / 2; // centred on the flow's axis
    col.forEach((id) => {
      const along = c * step;
      position.set(id, horizontal ? { x: along, y: across } : { x: across, y: along });
      across += extent(id) + SIBLING_GAP;
    });
  });

  return {
    nodes: nodes.map((node) => ({
      ...node,
      position: position.get(node.id),
      sourcePosition: horizontal ? 'right' : 'bottom',
      targetPosition: horizontal ? 'left' : 'top',
    })),
    edges,
  };
};
