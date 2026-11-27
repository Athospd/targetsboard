// tar_map() families on the canvas (#113): a labelled box around each
// family's targets, and its button folds the family into one node. R names
// each target's family (add_families()); the box follows the targets as
// they move, drawn over the canvas, so the layout and the saved positions
// never know about it.
import { useMemo } from 'react';
import { ViewportPortal } from '@xyflow/react';

const PAD = 10; // between the box and its targets
// A folded family's node takes the status its targets would worry most about.
const WORST = ['errored', 'running', 'outdated', 'uptodate'];

export const familyOf = (node) => (typeof node.data?.family === 'string' ? node.data.family : null);

// The graph as shown with the families in `folded` folded: each into its
// first target, which stands in for all of them, with their edges and their
// worst status; the others hidden.
export const foldFamilies = (nodes, edges, folded) => {
  if (!folded.size) return { nodes, edges };
  const members = new Map();
  nodes.forEach((n) => {
    const family = familyOf(n);
    if (!family || !folded.has(family)) return;
    if (!members.has(family)) members.set(family, []);
    members.get(family).push(n);
  });
  const standIn = new Map();
  members.forEach((list) => list.forEach((n) => standIn.set(n.id, list[0].id)));

  const shown = nodes.map((n) => {
    const stand = standIn.get(n.id);
    if (!stand) return n;
    if (stand !== n.id) return { ...n, hidden: true };
    const list = members.get(familyOf(n));
    const type = WORST.find((status) => list.some((m) => m.type === status)) ?? n.type;
    return { ...n, type, data: { ...n.data, label: `${familyOf(n)} (${list.length})`, folded: list.length } };
  });

  const seen = new Set();
  const shownEdges = [];
  edges.forEach((e) => {
    const source = standIn.get(e.source) ?? e.source;
    const target = standIn.get(e.target) ?? e.target;
    if (source === target || seen.has(`${source}|${target}`)) return;
    seen.add(`${source}|${target}`);
    shownEdges.push(source === e.source && target === e.target ? e : { ...e, id: `${source}->${target}`, source, target });
  });
  return { nodes: shown, edges: shownEdges };
};

// Each family's box, around its shown targets, in the canvas' coordinates.
export const familyBoxes = (nodes) => {
  const boxes = new Map();
  nodes.forEach((n) => {
    const family = familyOf(n);
    const { width, height } = n.measured ?? {};
    if (!family || n.hidden || !width || !height || !n.position) return;
    const box = boxes.get(family) ?? { family, x0: Infinity, y0: Infinity, x1: -Infinity, y1: -Infinity, folded: 0 };
    box.x0 = Math.min(box.x0, n.position.x);
    box.y0 = Math.min(box.y0, n.position.y);
    box.x1 = Math.max(box.x1, n.position.x + width);
    box.y1 = Math.max(box.y1, n.position.y + height);
    box.folded = Math.max(box.folded, n.data?.folded ?? 0);
    boxes.set(family, box);
  });
  return [...boxes.values()];
};

export default function FamilyBoxes({ nodes, onToggle }) {
  const boxes = useMemo(() => familyBoxes(nodes), [nodes]);
  if (!boxes.length) return null;
  return (
    <ViewportPortal>
      {boxes.map((b) => (
        <div
          key={b.family}
          className={`tb-family${b.folded ? ' tb-family-folded' : ''}`}
          style={{
            transform: `translate(${b.x0 - PAD}px, ${b.y0 - PAD}px)`,
            width: b.x1 - b.x0 + 2 * PAD,
            height: b.y1 - b.y0 + 2 * PAD,
          }}
        >
          <div className="tb-family-header">
            <span className="tb-family-label">{b.family}</span>
            <button
              type="button"
              className="tb-family-toggle nodrag nopan"
              aria-expanded={!b.folded}
              title={b.folded ? `Unfold ${b.family}'s ${b.folded} targets` : `Fold ${b.family} into one node`}
              onClick={() => onToggle(b.family)}
            >
              {b.folded ? '+' : '−'}
            </button>
          </div>
        </div>
      ))}
    </ViewportPortal>
  );
}
