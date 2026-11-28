import React, { createContext, useContext } from 'react';
import { NodeToolbar, Position } from '@xyflow/react';

// What every node needs to draw the toolbar of the focused target: which one
// is focused (null while a group is selected: the group has its own
// toolbar), the toolbar's actions ([{id, label}]) and what to do with one;
// and the unfolded pattern's branches (#147), with what a click on one does.
export const NodeActionsContext = createContext({
  focusId: null,
  actions: [],
  onAction: () => {},
  unfold: null,
  onBranch: () => {},
  queued: new Set(),
});

// A group action's label, with `{n}` as the number of targets.
export const groupLabel = (label, n) => label.replace(/\{n\}/g, String(n));

// An action's label on targets: Run on targets all in the run queue takes
// them out of it, and says so (#106).
export const actionLabel = (action, ids, queued) => {
  const label = ids.length > 1 ? groupLabel(action.label, ids.length) : action.label;
  return action.id === 'run' && ids.length > 0 && ids.every((id) => queued?.has(id)) ? 'Unqueue' : label;
};

// Functions and globals (targets_only = FALSE) are drawn with the targets,
// but aren't targets: no run counts, no Run or Invalidate (#305).
export const isTarget = (node) => !['function', 'object'].includes(node?.data?.target_type);
const TARGET_ACTIONS = ['run', 'invalidate'];
// The actions a menu or a toolbar offers on `nodes`: Run and Invalidate
// only when one of them is a target.
export const actionsOn = (actions, nodes) =>
  nodes.some(isTarget) ? actions : actions.filter((action) => !TARGET_ACTIONS.includes(action.id));

// Buttons over the focused target. Shown only when there are actions.
// A pattern's toolbar also unfolds it into its branches, or folds it.
export const FocusedNodeToolbar = ({ id, pattern = false, target = true }) => {
  const { focusId, actions: all, onAction, unfold, queued } = useContext(NodeActionsContext);
  const actions = target ? all : actionsOn(all, []);
  if (!actions.length) return null;
  return (
    <NodeToolbar isVisible={focusId === id} position={Position.Top} className="tb-node-toolbar">
      {actions.map((action) => (
        <button key={action.id} data-action={action.id} onClick={() => onAction(action.id, id, [id])}>
          {actionLabel(action, [id], queued)}
        </button>
      ))}
      {pattern && (
        <button data-action="unfold" onClick={() => onAction('unfold', id, [id])}>
          {unfold?.pattern === id ? 'Fold' : 'Branches'}
        </button>
      )}
    </NodeToolbar>
  );
};

// The unfolded pattern's branches, under it: a small node each, in its
// status's colours, failing ones first; past the cap, how many more (#147).
export const UnfoldedBranches = ({ id }) => {
  const { unfold, onBranch } = useContext(NodeActionsContext);
  if (unfold?.pattern !== id) return null;
  const branches = unfold.branches ?? [];
  return (
    <NodeToolbar isVisible position={Position.Bottom} className="tb-branches">
      {branches.length === 0 && <span className="tb-branches-none">No branches built yet</span>}
      {branches.map((b) => (
        <button
          key={b.name}
          type="button"
          className={`tb-branch tb-status-${b.status}`}
          data-branch={b.name}
          title={`${b.name} (branch ${b.position}): ${b.status}`}
          onClick={() => onBranch(id, b.position)}
        >
          {b.position}
        </button>
      ))}
      {unfold.more > 0 && <span className="tb-branches-more">+{unfold.more} more</span>}
    </NodeToolbar>
  );
};

// Buttons over a selected group of targets, centred over all of them.
// `node` is the one the action is reported on: the focused target, if in
// the group.
export const GroupToolbar = ({ ids, node, actions, onAction, queued }) => {
  if (!ids || !actions.length) return null;
  return (
    <NodeToolbar nodeId={ids} isVisible position={Position.Top} className="tb-node-toolbar tb-group-toolbar">
      {actions.map((action) => (
        <button key={action.id} data-action={action.id} onClick={() => onAction(action.id, node, ids)}>
          {actionLabel(action, ids, queued)}
        </button>
      ))}
    </NodeToolbar>
  );
};
