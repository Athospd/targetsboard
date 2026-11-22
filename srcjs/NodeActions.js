import React, { createContext, useContext } from 'react';
import { NodeToolbar, Position } from '@xyflow/react';

// What every node needs to draw the toolbar of the focused target: which one
// is focused (null while a group is selected: the group has its own
// toolbar), the toolbar's actions ([{id, label}]) and what to do with one.
export const NodeActionsContext = createContext({
  focusId: null,
  actions: [],
  onAction: () => {},
});

// A group action's label, with `{n}` as the number of targets.
export const groupLabel = (label, n) => label.replace(/\{n\}/g, String(n));

// Buttons over the focused target. Shown only when there are actions.
export const FocusedNodeToolbar = ({ id }) => {
  const { focusId, actions, onAction } = useContext(NodeActionsContext);
  if (!actions.length) return null;
  return (
    <NodeToolbar isVisible={focusId === id} position={Position.Top} className="tb-node-toolbar">
      {actions.map((action) => (
        <button key={action.id} data-action={action.id} onClick={() => onAction(action.id, id, [id])}>
          {action.label}
        </button>
      ))}
    </NodeToolbar>
  );
};

// Buttons over a selected group of targets, centred over all of them.
// `node` is the one the action is reported on: the focused target, if in
// the group.
export const GroupToolbar = ({ ids, node, actions, onAction }) => {
  if (!ids || !actions.length) return null;
  return (
    <NodeToolbar nodeId={ids} isVisible position={Position.Top} className="tb-node-toolbar tb-group-toolbar">
      {actions.map((action) => (
        <button key={action.id} data-action={action.id} onClick={() => onAction(action.id, node, ids)}>
          {groupLabel(action.label, ids.length)}
        </button>
      ))}
    </NodeToolbar>
  );
};
