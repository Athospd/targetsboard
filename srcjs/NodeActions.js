import React, { createContext, useContext } from 'react';
import { NodeToolbar, Position } from '@xyflow/react';

// What every node needs to draw the toolbar of the focused target: which one
// is focused, the toolbar's actions ([{id, label}]) and what to do with one.
export const NodeActionsContext = createContext({
  focusId: null,
  actions: [],
  onAction: () => {},
});

// Buttons over the focused target. Shown only when there are actions.
export const FocusedNodeToolbar = ({ id }) => {
  const { focusId, actions, onAction } = useContext(NodeActionsContext);
  if (!actions.length) return null;
  return (
    <NodeToolbar isVisible={focusId === id} position={Position.Top} className="tb-node-toolbar">
      {actions.map((action) => (
        <button key={action.id} data-action={action.id} onClick={() => onAction(action.id, id)}>
          {action.label}
        </button>
      ))}
    </NodeToolbar>
  );
};
