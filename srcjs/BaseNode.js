import React from 'react';
import { Handle, Position } from '@xyflow/react';
import { FocusedNodeToolbar } from './NodeActions';
import TypeIcon, { targetKind } from './TypeIcon';

// A target, drawn in the colours of its status: uptodate, outdated, errored
// or running. Hovered, an outdated one says why (data.why). Its kind, when
// not a plain target, shows as an icon (TypeIcon); its last build's time and
// size, as a badge under it (data.cost).
const BaseNode = ({
  id,
  data,
  isConnectable,
  sourcePosition = Position.Top,
  targetPosition = Position.Bottom,
  status
}) => {
  // Its colours come from the status's tokens, in targetsboardCustomCSS.css.
  const kind = targetKind(data?.target_type, data?.kind);
  return (
    <div className={`base-node tb-status-${status}` + (kind ? ' tb-has-type-icon' : '')} title={data?.why ?? undefined}>
      <FocusedNodeToolbar id={id} />
      <Handle type="target" position={targetPosition} isConnectable={isConnectable} />
      <TypeIcon kind={kind} />
      {data?.cost && (
        // Its time and size: each shows when the legend's switch is on
        // (the wrapper's tb-show-times, tb-show-sizes).
        <span className="tb-cost" title={data.cost.title}>
          <span className={'tb-cost-time' + (data.cost.slow ? ' tb-cost-hot' : '')}>{data.cost.time}</span>
          {data.cost.size && (
            <span className={'tb-cost-size' + (data.cost.large ? ' tb-cost-hot' : '')}>{data.cost.size}</span>
          )}
        </span>
      )}
      {status === 'running' && <div className="extra-content"><div className="spinner"></div></div>}
      {data?.warning && (
        <span className="warning-mark" title="Its last build raised warnings">!</span>
      )}
      {data?.branches && (
        <span
          className={'branch-progress' + (data.branches.errored > 0 ? ' branch-progress-errored' : '')}
          title={`${data.branches.done} of ${data.branches.total} branches done` +
            (data.branches.errored > 0 ? `, ${data.branches.errored} errored` : '')}
        >
          {data.branches.done}/{data.branches.total}
        </span>
      )}
        {data?.label}
      <Handle type="source" position={sourcePosition} isConnectable={isConnectable} />
    </div>
  );
};

export default BaseNode;
