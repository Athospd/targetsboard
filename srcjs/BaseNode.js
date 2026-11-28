import React from 'react';
import { Handle, Position } from '@xyflow/react';
import { FocusedNodeToolbar, UnfoldedBranches, isTarget } from './NodeActions';
import TypeIcon, { targetKind } from './TypeIcon';

// A target, drawn in the colours of its status: uptodate, outdated, errored
// or running. Hovered, an outdated one says why (data.why). Its kind, when
// not a plain target, shows as an icon (TypeIcon); its last build's time and
// size, as a badge under it (data.cost).
// The hover text: the target's description (tar_target(description = )),
// then why it is outdated, if it is.
const nodeTitle = (data) => {
  const parts = [data?.description, data?.why].filter((part) => typeof part === 'string' && part.trim() !== '');
  return parts.length ? parts.join('\n\n') : undefined;
};


// A small clock, in the mark's colour.
const clockIcon = (
  <svg viewBox="0 0 16 16" width="10" height="10" aria-hidden="true">
    <circle cx="8" cy="8" r="6.5" fill="none" stroke="currentColor" strokeWidth="1.8" />
    <path d="M8 4.5V8l2.5 1.5" fill="none" stroke="currentColor" strokeWidth="1.8" strokeLinecap="round" />
  </svg>
);
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
  // A function or a global: outlined, not filled (#305).
  const target = isTarget({ data });
  return (
    <div
      className={`base-node tb-status-${status}` + (kind ? ' tb-has-type-icon' : '') + (target ? '' : ' tb-not-target')}
      title={nodeTitle(data)}
    >
      <FocusedNodeToolbar id={id} pattern={data?.target_type === 'pattern'} target={target} />
      <UnfoldedBranches id={id} />
      <Handle type="target" position={targetPosition} isConnectable={isConnectable} />
      <TypeIcon kind={kind} raw={data?.kind} />
      {data?.cost && (
        // Its time and size: each shows when the legend's switch is on
        // (the wrapper's tb-show-times, tb-show-sizes), in its corner.
        <span className="tb-cost" title={data.cost.title}>
          <span className={'tb-cost-time' + (data.cost.slow ? ' tb-cost-hot' : '')}>{data.cost.time}</span>
          {data.cost.size && (
            <span className={'tb-cost-size' + (data.cost.large ? ' tb-cost-hot' : '')}>{data.cost.size}</span>
          )}
        </span>
      )}
      {status === 'running' && <div className="extra-content"><div className="spinner"></div></div>}
      {/* In the run queue, a clock takes the warning's place until the queued run starts (#106). */}
      {data?.queued ? (
        <span className="queued-mark" title="In the run queue: it runs when the current run ends">{clockIcon}</span>
      ) : data?.warning && (
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
