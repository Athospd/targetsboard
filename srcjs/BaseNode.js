import React from 'react';
import { Handle, Position } from '@xyflow/react';
import { FocusedNodeToolbar } from './NodeActions';

const BaseNode = ({
  id,
  data,
  isConnectable,
  sourcePosition = Position.Top,
  targetPosition = Position.Bottom,
  backgroundColor = "white",
  textColor = "black",
  extraContent
}) => {
  return (
    <div className='base-node' style={{ backgroundColor: backgroundColor, color: textColor }}>
      <FocusedNodeToolbar id={id} />
      <Handle type="target" position={targetPosition} isConnectable={isConnectable} />
      {extraContent && <div className="extra-content">{extraContent}</div>}
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
