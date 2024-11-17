import React from 'react';
import { Handle, Position } from '@xyflow/react';

const BaseNode = ({ 
  data, 
  isConnectable, 
  sourcePosition = Position.Top, 
  targetPosition = Position.Bottom, 
  backgroundColor = "white", 
  textColor = "black", 
  extraContent 
}) => {
  return (
    <div className='base-node' style={{backgroundColor: backgroundColor, textColor: textColor}}>
      <Handle type="target" position={targetPosition} isConnectable={isConnectable} />
      {extraContent && <div className="extra-content">{extraContent}</div>}
        {data?.label}
      <Handle type="source" position={sourcePosition} isConnectable={isConnectable} />
    </div>
  );
};

export default BaseNode;
