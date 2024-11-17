import React from 'react';
import BaseNode from './BaseNode';

const OutdatedNode = (props) => {
  return (
    <BaseNode 
      {...props} 
      backgroundColor="#78B7C5" 
      textColor="#10363f"
    />
  );
};

export default OutdatedNode;
