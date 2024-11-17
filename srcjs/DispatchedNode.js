import React from 'react';
import BaseNode from './BaseNode';

const DispatchedNode = (props) => {
  return (
    <BaseNode 
      {...props} 
      backgroundColor="#d49536" 
      textColor="#3f2f11" 
      extraContent={<div className="spinner"></div>}
    />
  );
};

export default DispatchedNode;
