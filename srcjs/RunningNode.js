import React from 'react';
import BaseNode from './BaseNode';

const RunningNode = (props) => {
  return (
    <BaseNode 
      {...props} 
      backgroundColor="#d49536" 
      textColor="#3f2f11" 
      extraContent={<div className="spinner"></div>}
    />
  );
};

export default RunningNode;
