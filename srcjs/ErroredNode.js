import React from 'react';
import BaseNode from './BaseNode';

const ErroredNode = (props) => {
  return (
    <BaseNode 
      {...props} 
      backgroundColor="#C93312" 
      textColor="#ffb6a6"
    />
  );
};

export default ErroredNode;
