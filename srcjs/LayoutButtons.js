import React from 'react';
import { Panel } from '@xyflow/react';
import './buttonStyles.css';

const LayoutButtons = ({ onLayout }) => (
  <Panel position="top-right">
    <div className="button-container">
      <button onClick={() => onLayout('TB')}>Vertical Layout</button>
      <button onClick={() => onLayout('LR')}>Horizontal Layout</button>
    </div>
  </Panel>
);

export default LayoutButtons;
