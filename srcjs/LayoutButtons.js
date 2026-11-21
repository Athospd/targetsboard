import React from 'react';
import { Panel } from '@xyflow/react';
import './buttonStyles.css';

const LayoutButtons = ({ onLayout }) => (
  <Panel position="top-right">
    <div className="button-container">
      <button onClick={() => onLayout('TB')}>Tidy vertically</button>
      <button onClick={() => onLayout('LR')}>Tidy horizontally</button>
    </div>
  </Panel>
);

export default LayoutButtons;
