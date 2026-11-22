import React from 'react';
import { Panel } from '@xyflow/react';
import './buttonStyles.css';

const LayoutButtons = ({ onLayout }) => (
  <Panel position="top-right">
    <div className="tb-layout-buttons">
      <button type="button" onClick={() => onLayout('TB')}>Tidy vertically</button>
      <button type="button" onClick={() => onLayout('LR')}>Tidy horizontally</button>
    </div>
  </Panel>
);

export default LayoutButtons;
