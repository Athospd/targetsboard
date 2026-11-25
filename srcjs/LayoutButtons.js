import React from 'react';
import './buttonStyles.css';

// In the canvas's top-right panel, above the status legend.
const LayoutButtons = ({ onLayout }) => (
  <div className="tb-layout-buttons">
    <button type="button" onClick={() => onLayout('TB')}>Tidy vertically</button>
    <button type="button" onClick={() => onLayout('LR')}>Tidy horizontally</button>
  </div>
);

export default LayoutButtons;
