import React, { useRef, useEffect } from 'react';
import { Panel } from '@xyflow/react';
import './buttonStyles.css'; // Assuming you have a separate CSS file for button styles

const LayoutButtons = ({ onLayout }) => {
  const verticalButtonRef = useRef(null);

  useEffect(() => {
    if (verticalButtonRef.current) {
      setTimeout(() => {
        verticalButtonRef.current.click();
      }, 10);
    }
  }, []);

  return (
    <Panel position="top-right">
      <div className="button-container">
        <button  onClick={() => onLayout('TB')}>Vertical Layout</button>
        <button ref={verticalButtonRef} onClick={() => onLayout('LR')}>Horizontal Layout</button>
      </div>
    </Panel>
  );
};

export default LayoutButtons;
