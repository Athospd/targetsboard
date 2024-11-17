import { reactWidget, reactShinyInput } from 'reactR';
import { useEffect, useCallback, useRef, useState } from 'react';
import Dagre from '@dagrejs/dagre';
import {
  ReactFlow,
  ReactFlowProvider,
  MiniMap,
  Background,
  BackgroundVariant,
  Controls,
  useNodesState,
  useEdgesState,
  applyNodeChanges,
  applyEdgeChanges,
  getIncomers,
  addEdge,
  Panel,
  useReactFlow,
  Handle,
  Position 
} from '@xyflow/react';
import '@xyflow/react/dist/style.css';
import './targetsboardCustomCSS.css';
import LayoutButtons from './LayoutButtons';

import UptodateNode from './UptodateNode'; 
import OutdatedNode from './OutdatedNode'; 
import ErroredNode from './ErroredNode';
import DispatchedNode from './DispatchedNode';



// reactWidget('targetsboard', 'output', {
//   ReactFlow: targetsboardWidget,
//   MiniMap: MiniMap,
//   Background: Background,
//   Controls: Controls
// });

const getLayoutedElements = (nodes, edges, options) => {
  nodes = Array.isArray(nodes) ? nodes : []; 
  edges = Array.isArray(edges) ? edges : [];
  
  const g = new Dagre.graphlib.Graph().setDefaultEdgeLabel(() => ({}));
  g.setGraph({ rankdir: options.direction });
 
  edges.forEach((edge) => g.setEdge(edge.source, edge.target));
  nodes.forEach((node) =>
    g.setNode(node.id, {
      ...node,
      width: node.measured?.width ?? 0,
      height: node.measured?.height ?? 0,
    }),
  );
 
  Dagre.layout(g);
 
  return {
    nodes: nodes.map((node) => {
      const position = g.node(node.id);
      // We are shifting the dagre node position (anchor=center center) to the top left
      // so it matches the React Flow node anchor point (top left).
      const x = position.x - (node.measured?.width ?? 0) / 2;
      const y = position.y - (node.measured?.height ?? 0) / 2;
      const sourcePosition = options.direction === 'LR' ? 'right' : 'bottom'; 
      const targetPosition = options.direction === 'LR' ? 'left' : 'top'
      return { ...node, position: { x, y }, sourcePosition: sourcePosition, targetPosition: targetPosition };
    }),
    edges,
  };
};

export default function targetsboard({ configuration, value, setValue }) {
  
  const reactFlowInstance = useRef(null);
  const [nodes, setNodes] = useState(configuration.nodes);
  const [edges, setEdges] = useState(configuration.edges);
  
  const onNodesChange = useCallback(
    (changes) => {
      return(setNodes((nds) => applyNodeChanges(changes, nds)))
    },
    [],
  );
  const onEdgesChange = useCallback(
    (changes) => setEdges((eds) => applyEdgeChanges(changes, eds)),
    [],
  );

  const onConnect = useCallback(
    (params) => {
      setEdges((eds) => addEdge({ ...params }, eds));
    }, [setEdges]);

  const handleInit = (instance) => { 
    reactFlowInstance.current = instance; 
  };

  const onLayout = useCallback(
    (direction) => {
      const layouted = getLayoutedElements(nodes, edges, { direction });
      setTimeout(() => {
        setNodes([...layouted.nodes]);
        setEdges([...layouted.edges]);
        if(reactFlowInstance.current) {
          reactFlowInstance.current.fitView();
        }
      }, 0);
    },
    [nodes, edges],
  );

  useEffect(() => {
    const selected_nodes = nodes.filter(item => item.selected).map(item => item.id);
    window.Shiny.setInputValue("selected_nodes", selected_nodes);

    const dispatched_nodes = nodes.filter(item => item.type === "dispatched");
    const dispatched_nodes_ids = dispatched_nodes.map(item => item.id)
    window.Shiny.setInputValue("dispatched_nodes", dispatched_nodes_ids);

    const dispatched_nodes_incomers_ids = dispatched_nodes.map((node) => getIncomers(node, nodes, edges).map(item => item.id)).flat();
    window.Shiny.setInputValue("dispatched_nodes_incomers", dispatched_nodes_incomers_ids);

    setEdges((current_edges) => current_edges.map((current_edge) => {
      const source_candidate_edges = dispatched_nodes_incomers_ids.includes(current_edge.source);
      const target_candidate_edges = dispatched_nodes_ids.includes(current_edge.target);
      return {
      ...current_edge,
      animated: source_candidate_edges & target_candidate_edges
    }
  }));
  }, [nodes]);

  useEffect(() => {
    setNodes((current_nodes) =>
      current_nodes.map((current_node) => {
        const new_node = configuration.nodes.find(item => item.id === current_node.id);
        return { ...current_node, type: new_node.type, style: new_node.style, data: new_node.data };
      }),
    );
  }, [configuration.nodes]);

  const nodeTypes = { 
    uptodate: UptodateNode, 
    outdated: OutdatedNode, 
    errored: ErroredNode,
    dispatched: DispatchedNode
  };

  return (
    <div className="providerflow">
      <ReactFlowProvider>
        <div className="reactflow-wrapper" style={{ height: configuration.height, width: configuration.width }}>
          <ReactFlow
            nodes={nodes}
            edges={edges}
            onNodesChange={onNodesChange}
            onEdgesChange={onEdgesChange}
            onConnect={onConnect}
            fitView
            style={{ background: configuration.background_color }}
            onInit={handleInit}
            nodeTypes={nodeTypes}
          >
            <MiniMap /> 
            <Background
              id="1"
              gap={15}
              color={configuration.background_marks_color}
              size={1}
            />
            <Controls />
            <LayoutButtons onLayout={onLayout} />
          </ReactFlow>
        </div>
      </ReactFlowProvider>
    </div>
  );
}





reactShinyInput(
  '.targetsboard',
  'targetsboard.targetsboard',
  targetsboard
);

