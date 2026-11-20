import { reactShinyInput } from 'reactR';
import { useEffect, useCallback, useRef, useState } from 'react';
import Dagre from '@dagrejs/dagre';
import {
  ReactFlow,
  ReactFlowProvider,
  MiniMap,
  Background,
  Controls,
  applyNodeChanges,
  applyEdgeChanges,
  getIncomers,
  addEdge,
  useReactFlow,
} from '@xyflow/react';
import '@xyflow/react/dist/style.css';
import './targetsboardCustomCSS.css';
import LayoutButtons from './LayoutButtons';

import UptodateNode from './UptodateNode';
import OutdatedNode from './OutdatedNode';
import ErroredNode from './ErroredNode';
import DispatchedNode from './DispatchedNode';

// Defined at module scope: a new object on every render makes React Flow
// re-mount all nodes (and warn about it).
const nodeTypes = {
  uptodate: UptodateNode,
  outdated: OutdatedNode,
  errored: ErroredNode,
  dispatched: DispatchedNode
};

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


// Status-only updates from R (update_targetsboard_status()) arrive as a Shiny
// custom message; each widget picks the ones addressed to its id.
const STATUS_EVENT = 'targetsboard:status';
if (window.Shiny && !window.__targetsboardStatusHandler) {
  window.__targetsboardStatusHandler = true;
  window.Shiny.addCustomMessageHandler(STATUS_EVENT, (message) => {
    window.dispatchEvent(new CustomEvent(STATUS_EVENT, { detail: message }));
  });
}

const sortedKey = (ids) => [...ids].sort().join('|');
const structureKey = (nodes, edges) =>
  sortedKey(nodes.map((n) => n.id)) + '#' + sortedKey(edges.map((e) => e.id));

// Merge a fresh node list from R into the current React Flow nodes: keep what
// React Flow owns (position, measured size, selection), take status and data
// from R, add new targets and drop removed ones.
const syncNodes = (currentNodes, incomingNodes) => {
  const byId = new Map(currentNodes.map((n) => [n.id, n]));
  return incomingNodes.map((incoming) => {
    const current = byId.get(incoming.id);
    return current
      ? { ...current, type: incoming.type, style: incoming.style, data: incoming.data }
      : incoming;
  });
};

function Flow({ configuration }) {
  const { fitView } = useReactFlow();
  const [nodes, setNodes] = useState(configuration.nodes ?? []);
  const [edges, setEdges] = useState(configuration.edges ?? []);
  const [direction, setDirection] = useState('LR');
  // Lay out once all nodes have been measured: on first render, when the
  // pipeline structure changes, and when a layout button is pressed.
  const [layoutPending, setLayoutPending] = useState(true);

  const nodesRef = useRef(nodes);
  const edgesRef = useRef(edges);
  nodesRef.current = nodes;
  edgesRef.current = edges;

  const onNodesChange = useCallback(
    (changes) => setNodes((nds) => applyNodeChanges(changes, nds)),
    [],
  );
  const onEdgesChange = useCallback(
    (changes) => setEdges((eds) => applyEdgeChanges(changes, eds)),
    [],
  );
  const onConnect = useCallback(
    (params) => setEdges((eds) => addEdge({ ...params }, eds)),
    [],
  );

  const onLayout = useCallback((newDirection) => {
    setDirection(newDirection);
    setLayoutPending(true);
  }, []);

  // Our own nodes carry `measured` once React Flow has sized them (it comes
  // back through onNodesChange). useNodesInitialized() turned true one
  // render too early for nodes added later, so check the nodes directly.
  const allMeasured = nodes.length > 0 && nodes.every((n) => n.measured?.width && n.measured?.height);

  useEffect(() => {
    if (!layoutPending || !allMeasured) return;
    const layouted = getLayoutedElements(nodes, edges, { direction });
    setNodes(layouted.nodes);
    setLayoutPending(false);
    window.requestAnimationFrame(() => fitView());
  }, [layoutPending, allMeasured, nodes, edges, direction, fitView]);

  // New data from R (statuses every tick; sometimes a changed pipeline).
  useEffect(() => {
    const incomingNodes = configuration.nodes ?? [];
    const incomingEdges = configuration.edges ?? [];
    const structureChanged =
      structureKey(nodesRef.current, edgesRef.current) !==
      structureKey(incomingNodes, incomingEdges);

    setNodes((current) => syncNodes(current, incomingNodes));
    if (structureChanged) {
      setEdges(incomingEdges);
      setLayoutPending(true);
    }
  }, [configuration.nodes, configuration.edges]);

  // Patch statuses in place: no new nodes, no re-layout.
  useEffect(() => {
    const onStatus = (event) => {
      const message = event.detail;
      if (message?.id !== configuration.id) return;
      const updates = new Map((message.nodes ?? []).map((u) => [u.id, u]));
      setNodes((current) =>
        current.map((node) => {
          const update = updates.get(node.id);
          return update
            ? { ...node, type: update.type, data: { ...node.data, target_status: update.target_status } }
            : node;
        }),
      );
    };
    window.addEventListener(STATUS_EVENT, onStatus);
    return () => window.removeEventListener(STATUS_EVENT, onStatus);
  }, [configuration.id]);

  // Report selection to Shiny, as <id>_selected_nodes. Keyed on the ids, so
  // dragging nodes around doesn't send anything.
  const inputId = configuration.id;
  const selectedKey = sortedKey(nodes.filter((n) => n.selected).map((n) => n.id));
  const dispatchedKey = sortedKey(nodes.filter((n) => n.type === 'dispatched').map((n) => n.id));
  const edgesKey = sortedKey(edges.map((e) => e.id));

  useEffect(() => {
    if (!window.Shiny || !inputId) return;
    window.Shiny.setInputValue(`${inputId}_selected_nodes`, selectedKey ? selectedKey.split('|') : null);
  }, [inputId, selectedKey]);

  // Animate the edges feeding running (dispatched) targets.
  useEffect(() => {
    const currentNodes = nodesRef.current;
    const dispatched = currentNodes.filter((n) => n.type === 'dispatched');
    const dispatchedIds = new Set(dispatched.map((n) => n.id));
    const incomerIds = new Set(
      dispatched.flatMap((n) => getIncomers(n, currentNodes, edgesRef.current).map((i) => i.id)),
    );

    if (window.Shiny && inputId) {
      window.Shiny.setInputValue(`${inputId}_dispatched_nodes`, [...dispatchedIds]);
      window.Shiny.setInputValue(`${inputId}_dispatched_nodes_incomers`, [...incomerIds]);
    }

    setEdges((current) => current.map((edge) => {
      const animated = incomerIds.has(edge.source) && dispatchedIds.has(edge.target);
      return edge.animated === animated ? edge : { ...edge, animated };
    }));
  }, [inputId, dispatchedKey, edgesKey]);

  return (
    <div className="reactflow-wrapper" style={{ height: configuration.height, width: configuration.width }}>
      <ReactFlow
        nodes={nodes}
        edges={edges}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        onConnect={onConnect}
        fitView
        style={{ background: configuration.background_color }}
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
  );
}

export default function targetsboard({ configuration }) {
  return (
    <div className="providerflow">
      <ReactFlowProvider>
        <Flow configuration={configuration} />
      </ReactFlowProvider>
    </div>
  );
}

reactShinyInput(
  '.targetsboard',
  'targetsboard.targetsboard',
  targetsboard
);
