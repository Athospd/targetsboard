import { reactShinyInput } from 'reactR';
import { useEffect, useLayoutEffect, useCallback, useRef, useState } from 'react';
import {
  ReactFlow,
  ReactFlowProvider,
  MiniMap,
  Background,
  BackgroundVariant,
  Controls,
  applyNodeChanges,
  applyEdgeChanges,
  getIncomers,
  useReactFlow,
} from '@xyflow/react';
import '@xyflow/react/dist/style.css';
import './targetsboardCustomCSS.css';
import LayoutButtons from './LayoutButtons';
import { layoutGraph } from './layout';

import UptodateNode from './UptodateNode';
import OutdatedNode from './OutdatedNode';
import ErroredNode from './ErroredNode';
import RunningNode from './RunningNode';

// One node component per status the board shows. Defined at module scope: a
// new object on every render makes React Flow re-mount all nodes (and warn
// about it).
const nodeTypes = {
  uptodate: UptodateNode,
  outdated: OutdatedNode,
  errored: ErroredNode,
  running: RunningNode
};

// The node colours of each status, for the minimap.
const STATUS_COLORS = {
  uptodate: '#354823',
  outdated: '#78B7C5',
  errored: '#C93312',
  running: '#d49536',
};

// Display options, as targetsboard_options() sets them in R.
const DEFAULT_OPTIONS = {
  color_mode: 'light',
  minimap: true,
  controls: true,
  background: 'dots',
  edge_type: 'default',
  only_render_visible: false,
  node_actions: [], // [{id, label}]: the right-click menu's items
};

// Report an event to Shiny: with priority 'event', the same value twice
// (double-clicking the same target again) still reaches the server.
const sendEvent = (name, value) =>
  window.Shiny?.setInputValue?.(name, value, { priority: 'event' });

// The right-click menu of a target. `copy_name` is done here: the server
// can't reach the clipboard. Every other action goes to Shiny as
// <id>_node_action.
function NodeMenu({ menu, actions, onAction, onClose }) {
  // Opened where the click was, then moved back inside the widget if it
  // overflows: measured once drawn, before the browser paints it.
  const ref = useRef(null);
  const [place, setPlace] = useState({ left: menu.x, top: menu.y });
  useLayoutEffect(() => {
    const el = ref.current;
    const parent = el?.offsetParent;
    if (!el || !parent) return;
    const maxLeft = parent.clientWidth - el.offsetWidth;
    const maxTop = parent.clientHeight - el.offsetHeight;
    setPlace({
      left: Math.max(0, Math.min(menu.x, maxLeft)),
      top: Math.max(0, Math.min(menu.y, maxTop)),
    });
  }, [menu.x, menu.y]);
  useEffect(() => {
    const onKey = (event) => event.key === 'Escape' && onClose();
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);
  return (
    <div className="tb-node-menu" role="menu" ref={ref} style={place}>
      {actions.map((action) => (
        <button
          key={action.id}
          role="menuitem"
          data-action={action.id}
          onClick={() => onAction(action.id, menu.node)}
        >
          {action.label}
        </button>
      ))}
    </div>
  );
}
const BACKGROUND_VARIANTS = {
  dots: BackgroundVariant.Dots,
  lines: BackgroundVariant.Lines,
  cross: BackgroundVariant.Cross,
};

// 'system' follows the browser's preference.
const resolveColorMode = (mode) =>
  mode === 'system'
    ? window.matchMedia?.('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'
    : mode;

const handlePositions = (direction) => ({
  sourcePosition: direction === 'LR' ? 'right' : 'bottom',
  targetPosition: direction === 'LR' ? 'left' : 'top',
});

// Place only the targets in `newIds` and leave the others where they are, so
// the author's picture of the graph survives edits to the pipeline. A new
// target goes where a full layout would put it relative to a neighbour that is
// already placed; without one, where the full layout would put it, shifted
// the way the placed targets are.
const placeNewNodes = (nodes, edges, newIds, direction) => {
  const ideal = new Map(
    layoutGraph(nodes, edges, { direction }).nodes.map((n) => [n.id, n.position]),
  );
  const placed = nodes.filter((n) => !newIds.has(n.id));
  const byId = new Map(placed.map((n) => [n.id, n]));

  const shift = { x: 0, y: 0 };
  placed.forEach((n) => {
    shift.x += (n.position.x - ideal.get(n.id).x) / placed.length;
    shift.y += (n.position.y - ideal.get(n.id).y) / placed.length;
  });

  const anchors = new Map();
  edges.forEach((e) => {
    if (newIds.has(e.target) && byId.has(e.source) && !anchors.has(e.target))
      anchors.set(e.target, e.source);
    if (newIds.has(e.source) && byId.has(e.target) && !anchors.has(e.source))
      anchors.set(e.source, e.target);
  });

  return nodes.map((node) => {
    if (!newIds.has(node.id)) return node;
    const own = ideal.get(node.id);
    const anchor = byId.get(anchors.get(node.id));
    const position = anchor
      ? {
          x: anchor.position.x + own.x - ideal.get(anchor.id).x,
          y: anchor.position.y + own.y - ideal.get(anchor.id).y,
        }
      : { x: own.x + shift.x, y: own.y + shift.y };
    return { ...node, position, ...handlePositions(direction) };
  });
};

// The focused target with everything upstream and downstream of it.
const lineageOf = (focusId, edges) => {
  const lineage = new Set([focusId]);
  const walk = (from, to) => {
    const next = new Map();
    edges.forEach((e) => {
      if (!next.has(e[from])) next.set(e[from], []);
      next.get(e[from]).push(e[to]);
    });
    const stack = [focusId];
    while (stack.length) {
      (next.get(stack.pop()) ?? []).forEach((id) => {
        if (!lineage.has(id)) {
          lineage.add(id);
          stack.push(id);
        }
      });
    }
  };
  walk('source', 'target'); // downstream
  walk('target', 'source'); // upstream
  return lineage;
};

// Light updates from R arrive as Shiny custom messages: statuses
// (update_targetsboard_status()) and display options
// (update_targetsboard_options()). Each widget picks the ones addressed to
// its id.
const STATUS_EVENT = 'targetsboard:status';
const OPTIONS_EVENT = 'targetsboard:options';
if (window.Shiny && !window.__targetsboardStatusHandler) {
  window.__targetsboardStatusHandler = true;
  [STATUS_EVENT, OPTIONS_EVENT].forEach((type) =>
    window.Shiny.addCustomMessageHandler(type, (message) => {
      window.dispatchEvent(new CustomEvent(type, { detail: message }));
    }),
  );
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
  const [focusId, setFocusId] = useState(null);
  const [options, setOptions] = useState({ ...DEFAULT_OPTIONS, ...configuration.options });
  // Lay out once all nodes have been measured: everything ('all') on first
  // render and when a Tidy button is pressed; only the new targets ('new')
  // when the pipeline's structure changes.
  const [layoutPending, setLayoutPending] = useState('all');
  // Fit the view to the graph once the laid-out targets are drawn.
  const [fitPending, setFitPending] = useState(false);
  const newIdsRef = useRef(new Set());

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
  const [menu, setMenu] = useState(null); // {node, x, y}
  const wrapperRef = useRef(null);
  const inputId = configuration.id;

  const onNodeClick = useCallback((_, node) => {
    setFocusId(node.id);
    setMenu(null);
  }, []);
  const onPaneClick = useCallback(() => {
    setFocusId(null);
    setMenu(null);
  }, []);
  // A double-click also focuses its target, whatever clicks came before.
  const onNodeDoubleClick = useCallback(
    (_, node) => {
      setFocusId(node.id);
      sendEvent(`${inputId}_double_clicked_node`, node.id);
    },
    [inputId],
  );
  // Right-click focuses the target, reports it, and opens the menu if R
  // gave it actions.
  const onNodeContextMenu = useCallback(
    (event, node) => {
      event.preventDefault();
      setFocusId(node.id);
      sendEvent(`${inputId}_context_node`, node.id);
      const box = wrapperRef.current?.getBoundingClientRect() ?? { left: 0, top: 0 };
      setMenu({ node: node.id, x: event.clientX - box.left, y: event.clientY - box.top });
    },
    [inputId],
  );
  const onMenuAction = useCallback(
    (action, node) => {
      setMenu(null);
      if (action === 'copy_name') {
        navigator.clipboard?.writeText(node).catch(() => {});
        return;
      }
      sendEvent(`${inputId}_node_action`, { action, node });
    },
    [inputId],
  );
  const closeMenu = useCallback(() => setMenu(null), []);
  // Where the view is, after each pan or zoom.
  const onMoveEnd = useCallback(
    (_, viewport) => window.Shiny?.setInputValue?.(`${inputId}_viewport`, viewport),
    [inputId],
  );

  const onLayout = useCallback((newDirection) => {
    setDirection(newDirection);
    setLayoutPending('all');
  }, []);

  // Our own nodes carry `measured` once React Flow has sized them (it comes
  // back through onNodesChange). useNodesInitialized() turned true one
  // render too early for nodes added later, so check the nodes directly.
  // Targets rendered only when visible are never measured off screen: then
  // one measured target is enough, the layout sizes the others like it.
  const isMeasured = (n) => n.measured?.width && n.measured?.height;
  const allMeasured = nodes.length > 0 &&
    (options.only_render_visible ? nodes.some(isMeasured) : nodes.every(isMeasured));

  useEffect(() => {
    if (!layoutPending || !allMeasured) return;
    if (layoutPending === 'all') {
      setNodes(layoutGraph(nodes, edges, { direction }).nodes);
      setFitPending(true);
    } else {
      setNodes(placeNewNodes(nodes, edges, newIdsRef.current, direction));
    }
    newIdsRef.current = new Set();
    setLayoutPending(null);
  }, [layoutPending, allMeasured, nodes, edges, direction, fitView]);

  // Runs after the render that drew the new positions: React Flow's store,
  // updated by its own (child) effects, has them by now. Fitting in the next
  // animation frame instead raced that render, and now and then fitted the
  // targets still stacked at the origin: zoomed all the way in.
  useEffect(() => {
    if (!fitPending) return;
    fitView();
    setFitPending(false);
  }, [fitPending, nodes, fitView]);

  // New data from R (statuses every tick; sometimes a changed pipeline).
  useEffect(() => {
    const incomingNodes = configuration.nodes ?? [];
    const incomingEdges = configuration.edges ?? [];
    const structureChanged =
      structureKey(nodesRef.current, edgesRef.current) !==
      structureKey(incomingNodes, incomingEdges);

    setNodes((current) => syncNodes(current, incomingNodes));
    if (structureChanged) {
      const known = new Set(nodesRef.current.map((n) => n.id));
      const added = incomingNodes.filter((n) => !known.has(n.id)).map((n) => n.id);
      added.forEach((id) => newIdsRef.current.add(id));
      setEdges(incomingEdges);
      // Nothing placed yet: lay out everything. Otherwise place only the new.
      setLayoutPending((pending) =>
        pending === 'all' || known.size === 0 ? 'all' : added.length ? 'new' : pending,
      );
    }
  }, [configuration.nodes, configuration.edges]);

  // Options sent with a whole new configuration replace the current ones;
  // without any, the current ones stay.
  useEffect(() => {
    if (configuration.options) setOptions({ ...DEFAULT_OPTIONS, ...configuration.options });
  }, [configuration.options]);

  // update_targetsboard_options() changes only the options it names.
  useEffect(() => {
    const onOptions = (event) => {
      if (event.detail?.id !== configuration.id) return;
      setOptions((current) => ({ ...current, ...event.detail.options }));
    };
    window.addEventListener(OPTIONS_EVENT, onOptions);
    return () => window.removeEventListener(OPTIONS_EVENT, onOptions);
  }, [configuration.id]);

  // A focused target that left the pipeline is no longer focused.
  useEffect(() => {
    if (focusId && !nodes.some((n) => n.id === focusId)) setFocusId(null);
  }, [focusId, nodes]);

  // Patch statuses and warning marks in place: no new nodes, no re-layout.
  useEffect(() => {
    const onStatus = (event) => {
      const message = event.detail;
      if (message?.id !== configuration.id) return;
      const updates = new Map((message.nodes ?? []).map((u) => [u.id, u]));
      // `warnings`, when sent, lists every target that carries a warning mark.
      const warned = Array.isArray(message.warnings) ? new Set(message.warnings) : null;
      setNodes((current) =>
        current.map((node) => {
          const update = updates.get(node.id);
          const warning = warned ? warned.has(node.id) : node.data?.warning;
          if (!update && warning === node.data?.warning) return node;
          return {
            ...node,
            type: update ? update.type : node.type,
            data: {
              ...node.data,
              ...(update ? { target_status: update.target_status } : {}),
              warning,
            },
          };
        }),
      );
    };
    window.addEventListener(STATUS_EVENT, onStatus);
    return () => window.removeEventListener(STATUS_EVENT, onStatus);
  }, [configuration.id]);

  // Report focus and selection to Shiny, as <id>_focused_node and
  // <id>_selected_nodes. Keyed on the ids, so dragging nodes around doesn't
  // send anything.
  const selectedKey = sortedKey(nodes.filter((n) => n.selected).map((n) => n.id));
  const runningKey = sortedKey(nodes.filter((n) => n.type === 'running').map((n) => n.id));
  const edgesKey = sortedKey(edges.map((e) => e.id));

  useEffect(() => {
    if (!window.Shiny || !inputId) return;
    window.Shiny.setInputValue(`${inputId}_selected_nodes`, selectedKey ? selectedKey.split('|') : null);
  }, [inputId, selectedKey]);

  useEffect(() => {
    if (!window.Shiny || !inputId) return;
    window.Shiny.setInputValue(`${inputId}_focused_node`, focusId);
  }, [inputId, focusId]);

  // Animate the edges feeding running targets.
  useEffect(() => {
    const currentNodes = nodesRef.current;
    const running = currentNodes.filter((n) => n.type === 'running');
    const runningIds = new Set(running.map((n) => n.id));
    const incomerIds = new Set(
      running.flatMap((n) => getIncomers(n, currentNodes, edgesRef.current).map((i) => i.id)),
    );

    if (window.Shiny && inputId) {
      window.Shiny.setInputValue(`${inputId}_running_nodes`, [...runningIds]);
    }

    setEdges((current) => current.map((edge) => {
      const animated = incomerIds.has(edge.source) && runningIds.has(edge.target);
      return edge.animated === animated ? edge : { ...edge, animated };
    }));
  }, [inputId, runningKey, edgesKey]);

  // Focus highlighting is derived, never stored: dim what lies outside the
  // focused target's lineage.
  const lineage = focusId ? lineageOf(focusId, edges) : null;
  const shownNodes = lineage
    ? nodes.map((n) => ({
        ...n,
        className: n.id === focusId ? 'tb-focused' : lineage.has(n.id) ? undefined : 'tb-dimmed',
      }))
    : nodes;
  const shownEdges = edges.map((e) => ({
    ...e,
    type: options.edge_type,
    className: !lineage || (lineage.has(e.source) && lineage.has(e.target)) ? undefined : 'tb-dimmed',
  }));

  // The board's own colours are for light mode; dark mode keeps React Flow's.
  const colorMode = resolveColorMode(options.color_mode);
  const light = colorMode === 'light';

  return (
    <div
      className="reactflow-wrapper"
      ref={wrapperRef}
      style={{ height: configuration.height, width: configuration.width, position: 'relative' }}
    >
      <ReactFlow
        nodes={shownNodes}
        edges={shownEdges}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        onNodeClick={onNodeClick}
        onPaneClick={onPaneClick}
        onNodeDoubleClick={onNodeDoubleClick}
        onNodeContextMenu={onNodeContextMenu}
        onPaneContextMenu={closeMenu}
        onMoveStart={closeMenu}
        onMoveEnd={onMoveEnd}
        zoomOnDoubleClick={false} // a double-click is for the target
        nodesConnectable={false}
        deleteKeyCode={null} // read-only: Backspace and Delete remove nothing
        fitView
        minZoom={0.05} // fit big pipelines whole, as tar_visnetwork() does
        colorMode={colorMode}
        onlyRenderVisibleElements={options.only_render_visible}
        style={light ? { background: configuration.background_color } : undefined}
        nodeTypes={nodeTypes}
      >
        {options.minimap && (
          <MiniMap nodeColor={(n) => STATUS_COLORS[n.type] ?? STATUS_COLORS.outdated} pannable zoomable />
        )}
        {options.background !== 'none' && (
          <Background
            id="1"
            gap={15}
            variant={BACKGROUND_VARIANTS[options.background]}
            color={light ? configuration.background_marks_color : undefined}
            size={1}
          />
        )}
        {options.controls && <Controls />}
        <LayoutButtons onLayout={onLayout} />
      </ReactFlow>
      {menu && options.node_actions.length > 0 && (
        <NodeMenu key={`${menu.node}@${menu.x},${menu.y}`} menu={menu} actions={options.node_actions} onAction={onMenuAction} onClose={closeMenu} />
      )}
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
