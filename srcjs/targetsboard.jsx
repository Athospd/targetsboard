import { reactShinyInput } from 'reactR';
import { useEffect, useLayoutEffect, useCallback, useMemo, useRef, useState } from 'react';
import {
  ReactFlow,
  ReactFlowProvider,
  MiniMap,
  Background,
  BackgroundVariant,
  Controls,
  ControlButton,
  getNodesBounds,
  applyNodeChanges,
  applyEdgeChanges,
  getIncomers,
  useReactFlow,
} from '@xyflow/react';
import { toPng, toSvg } from 'html-to-image';
import '@xyflow/react/dist/style.css';
import './targetsboardCustomCSS.css';
import LayoutButtons from './LayoutButtons';
import { layoutGraph } from './layout';
import { NodeActionsContext, GroupToolbar, groupLabel } from './NodeActions';

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
  toolbar_actions: [], // [{id, label}]: the focused target's toolbar
  node_group_actions: [], // the menu of a selected group; `{n}` in a label: its size
  toolbar_group_actions: [], // the toolbar over a selected group
  download_image: false, // a download button in the zoom controls
};

// An image of the whole graph, whatever is in view: the viewport element,
// drawn at one pixel per unit with a margin, the toolbar left out. A data
// URL: PNG (at twice the resolution) or SVG.
const IMAGE_MARGIN = 40;
const DARK_BACKGROUND = '#141414'; // React Flow's own, in dark mode
const graphImage = (wrapper, nodes, format, background) => {
  const viewport = wrapper?.querySelector('.react-flow__viewport');
  if (!viewport || nodes.length === 0) return Promise.reject(new Error('Nothing to draw.'));
  const bounds = getNodesBounds(nodes);
  const width = Math.ceil(bounds.width + 2 * IMAGE_MARGIN);
  const height = Math.ceil(bounds.height + 2 * IMAGE_MARGIN);
  const render = format === 'svg' ? toSvg : toPng;
  return render(viewport, {
    backgroundColor: background,
    width,
    height,
    pixelRatio: 2,
    // Leave the page's web fonts out: embedding them made an SVG of eight
    // targets weigh 5.6 MB. The labels fall back to the font stack's system fonts.
    skipFonts: true,
    filter: (el) => !el.classList?.contains('react-flow__node-toolbar'),
    style: {
      width: `${width}px`,
      height: `${height}px`,
      transform: `translate(${IMAGE_MARGIN - bounds.x}px, ${IMAGE_MARGIN - bounds.y}px) scale(1)`,
    },
  });
};

const downloadIcon = (
  <svg viewBox="0 0 24 24" width="12" height="12" aria-hidden="true">
    <path d="M12 3v11m0 0l-4-4m4 4l4-4M4 17v3h16v-3" fill="none" stroke="currentColor" strokeWidth="2.5" />
  </svg>
);

// Report an event to Shiny: with priority 'event', the same value twice
// (double-clicking the same target again) still reaches the server.
const sendEvent = (name, value) =>
  window.Shiny?.setInputValue?.(name, value, { priority: 'event' });

// Target names as R code: "model", or c("a", "b") for several.
const rNames = (ids) => {
  const quoted = ids.map((id) => JSON.stringify(id));
  return quoted.length === 1 ? quoted[0] : `c(${quoted.join(', ')})`;
};

// The right-click menu of a target, or of the selected group it is part of
// (`menu.nodes`). `copy_name` and the selection actions are done here: the
// server can't reach the clipboard, and selecting is the widget's own. Every
// other action goes to Shiny as <id>_node_action.
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
          onClick={() => onAction(action.id, menu.node, menu.nodes)}
        >
          {menu.nodes.length > 1 ? groupLabel(action.label, menu.nodes.length) : action.label}
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

// Targets `ids` with every target reached from them along the edges, from
// `from` to `to`: all levels, not only the direct neighbours.
const reachable = (ids, edges, from, to) => {
  const next = new Map();
  edges.forEach((e) => {
    if (!next.has(e[from])) next.set(e[from], []);
    next.get(e[from]).push(e[to]);
  });
  const seen = new Set(ids);
  const stack = [...ids];
  while (stack.length) {
    (next.get(stack.pop()) ?? []).forEach((id) => {
      if (!seen.has(id)) {
        seen.add(id);
        stack.push(id);
      }
    });
  }
  return seen;
};
const upstreamOf = (ids, edges) => reachable(ids, edges, 'target', 'source');
const downstreamOf = (ids, edges) => reachable(ids, edges, 'source', 'target');
// Targets with everything upstream and downstream of them.
const lineageOf = (ids, edges) => new Set([...upstreamOf(ids, edges), ...downstreamOf(ids, edges)]);

// The menu's selection actions: what to select, from the targets it covers.
const SELECT_ACTIONS = {
  select_upstream: upstreamOf,
  select_downstream: downstreamOf,
  select_lineage: lineageOf,
};

// Light updates from R arrive as Shiny custom messages: statuses
// (update_targetsboard_status()) and display options
// (update_targetsboard_options()). Each widget picks the ones addressed to
// its id.
const STATUS_EVENT = 'targetsboard:status';
const OPTIONS_EVENT = 'targetsboard:options';
// Commands from R: focus a target (update_targetsboard_focus()), select
// targets (update_targetsboard_selection()), fit the view to targets
// (targetsboard_fit_view()), draw an image (targetsboard_export_image()).
const COMMAND_EVENT = 'targetsboard:command';
if (window.Shiny && !window.__targetsboardStatusHandler) {
  window.__targetsboardStatusHandler = true;
  [STATUS_EVENT, OPTIONS_EVENT, COMMAND_EVENT].forEach((type) =>
    window.Shiny.addCustomMessageHandler(type, (message) => {
      window.dispatchEvent(new CustomEvent(type, { detail: message }));
    }),
  );
}

// Run `fn` once `el` is shown and sized: a widget in a hidden tab has no size
// to fit a view to, so wait (up to 5 s) for its tab to show, then give
// React Flow two frames to notice the new size.
const whenVisible = (el, fn, deadline = Date.now() + 5000) => {
  if (!el) return;
  if (el.offsetWidth > 0 && el.offsetHeight > 0) {
    requestAnimationFrame(() => requestAnimationFrame(fn));
  } else if (Date.now() < deadline) {
    setTimeout(() => whenVisible(el, fn, deadline), 50);
  }
};

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

// The positions of a layout given by R (targetsboard(layout = )), checked:
// a Map of target id to {x, y}.
const savedPositions = (layout) =>
  new Map(
    Object.entries(layout?.positions ?? {}).filter(
      ([, p]) => Number.isFinite(p?.x) && Number.isFinite(p?.y),
    ).map(([id, p]) => [id, { x: p.x, y: p.y }]),
  );
const savedViewport = (viewport) =>
  ['x', 'y', 'zoom'].every((k) => Number.isFinite(viewport?.[k])) ? viewport : null;

function Flow({ configuration }) {
  const { fitView, getNodes, getViewport, setViewport } = useReactFlow();
  const [nodes, setNodes] = useState(configuration.nodes ?? []);
  const [edges, setEdges] = useState(configuration.edges ?? []);
  // A saved layout is read once, when the widget is first drawn.
  const savedLayout = useRef(configuration.layout);
  const [direction, setDirection] = useState(
    ['LR', 'TB'].includes(configuration.layout?.direction) ? configuration.layout.direction : 'LR',
  );
  const directionRef = useRef(direction);
  directionRef.current = direction;
  const [focusId, setFocusId] = useState(null);
  const focusRef = useRef(focusId);
  focusRef.current = focusId;
  const [options, setOptions] = useState({ ...DEFAULT_OPTIONS, ...configuration.options });
  // Lay out once all nodes have been measured: everything ('all') on first
  // render and when a Tidy button is pressed; only the new targets ('new')
  // when the pipeline's structure changes. With a saved layout, the first
  // render puts the targets back where they were ('restore').
  const [layoutPending, setLayoutPending] = useState(
    savedPositions(configuration.layout).size > 0 ? 'restore' : 'all',
  );
  // Once the laid-out targets are drawn: true fits the view to the graph, a
  // viewport ({x, y, zoom}) restores that one.
  const [fitPending, setFitPending] = useState(false);
  // Report the layout once the new positions are drawn.
  const [reportPending, setReportPending] = useState(false);
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
  const [menu, setMenu] = useState(null); // {node, nodes, x, y}
  const menuRef = useRef(menu);
  menuRef.current = menu;
  // Select exactly these targets.
  const selectOnly = useCallback((ids) => {
    const chosen = new Set(ids);
    setNodes((nds) =>
      nds.map((n) => (!!n.selected === chosen.has(n.id) ? n : { ...n, selected: chosen.has(n.id) })),
    );
  }, []);
  const wrapperRef = useRef(null);
  const colorModeRef = useRef('light');
  const imageOf = useCallback(
    (format) =>
      graphImage(
        wrapperRef.current,
        getNodes(),
        format,
        colorModeRef.current === 'light' ? configuration.background_color : DARK_BACKGROUND,
      ),
    [getNodes, configuration.background_color],
  );
  const downloadImage = useCallback(() => {
    imageOf('png')
      .then((url) => {
        const link = document.createElement('a');
        link.download = 'targets-graph.png';
        link.href = url;
        link.click();
      })
      .catch(() => {});
  }, [imageOf]);
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
  // gave it actions. On a target of a selected group, the menu is the
  // group's; on any other, that target alone becomes the selection.
  const onNodeContextMenu = useCallback(
    (event, node) => {
      event.preventDefault();
      const selected = nodesRef.current.filter((n) => n.selected).map((n) => n.id);
      const nodes = selected.length > 1 && selected.includes(node.id) ? selected.sort() : [node.id];
      if (nodes.length === 1) selectOnly(nodes);
      setFocusId(node.id);
      sendEvent(`${inputId}_context_node`, node.id);
      const box = wrapperRef.current?.getBoundingClientRect() ?? { left: 0, top: 0 };
      setMenu({ node: node.id, nodes, x: event.clientX - box.left, y: event.clientY - box.top });
    },
    [inputId, selectOnly],
  );
  // Right-click on the box around a Shift-drag selection: the group's menu,
  // for the focused target if it is in the group.
  const onSelectionContextMenu = useCallback(
    (event, selection) => {
      const ids = selection.map((n) => n.id);
      if (ids.length === 0) return;
      onNodeContextMenu(event, { id: ids.includes(focusRef.current) ? focusRef.current : ids[0] });
    },
    [onNodeContextMenu],
  );
  // An action of a menu or a toolbar, on `node` or, for a group, on `nodes`.
  const onMenuAction = useCallback(
    (action, node, nodes = [node]) => {
      setMenu(null);
      if (action === 'copy_name') {
        navigator.clipboard?.writeText(rNames(nodes)).catch(() => {});
        return;
      }
      if (SELECT_ACTIONS[action]) {
        selectOnly([...SELECT_ACTIONS[action](nodes, edgesRef.current)]);
        return;
      }
      sendEvent(`${inputId}_node_action`, { action, node, nodes });
    },
    [inputId, selectOnly],
  );
  const closeMenu = useCallback(() => setMenu(null), []);
  // The layout, for R to keep: positions by target, the view, the direction.
  const reportLayout = useCallback(() => {
    const positions = {};
    getNodes().forEach((n) => {
      positions[n.id] = { x: n.position.x, y: n.position.y };
    });
    window.Shiny?.setInputValue?.(`${inputId}_layout`, {
      direction: directionRef.current,
      viewport: getViewport(),
      positions,
    });
  }, [inputId, getNodes, getViewport]);
  // Where the view is, after each pan or zoom.
  const onMoveEnd = useCallback(
    (_, viewport) => {
      window.Shiny?.setInputValue?.(`${inputId}_viewport`, viewport);
      reportLayout();
    },
    [inputId, reportLayout],
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
    const saved = layoutPending === 'restore' ? savedPositions(savedLayout.current) : null;
    const unsaved = saved && new Set(nodes.filter((n) => !saved.has(n.id)).map((n) => n.id));
    if (layoutPending === 'all' || unsaved?.size === nodes.length) {
      setNodes(layoutGraph(nodes, edges, { direction }).nodes);
      setFitPending(true);
    } else if (saved) {
      // Saved targets where they were; targets the layout doesn't know (new
      // in the pipeline since) next to their neighbours. The saved view only
      // if it shows every target as it was.
      const restored = nodes.map((n) =>
        saved.has(n.id) ? { ...n, position: saved.get(n.id), ...handlePositions(direction) } : n,
      );
      setNodes(unsaved.size ? placeNewNodes(restored, edges, unsaved, direction) : restored);
      setFitPending((unsaved.size === 0 && savedViewport(savedLayout.current?.viewport)) || true);
    } else {
      setNodes(placeNewNodes(nodes, edges, newIdsRef.current, direction));
      setReportPending(true);
    }
    newIdsRef.current = new Set();
    setLayoutPending(null);
  }, [layoutPending, allMeasured, nodes, edges, direction]);

  // Runs after the render that drew the new positions: React Flow's store,
  // updated by its own (child) effects, has them by now. Fitting in the next
  // animation frame instead raced that render, and now and then fitted the
  // targets still stacked at the origin: zoomed all the way in.
  useEffect(() => {
    if (!fitPending) return;
    if (fitPending === true) fitView();
    else setViewport(fitPending);
    setFitPending(false);
    setReportPending(true);
  }, [fitPending, nodes, fitView, setViewport]);

  useEffect(() => {
    if (!reportPending) return;
    setReportPending(false);
    requestAnimationFrame(reportLayout);
  }, [reportPending, reportLayout]);

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
      // Nothing placed yet: lay out everything (or restore the saved
      // layout). Otherwise place only the new.
      setLayoutPending((pending) =>
        pending === 'all' || pending === 'restore'
          ? pending
          : known.size === 0 ? 'all' : added.length ? 'new' : pending,
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

  // Commands from R.
  useEffect(() => {
    const onCommand = (event) => {
      const message = event.detail;
      if (message?.id !== configuration.id) return;
      if (message.command === 'focus') {
        // As a click does: the target alone is selected, so a group selected
        // before doesn't keep the highlighting and the toolbar from it.
        setFocusId(message.node ?? null);
        selectOnly(message.node ? [message.node] : []);
        setMenu(null);
      } else if (message.command === 'select') {
        const known = new Set(nodesRef.current.map((n) => n.id));
        selectOnly((message.nodes ?? []).filter((id) => known.has(id)));
        setMenu(null);
      } else if (message.command === 'export_image') {
        // The image goes back to R as <id>_image, or the reason it failed.
        const format = message.format === 'svg' ? 'svg' : 'png';
        imageOf(format)
          .then((data_url) => sendEvent(`${configuration.id}_image`, { format, data_url }))
          .catch((e) => sendEvent(`${configuration.id}_image`, { format, error: String(e?.message ?? e) }));
      } else if (message.command === 'fit_view') {
        const known = new Set(nodesRef.current.map((n) => n.id));
        const ids = (message.nodes ?? []).filter((id) => known.has(id));
        if (message.nodes && ids.length === 0) return; // none of them drawn
        const fit = () =>
          fitView({
            nodes: message.nodes ? ids.map((id) => ({ id })) : undefined,
            padding: message.padding,
            duration: message.duration,
            maxZoom: message.max_zoom,
          });
        whenVisible(wrapperRef.current, fit);
      }
    };
    window.addEventListener(COMMAND_EVENT, onCommand);
    return () => window.removeEventListener(COMMAND_EVENT, onCommand);
  }, [configuration.id, fitView, imageOf, selectOnly]);

  // Escape closes the menu, if one is open (the menu does it); else it
  // clears the focus and the selection. Not while typing elsewhere on the
  // page, or in a dialog.
  useEffect(() => {
    const onKey = (event) => {
      if (event.key !== 'Escape' || menuRef.current) return;
      if (event.target?.closest?.('input, textarea, select, [contenteditable="true"], .modal')) return;
      const unselect = (items) =>
        items.some((x) => x.selected) ? items.map((x) => (x.selected ? { ...x, selected: false } : x)) : items;
      setFocusId(null);
      setNodes(unselect);
      setEdges(unselect);
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, []);

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
      // `branches`, when sent, has the branch counts of every pattern that
      // shows one, by id; an empty one (which may arrive as []) clears them.
      const counts = message.branches == null ? null
        : (Array.isArray(message.branches) ? {} : message.branches);
      setNodes((current) =>
        current.map((node) => {
          const update = updates.get(node.id);
          const warning = warned ? warned.has(node.id) : node.data?.warning;
          const branches = counts ? (counts[node.id] ?? null) : (node.data?.branches ?? null);
          const sameBranches = JSON.stringify(branches) === JSON.stringify(node.data?.branches ?? null);
          if (!update && warning === node.data?.warning && sameBranches) return node;
          return {
            ...node,
            type: update ? update.type : node.type,
            data: {
              ...node.data,
              ...(update ? { target_status: update.target_status } : {}),
              warning,
              branches,
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

  // A selected group: two targets or more. It takes over from the focused
  // target for the toolbar and the highlighting.
  const group = useMemo(() => (selectedKey.includes('|') ? selectedKey.split('|') : null), [selectedKey]);
  const toolbarFocus = group ? null : focusId;
  const nodeActions = useMemo(
    () => ({ focusId: toolbarFocus, actions: options.toolbar_actions, onAction: onMenuAction }),
    [toolbarFocus, options.toolbar_actions, onMenuAction],
  );

  // Highlighting is derived, never stored: dim what lies outside the lineage
  // of the selected group, or else of the focused target.
  const marked = group ?? (focusId ? [focusId] : null);
  const markedIds = new Set(marked ?? []);
  const lineage = marked ? lineageOf(marked, edges) : null;
  const shownNodes = lineage
    ? nodes.map((n) => ({
        ...n,
        className: markedIds.has(n.id) ? 'tb-focused' : lineage.has(n.id) ? undefined : 'tb-dimmed',
      }))
    : nodes;
  const shownEdges = edges.map((e) => ({
    ...e,
    type: options.edge_type,
    className: !lineage || (lineage.has(e.source) && lineage.has(e.target)) ? undefined : 'tb-dimmed',
  }));

  const menuActions = menu?.nodes.length > 1 ? options.node_group_actions : options.node_actions;

  // The board's own colours are for light mode; dark mode keeps React Flow's.
  const colorMode = resolveColorMode(options.color_mode);
  const light = colorMode === 'light';
  colorModeRef.current = colorMode;

  return (
    <div
      className="reactflow-wrapper"
      ref={wrapperRef}
      style={{ height: configuration.height, width: configuration.width, position: 'relative' }}
    >
      <NodeActionsContext.Provider value={nodeActions}>
      <ReactFlow
        nodes={shownNodes}
        edges={shownEdges}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        onNodeClick={onNodeClick}
        onPaneClick={onPaneClick}
        onNodeDoubleClick={onNodeDoubleClick}
        onNodeContextMenu={onNodeContextMenu}
        onSelectionContextMenu={onSelectionContextMenu}
        onPaneContextMenu={closeMenu}
        onMoveStart={closeMenu}
        onMoveEnd={onMoveEnd}
        onNodeDragStop={reportLayout}
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
        {options.controls && (
          <Controls>
            {options.download_image && (
              <ControlButton onClick={downloadImage} title="Download image" aria-label="Download image"
                className="tb-download-image">
                {downloadIcon}
              </ControlButton>
            )}
          </Controls>
        )}
        <LayoutButtons onLayout={onLayout} />
        <GroupToolbar
          ids={group}
          node={group?.includes(focusId) ? focusId : group?.[0]}
          actions={options.toolbar_group_actions}
          onAction={onMenuAction}
        />
      </ReactFlow>
      </NodeActionsContext.Provider>
      {menu && menuActions.length > 0 && (
        <NodeMenu key={`${menu.node}@${menu.x},${menu.y}`} menu={menu} actions={menuActions} onAction={onMenuAction} onClose={closeMenu} />
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
