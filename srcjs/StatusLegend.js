import React, { useCallback, useMemo, useState } from 'react';

// The statuses' names, in the legend's order.
const LABELS = {
  uptodate: 'Up to date',
  outdated: 'Outdated',
  errored: 'Errored',
  running: 'Running',
};

// Open or closed, as the author left it: the browser remembers.
const KEY = 'targetsboard.legend';
const storage = (use) => {
  try {
    return use(window.localStorage);
  } catch (e) {
    return null; // storage off (private window, file://): open every time
  }
};

// Functions and globals (targets_only = FALSE) have a status too, but the
// legend counts targets.
const isTarget = (node) => !['function', 'object'].includes(node.data?.target_type);

// What each status's colour means, with how many targets have it; a click
// on one selects them. The colours are the nodes' own (statusColor()), so
// the legend follows the theme and the mode by itself.
const StatusLegend = ({ nodes, statuses, statusColor, onSelect }) => {
  const [open, setOpen] = useState(() => storage((s) => s.getItem(KEY)) !== 'closed');
  const toggle = useCallback(() => {
    setOpen((was) => {
      storage((s) => s.setItem(KEY, was ? 'closed' : 'open'));
      return !was;
    });
  }, []);

  const byStatus = useMemo(() => {
    const ids = Object.fromEntries(statuses.map((status) => [status, []]));
    nodes.forEach((node) => {
      if (isTarget(node) && ids[node.type]) ids[node.type].push(node.id);
    });
    return ids;
  }, [nodes, statuses]);

  return (
    <div className="tb-legend">
      <button type="button" className="tb-legend-toggle" aria-expanded={open} onClick={toggle}>
        Legend
        <span className="tb-legend-chevron" aria-hidden="true">{open ? '▴' : '▾'}</span>
      </button>
      {open && (
        <ul className="tb-legend-list">
          {statuses.map((status) => {
            const ids = byStatus[status];
            const n = ids.length;
            return (
              <li key={status}>
                <button
                  type="button"
                  className="tb-legend-entry"
                  data-status={status}
                  disabled={n === 0}
                  onClick={() => onSelect(ids)}
                  title={n === 0 ? `No ${LABELS[status].toLowerCase()} targets` :
                    `Select the ${n} ${LABELS[status].toLowerCase()} target${n === 1 ? '' : 's'}`}
                >
                  <span className="tb-legend-swatch" style={{ background: statusColor(status) }} />
                  <span className="tb-legend-label">{LABELS[status]}</span>
                  <span className="tb-legend-count">{n}</span>
                </button>
              </li>
            );
          })}
        </ul>
      )}
    </div>
  );
};

export default StatusLegend;
