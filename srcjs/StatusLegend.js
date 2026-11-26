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

// The time and size badges' switches, as the author left them.
const COSTS_KEY = 'targetsboard.costs';
export const readCostsShown = () => {
  const saved = storage((s) => s.getItem(COSTS_KEY));
  try {
    const parsed = saved ? JSON.parse(saved) : {};
    return { times: parsed.times === true, sizes: parsed.sizes === true };
  } catch (e) {
    return { times: false, sizes: false };
  }
};
export const writeCostsShown = (shown) => storage((s) => s.setItem(COSTS_KEY, JSON.stringify(shown)));

// Functions and globals (targets_only = FALSE) have a status too, but the
// legend counts targets.
const isTarget = (node) => !['function', 'object'].includes(node.data?.target_type);

// What each status's colour means, with how many targets have it; a click
// on one selects them. Under them, the switches of the targets' time and
// size badges. The colours are the nodes' own (statusColor()), so
// the legend follows the theme and the mode by itself.
const StatusLegend = ({ nodes, statuses, statusColor, onSelect, costsShown, onToggleCost }) => {
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
      {open && costsShown && (
        <div className="tb-legend-costs">
          {[['times', 'Times', 'Each built target\'s last build time'],
            ['sizes', 'Sizes', 'Each built target\'s stored size']].map(([part, label, title]) => (
            <label key={part} className="tb-legend-switch" title={title}>
              <input
                type="checkbox"
                role="switch"
                data-part={part}
                checked={costsShown[part]}
                onChange={() => onToggleCost(part)}
              />
              {label}
            </label>
          ))}
        </div>
      )}
    </div>
  );
};

export default StatusLegend;
