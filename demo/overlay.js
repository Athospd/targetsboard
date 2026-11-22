(() => {
const css = document.createElement('style');
css.textContent = `
#demo-cursor{position:fixed;left:0;top:0;width:22px;height:22px;z-index:2147483647;pointer-events:none;
  filter:drop-shadow(0 2px 3px rgba(0,0,0,.35))}
.demo-ripple{position:fixed;width:34px;height:34px;margin:-17px 0 0 -17px;border-radius:50%;z-index:2147483646;
  pointer-events:none;border:3px solid #d49536;animation:demo-ripple .6s ease-out forwards}
@keyframes demo-ripple{from{transform:scale(.3);opacity:1}to{transform:scale(1.4);opacity:0}}
#demo-caption{position:fixed;left:50%;bottom:30px;transform:translate(-50%,12px);z-index:2147483645;pointer-events:none;
  background:rgba(47,34,61,.94);color:#f7efe4;border-radius:16px;padding:11px 24px 13px;text-align:center;
  font-family:'Segoe UI',Helvetica,Arial,sans-serif;opacity:0;transition:opacity .45s,transform .45s;
  box-shadow:0 10px 30px rgba(0,0,0,.25);max-width:900px}
#demo-caption.on{opacity:1;transform:translate(-50%,0)}
#demo-caption .k{font-size:10.5px;letter-spacing:.18em;text-transform:uppercase;color:#f2c94c;font-weight:700;margin-bottom:4px}
#demo-caption .t{font-size:21px;font-weight:600}
#demo-caption code{font-family:Consolas,monospace;color:#9fd3df}
#demo-code{position:fixed;left:50%;top:42%;transform:translate(-50%,-50%) scale(.96);z-index:2147483645;pointer-events:none;
  background:#1f1a29;color:#e8e2f0;border-radius:14px;padding:18px 26px 22px;font:15px/1.6 Consolas,monospace;
  box-shadow:0 18px 50px rgba(0,0,0,.45);opacity:0;transition:opacity .4s,transform .4s}
#demo-code.on{opacity:1;transform:translate(-50%,-50%) scale(1)}
#demo-code .f{color:#a99fb8;font-size:12px;margin-bottom:8px}
#demo-code .m{background:rgba(201,51,18,.28);color:#ffb6a6;display:block;padding:0 8px}
#demo-code .p{background:rgba(53,72,35,.55);color:#d3f0b7;display:block;padding:0 8px}
`;
document.head.appendChild(css);
const c = document.createElement('div'); c.id = 'demo-cursor';
c.innerHTML = '<svg viewBox="0 0 24 24" width="22" height="22"><path d="M3 2 L3 19 L7.5 14.8 L10.6 21.5 L13.4 20.3 L10.3 13.7 L16.5 13.7 Z" fill="#fff" stroke="#1d1d1d" stroke-width="1.4" stroke-linejoin="round"/></svg>';
c.style.transform = 'translate(1150px, 660px)';
document.body.appendChild(c);
const cap = document.createElement('div'); cap.id = 'demo-caption';
cap.innerHTML = '<div class="k"></div><div class="t"></div>';
document.body.appendChild(cap);
const code = document.createElement('div'); code.id = 'demo-code';
document.body.appendChild(code);
})();
