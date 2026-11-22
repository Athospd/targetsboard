# Take two's music: upbeat synth-funk, synthesized from scratch (no samples,
# no licences). 118 BPM in A minor (Am - F - C - G): four-on-the-floor kick,
# claps on 2 and 4, driving hats, plucky detuned-saw stabs, a pumping
# off-beat bass ducked by the kick, an arpeggio hook in the full sections,
# and noise risers into each 8-bar phrase.
#
#   python demo/v2/soundtrack.py <seconds> <out.wav>
import sys
import wave

import numpy as np

SR = 44100
LENGTH = float(sys.argv[1]) if len(sys.argv) > 1 else 115.0
OUT = sys.argv[2] if len(sys.argv) > 2 else "soundtrack-v2.wav"
BPM = 118
beat = 60 / BPM
bar = 4 * beat
s16 = beat / 4
n = int(LENGTH * SR)
rng = np.random.default_rng(11)
L = np.zeros(n); R = np.zeros(n)
duck = np.ones(n)  # the sidechain envelope, from the kicks


def midi(m): return 440.0 * 2 ** ((m - 69) / 12)


def place(sig, t, gain=1.0, pan=0.0, ducked=False):
    i = int(t * SR)
    if i >= n or i < 0: return
    sig = sig[: n - i] * gain
    if ducked: sig = sig * duck[i:i + len(sig)]
    L[i:i + len(sig)] += sig * np.sqrt(0.5 * (1 - pan))
    R[i:i + len(sig)] += sig * np.sqrt(0.5 * (1 + pan))


def fft_filter(x, lo=None, hi=None):
    X = np.fft.rfft(x); f = np.fft.rfftfreq(len(x), 1 / SR)
    g = np.ones_like(f)
    if hi: g *= 1 / np.sqrt(1 + (f / hi) ** 4)
    if lo: g *= 1 / np.sqrt(1 + (lo / np.maximum(f, 1e-3)) ** 4)
    return np.fft.irfft(X * g, len(x))


def saw(freq, t, harmonics=24, bright=1.0):
    out = np.zeros_like(t)
    for k in range(1, harmonics + 1):
        if k * freq > 12000: break
        out += np.sin(2 * np.pi * k * freq * t) / k * np.exp(-(k - 1) * (0.18 / bright))
    return out


# ---- Instruments ------------------------------------------------------------------------
def kick():
    t = np.arange(int(0.32 * SR)) / SR
    f = 48 + 110 * np.exp(-t * 35)
    body = np.sin(2 * np.pi * np.cumsum(f) / SR) * np.exp(-t * 9)
    click = rng.standard_normal(len(t)) * np.exp(-t * 400) * 0.25
    return np.tanh((body + click) * 1.8)


def clap():
    t = np.arange(int(0.3 * SR)) / SR
    noise = fft_filter(rng.standard_normal(len(t)), lo=900, hi=5000)
    env = np.zeros_like(t)
    for d in (0, 0.011, 0.022):
        env += (t >= d) * np.exp(-np.maximum(t - d, 0) * 60)
    env += (t >= 0.03) * np.exp(-np.maximum(t - 0.03, 0) * 14) * 0.6
    return noise * env


def hat(open_=False):
    t = np.arange(int((0.22 if open_ else 0.045) * SR)) / SR
    return fft_filter(rng.standard_normal(len(t)), lo=8000) * np.exp(-t * (14 if open_ else 90))


def stab(notes, dur):
    t = np.arange(int(dur * SR)) / SR
    env = (1 - np.exp(-t * 400)) * np.exp(-t * 9)
    sig = np.zeros_like(t)
    for m in notes:
        for det in (-0.12, 0.0, 0.12):  # detuned voices
            sig += saw(midi(m + det), t, harmonics=18, bright=0.7 + 1.5 * np.exp(-0))
    sig = fft_filter(sig, hi=3800)
    return sig * env / (len(notes) * 3)


def bass(m, dur):
    t = np.arange(int(dur * SR)) / SR
    env = (1 - np.exp(-t * 300)) * np.exp(-t * 7)
    return fft_filter(saw(midi(m), t, harmonics=14), hi=900) * env + 0.6 * np.sin(2 * np.pi * midi(m) * t) * env


def pluck(m, dur):
    t = np.arange(int(dur * SR)) / SR
    env = (1 - np.exp(-t * 500)) * np.exp(-t * 11)
    sq = np.sign(np.sin(2 * np.pi * midi(m) * t)) * 0.5 + np.sin(2 * np.pi * midi(m) * t) * 0.5
    return fft_filter(sq, hi=5000) * env


def riser(dur):
    t = np.arange(int(dur * SR)) / SR
    noise = rng.standard_normal(len(t))
    # sweep: blend from a dull to a bright noise, swelling
    dull, bright = fft_filter(noise, hi=900), fft_filter(noise, lo=3000)
    f = (t / dur) ** 2
    return (dull * (1 - f) + bright * f) * f * 0.9


# ---- Arrangement ---------------------------------------------------------------------------
progression = [  # (stab notes, bass note)
    ([69, 72, 76], 45),  # Am
    ([65, 69, 72], 41),  # F
    ([67, 72, 76], 48),  # C
    ([67, 71, 74], 43),  # G
]
stab_steps = [0, 3, 6, 10, 12]  # syncopated 16ths
arp = [0, 7, 12, 7, 3, 7, 12, 15]  # over the chord's root, in 8ths (minor feel)
bars = int(LENGTH / bar) + 1
K, C = kick(), clap()

# Sidechain first: duck after every kick.
for b in range(bars):
    for q in range(4):
        i = int((b * bar + q * beat) * SR)
        if b >= 1 and i < n:
            m = min(n - i, int(beat * SR))
            tt = np.arange(m) / SR
            duck[i:i + m] = np.minimum(duck[i:i + m], 1 - 0.6 * np.exp(-tt * 9))

for b in range(bars):
    t0 = b * bar
    notes, root = progression[b % 4]
    section = "intro" if b < 2 else ("break" if (b // 8) % 4 == 2 and b % 8 < 4 else "full")
    outro = t0 > LENGTH - 2 * bar
    # Stabs, brighter in full sections.
    for st in stab_steps:
        place(stab(notes, s16 * 2.2), t0 + st * s16, 0.55 if section != "intro" else 0.35, pan=0.0, ducked=True)
    if section != "intro":
        # Bass: off-beat 8ths, octave jumps.
        for e in range(8):
            if e % 2 == 1:
                m = root + (12 if e in (3, 7) else 0)
                place(bass(m - 12 + 12, beat * 0.45), t0 + e * beat / 2, 0.42, ducked=True)
    if section == "full" and not outro:
        for q in range(4):
            place(K, t0 + q * beat, 0.8)
        for q in (1, 3):
            place(C, t0 + q * beat, 0.32, pan=0.05)
    elif section == "break" and not outro:
        place(K, t0, 0.6)
        place(C, t0 + 3 * beat, 0.25)
    # Hats: 16ths, accented off-beats, an open hat at each bar's end.
    for h in range(16):
        if section == "intro" and h % 2: continue
        acc = 0.11 if h % 4 == 2 else 0.05
        place(hat(open_=(h == 14 and section == "full")), t0 + h * s16, acc + 0.02 * rng.random(), pan=0.3)
    # The hook: an arpeggio in the full sections, every other bar pair.
    if section == "full" and b >= 4 and (b // 2) % 2 == 0 and not outro:
        for e, step in enumerate(arp):
            place(pluck(root + 24 + step, beat * 0.5), t0 + e * beat / 2, 0.16, pan=-0.25 + 0.5 * (e % 2), ducked=True)
    # A riser into every 8-bar phrase.
    if b % 8 == 7 and not outro:
        place(riser(bar), t0, 0.12, pan=0.0)

# Master: a touch of saturation, fades, normalise.
L = fft_filter(L, lo=28, hi=15000); R = fft_filter(R, lo=28, hi=15000)
mix = np.stack([L, R], axis=1)
mix = np.tanh(mix * 1.3) / 1.3
t = np.arange(n) / SR
mix *= (np.minimum(1, t / 0.4) * np.minimum(1, (LENGTH - t) / 3))[:, None]
mix *= 0.9 / np.max(np.abs(mix))
with wave.open(OUT, "wb") as w:
    w.setnchannels(2); w.setsampwidth(2); w.setframerate(SR)
    w.writeframes((mix * 32767).astype("<i2").tobytes())
print(OUT, f"{LENGTH:.1f} s")
