# Take four's music: bright future-house, synthesized from scratch (no
# samples, no licences). 122 BPM in D major (D - A - Bm - G): wide supersaw
# chords pumping under a four-on-the-floor kick, a sub bass on the roots, a
# marimba-like FM lead over an eight-bar melody, shakers, claps on 2 and 4,
# and a filtered intro that opens into the full mix, with a riser before
# each eight-bar phrase. It ends on a ringing D chord.
#
#   python demo/v4/soundtrack.py <seconds> <out.wav>
import sys
import wave

import numpy as np

SR = 44100
LENGTH = float(sys.argv[1]) if len(sys.argv) > 1 else 96.0
OUT = sys.argv[2] if len(sys.argv) > 2 else "soundtrack-v4.wav"
BPM = 122
beat = 60 / BPM
bar = 4 * beat
s16 = beat / 4
n = int(LENGTH * SR)
rng = np.random.default_rng(4)
L = np.zeros(n); R = np.zeros(n)
duck = np.ones(n)  # the sidechain envelope, from the kicks


def midi(m): return 440.0 * 2 ** ((m - 69) / 12)


def place(sig, t, gain=1.0, pan=0.0, ducked=False, stereo=None):
    i = int(t * SR)
    if i >= n or i < 0: return
    if stereo is not None:
        l, r = stereo[0][: n - i] * gain, stereo[1][: n - i] * gain
    else:
        l = r = sig[: n - i] * gain
    if ducked:
        l = l * duck[i:i + len(l)]; r = r * duck[i:i + len(r)]
    L[i:i + len(l)] += l * np.sqrt(0.5 * (1 - pan))
    R[i:i + len(r)] += r * np.sqrt(0.5 * (1 + pan))


def fft_filter(x, lo=None, hi=None):
    X = np.fft.rfft(x); f = np.fft.rfftfreq(len(x), 1 / SR)
    g = np.ones_like(f)
    if hi: g *= 1 / np.sqrt(1 + (f / hi) ** 4)
    if lo: g *= 1 / np.sqrt(1 + (lo / np.maximum(f, 1e-3)) ** 4)
    return np.fft.irfft(X * g, len(x))


def saw(freq, t, harmonics=20):
    out = np.zeros_like(t)
    for k in range(1, harmonics + 1):
        if k * freq > 11000: break
        out += np.sin(2 * np.pi * k * freq * t + k * 0.7) / k
    return out


# ---- Instruments ------------------------------------------------------------------------
def kick():
    t = np.arange(int(0.3 * SR)) / SR
    f = 46 + 120 * np.exp(-t * 40)
    body = np.sin(2 * np.pi * np.cumsum(f) / SR) * np.exp(-t * 8)
    click = rng.standard_normal(len(t)) * np.exp(-t * 500) * 0.2
    return np.tanh((body + click) * 2.0)


def clap():
    t = np.arange(int(0.28 * SR)) / SR
    noise = fft_filter(rng.standard_normal(len(t)), lo=1100, hi=6000)
    env = np.zeros_like(t)
    for d in (0, 0.009, 0.019):
        env += (t >= d) * np.exp(-np.maximum(t - d, 0) * 70)
    env += (t >= 0.025) * np.exp(-np.maximum(t - 0.025, 0) * 16) * 0.5
    return noise * env


def shaker():
    t = np.arange(int(0.07 * SR)) / SR
    env = (1 - np.exp(-t * 300)) * np.exp(-t * 60)
    return fft_filter(rng.standard_normal(len(t)), lo=6000, hi=13000) * env


def supersaw(notes, dur, cutoff):
    """A chord of seven detuned saws per note, spread left and right."""
    t = np.arange(int(dur * SR)) / SR
    env = (1 - np.exp(-t * 60)) * np.minimum(1, (dur - t) / 0.05)
    left, right = np.zeros_like(t), np.zeros_like(t)
    for m in notes:
        for k, det in enumerate((-0.22, -0.13, -0.05, 0.0, 0.05, 0.13, 0.22)):
            v = saw(midi(m + det), t, harmonics=14)
            pan = (k - 3) / 3
            left += v * (1 - pan) / 2; right += v * (1 + pan) / 2
    scale = env / (len(notes) * 4)
    return fft_filter(left, hi=cutoff) * scale, fft_filter(right, hi=cutoff) * scale


def sub(m, dur):
    t = np.arange(int(dur * SR)) / SR
    env = (1 - np.exp(-t * 200)) * np.exp(-t * 2.5) * np.minimum(1, (dur - t) / 0.03)
    return (np.sin(2 * np.pi * midi(m) * t) + 0.25 * np.sin(4 * np.pi * midi(m) * t)) * env


def marimba(m, dur):
    """Two-operator FM: a bright attack that mellows."""
    t = np.arange(int(dur * SR)) / SR
    f = midi(m)
    index = 3.0 * np.exp(-t * 18)
    env = (1 - np.exp(-t * 800)) * np.exp(-t * 5)
    return np.sin(2 * np.pi * f * t + index * np.sin(2 * np.pi * 4 * f * t)) * env


def riser(dur):
    t = np.arange(int(dur * SR)) / SR
    noise = rng.standard_normal(len(t))
    dull, bright = fft_filter(noise, hi=1200), fft_filter(noise, lo=3500)
    f = (t / dur) ** 2
    return (dull * (1 - f) + bright * f) * f


# ---- Arrangement ---------------------------------------------------------------------------
progression = [  # (chord, bass root)
    ([62, 66, 69, 74], 38),  # D
    ([61, 64, 69, 73], 45),  # A
    ([62, 66, 71, 74], 47),  # Bm
    ([62, 67, 71, 74], 43),  # G
]
# The lead: eight bars of (eighth-note step, midi, length in eighths), D major pentatonic.
melody = [
    [(0, 78, 2), (2, 81, 1), (3, 78, 1), (4, 76, 2), (6, 74, 2)],
    [(0, 76, 3), (3, 73, 1), (4, 76, 2), (6, 78, 2)],
    [(0, 78, 2), (2, 81, 1), (3, 83, 1), (4, 81, 2), (6, 78, 2)],
    [(0, 79, 3), (3, 78, 1), (4, 76, 4)],
    [(0, 78, 2), (2, 81, 1), (3, 78, 1), (4, 76, 2), (6, 74, 2)],
    [(0, 76, 3), (3, 78, 1), (4, 81, 2), (6, 83, 2)],
    [(0, 86, 2), (2, 83, 2), (4, 81, 2), (6, 78, 2)],
    [(0, 79, 2), (2, 78, 2), (4, 74, 4)],
]
bars = int(LENGTH / bar) + 1
K, C = kick(), clap()
INTRO = 4

for b in range(INTRO, bars):  # the sidechain, from the kicks of the full mix
    for q in range(4):
        i = int((b * bar + q * beat) * SR)
        if i < n:
            m = min(n - i, int(beat * SR))
            tt = np.arange(m) / SR
            duck[i:i + m] = np.minimum(duck[i:i + m], 1 - 0.7 * np.exp(-tt * 7))

for b in range(bars):
    t0 = b * bar
    chord, root = progression[b % 4]
    intro = b < INTRO
    last = t0 + bar > LENGTH - 2.5
    # Chords: a filter that opens over the intro, then bright.
    cutoff = 900 + 3200 * (b / INTRO) if intro else 5200
    if last:
        place(None, t0, 0.5, stereo=supersaw([50, 57, 62, 66, 69], min(bar * 2, LENGTH - t0), 4000))
        place(sub(38, min(bar * 2, LENGTH - t0)), t0, 0.5)
        place(marimba(74, 2.0), t0, 0.2)
        break
    place(None, t0, 0.55, ducked=True, stereo=supersaw(chord, bar, cutoff))
    # Sub bass: the root on every eighth, off the beat once the kick is in.
    if not intro:
        for e in range(8):
            if e % 2 == 1:
                place(sub(root, beat * 0.48), t0 + e * beat / 2, 0.55, ducked=True)
        for q in range(4):
            place(K, t0 + q * beat, 0.85)
        for q in (1, 3):
            place(C, t0 + q * beat, 0.3, pan=0.05)
    # Shakers: 16ths, swung a touch, accent on the off-beats.
    for h in range(16):
        if intro and h % 2: continue
        swing = s16 * 0.12 if h % 2 else 0
        place(shaker(), t0 + h * s16 + swing, (0.09 if h % 4 == 2 else 0.05) * (0.6 if intro else 1), pan=0.35)
    # The lead: from the first full bar, every other eight-bar phrase louder.
    if b >= INTRO - 2:
        gain = 0.16 if intro else (0.26 if ((b - INTRO) // 8) % 2 == 0 else 0.2)
        for step, m, length in melody[(b - INTRO + 2) % 8]:
            place(marimba(m, length * beat / 2 + 0.3), t0 + step * beat / 2, gain, pan=-0.2, ducked=not intro)
            place(marimba(m - 12, length * beat / 2 + 0.3), t0 + step * beat / 2 + 0.11, gain * 0.25, pan=0.4)  # echo
    # A riser into the drop and into every eight-bar phrase.
    if (b == INTRO - 1) or (b > INTRO and (b - INTRO) % 8 == 7):
        place(riser(bar), t0, 0.14)

# Master: glue, fades, normalise.
L = fft_filter(L, lo=30, hi=16000); R = fft_filter(R, lo=30, hi=16000)
mix = np.stack([L, R], axis=1)
mix = np.tanh(mix * 1.4) / 1.4
t = np.arange(n) / SR
mix *= (np.minimum(1, t / 0.3) * np.minimum(1, (LENGTH - t) / 2.5))[:, None]
mix *= 0.9 / np.max(np.abs(mix))
with wave.open(OUT, "wb") as w:
    w.setnchannels(2); w.setsampwidth(2); w.setframerate(SR)
    w.writeframes((mix * 32767).astype("<i2").tobytes())
print(OUT, f"{LENGTH:.1f} s")
