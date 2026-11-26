# Take four's music: a chill acoustic guitar, synthesized from scratch (no
# samples, no licences). 82 BPM in G major over a four-bar loop
# (Gmaj7 | Em7 | Cmaj7 | D6sus), fingerpicked: the thumb on the bass
# strings, the fingers rolling up the chord in swung eighths. Plucked
# strings by Karplus-Strong, through a small wooden body. Under it, a soft
# kick, a rim click, a shaker and a round bass come in after the intro; a
# second guitar picks a slow melody in the middle. It ends on a strummed
# Gmaj7.
#
#   python demo/v4/soundtrack.py <seconds> <out.wav>
import sys
import wave

import numpy as np

SR = 44100
LENGTH = float(sys.argv[1]) if len(sys.argv) > 1 else 96.0
OUT = sys.argv[2] if len(sys.argv) > 2 else "soundtrack-v4.wav"
BPM = 82
beat = 60 / BPM
bar = 4 * beat
e8 = beat / 2
swing = e8 * 0.14
n = int(LENGTH * SR)
rng = np.random.default_rng(82)
L = np.zeros(n); R = np.zeros(n)


def midi(m): return 440.0 * 2 ** ((m - 69) / 12)


def place(sig, t, gain=1.0, pan=0.0):
    i = int(t * SR)
    if i >= n or i < 0: return
    sig = sig[: n - i] * gain
    L[i:i + len(sig)] += sig * np.sqrt(0.5 * (1 - pan))
    R[i:i + len(sig)] += sig * np.sqrt(0.5 * (1 + pan))


def fft_filter(x, lo=None, hi=None):
    X = np.fft.rfft(x); f = np.fft.rfftfreq(len(x), 1 / SR)
    g = np.ones_like(f)
    if hi: g *= 1 / np.sqrt(1 + (f / hi) ** 4)
    if lo: g *= 1 / np.sqrt(1 + (lo / np.maximum(f, 1e-3)) ** 4)
    return np.fft.irfft(X * g, len(x))


def body(x):
    """A small wooden body: resonances around 100, 200 and 400 Hz, and the
    top end softened."""
    X = np.fft.rfft(x); f = np.fft.rfftfreq(len(x), 1 / SR)
    g = 0.55 + sum(a / (1 + ((f - fc) / bw) ** 2) for fc, bw, a in ((105, 18, 0.9), (210, 30, 0.6), (410, 60, 0.35)))
    g /= np.sqrt(1 + (f / 5500) ** 4)
    return np.fft.irfft(X * g, len(x))


# ---- The guitar -------------------------------------------------------------------------------
_strings = {}


def pluck(m, vel=0.7, dur=2.5, bright=0.5):
    """A plucked string (Karplus-Strong), computed a period at a time: each
    period is the last one, averaged and damped. `bright` is where along the
    string it's plucked: higher is brighter."""
    key = (m, round(vel, 2), round(dur, 2), round(bright, 2))
    if key in _strings: return _strings[key]
    period = SR / midi(m)
    N = int(period)
    frac = period - N
    length = int((dur + 0.4) * SR)
    burst = rng.uniform(-1, 1, N)
    burst = fft_filter(np.tile(burst, 4), hi=1500 + 5000 * bright)[:N]  # a softer pick
    out = np.zeros(length + N + 1)
    out[:N] = burst
    damp = 0.996 - 0.0035 * (m - 40) / 40  # higher strings ring shorter
    i = N
    while i < length:
        k = min(N, length - i)
        prev = out[i - N:i - N + k + 1]
        a, b = prev[:k], prev[1:k + 1]
        out[i:i + k] = damp * ((1 - frac) * a + frac * b) * 0.5 + damp * 0.5 * ((1 - frac) * b + frac * a)
        i += k
    out = out[:length]
    t = np.arange(length) / SR
    out *= np.where(t < dur, 1.0, np.exp(-(t - dur) * 12))  # a finger damps it
    out = body(out) * vel * 0.35
    _strings[key] = out
    return out


# ---- The rest of the band -----------------------------------------------------------------------
def kick():
    t = np.arange(int(0.35 * SR)) / SR
    f = 50 + 60 * np.exp(-t * 25)
    return np.sin(2 * np.pi * np.cumsum(f) / SR) * np.exp(-t * 9) * (1 - np.exp(-t * 300))


def rim():
    t = np.arange(int(0.08 * SR)) / SR
    tone = np.sin(2 * np.pi * 820 * t) * np.exp(-t * 90)
    click = fft_filter(rng.standard_normal(len(t)), lo=2000, hi=7000) * np.exp(-t * 200) * 0.6
    return tone + click


def shaker():
    t = np.arange(int(0.09 * SR)) / SR
    env = (1 - np.exp(-t * 200)) * np.exp(-t * 45)
    return fft_filter(rng.standard_normal(len(t)), lo=5000, hi=11000) * env


def bass(m, dur):
    t = np.arange(int(dur * SR)) / SR
    env = (1 - np.exp(-t * 120)) * np.exp(-t * 2.2) * np.clip((dur - t) / 0.05, 0, 1)
    f = midi(m)
    return (np.sin(2 * np.pi * f * t) + 0.3 * np.sin(4 * np.pi * f * t) + 0.1 * np.sin(6 * np.pi * f * t)) * env


# ---- Arrangement -------------------------------------------------------------------------------
# Each bar: the thumb's bass notes (on beats 1 and 3) and the four strings the
# fingers pick, low to high. Standard-tuning shapes.
loop = [
    ((43, 47), [55, 59, 62, 66]),  # Gmaj7: G B | G B D F#
    ((40, 47), [55, 59, 62, 64]),  # Em7:   E B | G B D E
    ((48, 43), [55, 59, 64, 67]),  # Cmaj7: C G | G B E G
    ((50, 45), [57, 62, 64, 66]),  # D6sus: D A | A D E F#
]
# The fingers' pattern over eight eighths: which of the four strings (0 = lowest).
fingers = [None, 1, 2, 3, None, 2, 1, 2]
# The second guitar's melody, eight bars: (eighth, midi, eighths long).
melody = [
    [(0, 74, 3), (3, 71, 1), (4, 74, 4)],
    [(0, 76, 2), (2, 74, 2), (4, 71, 4)],
    [(0, 72, 3), (3, 74, 1), (4, 76, 2), (6, 79, 2)],
    [(0, 78, 6), (6, 76, 2)],
    [(0, 74, 3), (3, 76, 1), (4, 79, 4)],
    [(0, 76, 2), (2, 74, 2), (4, 71, 2), (6, 67, 2)],
    [(0, 69, 2), (2, 71, 2), (4, 72, 2), (6, 74, 2)],
    [(0, 71, 8)],
]
INTRO = 4
bars = int(LENGTH / bar) + 1
K, RIM = kick(), rim()


def at(b, eighth):
    return b * bar + eighth * e8 + (swing if eighth % 2 else 0) + rng.normal(0, 0.004)  # a human hand


for b in range(bars):
    t0 = b * bar
    (thumb1, thumb3), strings = loop[b % 4]
    if t0 + bar > LENGTH - 3.2:  # the ending: a slow strum down a Gmaj7
        for k, m in enumerate([43, 47, 50, 55, 59, 66]):
            place(pluck(m, 0.75, 3.2, 0.45), t0 + k * 0.035, 1.0, pan=-0.35 + k * 0.12)
        place(bass(31, 3.0), t0, 0.35)
        break
    # Guitar one: thumb on 1 and 3, fingers rolling up in between.
    place(pluck(thumb1, 0.85, 2 * beat, 0.35), at(b, 0), 1.0, pan=-0.25)
    place(pluck(thumb3, 0.7, 2 * beat, 0.35), at(b, 4), 1.0, pan=-0.25)
    for e, s in enumerate(fingers):
        if s is None: continue
        m = strings[s]
        place(pluck(m, 0.5 + 0.08 * (e == 2 or e == 6), 1.6 * beat, 0.55), at(b, e), 1.0, pan=-0.05 + 0.1 * s)
    # Guitar two: a slow melody, from the second phrase, panned right.
    if b >= INTRO and ((b - INTRO) // 8) % 2 == 0:
        for step, m, length in melody[(b - INTRO) % 8]:
            place(pluck(m, 0.62, length * e8 + 0.3, 0.6), at(b, step), 0.95, pan=0.45)
    # The band, after the intro: soft and low.
    if b >= INTRO:
        place(bass(thumb1 - 12, 1.9 * beat), t0, 0.32)
        place(bass(thumb3 - 12, 1.9 * beat), t0 + 2 * beat, 0.28)
        for k_at in (0, 2.5 if b % 2 else 2):
            place(K, t0 + k_at * beat, 0.5)
        for r_at in (1, 3):
            place(RIM, t0 + r_at * beat, 0.08, pan=0.1)
        for h in range(8):
            place(shaker(), at(b, h), 0.05 if h % 2 else 0.035, pan=0.4)

# Master: a little room, warm, fades, normalise.
def room(x, delays):
    out = x.copy()
    for d, g in delays:
        k = int(d * SR)
        out[k:] += x[:-k] * g
    return fft_filter(out, lo=35, hi=12000)


L = room(L, ((0.029, 0.2), (0.053, 0.14), (0.087, 0.09), (0.131, 0.06)))
R = room(R, ((0.033, 0.2), (0.061, 0.14), (0.079, 0.09), (0.143, 0.06)))
mix = np.stack([L, R], axis=1)
mix = np.tanh(mix * 1.1) / 1.1
t = np.arange(n) / SR
mix *= (np.minimum(1, t / 0.3) * np.minimum(1, (LENGTH - t) / 2.5))[:, None]
mix *= 0.88 / np.max(np.abs(mix))
with wave.open(OUT, "wb") as w:
    w.setnchannels(2); w.setsampwidth(2); w.setframerate(SR)
    w.writeframes((mix * 32767).astype("<i2").tobytes())
print(OUT, f"{LENGTH:.1f} s")
