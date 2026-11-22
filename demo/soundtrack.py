# An original lo-fi beat, synthesized from scratch: no samples, no licences.
# 84 BPM, swung hi-hats, soft kick and snare, electric-piano 7th chords
# (Fmaj7 - Em7 - Dm9 - Cmaj7), sub bass, a pentatonic melody in the middle,
# vinyl crackle, and a warm low-pass over the whole mix.
#
#   python demo/soundtrack.py <seconds> <out.wav>
import sys, wave
import numpy as np

SR = 44100
LENGTH = float(sys.argv[1]) if len(sys.argv) > 1 else 160.0
OUT = sys.argv[2] if len(sys.argv) > 2 else "soundtrack.wav"
BPM = 84
beat = 60 / BPM
bar = 4 * beat
n = int(LENGTH * SR)
rng = np.random.default_rng(7)
L = np.zeros(n); R = np.zeros(n)

def midi(m): return 440.0 * 2 ** ((m - 69) / 12)

def place(sig, t, gain=1.0, pan=0.0):
    i = int(t * SR)
    if i >= n: return
    sig = sig[: n - i]
    L[i:i + len(sig)] += sig * gain * np.sqrt(0.5 * (1 - pan))
    R[i:i + len(sig)] += sig * gain * np.sqrt(0.5 * (1 + pan))

def fft_filter(x, lo=None, hi=None):
    X = np.fft.rfft(x); f = np.fft.rfftfreq(len(x), 1 / SR)
    g = np.ones_like(f)
    if hi: g *= 1 / np.sqrt(1 + (f / hi) ** 4)
    if lo: g *= 1 / np.sqrt(1 + (lo / np.maximum(f, 1e-3)) ** 4)
    return np.fft.irfft(X * g, len(x))

# ---- Instruments ----------------------------------------------------------------
def epiano(freq, dur):
    t = np.arange(int(dur * SR)) / SR
    env = np.exp(-t * 2.2) * (1 - np.exp(-t * 120))
    trem = 1 + 0.12 * np.sin(2 * np.pi * 4.5 * t)
    tone = (np.sin(2 * np.pi * freq * t) + 0.35 * np.sin(2 * np.pi * 2 * freq * t) * np.exp(-t * 5)
            + 0.12 * np.sin(2 * np.pi * 3.01 * freq * t) * np.exp(-t * 9)
            + 0.5 * np.sin(2 * np.pi * freq * 1.003 * t))
    return tone * env * trem

def bass(freq, dur):
    t = np.arange(int(dur * SR)) / SR
    env = (1 - np.exp(-t * 60)) * np.exp(-t * 1.1)
    return (np.sin(2 * np.pi * freq * t) + 0.25 * np.sin(2 * np.pi * 2 * freq * t)) * env

def lead(freq, dur):
    t = np.arange(int(dur * SR)) / SR
    vib = np.sin(2 * np.pi * 5.2 * t) * 0.004 * np.minimum(t * 3, 1)
    phase = 2 * np.pi * freq * np.cumsum(1 + vib) / SR
    env = (1 - np.exp(-t * 30)) * np.exp(-t * 1.6)
    return (np.sin(phase) + 0.18 * np.sin(2 * phase)) * env

def kick():
    t = np.arange(int(0.45 * SR)) / SR
    f = 45 + 75 * np.exp(-t * 28)
    return np.sin(2 * np.pi * np.cumsum(f) / SR) * np.exp(-t * 7.5)

def snare():
    t = np.arange(int(0.35 * SR)) / SR
    noise = fft_filter(rng.standard_normal(len(t)), lo=1200, hi=6000)
    return (noise * 0.9 * np.exp(-t * 16) + 0.4 * np.sin(2 * np.pi * 185 * t) * np.exp(-t * 25))

def hat(open_=False):
    t = np.arange(int((0.18 if open_ else 0.05) * SR)) / SR
    noise = fft_filter(rng.standard_normal(len(t)), lo=7000)
    return noise * np.exp(-t * (18 if open_ else 70))

# ---- Arrangement --------------------------------------------------------------------
chords = [  # midi notes, and the bass root
    ([53, 57, 60, 64], 41),       # Fmaj7
    ([52, 55, 59, 62], 40),       # Em7
    ([50, 53, 57, 60, 64], 38),   # Dm9
    ([48, 52, 55, 59], 36),       # Cmaj7
]
melody = [(0, 76), (1.5, 74), (2, 72), (3, 69), (4.5, 72), (5, 74), (6, 76), (7, 79),
          (8, 81), (9.5, 79), (10, 76), (11, 74), (12, 72), (13.5, 74), (14, 72), (15, 69)]
K, S = kick(), snare()
swing = 0.06 * beat
bars = int(LENGTH / bar) + 1
for b in range(bars):
    t0 = b * bar
    notes, root = chords[b % 4]
    # Chord stabs: on 1, and a softer push on the "and" of 2.
    for k, m in enumerate(notes):
        place(epiano(midi(m), bar * 1.1), t0 + k * 0.012, 0.11, pan=-0.3 + 0.15 * k)
        place(epiano(midi(m + 12), beat * 1.5), t0 + 1.5 * beat + swing + k * 0.01, 0.035, pan=0.25)
    if b >= 2:  # bass in from bar 3
        place(bass(midi(root), beat * 2.2), t0, 0.32)
        place(bass(midi(root + 7), beat * 1.2), t0 + 2.5 * beat + swing, 0.2)
    if 2 <= b and t0 < LENGTH - 2 * bar:  # drums from bar 3 to the outro
        for q in range(4):
            if q in (0,) or (q == 2 and b % 2 == 1): place(K, t0 + q * beat, 0.62)
            if q == 2 and b % 2 == 0: place(K, t0 + 2.5 * beat + swing, 0.45)
            if q in (1, 3): place(S, t0 + q * beat + 0.012, 0.3, pan=0.05)
        for e in range(8):
            t = t0 + e * beat / 2 + (swing if e % 2 else 0)
            place(hat(open_=(e == 7 and b % 4 == 3)), t, 0.09 + 0.05 * rng.random() * (e % 2 == 0), pan=0.35)
    if 10 <= b < bars - 4 and (b - 10) % 8 < 4:  # the melody, every other 4 bars
        for beat_at, m in melody:
            if int(beat_at // 4) == (b - 10) % 4:
                place(lead(midi(m), beat * 1.6), t0 + (beat_at % 4) * beat + (swing if beat_at % 1 else 0), 0.075, pan=-0.15)

# Vinyl: a soft hiss and sparse crackles.
hiss = fft_filter(rng.standard_normal(n), lo=900, hi=5000) * 0.0045
L += hiss; R += np.roll(hiss, 37)
for t in rng.uniform(0, LENGTH, int(LENGTH * 7)):
    c = rng.standard_normal(int(0.002 * SR)) * rng.uniform(0.01, 0.05)
    place(c, t, 1.0, pan=rng.uniform(-0.6, 0.6))

# Warm it up: low-pass, gentle saturation, fades, normalise.
L = fft_filter(L, lo=30, hi=10000); R = fft_filter(R, lo=30, hi=10000)
mix = np.stack([L, R], axis=1)
mix = np.tanh(mix * 1.6) / 1.6
t = np.arange(n) / SR
fade = np.minimum(1, t / 2.5) * np.minimum(1, (LENGTH - t) / 5)
mix *= fade[:, None]
mix *= 0.89 / np.max(np.abs(mix))
pcm = (mix * 32767).astype("<i2")
with wave.open(OUT, "wb") as w:
    w.setnchannels(2); w.setsampwidth(2); w.setframerate(SR); w.writeframes(pcm.tobytes())
print(OUT, f"{LENGTH:.1f} s")
