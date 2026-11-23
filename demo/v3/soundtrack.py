# Take three's music: a cozy lo-fi jazz trio, synthesized from scratch (no
# samples, no licences). 84 BPM, swung eighths, in F major over an eight-bar
# loop (Gm9 C9 Fmaj9 Dm9 | Bbmaj9 A7b9 Dm9 G9-C9): Rhodes-style electric
# piano comping, a walking upright bass, brushes on 2 and 4, a ride cymbal's
# swing pattern, a vibraphone melody in the middle choruses, and a little
# vinyl crackle under it all. It ends on a ringing Fmaj9.
#
#   python demo/v3/soundtrack.py <seconds> <out.wav>
import sys
import wave

import numpy as np

SR = 44100
LENGTH = float(sys.argv[1]) if len(sys.argv) > 1 else 120.0
OUT = sys.argv[2] if len(sys.argv) > 2 else "soundtrack-v3.wav"
BPM = 84
beat = 60 / BPM
bar = 4 * beat
SWING = 0.64  # where the off-beat eighth falls, as a fraction of the beat
n = int(LENGTH * SR)
rng = np.random.default_rng(7)
L = np.zeros(n); R = np.zeros(n)


def midi(m): return 440.0 * 2 ** ((m - 69) / 12)


def at(b, eighth=0):
    """Time of beat b (from the start), on its downbeat (0) or swung off-beat (1)."""
    return b * beat + (SWING * beat if eighth else 0)


def place(sig, t, gain=1.0, pan=0.0):
    i = int(t * SR)
    if i >= n or i < 0: return
    sig = sig[: n - i] * gain
    L[i:i + len(sig)] += sig * np.sqrt(0.5 * (1 - pan))
    R[i:i + len(sig)] += sig * np.sqrt(0.5 * (1 + pan))


def fft_filter(x, lo=None, hi=None, order=2):
    X = np.fft.rfft(x); f = np.fft.rfftfreq(len(x), 1 / SR)
    g = np.ones_like(f)
    if hi: g *= 1 / np.sqrt(1 + (f / hi) ** (2 * order))
    if lo: g *= 1 / np.sqrt(1 + (lo / np.maximum(f, 1e-3)) ** (2 * order))
    return np.fft.irfft(X * g, len(x))


# ---- Instruments ------------------------------------------------------------------------
def rhodes(m, dur, vel=1.0):
    """An electric piano tine: two-operator FM, its bell fading fast, with tremolo."""
    t = np.arange(int((dur + 1.2) * SR)) / SR
    f = midi(m)
    index = (2.2 * vel) * np.exp(-t * 6) + 0.6
    tone = np.sin(2 * np.pi * f * t + index * np.sin(2 * np.pi * f * t))
    bell = 0.18 * vel * np.sin(2 * np.pi * f * 14 * t) * np.exp(-t * 40)
    env = (1 - np.exp(-t * 300)) * np.exp(-t * 1.1)
    release = np.clip(1 - (t - dur) / 1.2, 0, 1) ** 2  # damper after dur
    trem = 1 + 0.12 * np.sin(2 * np.pi * 4.6 * t)
    return (tone + bell) * env * release * trem


def chord(notes, dur, vel=1.0):
    sig = None
    for k, m in enumerate(notes):
        s = rhodes(m, dur, vel * (0.85 + 0.3 * rng.random()))
        # a slight roll, low to high, as a hand plays it
        s = np.concatenate([np.zeros(int(k * 0.012 * SR)), s])
        sig = s if sig is None else np.pad(sig, (0, max(0, len(s) - len(sig)))) + np.pad(s, (0, max(0, len(sig) - len(s))))
    return fft_filter(sig, hi=4200) / len(notes)


def upright(m, dur):
    """Plucked upright bass: a round fundamental, a woody second harmonic, a thump."""
    t = np.arange(int((dur + 0.15) * SR)) / SR
    f = midi(m)
    env = (1 - np.exp(-t * 600)) * np.exp(-t * 3.2) * np.clip(1 - (t - dur) / 0.15, 0, 1)
    body = np.sin(2 * np.pi * f * t) + 0.45 * np.sin(2 * np.pi * 2 * f * t) * np.exp(-t * 6) \
        + 0.15 * np.sin(2 * np.pi * 3 * f * t) * np.exp(-t * 10)
    thump = fft_filter(rng.standard_normal(len(t)), hi=500) * np.exp(-t * 60) * 0.6
    return fft_filter(body * env + thump * env, hi=1600)


def soft_kick():
    t = np.arange(int(0.35 * SR)) / SR
    f = 52 + 50 * np.exp(-t * 30)
    return np.sin(2 * np.pi * np.cumsum(f) / SR) * np.exp(-t * 11)


def brush(long=True):
    """A brush on the snare: a filtered swish, slower to rise when swept."""
    d = 0.42 if long else 0.16
    t = np.arange(int(d * SR)) / SR
    noise = fft_filter(rng.standard_normal(len(t)), lo=1800, hi=7500)
    rise = 1 - np.exp(-t * (40 if long else 400))
    return noise * rise * np.exp(-t * (9 if long else 28))


def ride(accent=1.0):
    """A ride cymbal: inharmonic partials over a bright noise wash."""
    t = np.arange(int(1.1 * SR)) / SR
    partials = [523, 811, 1187, 1622, 2210, 3170, 4410]
    tone = sum(np.sin(2 * np.pi * p * 1.7 * t + rng.random() * 6) for p in partials) / len(partials)
    wash = fft_filter(rng.standard_normal(len(t)), lo=5000, hi=12000)
    ping = np.exp(-t * 40)
    return fft_filter((0.5 * tone * ping + wash * 0.7) * np.exp(-t * 4.5) * accent, lo=2500)


def vibes(m, dur):
    """Vibraphone: a pure bar tone with its fourth-octave partial, and the motor's vibrato."""
    t = np.arange(int((dur + 1.0) * SR)) / SR
    f = midi(m)
    env = (1 - np.exp(-t * 500)) * np.exp(-t * 1.8)
    tone = np.sin(2 * np.pi * f * t) + 0.25 * np.sin(2 * np.pi * 4 * f * t) * np.exp(-t * 9)
    return tone * env * (1 + 0.22 * np.sin(2 * np.pi * 5.5 * t))


# ---- Harmony --------------------------------------------------------------------------------
# Each bar: (Rhodes voicing, bass root, walking-line tones, chord tones for the melody)
F, G, A, Bb, C, D, E = 53, 55, 57, 58, 60, 62, 64  # around F3
BARS = [
    ([58, 62, 65, 69], 43, [43, 46, 50, 49]),   # Gm9:   Bb D F A        bass G Bb D Db
    ([58, 62, 64, 67], 48, [48, 52, 55, 52]),   # C9:    Bb D E G        bass C E G E
    ([57, 60, 64, 67], 41, [41, 45, 48, 50]),   # Fmaj9: A C E G         bass F A C D
    ([57, 60, 64, 65], 50, [50, 53, 57, 55]),   # Dm9:   A C E F         bass D F A G
    ([57, 60, 62, 65], 46, [46, 50, 53, 46]),   # Bbmaj9: A C D F        bass Bb D F Bb
    ([55, 58, 61, 64], 45, [45, 49, 52, 51]),   # A7b9:  G Bb C# E       bass A C# E Eb
    ([57, 60, 64, 65], 50, [50, 53, 57, 56]),   # Dm9                    bass D F A Ab
    ([57, 59, 62, 65], 43, [43, 47, 48, 47]),   # G9 -> C9: F A B D      bass G B C B
]
# The vibraphone's tune: (beat in the 8-bar loop, eighth, midi, length in beats)
TUNE = [
    (0, 1, 74, 1), (1, 1, 72, 0.5), (2, 0, 70, 1.5), (4, 0, 72, 0.5), (4, 1, 74, 0.5), (5, 0, 76, 2),
    (8, 1, 77, 1), (9, 1, 76, 0.5), (10, 0, 72, 1.5), (12, 0, 69, 3),
    (16, 1, 74, 1), (17, 1, 72, 0.5), (18, 0, 69, 1), (19, 1, 70, 0.5), (20, 0, 69, 2),
    (24, 0, 67, 0.5), (24, 1, 69, 0.5), (25, 1, 72, 1), (27, 0, 74, 1), (28, 0, 72, 2), (30, 1, 71, 1),
]
# Comping rhythms, picked per bar: (beat, eighth, length in beats, velocity)
COMPS = [
    [(0, 0, 1.4, 1.0), (1, 1, 1.0, 0.7), (3, 0, 0.8, 0.6)],
    [(0, 1, 1.2, 0.9), (2, 0, 0.6, 0.6), (2, 1, 1.5, 0.8)],
    [(0, 0, 2.2, 0.9), (2, 1, 1.2, 0.7)],
    [(0, 0, 0.9, 0.9), (1, 1, 0.7, 0.6), (3, 1, 1.0, 0.8)],
]

bars_total = int(LENGTH / bar)
final = bars_total - 1  # the last bar holds the tonic
K = soft_kick()
for b in range(bars_total):
    t0 = b * bar
    voicing, root, walk = BARS[b % 8]
    beat0 = b * 4
    intro = b < 2
    chorus = (b // 8)
    last = b == final
    if last:
        # The tonic, rung out, with the bass's low F and a ride shimmer.
        place(chord([57, 60, 64, 67, 72], bar * 0.9, 1.1), t0, 0.5)
        place(upright(29 + 12, bar * 0.8), t0, 0.34)
        place(ride(0.7), t0, 0.08, pan=0.35)
        place(vibes(77, bar * 0.8), t0 + 0.05, 0.10, pan=-0.3)
        break
    # Rhodes comping.
    for (q, e, ln, vel) in COMPS[(b * 3 + chorus) % 4]:
        place(chord(voicing, ln * beat, vel), at(beat0 + q, e), 0.62 if not intro else 0.5, pan=-0.12)
    # The bass walks from the third bar.
    if not intro:
        for q in range(4):
            place(upright(walk[q], beat * 0.9), at(beat0 + q), 0.36, pan=0.05)
            if q == 3 and rng.random() < 0.35:  # a ghosted skip into the next bar
                place(upright(walk[q] + 1, beat * 0.25), at(beat0 + q, 1), 0.15, pan=0.05)
    # Drums: from the third bar; a lighter chorus in the middle.
    light = chorus == 2
    if not intro:
        place(K, at(beat0), 0.32)
        if not light: place(K, at(beat0 + 2, 1 if b % 2 else 0), 0.3)
        for q in (1, 3):
            place(brush(True), at(beat0 + q) - 0.06, 0.14, pan=0.15)
        if rng.random() < 0.5 and not light:
            place(brush(False), at(beat0 + 3, 1), 0.08, pan=0.15)
        # The ride's swing: ding, ding-da, ding, ding-da.
        for q in range(4):
            place(ride(1.0 if q % 2 == 0 else 0.85), at(beat0 + q), 0.15 if not light else 0.1, pan=0.35)
            if q % 2 == 1:
                place(ride(0.6), at(beat0 + q, 1), 0.1 if not light else 0.07, pan=0.35)
    else:
        for q in (1, 3):
            place(brush(True), at(beat0 + q) - 0.06, 0.04, pan=0.15)
    # The vibraphone plays the tune in choruses 1 and 3 (counting from 0).
    if chorus in (1, 3) and b % 8 == 0:
        for (q, e, m, ln) in TUNE:
            place(vibes(m, ln * beat), at(beat0 + q, e), 0.24, pan=-0.3)

# Vinyl: a low hiss and sparse crackle.
hiss = fft_filter(rng.standard_normal(n), lo=1500, hi=6000) * 0.004
pops = np.zeros(n)
for i in rng.integers(0, n, int(LENGTH * 7)):
    pops[i] = rng.choice([-1, 1]) * (0.02 + 0.08 * rng.random() ** 3)
crackle = fft_filter(pops, lo=1200, hi=9000)
L += hiss + crackle; R += hiss + np.roll(crackle, 37)

# Master: warm (rolled-off top), gently saturated, faded, normalised.
L = fft_filter(L, lo=35, hi=9500); R = fft_filter(R, lo=35, hi=9500)
mix = np.stack([L, R], axis=1)
mix = np.tanh(mix * 1.6) / 1.6
mix *= 0.89 / np.max(np.abs(mix))
t = np.arange(n) / SR
mix *= (np.minimum(1, t / 0.6) * np.minimum(1, (LENGTH - t) / 2.5))[:, None]
with wave.open(OUT, "wb") as w:
    w.setnchannels(2); w.setsampwidth(2); w.setframerate(SR)
    w.writeframes((mix * 32767).astype("<i2").tobytes())
print(OUT, f"{LENGTH:.1f} s")
