"""Assembles the demo: the recorded frames, the title and end cards and the
soundtrack, all in <out>, into <video>.

    python demo/assemble.py <out> <video>
"""
import csv, os, subprocess, sys
import imageio_ffmpeg
OUT, VIDEO = os.path.abspath(sys.argv[1]), os.path.abspath(sys.argv[2])
os.chdir(OUT)
FF = imageio_ffmpeg.get_ffmpeg_exe()
def run(args):
    r = subprocess.run([FF, "-y", "-loglevel", "error"] + args, capture_output=True, text=True)
    if r.returncode: raise SystemExit(r.stderr)

rows = [(int(r["n"]), float(r["t"])) for r in csv.DictReader(open("frames.csv"))]
with open("frames.txt", "w") as f:
    for (n, t), (_, t_next) in zip(rows, rows[1:] + [(0, rows[-1][1] + 0.5)]):
        f.write(f"file 'frames/{n:06d}.jpg'\nduration {max(t_next - t, 0.001):.4f}\n")
    f.write(f"file 'frames/{rows[-1][0]:06d}.jpg'\n")
board_len = rows[-1][1] - rows[0][1] + 0.5
enc = ["-c:v", "libx264", "-preset", "slow", "-crf", "18", "-pix_fmt", "yuv420p", "-r", "30"]
run(["-f", "concat", "-safe", "0", "-i", "frames.txt",
     "-vf", f"fps=30,scale=1920:1080:flags=lanczos,fade=t=in:st=0:d=0.6,fade=t=out:st={board_len - 0.7:.2f}:d=0.7"] + enc + ["board.mp4"])
run(["-loop", "1", "-t", "5", "-i", "title.png", "-vf", "fps=30,fade=t=in:st=0:d=0.8,fade=t=out:st=4.3:d=0.7"] + enc + ["title.mp4"])
run(["-loop", "1", "-t", "7.5", "-i", "end.png", "-vf", "fps=30,fade=t=in:st=0:d=0.8,fade=t=out:st=6:d=1.5"] + enc + ["end.mp4"])
open("parts.txt", "w").write("file 'title.mp4'\nfile 'board.mp4'\nfile 'end.mp4'\n")
run(["-f", "concat", "-safe", "0", "-i", "parts.txt", "-i", "soundtrack.wav",
     "-map", "0:v", "-map", "1:a", "-c:v", "copy", "-c:a", "aac", "-b:a", "192k",
     "-af", f"afade=t=out:st={5 + board_len + 7.5 - 4:.2f}:d=4", "-shortest", "-movflags", "+faststart", VIDEO])
print("board", round(board_len, 1), "s")
