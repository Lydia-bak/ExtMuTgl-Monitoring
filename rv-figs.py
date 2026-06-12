import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

import csv
import re



##############################################################################
# Experiment 1
##############################################################################

##############################################################################
# Parsing
##############################################################################

EVENT_RE = re.compile(r"\(\s*([0-9eE+.\-]+)\s*,\s*(Top|Bot)\s*\)")


def parse_event(cell):
    m = EVENT_RE.fullmatch(cell.strip())
    if m is None:
        raise ValueError(f"Cannot parse event '{cell}'")

    return float(m.group(1)), m.group(2)


##############################################################################
# Signal evaluation
##############################################################################

def value_at(events, t):
    """
    events = [(t1,v1), ..., (tn,vn)]

    Convention:
        before t1      -> Bot
        from ti onward -> vi until next change
    """

    current = "Bot"

    for time, value in events:
        if time > t:
            break
        current = value

    return current


##############################################################################
# Read CSV
##############################################################################

signals = {}

with open("experiment1.csv", newline="") as f:
    reader = csv.reader(f)

    for row in reader:
        if len(row) < 3:
            continue

        bound = int(row[0])

        events = [
            parse_event(cell)
            for cell in row[2:]
            if cell.strip()
        ]

        events.sort(key=lambda p: p[0])

        signals[bound] = events


##############################################################################
# Collect all change points
##############################################################################

all_times = sorted({
    t
    for events in signals.values()
    for t, _ in events
})

if len(all_times) < 2:
    raise RuntimeError("Need at least two timestamps")


##############################################################################
# Compute minimal bound yielding Top
##############################################################################

xs = []
ys = []

bounds = sorted(signals.keys())

for t0, t1 in zip(all_times[:-1], all_times[1:]):
    mid = (t0 + t1) / 2.0

    minimal_bound = None

    for bound in bounds:
        if value_at(signals[bound], mid) == "Top":
            minimal_bound = bound
            break

    xs.append(t0)

    if minimal_bound is None:
        ys.append(10)
    else:
        ys.append(minimal_bound)

# extend to last interval
xs.append(all_times[-1])
ys.append(ys[-1] if ys else np.nan)


##############################################################################
# Plot
##############################################################################

plt.figure(figsize=(10, 5))

plt.step(xs, ys, where="post")

plt.xlabel("Time")
plt.ylabel("Diameter")
# plt.title("Minimal satisfying bound over time")
plt.yticks(
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    ["1", "2", "3", "4", "5", "6", "7", "8", "9", "∞"]
)
plt.grid(True)

plt.tight_layout()
plt.savefig("../../rv26/minimal_bound.png")
plt.close()

print("Generated minimal_bound.png")

##############################################################################
# Experiment 2
##############################################################################

csv_file = "experiment2.csv"

agent_values = [3, 5, 10, 20, 50]
agent_to_x = {a: i + 1 for i, a in enumerate(agent_values)}

# CSV columns:
# 0: number of agents
# 1: computation time
# 2: trace length
df = pd.read_csv(
    csv_file,
    header=None,
    names=[
        "num_agents",
        "computation_time",
        "trace_length"
    ]
)

df["agent_pos"] = df["num_agents"].map(agent_to_x)

# --------------------------------------------------
# Graph 1: Number of agents vs Trace length
# --------------------------------------------------

plt.figure(figsize=(8, 6))

plt.scatter(
    df["agent_pos"],
    df["trace_length"],
    alpha=0.6
)
plt.xticks(
    range(1, len(agent_values) + 1),
    [str(a) for a in agent_values]
)
plt.xlabel("Number of agents")
plt.ylabel("Trace length")
plt.title("Trace length vs Number of agents")
plt.grid(True)

plt.tight_layout()
plt.savefig("../../rv26/agents_vs_trace_length.png")
plt.close()

# --------------------------------------------------
# Graph 2:
# Number of agents vs computation_time / trace_length
# --------------------------------------------------

df["normalized_time"] = (
    df["computation_time"] / df["trace_length"]
)

plt.figure(figsize=(8, 6))

# All samples
plt.scatter(
    df["agent_pos"],
    df["normalized_time"],
    alpha=0.5,
    label="Samples"
)
plt.xticks(
    range(1, len(agent_values) + 1),
    [str(a) for a in agent_values]
)
plt.yscale("log")
# Mean and standard deviation by number of agents
stats = (
    df.groupby("agent_pos")["normalized_time"]
      .agg(["mean", "std"])
      .reset_index()
)

plt.errorbar(
    stats["agent_pos"],
    stats["mean"],
    yerr=stats["std"],
    fmt="o-",
    capsize=5,
    linewidth=2,
    label="Mean ± Std"
)

plt.xlabel("Number of agents")
plt.ylabel("Computation time / Trace length")
plt.title("Normalized computation time vs Number of agents")
plt.grid(True)
plt.legend()

plt.tight_layout()
plt.savefig("../../rv26/agents_vs_normalized_time.png")
plt.close()

print("Generated:")
print("  agents_vs_trace_length.png")
print("  agents_vs_normalized_time.png")