#!/usr/bin/env python

import sys
from math import log

weights = {}
for line in sys.stdin:
    line = line.strip()
    if line.startswith(';'):
        continue

    n,v,w = line.split(',')
    if v.isdigit():
        v = int(v)
    w = float(w)
    try:
        weights[n][v]+= w
    except KeyError:
        if n not in weights:
            weights[n] = {}
        weights[n][v] = w

for n in sorted(weights.keys()):
    nws = weights[n]
    total_weight = sum(nws.values())
    for v in sorted(nws.keys()):
        pr = nws[v]/total_weight
        print "%s, %s, %6g, %6g" % (n, v, pr, log(pr))
