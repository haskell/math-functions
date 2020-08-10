#!/usr/bin/env python3
"""
"""
import itertools
import re
import mpmath
import sys

# We want to require quad precision (it never hurts to have some
# buffer for errors
mpmath.mp.dps = 30

def skip_comments(gen):
    "Skip comments and empty lines"
    for s in gen:
        s = s.strip()
        if not (s == '' or s[0] == '#'):
            yield s

def tokenize_stream(gen):
    "Extremeli ugly code which splits code on separator"
    nm  = None
    acc = None
    for s in gen:
        r = re.match(r'^([a-zA-Z]+(, *[a-zA-Z]+)*) *= *', s)
        if r:
            if acc is not None:
                for _ in nm:
                    yield(acc)
            nm  = r.groups()[0].split(',')
            acc = []
        elif acc is None:
            raise Exception("Invalid format")
        else:
            acc.append(float(s))
    for _ in nm:
        yield(acc)



def load_inputs(path):
    "Load inputs for function from tsv file"
    with open(path) as f:
        for s in skip_comments(f):
            yield tuple(map(float, s.split()))


def load_inputs_cartesian(path):
    "Load inputs for several variables where we want to genrate all pair"
    with open(path) as f:
        for x in itertools.product(*tokenize_stream(skip_comments(f))):
            yield x

def fmt(f, inputs, out):
    # Here we set up output. We hope that refcounting will collect fds
    # promptly
    if out is None:
        out = sys.stdout
    else:
        out = open(out, 'w')
    for xs in inputs:
        param = ["%.18g" % x for x in xs]
        sOut  = mpmath.nstr( f(*xs), mpmath.mp.dps )
        print( '\t'.join( param + [sOut]),
               file=out)       


## ================================================================

fmt( mpmath.erf,
     load_inputs('inputs/erf.dat'),
     'erf.dat')
fmt( mpmath.erf,
     load_inputs('inputs/erf.dat'),
     'erf.dat')
fmt( mpmath.erfc,
     load_inputs('inputs/erfc.dat'),
     'erfc.dat')
fmt( mpmath.erfc,
     load_inputs('inputs/erfc-large.dat'),
     'erfc-large.dat')
fmt( mpmath.loggamma,
     load_inputs('inputs/loggamma.dat'),
     'loggamma.dat')
fmt( mpmath.digamma,
     load_inputs('inputs/digamma.dat'),
     'digamma.dat')
fmt( mpmath.expm1,
     load_inputs('inputs/expm1.dat'),
     'expm1.dat')
fmt( mpmath.log1p,
     load_inputs('inputs/log1p.dat'),
     'log1p.dat')
fmt( lambda x: mpmath.log(mpmath.factorial(x)),
     map(lambda x: (x,), range(0, 20000)),
     'factorial.dat')
fmt( lambda a, x: mpmath.gammainc(z=a, a=0, b=x, regularized=True),
     load_inputs_cartesian('inputs/igamma.dat'),
     'igamma.dat')
fmt( lambda p, q: mpmath.log(mpmath.beta(p,q)),
     load_inputs_cartesian('inputs/logbeta.dat'),
     'logbeta.dat')
