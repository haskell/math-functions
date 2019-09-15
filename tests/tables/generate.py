#!/usr/bin/python
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
    acc = None
    for s in gen:
        r = re.match(r'^([a-zA-Z])+ *= *', s)
        if r:
            if acc is not None:
                yield acc
            acc = []
        elif acc is None:
            raise Exception("Invalid format")
        else:
            acc.append(float(s))
    yield acc



def load_inputs(path):
    "Load inputs for function from tsv file"
    with open(path) as f:
        for s in skip_comments(f):
            yield (s, tuple(map(float, s.split())))

def load_inputs_cartesian(path):
    "Load inputs for several variables where we want to genrate all pair"
    with open(path) as f:
        for x in itertools.product(*tokenize_stream(skip_comments(f))):
            yield x

def fmt(f, inputs, out):
    if out is None:
        out = sys.stdout
    else:
        # Lets hope for cpython's refcounting
        out = open(out, 'w')
    for (sIn,i) in load_inputs(inputs):
        sOut = mpmath.nstr( f(*i), mpmath.mp.dps )
        print(sIn + "\t" + sOut, file=out)


def fmt_cartesian(f, inputs, out):
    if out is None:
        out = sys.stdout
    else:
        # Lets hope for cpython's refcounting
        out = open(out, 'w')
    for xs in load_inputs_cartesian(inputs):
        param = ["%.18g" % x for x in xs]
        sOut  = mpmath.nstr( f(*xs), mpmath.mp.dps )
        print( '\t'.join( param + [sOut]), file=out)


fmt( mpmath.erf,      'inputs/erf.dat'        , 'erf.dat')
fmt( mpmath.erfc,     'inputs/erfc.dat'       , 'erfc.dat')
fmt( mpmath.erfc,     'inputs/erfc-large.dat' , 'erfc-large.dat')
fmt( mpmath.loggamma, 'inputs/loggamma.dat'   , 'loggamma.dat')
fmt( mpmath.digamma,  'inputs/digamma.dat'    , 'digamma.dat')
fmt( mpmath.expm1,    'inputs/expm1.dat'      , 'expm1.dat')
fmt( mpmath.log1p,    'inputs/log1p.dat'      , 'log1p.dat')
fmt_cartesian( lambda a, x: mpmath.gammainc(z=a, a=0, b=x, regularized=True),
               'inputs/igamma.dat', 'igamma.dat')
