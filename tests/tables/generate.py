#!/usr/bin/python
"""
"""
import mpmath
import sys

# We want to require quad precision (it never hurts to have some
# buffer for errors
mpmath.mp.dps = 30

def load_inputs(path):
    "Load inputs for function from tsv file"
    with open(path) as f:
        for s in f:
            s = s.strip()
            if s == '' or s[0] == '#':
                continue
            yield (s, tuple(map(float, s.split())))


def fmt(f, inputs, out):
    if out is None:
        out = sys.stdout
    else:
        # Lets hope for cpython's refcounting
        out = open(out, 'w')
    for (sIn,i) in load_inputs(inputs):
        sOut = mpmath.nstr( f(*i), mpmath.mp.dps )
        print(sIn + "\t" + sOut, file=out)

fmt( mpmath.erf,      'inputs/erf.dat'        , 'erf.dat')
fmt( mpmath.erfc,     'inputs/erfc.dat'       , 'erfc.dat')
fmt( mpmath.erfc,     'inputs/erfc-large.dat' , 'erfc-large.dat')
fmt( mpmath.loggamma, 'inputs/loggamma.dat'   , 'loggamma.dat')
fmt( mpmath.digamma,  'inputs/digamma.dat'    , 'digamma.dat')
