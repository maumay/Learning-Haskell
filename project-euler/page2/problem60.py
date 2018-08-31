# -*- coding: utf-8 -*-
"""
Created on Sun Jan 28 16:23:52 2018

@author: t

Project Euler problem 60 solution.

The family is [13, 5197, 5701, 6733, 8389] and it takes ~10 mins to compute
which is really poor but hey ho.
"""

import time
import itertools as it
from array import array
from sympy import sieve
from sympy.ntheory import isprime


def concat(left, right):
    ''' Concatenates two integers '''
    for x in (10**i for i in range(1, 15)):
        if right < x:
            return left*x + right
    raise AssertionError()
    

def is_two_family(prime_pair):
    ''' Checks if a given prime pair is a 2-family '''
    x = prime_pair
    return isprime(concat(*x)) and isprime(concat(x[1], x[0]))


def is_family(ps):
    ''' Checks if a given prime set of size k forms a k-family '''
    for x in it.combinations(ps, r=2):
        if not is_two_family(x):
            return False
    return True


def get_potential_families(prime_pair_families):
    ''' Given some list of ordered 2-tuples this function slices out
        ranges where the first element of the tuple is the same at least
        4 times in a row. It then extracts this common repeated element
        and puts the other elements into an array, the common element and
        the array are kept together in a tuple and a list of all these
        is returned.
    '''
    ps = prime_pair_families
    xs, ys, counter, placeholder = array('i', []), array('i', []), 1, ps[0][0]
    for i in range(1, len(ps)):
        if ps[i][0] == placeholder:
            counter += 1
        else:
            if counter > 3:
                xs.append(i - counter); ys.append(counter);
            placeholder = ps[i][0]
            counter = 1
    if counter > 3:
        xs.append(len(ps) - counter); ys.append(counter);
    # Slicing part
    zs = (ps[z] for z in (slice(xs[i], xs[i] + ys[i]) for i in range(len(xs))))
    # Extracting common element part
    f = lambda x: (x[0][0], array('i', map(lambda z: z[1], x)))
    return list(map(f, zs))


def get_k_fams(ps, k):
    ''' From a list of primes extract all k families '''
    kfams = []
    for x in it.combinations(ps, r=k):
        if is_family(x):
            kfams.append(array('i', x))
    return kfams


def get_five_sets(xs):
    ''' xs is a list of elements in the form returned by get_potential_families
        and this function gets all five families from the list.
    '''
    five_sets = []
    for i in range(len(xs)):
        for four_fam in get_k_fams(xs[i][1], 4):
            four_fam.append(xs[i][0])
            five_sets.append(four_fam)
    return five_sets


sieve._reset()
sieve.extend(10000)

start_time = time.time()
pairs = list(filter(is_two_family, it.combinations(sieve._list, r=2)))
potential_families = get_potential_families(pairs)
ans = get_five_sets(potential_families)
ans.sort(key=sum)
total_time = time.time() - start_time

print(ans)
print()
print(array('i', map(sum, ans)))
print()
print('Total time: ', total_time)