from numbers import Real
from operator import add, sub
from itertools import starmap
from math import sqrt, sin, cos, pi


class Point:
    ''' Pythonic (hopefully) 'immutable' cartesian point class.
    '''
    def __init__(self, x=0, y=0):
        self._x = x
        self._y = y

    @property
    def x(self):
        return self._x

    @property
    def y(self):
        return self._y

    def __str__(self):
        return "({}, {})".format(*self)

    def __repr__(self):
        return "Point({}, {})".format(*self)

    def __abs__(self):
        x, y = self
        return sqrt(x*x + y*y)

    def __add__(self, other):
        if len(other) != 2:
            raise IndexError()
        return Point(*starmap(add, zip(self, other)))

    def __radd__(self, other):
        return self + other

    def __sub__(self, other):
        if len(other) != 2:
            raise IndexError()
        return Point(*starmap(sub, zip(self, other)))

    def __rsub__(self, other):
        return self - other

    def __mul__(self, a):
        if not isinstance(a, Real):
            raise TypeError()
        return Point(a*self.x, a*self.y)

    def __rmul__(self, a):
        return self*a

    def __getitem__(self, index):
        if index == 0: return self.x
        elif index == 1: return self.y
        else: raise IndexError()

    def __len__(self):
        return 2

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self):
        return hash(self.x) ^ hash(self.y)

    def rotate(self, angle, rotation_centre=(0, 0)):
        theta, cor = angle, rotation_centre
        shifted_p = self - cor
        x, y, cos_theta, sin_theta = *shifted_p, cos(theta), sin(theta)
        rotated = Point(x*cos_theta - y*sin_theta, x*sin_theta + y*cos_theta)
        return rotated + cor
