import unittest

from math import pi
from collections import namedtuple
from snapsolve.geomutils.point import Point

class PointTest(unittest.TestCase):

    def test_equality(self):
        p1, p2 = Point(3.6, 4.5), Point(3.6, 4.5)
        self.assertEqual(p1, p2)
        self.assertEqual(hash(p1), hash(p2))

    def test_add(self):
        p1, p2 = Point(1, 2), Point(-3.2, 9.4)
        self.assertEqual(p1 + p2, Point(-2.2, 11.4))
        self.assertEqual(p2 + p1, Point(-2.2, 11.4))

    def test_sub(self):
        p1, p2 = Point(1, 2), Point(-3.2, 9.4)
        self.assertEqual(p1 - p2, Point(4.2, -7.4))
        self.assertEqual(p2 - p1, Point(-4.2, 7.4))

    def test_mul(self):
        a, p = 5, Point(1, 2)
        self.assertEqual(a*p, Point(5, 10))
        self.assertEqual(p*a, Point(5, 10))

    def test_rotate(self):
        TestInput = namedtuple("TestInput", "src angle cor expected")
        delta, test_set = 0.001, (
            TestInput(Point(1, 0), pi/2, Point(0, 0), Point(0, 1)),
            TestInput(Point(1, 0), -pi/2, Point(0, 0), Point(0, -1)),
            TestInput(Point(1, 0), pi, Point(1, 1), Point(1, 2))
        )
        for test_input in test_set:
            src, angle, cor, expected = test_input
            l2_dist = abs(src.rotate(angle, cor) - expected)
            self.assertAlmostEqual(l2_dist, 0, delta=delta)
