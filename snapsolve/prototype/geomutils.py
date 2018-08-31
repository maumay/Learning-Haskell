# -*- coding: utf-8 -*-
"""
Created on Fri Oct 20 10:44:29 2017

@author: ThomasB
"""
import math as m


class Point:
    ''' Basic point class
    '''
    def __init__(self, x, y):
        self.x, self.y = x, y


    def __neg__(self):
        return Point(-self.x, -self.y)


    def __abs__(self):
        return m.sqrt(self.x**2 + self.y**2)


    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)


    def __sub__(self, other):
        return self + (-other)


    def __str__(self):
        return '(' + str(self.x) + ', ' + str(self.y) + ')'


    def dist(self, other):
        return abs(self - other)


    def rotate(self, angle, centre):
        ''' Rotate this point ccw through the specified angle around centre
        angle must be in radians
        '''
        assert type(centre) is Point
        t = angle
        rot_matrix = ((m.cos(t), -m.sin(t)), (m.sin(t), m.cos(t)))
        trans = self - centre
        rot = Point(rot_matrix[0][0]*trans.x + rot_matrix[0][1]*trans.y,
                    rot_matrix[1][0]*trans.x + rot_matrix[1][1]*trans.y)
        return rot + centre


class Line:
    '''
    '''
    def __init__(self, p0, p1):
        assert all(type(param) is Point for param in (p0, p1))
        self.p0, self.p1 = p0, p1


    def distance_from(self, p):
        x, y = p.x, p.y
        p0, p1 = self.p0, self.p1
        numerator = abs((p1.y - p0.y)*x - (p1.x - p0.x)*y +
                        p1.x*p0.y - p1.y*p0.x)
        denominator = m.sqrt((p1.y - p0.y)**2 + (p1.x - p0.x)**2)
        return numerator/denominator


    def __str__(self):
        return 'From: ' + str(self.p0) + ', To: ' + str(self.p1)


    @staticmethod
    def get_corner_finders(sq_len):
        ''' For a square aligned at the origin with side length sq_len this
        method returns the four lines which make up a circumscribed
        diamond enclosing this square. Starting from leftmost corner and
        moving in a clockwise direction.
        '''
        smx, smy = sq_len/2, sq_len/2 # Square mid x and y
        # Diamond corners from left moving clockwise
        corners = [Point(smx - sq_len, smy), Point(smx, smy - sq_len),
                   Point(smx + sq_len, smy), Point(smx, smy + sq_len)]
        # Diagonal lines forming circumscribed diamond shape
        return [Line(corners[i % 4], corners[(i+1) % 4]) for i in range(4)]


