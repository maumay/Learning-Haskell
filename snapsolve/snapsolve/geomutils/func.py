from math import sin, cos

from .point import Point
from .linestring import LineString


def translate(translatable, dp):
    if isinstance(translatable, Point):
        return translatable + dp
    elif isinstance(translatable, LineString):
        return LineString(p + dp for p in translatable)
    else:
        raise TypeError()


def scale(scalable, angle, rotation_centre=Point(0, 0)):
    ps, theta, cor = scalable, angle, rotation_centre
    if isinstance(ps, Point):
        shifted_p = ps - cor
        x, y, cos_theta, sin_theta = *shifted_p, cos(theta), sin(theta)
        scaled = Point(x*cos_theta - y*sin_theta, x*sin_theta + y*cos_theta)
        return scaled + cor
    elif isinstance(ps, LineString):
        return LineString(scale(p, theta, cor) for p in ps)
    else:
        raise TypeError()


def rotate(rotatable, angle, rotation_centre=Point(0, 0)):
    ps, theta, cor = rotatable, angle, rotation_centre
    if isinstance(ps, Point):
        shifted_p = ps - cor
        x, y, cos_theta, sin_theta = *shifted_p, cos(theta), sin(theta)
        rotated = Point(x*cos_theta - y*sin_theta, x*sin_theta + y*cos_theta)
        return rotated + cor
    elif isinstance(ps, LineString):
        return LineString(rotate(p, theta, cor) for p in ps)
    else:
        raise TypeError()