from .point import Point


class LineString:

    def __init__(self, points):
        self._ps = list(points)
        for p in self._ps:
            if not isinstance(p, Point):
                raise TypeError()

    @property
    def points(self):
        return iter(self._ps)

    def __iter__(self):
        return self.points

    def rotate(self, angle, rotation_centre=(0, 0)):
        cor = rotation_centre
        return LineString(p.rotate(angle, cor) for p in self.points)