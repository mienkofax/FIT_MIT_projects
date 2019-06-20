import argparse

FP = 0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
A = -0x3
B = 0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
X = 0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296
Y = 0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5


class ECPoint:
    def __init__(self, x, y):
        self.__x = x
        self.__y = y

    def __eq__(self, other):
        if self.x == other.x and self.y == other.y:
            return True
        return False

    def __add__(self, other):
        if self.y == (-other.y) % FP and self.x == other.x:
            return ECPoint(0, 0)

        if self == other:
            s = ((3 * (self.x ** 2) + A) * self.__div(FP, 2 * self.y)) % FP
        else:
            s = ((other.y - self.y) * self.__div(FP, other.x - self.x)) % FP

        x_r = s ** 2 - self.x - other.x
        y_r = s * (self.x - x_r) - self.y
        return ECPoint(x_r % FP, y_r % FP)

    @staticmethod
    def __div(i, j):
        (s, t, u, v) = (1, 0, 0, 1)
        j = j % FP
        while j != 0:
            (q, r) = (i // j, i % j)
            (unew, vnew) = (s, t)
            s = u - (q * s)
            t = v - (q * t)
            (i, j) = (j, r)
            (u, v) = (unew, vnew)

        return u % FP

    @property
    def x(self):
        return self.__x

    @property
    def y(self):
        return self.__y


if __name__ == '__main__':
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument('key')
    args = argument_parser.parse_args()

    # tuple of two hex number to list of numbers
    point = str(args.key).strip()
    point = point.replace('(', '').replace(')', '')
    points = point.split(',')

    # Q = dP
    Q = ECPoint(int(points[0].strip(), 0), int(points[1].strip(), 0))
    R = ECPoint(X, Y)
    P = ECPoint(X, Y)

    d = 1
    while True:
        if R == Q:
            break

        R = P + R
        d += 1

    print(d, end='')
