from collections.abc import Callable
from math import sqrt

import numpy as np
import pyvista as pv


class SignedDistanceFunction:
    def __init__(self, function: Callable[[float, float, float], float]) -> None:
        self.function = function

    def mesh(
        self,
        x: int | None = None,
        y: int | None = None,
        z: int | None = None,
        distance: float = 1e5,
        buffer: float = 0.1,
    ) -> pv.MultiBlock:
        x = x or y or z or 50
        y = y or x or z or 50
        z = z or x or y or 50

        xs, ys, zs = np.meshgrid(
            np.linspace(
                -distance + self.function(-distance, 0, 0) - buffer,
                distance - self.function(distance, 0, 0) + buffer,
                x,
            ),
            np.linspace(
                -distance + self.function(0, -distance, 0) - buffer,
                distance - self.function(0, distance, 0) + buffer,
                y,
            ),
            np.linspace(
                -distance + self.function(0, 0, -distance) - buffer,
                distance - self.function(0, 0, distance) + buffer,
                z,
            ),
        )

        coordinates = np.vstack([xs.ravel(), ys.ravel(), zs.ravel()]).T
        grid = pv.StructuredGrid(xs, ys, zs)

        grid.point_data["sdf"] = np.vectorize(
            lambda coordinate: self.function(
                coordinate[0], coordinate[1], coordinate[2]
            ),
            signature="(n)->()",
        )(coordinates)

        contours = grid.contour([0.0], scalars="sdf")
        del contours.point_data["sdf"]
        return contours

    def grow(self, distance: float) -> "SignedDistanceFunction":
        return SignedDistanceFunction(lambda x, y, z: self.function(x, y, z) - distance)

    def clip(self, max_x: float) -> "SignedDistanceFunction":
        return SignedDistanceFunction(
            lambda x, y, z: (max_x - x) if x < max_x else self.function(x, y, z)
        )


def point() -> SignedDistanceFunction:
    return SignedDistanceFunction(lambda x, y, z: sqrt(x**2 + y**2 + z**2))


def sphere(radius: float) -> SignedDistanceFunction:
    return point().grow(radius)
