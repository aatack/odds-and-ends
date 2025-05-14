from collections.abc import Callable
from math import sqrt

import numpy as np
import pyvista as pv


class SignedDistanceFunction:
    def __init__(self, function: Callable[[float, float, float], float]) -> None:
        self.function = function

    def mesh(
        self, x: int | None = None, y: int | None = None, z: int | None = None
    ) -> pv.MultiBlock:
        x = x or y or z or 50
        y = y or x or z or 50
        z = z or x or y or 50

        xs, ys, zs = np.meshgrid(
            np.linspace(-1, 1, x), np.linspace(-1, 1, y), np.linspace(-1, 1, z)
        )

        coordinates = np.vstack([xs.ravel(), ys.ravel(), zs.ravel()]).T
        grid = pv.StructuredGrid(xs, ys, zs)

        grid.point_data["sdf"] = np.vectorize(
            lambda coordinate: self.function(
                coordinate[0], coordinate[1], coordinate[2]
            ),
            signature="(n)->()",
        )(coordinates)

        return grid.contour([0.0], scalars="sdf")


def point() -> SignedDistanceFunction:
    return SignedDistanceFunction(lambda x, y, z: sqrt(x**2 + y**2 + z**2))


def sphere(radius: float) -> SignedDistanceFunction:
    return SignedDistanceFunction(lambda x, y, z: sqrt(x**2 + y**2 + z**2) - radius)
