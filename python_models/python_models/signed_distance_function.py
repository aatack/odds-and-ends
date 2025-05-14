from collections.abc import Callable
from math import sqrt

import numpy as np
import pyvista as pv


class SignedDistanceFunction:
    def __init__(self, function: Callable[[float, float, float], float]) -> None:
        self.function = function

    def mesh(
        self,
        bounds: float = 1,
        *,
        x: int | None = None,
        y: int | None = None,
        z: int | None = None,
    ) -> pv.MultiBlock:
        x = x or y or z or 50
        y = y or x or z or 50
        z = z or x or y or 50

        xs, ys, zs = np.meshgrid(
            np.linspace(-bounds, bounds, x),
            np.linspace(-bounds, bounds, y),
            np.linspace(-bounds, bounds, z),
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

    def distort(
        self,
        coordinate_transform: Callable[
            [float, float, float], tuple[float, float, float]
        ],
    ) -> "SignedDistanceFunction":
        return SignedDistanceFunction(
            lambda x, y, z: self.function(*coordinate_transform(x, y, z))
        )


def point() -> SignedDistanceFunction:
    return SignedDistanceFunction(lambda x, y, z: sqrt(x**2 + y**2 + z**2))


def sphere(radius: float) -> SignedDistanceFunction:
    return point().grow(radius)
