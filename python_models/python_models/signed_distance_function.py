from collections.abc import Callable

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
            np.linspace(0, 1, x), np.linspace(0, 1, y), np.linspace(0, 1, z)
        )

        points = np.vstack([xs.ravel(), ys.ravel(), zs.ravel()]).T
        grid = pv.StructuredGrid(xs, ys, zs)

        grid.point_data["sdf"] = np.vectorize(
            lambda point: self.function(point[0], point[1], point[2]),
            signature="(n)->()",
        )(points)

        return grid.contour([0.0], scalars="sdf")
