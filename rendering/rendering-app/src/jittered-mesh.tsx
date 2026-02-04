import { useMemo } from "react";
import { Edges } from "@react-three/drei";
import * as THREE from "three";

export function JitteredMesh({
  geometry,
  material, // Receiving the <meshStandardMaterial /> here
  position,
  rotation,
  scale,
  edgeColor,
}: any) {
  const jitteredGeom = useMemo(() => {
    if (!geometry) return null;

    const geom = geometry.clone();
    const pos = geom.attributes.position;
    const jitterAmount = 0.00;
    const jitterCache: Record<string, { x: number; y: number; z: number }> = {};

    for (let i = 0; i < pos.count; i++) {
      const x = pos.getX(i);
      const y = pos.getY(i);
      const z = pos.getZ(i);
      const key = `${x.toFixed(4)},${y.toFixed(4)},${z.toFixed(4)}`;

      if (!jitterCache[key]) {
        jitterCache[key] = {
          x: (Math.random() - 0.5) * jitterAmount,
          y: (Math.random() - 0.5) * jitterAmount,
          z: (Math.random() - 0.5) * jitterAmount,
        };
      }

      const offset = jitterCache[key];
      pos.setXYZ(i, x + offset.x, y + offset.y, z + offset.z);
    }

    pos.needsUpdate = true;
    geom.computeVertexNormals();
    return geom;
  }, [geometry]);

  return (
    <mesh position={position} rotation={rotation} scale={scale}>
      <primitive object={jitteredGeom} attach="geometry" />
      {/* Render the material prop as a child. 
          R3F sees this JSX element and attaches it to the parent mesh.
      */}
      {material}
      <Edges threshold={5} color={edgeColor} />
    </mesh>
  );
}
