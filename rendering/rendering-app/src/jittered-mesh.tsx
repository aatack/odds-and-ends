import { useLayoutEffect, useRef, useState } from "react";
import { Edges } from "@react-three/drei";
import * as THREE from "three";

export function JitteredMesh({
  geometry,
  material,
  position,
  rotation,
  scale,
  edgeColor,
}: any) {
  const meshRef = useRef<THREE.Mesh>(null!);
  // We use a key or state to force the Edges component to re-calculate
  const [ready, setReady] = useState(false);

  useLayoutEffect(() => {
    if (!meshRef.current) return;

    // 1. IMPORTANT: Clone the geometry so we don't mutate the source prop
    const geom = meshRef.current.geometry.clone();
    const pos = geom.attributes.position;
    const jitterAmount = 0.02;
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

    // 2. Assign the jittered geometry back to the mesh
    meshRef.current.geometry = geom;

    // 3. Signal that Edges can now be rendered against the new geometry
    setReady(true);
  }, [geometry]);

  return (
    <mesh ref={meshRef} position={position} rotation={rotation} scale={scale}>
      {/* We pass the original geometry here, but the Effect replaces it immediately */}
      {geometry}
      {material}
      {/* Only render Edges once the geometry has been jittered */}
      {ready && <Edges threshold={5} color={edgeColor} />}
    </mesh>
  );
}
