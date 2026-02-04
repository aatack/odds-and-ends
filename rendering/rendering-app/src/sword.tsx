import { useLayoutEffect, useRef } from "react";
import { Edges } from "@react-three/drei";
import * as THREE from "three";

function JitteredMesh({
  geometry,
  material,
  position,
  rotation,
  scale,
  edgeColor,
}: any) {
  const meshRef = useRef<THREE.Mesh>(null!);

  useLayoutEffect(() => {
    // We do NOT call toNonIndexed() here.
    // This ensures shared vertices move as one unit.
    const geom = meshRef.current.geometry;
    const pos = geom.attributes.position;
    const jitter = 0.05;

    // Use a Map to ensure we don't double-jitter vertices if they are indexed
    // Though for standard geometries, iterating once is usually sufficient
    for (let i = 0; i < pos.count; i++) {
      pos.setXYZ(
        i,
        pos.getX(i) + (Math.random() - 0.5) * jitter,
        pos.getY(i) + (Math.random() - 0.5) * jitter,
        pos.getZ(i) + (Math.random() - 0.5) * jitter
      );
    }

    pos.needsUpdate = true;
    geom.computeVertexNormals();
  }, []);

  return (
    <mesh ref={meshRef} position={position} rotation={rotation} scale={scale}>
      {geometry}
      {material}
      <Edges threshold={5} color={edgeColor} />
    </mesh>
  );
}

export function Sword() {
  const edgeColor = "#111111";
  const dullProps = { metalness: 0, roughness: 1 };

  return (
    <group>
      <JitteredMesh
        position={[0, 1.5, 0]}
        scale={[0.6, 1, 0.2]}
        geometry={<cylinderGeometry args={[0, 0.4, 3, 4]} />}
        material={<meshStandardMaterial color="#94a3b8" {...dullProps} />}
        edgeColor={edgeColor}
      />

      <JitteredMesh
        position={[0, 0, 0]}
        rotation={[0, 0, Math.PI / 2]}
        geometry={<boxGeometry args={[0.3, 1.2, 0.2]} />}
        material={<meshStandardMaterial color="#a16207" {...dullProps} />}
        edgeColor={edgeColor}
      />

      <JitteredMesh
        position={[0, 0, 0.12]}
        geometry={<sphereGeometry args={[0.12, 6, 4]} />}
        material={
          <meshStandardMaterial
            color="#06b6d4"
            emissive="#06b6d4"
            emissiveIntensity={0.5}
            {...dullProps}
          />
        }
        edgeColor={edgeColor}
      />

      <JitteredMesh
        position={[0, -0.5, 0]}
        scale={[0.9, 1, 0.9]}
        geometry={<cylinderGeometry args={[0.1, 0.1, 0.8, 6]} />}
        material={<meshStandardMaterial color="#451a03" {...dullProps} />}
        edgeColor={edgeColor}
      />

      <JitteredMesh
        position={[0, -0.9, 0]}
        geometry={<sphereGeometry args={[0.18, 5, 5]} />}
        material={<meshStandardMaterial color="#a16207" {...dullProps} />}
        edgeColor={edgeColor}
      />
    </group>
  );
}
