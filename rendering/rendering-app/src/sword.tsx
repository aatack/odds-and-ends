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
    const geom = meshRef.current.geometry;
    const pos = geom.attributes.position;
    const jitter = 0.00; // Subtle jitter to match the new shield style

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
      {/* Blade */}
      <JitteredMesh
        position={[0, 1.5, 0]}
        scale={[0.4, 1, 0.2]}
        geometry={<cylinderGeometry args={[0, 0.4, 3, 4]} />}
        material={<meshStandardMaterial color="#94a3b8" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* Crossguard - Now slimmer and shorter (0.2 height, 0.8 width) */}
      <JitteredMesh
        position={[0, 0, 0]}
        rotation={[0, 0, Math.PI / 2]}
        geometry={<boxGeometry args={[0.2, 0.8, 0.15]} />}
        material={<meshStandardMaterial color="#a16207" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* Gem */}
      <JitteredMesh
        position={[0, 0, 0.1]}
        geometry={<sphereGeometry args={[0.1, 6, 4]} />}
        material={
          <meshStandardMaterial
            color="#06b6d4"
            emissive="#06b6d4"
            emissiveIntensity={0.8}
            {...dullProps}
          />
        }
        edgeColor={edgeColor}
      />

      {/* Grip */}
      <JitteredMesh
        position={[0, -0.5, 0]}
        scale={[0.9, 1, 0.9]}
        geometry={<cylinderGeometry args={[0.1, 0.1, 0.8, 6]} />}
        material={<meshStandardMaterial color="#451a03" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* Pommel */}
      <JitteredMesh
        position={[0, -0.9, 0]}
        geometry={<sphereGeometry args={[0.18, 5, 5]} />}
        material={<meshStandardMaterial color="#a16207" {...dullProps} />}
        edgeColor={edgeColor}
      />
    </group>
  );
}