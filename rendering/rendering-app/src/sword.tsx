import * as THREE from "three";
import { useMemo } from "react";
import { JitteredMesh } from "./jittered-mesh";

export function Sword() {
  const edgeColor = "#111111";
  const dullProps = { metalness: 0, roughness: 1 };

  // Memoize geometries to prevent unnecessary re-instantiation
  const geoms = useMemo(
    () => ({
      blade: new THREE.CylinderGeometry(0, 0.4, 3, 4),
      hub: new THREE.BoxGeometry(0.25, 0.25, 0.2),
      quillon: new THREE.CylinderGeometry(0.12, 0.06, 0.4, 4),
      gem: new THREE.IcosahedronGeometry(0.08, 0),
      grip: new THREE.CylinderGeometry(0.1, 0.1, 0.8, 6),
      pommel: new THREE.SphereGeometry(0.18, 5, 5),
    }),
    []
  );

  return (
    <group>
      {/* Blade */}
      <JitteredMesh
        position={[0, 1.5, 0]}
        scale={[0.4, 1, 0.2]}
        geometry={geoms.blade}
        material={<meshStandardMaterial color="#94a3b8" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* Detailed Crossguard */}
      <group>
        <JitteredMesh
          position={[0, 0, 0]}
          geometry={geoms.hub}
          material={<meshStandardMaterial color="#a16207" {...dullProps} />}
          edgeColor={edgeColor}
        />
        <JitteredMesh
          position={[0.3, 0, 0]}
          rotation={[0, 0, -Math.PI / 2]}
          geometry={geoms.quillon}
          material={<meshStandardMaterial color="#a16207" {...dullProps} />}
          edgeColor={edgeColor}
        />
        <JitteredMesh
          position={[-0.3, 0, 0]}
          rotation={[0, 0, Math.PI / 2]}
          geometry={geoms.quillon}
          material={<meshStandardMaterial color="#a16207" {...dullProps} />}
          edgeColor={edgeColor}
        />
      </group>

      {/* Gem */}
      <JitteredMesh
        position={[0, 0, 0.1]}
        geometry={geoms.gem}
        material={
          <meshStandardMaterial
            color="#06b6d4"
            emissive="#06b6d4"
            emissiveIntensity={1}
            {...dullProps}
          />
        }
        edgeColor={edgeColor}
      />

      {/* Grip */}
      <JitteredMesh
        position={[0, -0.5, 0]}
        scale={[0.9, 1, 0.9]}
        geometry={geoms.grip}
        material={<meshStandardMaterial color="#451a03" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* Pommel */}
      <JitteredMesh
        position={[0, -0.9, 0]}
        geometry={geoms.pommel}
        material={<meshStandardMaterial color="#a16207" {...dullProps} />}
        edgeColor={edgeColor}
      />
    </group>
  );
}
