import * as THREE from "three";
import { useMemo } from "react";
import { JitteredMesh } from "./jittered-mesh";

export function SorcererStaff() {
  const edgeColor = "#111111";
  const dullProps = { metalness: 0, roughness: 1 };

  const geoms = useMemo(() => {
    // Custom diamond frame for the headpiece
    const frameShape = new THREE.Shape();
    frameShape.moveTo(0, 0.6); // Top
    frameShape.lineTo(0.4, 0); // Right
    frameShape.lineTo(0, -0.6); // Bottom
    frameShape.lineTo(-0.4, 0); // Left
    frameShape.lineTo(0, 0.6);

    // Inner cutout to make it a ring
    const hole = new THREE.Path();
    hole.moveTo(0, 0.4);
    hole.lineTo(0.25, 0);
    hole.lineTo(0, -0.4);
    hole.lineTo(-0.25, 0);
    hole.lineTo(0, 0.4);
    frameShape.holes.push(hole);

    return {
      shaft: new THREE.CylinderGeometry(0.08, 0.1, 4, 6),
      gripWrap: new THREE.CylinderGeometry(0.12, 0.12, 0.1, 8),
      eyeFrame: new THREE.TorusGeometry(0.15, 0.05, 4, 6),
      mainFrame: new THREE.ExtrudeGeometry(frameShape, {
        depth: 0.15,
        bevelEnabled: false,
      }),
      crystal: new THREE.OctahedronGeometry(0.22, 0),
      smallCrystal: new THREE.OctahedronGeometry(0.1, 0),
      spike: new THREE.ConeGeometry(0.08, 0.2, 4),
      buttCap: new THREE.ConeGeometry(0.15, 0.3, 6),
    };
  }, []);

  return (
    <group>
      {/* 1. MAIN SHAFT */}
      <JitteredMesh
        position={[0, -0.5, 0]}
        geometry={geoms.shaft}
        material={<meshStandardMaterial color="#451a03" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* 2. GRIP WRAPS (Red leather segments) */}
      {[1.2, 1.05, 0.9, 0.75].map((y, i) => (
        <JitteredMesh
          key={i}
          position={[0, y, 0]}
          geometry={geoms.gripWrap}
          material={<meshStandardMaterial color="#7f1d1d" {...dullProps} />}
          edgeColor={edgeColor}
        />
      ))}

      {/* 3. THE SECONDARY "EYE" */}
      <group position={[0, 1.7, 0]}>
        <JitteredMesh
          rotation={[0, 0, 0]}
          geometry={geoms.eyeFrame}
          material={<meshStandardMaterial color="#a16207" {...dullProps} />}
          edgeColor={edgeColor}
        />
        <JitteredMesh
          geometry={geoms.smallCrystal}
          material={
            <meshStandardMaterial
              color="#b91c1c"
              emissive="#7f1d1d"
              emissiveIntensity={1}
              {...dullProps}
            />
          }
          edgeColor={edgeColor}
        />
      </group>

      {/* 4. THE MAIN HEADPIECE */}
      <group position={[0, 2.5, 0]}>
        {/* Diamond Gold Frame */}
        <JitteredMesh
          position={[0, 0, -0.075]} // Center the extrusion
          geometry={geoms.mainFrame}
          material={<meshStandardMaterial color="#a16207" {...dullProps} />}
          edgeColor={edgeColor}
        />

        {/* Floating Ruby Crystal */}
        <JitteredMesh
          geometry={geoms.crystal}
          material={
            <meshStandardMaterial
              color="#ef4444"
              emissive="#b91c1c"
              emissiveIntensity={1.5}
              {...dullProps}
            />
          }
          edgeColor={edgeColor}
        />

        {/* External Spikes */}
        {[
          { pos: [0.45, 0, 0], rot: [0, 0, -Math.PI / 2] },
          { pos: [-0.45, 0, 0], rot: [0, 0, Math.PI / 2] },
          { pos: [0, 0.65, 0], rot: [0, 0, 0] },
        ].map((s, i) => (
          <JitteredMesh
            key={i}
            position={s.pos}
            rotation={s.rot}
            geometry={geoms.spike}
            material={<meshStandardMaterial color="#fde047" {...dullProps} />}
            edgeColor={edgeColor}
          />
        ))}
      </group>

      {/* 5. BUTT CAP (The bottom point) */}
      <JitteredMesh
        position={[0, -2.5, 0]}
        rotation={[Math.PI, 0, 0]}
        geometry={geoms.buttCap}
        material={<meshStandardMaterial color="#a16207" {...dullProps} />}
        edgeColor={edgeColor}
      />
    </group>
  );
}
