import { useLayoutEffect, useMemo, useRef } from "react";
import { Edges } from "@react-three/drei";
import * as THREE from "three";

function JitteredMesh({ geometry, material, position, rotation, scale, edgeColor }: any) {
  const meshRef = useRef<THREE.Mesh>(null!);

  useLayoutEffect(() => {
    if (!meshRef.current) return;

    // Clone the geometry so each mesh instance is uniquely jittered
    const geom = meshRef.current.geometry.clone();
    const pos = geom.attributes.position;
    const jitter = 0.01;

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
    meshRef.current.geometry = geom;
  }, [geometry]);

  return (
    <mesh ref={meshRef} position={position} rotation={rotation} scale={scale} geometry={geometry}>
      {material}
      <Edges threshold={5} color={edgeColor} />
    </mesh>
  );
}

export function Shield() {
  const edgeColor = "#111111";
  const dullProps = { metalness: 0, roughness: 1 };

  const kiteGeom = useMemo(() => {
    const shape = new THREE.Shape();
    shape.moveTo(0, 1.2);        
    shape.lineTo(0.7, 1.1);      
    shape.lineTo(0.8, 0.4);      
    shape.lineTo(0, -1.2);       
    shape.lineTo(-0.8, 0.4);     
    shape.lineTo(-0.7, 1.1);     
    shape.lineTo(0, 1.2);        

    const extrudeSettings = { depth: 0.1, bevelEnabled: false };
    return new THREE.ExtrudeGeometry(shape, extrudeSettings);
  }, []);

  return (
    <group>
      {/* 1. MAIN BODY */}
      <JitteredMesh
        position={[0, 0, 0]}
        geometry={kiteGeom}
        material={<meshStandardMaterial color="#7f1d1d" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* 2. REINFORCED BORDER */}
      <JitteredMesh
        position={[0, 0, -0.05]}
        scale={[1.1, 1.05, 1]}
        geometry={kiteGeom}
        material={<meshStandardMaterial color="#a16207" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* 3. VERTICAL SPINE */}
      <JitteredMesh
        position={[0, 0, 0.08]}
        geometry={new THREE.BoxGeometry(0.12, 2.2, 0.1)}
        material={<meshStandardMaterial color="#94a3b8" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* 4. UPPER CROSSBAR */}
      <JitteredMesh
        position={[0, 0.5, 0.08]}
        geometry={new THREE.BoxGeometry(1.4, 0.12, 0.1)}
        material={<meshStandardMaterial color="#94a3b8" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* 5. CENTER BOSS GEM */}
      <JitteredMesh
        position={[0, 0.5, 0.15]}
        geometry={new THREE.SphereGeometry(0.2, 6, 4)}
        material={<meshStandardMaterial 
          color="#06b6d4" 
          emissive="#06b6d4" 
          emissiveIntensity={0.8} 
          {...dullProps} 
        />}
        edgeColor={edgeColor}
      />
    </group>
  );
}