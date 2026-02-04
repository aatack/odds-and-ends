import { useLayoutEffect, useMemo, useRef } from "react";
import { Edges } from "@react-three/drei";
import * as THREE from "three";

function JitteredMesh({ geometry, material, position, rotation, scale, edgeColor }: any) {
  const meshRef = useRef<THREE.Mesh>(null!);

  useLayoutEffect(() => {
    if (!meshRef.current) return;

    // Lower jitter (0.02) for a more refined, "high-quality" craft look
    const geom = meshRef.current.geometry.clone();
    const pos = geom.attributes.position;
    const jitter = 0.00; 

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

  // The Kite silhouette
  const kiteGeom = useMemo(() => {
    const shape = new THREE.Shape();
    shape.moveTo(0, 1.2);        
    shape.lineTo(0.7, 1.1);      
    shape.lineTo(0.8, 0.4);      
    shape.lineTo(0, -1.2);       
    shape.lineTo(-0.8, 0.4);     
    shape.lineTo(-0.7, 1.1);     
    shape.lineTo(0, 1.2);        
    return new THREE.ExtrudeGeometry(shape, { depth: 0.1, bevelEnabled: false });
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

      {/* 2. OUTER TRIM */}
      <JitteredMesh
        position={[0, 0, -0.05]}
        scale={[1.1, 1.05, 1]}
        geometry={kiteGeom}
        material={<meshStandardMaterial color="#a16207" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* 3. STRUCTURAL CROSS */}
      <JitteredMesh
        position={[0, 0, 0.08]}
        geometry={new THREE.BoxGeometry(0.12, 2.2, 0.1)}
        material={<meshStandardMaterial color="#94a3b8" {...dullProps} />}
        edgeColor={edgeColor}
      />
      <JitteredMesh
        position={[0, 0.5, 0.08]}
        geometry={new THREE.BoxGeometry(1.4, 0.12, 0.1)}
        material={<meshStandardMaterial color="#94a3b8" {...dullProps} />}
        edgeColor={edgeColor}
      />

      {/* 4. DETAILED ORNAMENT - Layered Medallion */}
      <group position={[0, 0.5, 0.15]}>
        {/* The Golden Backing Plate (Octagonal) */}
        <JitteredMesh
          geometry={new THREE.CylinderGeometry(0.3, 0.3, 0.05, 8)}
          rotation={[Math.PI / 2, 0, 0]}
          material={<meshStandardMaterial color="#a16207" {...dullProps} />}
          edgeColor={edgeColor}
        />
        
        {/* The Faceted Central Gem */}
        <JitteredMesh
          position={[0, 0, 0.05]}
          geometry={new THREE.IcosahedronGeometry(0.18, 0)}
          material={<meshStandardMaterial 
            color="#06b6d4" 
            emissive="#06b6d4" 
            emissiveIntensity={1.2} 
            {...dullProps} 
          />}
          edgeColor={edgeColor}
        />

        {/* Decorative Steel Bolts */}
        {[0, Math.PI / 2, Math.PI, Math.PI * 1.5].map((angle, i) => (
          <JitteredMesh
            key={i}
            position={[Math.cos(angle) * 0.22, Math.sin(angle) * 0.22, 0.02]}
            geometry={new THREE.BoxGeometry(0.06, 0.06, 0.06)}
            material={<meshStandardMaterial color="#94a3b8" {...dullProps} />}
            edgeColor={edgeColor}
          />
        ))}
      </group>
    </group>
  );
}