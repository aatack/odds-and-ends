import { Edges } from "@react-three/drei";

export function Sword() {
  const edgeColor = "#222222";
  const dullProps = { metalness: 0, roughness: 1 };

  return (
    <group>
      {/* Blade - Reduced to 4 segments for a sharp, flat look */}
      <mesh position={[0, 1.5, 0]} scale={[0.6, 1, 0.2]}>
        <cylinderGeometry args={[0, 0.4, 3, 4]} />
        <meshStandardMaterial color="#94a3b8" {...dullProps} />
        <Edges scale={1} threshold={15} color={edgeColor} />
      </mesh>

      {/* Crossguard - Boxy by nature */}
      <mesh position={[0, 0, 0]} rotation={[0, 0, Math.PI / 2]}>
        <boxGeometry args={[0.3, 1.2, 0.2]} />
        <meshStandardMaterial color="#a16207" {...dullProps} />
        <Edges scale={1} threshold={15} color={edgeColor} />
      </mesh>

      {/* Gem - Reduced segments to look like a cut stone */}
      <mesh position={[0, 0, 0.12]}>
        <sphereGeometry args={[0.12, 6, 4]} />
        <meshStandardMaterial
          color="#06b6d4"
          emissive="#06b6d4"
          emissiveIntensity={0.5}
          {...dullProps}
        />
        <Edges scale={1} threshold={15} color={edgeColor} />
      </mesh>

      {/* Grip - Reduced to 6 segments for a hexagonal feel */}
      <mesh position={[0, -0.5, 0]} scale={[0.9, 1, 0.9]}>
        <cylinderGeometry args={[0.1, 0.1, 0.8, 6]} />
        <meshStandardMaterial color="#451a03" {...dullProps} />
        <Edges scale={1} threshold={15} color={edgeColor} />
      </mesh>

      {/* Pommel - Low segment sphere */}
      <mesh position={[0, -0.9, 0]}>
        <sphereGeometry args={[0.18, 5, 5]} />
        <meshStandardMaterial color="#a16207" {...dullProps} />
        <Edges scale={1} threshold={15} color={edgeColor} />
      </mesh>
    </group>
  );
}
