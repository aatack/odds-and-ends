import { useRef } from "react";
import * as THREE from "three";

// --- Configuration based on our computed parameters ---
const PARAMS = {
  bladeCount: 40, // Derived from Solidity (1.37)
  height: 3.5, // Aspect Ratio (2.53)
  twist: 0.38, // ~21.6 degrees in radians
  hubRadius: 1,
  metalColor: "#777",
};

const TwistedMaterial = ({
  color,
  twist,
}: {
  color: string;
  twist: number;
}) => (
  <meshStandardMaterial
    color={color}
    onBeforeCompile={(shader) => {
      shader.uniforms.uTwist = { value: twist };
      shader.vertexShader = `
        uniform float uTwist;
        ${shader.vertexShader}
      `.replace(
        `#include <begin_vertex>`,
        `
        #include <begin_vertex>
        // Twist along the x-axis (which is our radial length)
        float angle = position.x * uTwist;
        float c = cos(angle);
        float s = sin(angle);
        mat2 m = mat2(c, -s, s, c);
        transformed.yz = m * transformed.yz;
        `
      );
    }}
  />
);

function Blade({ index }: { index: number }) {
  const angle = (index / PARAMS.bladeCount) * Math.PI * 2;

  return (
    <group rotation={[0, angle, 0]}>
      <mesh
        position={[PARAMS.hubRadius, 0, 0]}
        rotation={[0, 0, 0]} // Fixed alignment
      >
        {/* We use 20 segments along width (x) to ensure the twist is smooth */}
        <boxGeometry args={[PARAMS.height, 0.6, 0.05, 20, 1, 1]} />
        <TwistedMaterial color={"grey"} twist={PARAMS.twist} />
      </mesh>
    </group>
  );
}

export function TurbineStage() {
  const groupRef = useRef<THREE.Group>(null!);

  return (
    <group ref={groupRef} rotation={[Math.PI / 2, 0, 0]}>
      {/* Central Hub Disk */}
      <mesh rotation={[0, 0, 0]}>
        <cylinderGeometry
          args={[PARAMS.hubRadius + 0.1, PARAMS.hubRadius + 0.1, 0.8, 64]}
        />
        <meshStandardMaterial color="grey" />
      </mesh>

      {/* Blade Array */}
      {Array.from({ length: PARAMS.bladeCount }).map((_, i) => (
        <Blade key={i} index={i} />
      ))}
    </group>
  );
}
