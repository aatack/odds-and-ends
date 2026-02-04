import { Canvas } from "@react-three/fiber";
import "./App.css";
import { OrbitControls, PerspectiveCamera } from "@react-three/drei";
import { useState } from "react";

function App() {
  const [hovered, setHovered] = useState(false);

  return (
    <div style={{ width: "100%", height: "100%", background: "#222" }}>
      <Canvas>
        <ambientLight intensity={0.5} />
        <PerspectiveCamera makeDefault position={[5, 3, 4]}>
          <directionalLight position={[0, 0, 2]} intensity={0.5} />
        </PerspectiveCamera>

        <OrbitControls makeDefault enableDamping={false} />

        <mesh
          onPointerEnter={() => setHovered(true)}
          onPointerLeave={() => setHovered(false)}
        >
          <boxGeometry args={[1, 1, 1]} />
          <meshStandardMaterial color={hovered ? "orange" : "lightblue"} />
        </mesh>
      </Canvas>
    </div>
  );
}

export default App;
