import { Canvas } from "@react-three/fiber";
import "./App.css";
import { OrbitControls, PerspectiveCamera } from "@react-three/drei";
import { useState } from "react";
import { TurbineStage } from "./core-stage";

function App() {
  const [hovered, setHovered] = useState(false);

  return (
    <div style={{ width: "100%", height: "100%", background: "white" }}>
      <Canvas>
        <ambientLight intensity={0.5} />
        <directionalLight position={[0, 0, 2]} intensity={0.5} />
        <directionalLight position={[0, 0, -2]} intensity={0.5} />
        <directionalLight position={[0, 2, 0]} intensity={0.5} />

        <OrbitControls makeDefault enableDamping={false} />

        <TurbineStage />
      </Canvas>
    </div>
  );
}

export default App;
