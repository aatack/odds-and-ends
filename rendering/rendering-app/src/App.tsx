import { Canvas } from "@react-three/fiber";
import "./App.css";
import { Environment, OrbitControls } from "@react-three/drei";
import { TurbineStage } from "./core-stage";
import { Sword, SwordLod } from "./sword";
import { Shield } from "./shield";

function App() {
  const lowDetail = false;

  return (
    <div style={{ width: "100%", height: "100%", background: "white" }}>
      <Canvas>
        <ambientLight intensity={0.2} />
        <directionalLight position={[5, 5, 5]} intensity={0.5} castShadow />
        <pointLight position={[-5, -2, 2]} intensity={0.5} color="#cbd5e1" />
        <spotLight
          position={[0, 5, -10]}
          intensity={2}
          angle={0.15}
          penumbra={1}
        />
        <Environment preset="studio" />

        <OrbitControls makeDefault enableDamping={false} />

        {/* <TurbineStage /> */}
        <group>
          <group position={[2, 0, 0]}>
            {lowDetail ? <SwordLod /> : <Sword />}
          </group>
          <Shield />
        </group>
      </Canvas>
    </div>
  );
}

export default App;
