import { Canvas } from "@react-three/fiber";
import "./App.css";

function App() {
  return (
    <div style={{ width: "100%", height: "100%", background: "#111" }}>
      <Canvas>
        <ambientLight intensity={0.5} />
        <pointLight position={[10, 10, 10]} />

        <mesh>
          <boxGeometry args={[1, 1, 1]} />
          <meshStandardMaterial color="orange" />
        </mesh>
      </Canvas>
    </div>
  );
}

export default App;
