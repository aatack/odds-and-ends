import { Canvas } from "@react-three/fiber";
import "./App.css";
import { Environment, OrbitControls } from "@react-three/drei";
import { Sword, SwordLod } from "./sword";
import { Shield } from "./shield";
import { SorcererStaff } from "./staff";
import { Conversation } from "./ai/conversation";
import { useChat } from "./ai/chat";

function App() {
  const chat = useChat();
  const lowDetail = false;

  return (
    <div
      style={{
        width: "100%",
        height: "100%",
        background: "white",
        display: "flex",
        flexDirection: "row",
      }}
    >
      <div style={{ width: "50%" }}>
        <Conversation messages={chat.messages} />
        <input
          onKeyDown={(event) => {
            if (event.key === "Enter") {
              const text = event.target.value;
              if (text) {
                chat.run(text);
                event.target.value = "";
              }
            }
          }}
        />
      </div>

      <div style={{ width: "50%" }}>
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
            <group position={[-2, 0, 0]}>
              <SorcererStaff />
            </group>
          </group>
        </Canvas>
      </div>
    </div>
  );
}

export default App;
