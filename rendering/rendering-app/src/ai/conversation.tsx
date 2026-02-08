import { type ModelMessage, type AssistantContent, type UserContent } from "ai";
import { useState } from "react";

export const Conversation = ({ messages }: { messages: ModelMessage[] }) => {
  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
      {messages.map((message) => {
        if (message.role === "assistant") {
          const content: Exclude<AssistantContent, string> =
            typeof message.content === "string"
              ? [{ type: "text", text: message.content }]
              : message.content;

          return (
            <div
              key={JSON.stringify(message)} // Not ideal
              style={{
                display: "flex",
                flexDirection: "column",
                maxWidth: "80%",
                padding: 8,
                backgroundColor: "white",
                borderRadius: 2,
              }}
            >
              {content.map((part) => {
                if (part.type === "text") {
                  return <p style={{ whiteSpace: "pre-wrap" }}>{part.text}</p>;
                } else if (part.type === "tool-call") {
                  return (
                    <ToolCallMessage
                      key={part.toolCallId}
                      name={part.toolName}
                      input={part.input}
                    />
                  );
                } else {
                  return <HiddenMessage message={part} />;
                }
              })}
            </div>
          );
        } else if (message.role === "user") {
          const content: Exclude<UserContent, string> =
            typeof message.content === "string"
              ? [{ type: "text", text: message.content }]
              : message.content;

          return (
            <div
              key={JSON.stringify(message)} // Not ideal
              style={{
                display: "flex",
                flexDirection: "column",
                maxWidth: "80%",
                padding: 8,
                marginLeft: "auto",
              }}
            >
              {content.map((part) => {
                if (part.type === "text") {
                  return <p>{part.text}</p>;
                } else {
                  return <HiddenMessage message={part} />;
                }
              })}
            </div>
          );
        } else {
          return null;
        }
      })}
    </div>
  );
};

const ToolCallMessage = ({ name, input }: { name: string; input: any }) => {
  const [show, setShow] = useState(false);
  return (
    <div
      onClick={() => setShow((current) => !current)}
      style={{ cursor: "pointer" }}
    >
      Called {name}
      {show && <Debug value={input} />}
    </div>
  );
};

const HiddenMessage = ({ message }: { message: any }) => {
  const [show, setShow] = useState(false);
  return (
    <div
      onClick={() => setShow((current) => !current)}
      style={{ cursor: "pointer" }}
    >
      The content of this message is hidden. Click to show
      {show && <Debug value={message} />}
    </div>
  );
};

export const Debug = ({ value }: { value: any }) => {
  return (
    <pre>
      <h4
        style={{
          fontWeight: "lighter",
          opacity: 0.5,
          fontFamily: "monospace",
        }}
      >
        {JSON.stringify(value, null, 2)}
      </h4>
    </pre>
  );
};
