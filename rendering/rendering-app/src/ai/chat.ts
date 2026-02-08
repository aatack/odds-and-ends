import { createGoogleGenerativeAI } from "@ai-sdk/google";
import { z } from "zod";
import {
  stepCountIs,
  streamText,
  tool,
  type ModelMessage,
  type UserModelMessage,
} from "ai";
import { createOpenAI } from "@ai-sdk/openai";
import { useMemo, useState } from "react";
import { systemPrompt } from "./system-prompt";

const models = {
  gemini: createGoogleGenerativeAI({
    apiKey: import.meta.env.VITE_GEMINI_API_KEY,
  }).languageModel("gemini-2.5-flash"),
  chatGpt: createOpenAI({
    apiKey: import.meta.env.VITE_OPENAI_API_KEY,
  }).languageModel("gpt-4.1"),
} as const;

export const useChat = () => {
  const [messages, setMessages] = useState<ModelMessage[]>([]);

  const [pendingMessages, setPendingMessages] = useState<ModelMessage[]>([]);

  const tools = useTools();

  const run = async (userPrompt: string) => {
    const message: UserModelMessage = { role: "user", content: userPrompt };
    setMessages((current) => [...current, message]);

    const result = streamText({
      model: models.gemini,
      tools,
      stopWhen: stepCountIs(50),
      onChunk: ({ chunk }) => {
        if (chunk.type === "tool-call") {
          setPendingMessages((current) => [
            ...current,
            {
              role: "assistant",
              content: [chunk],
            },
          ]);
        }

        if (chunk.type === "text-delta") {
          setPendingMessages((current) => {
            const last = current[current.length - 1];

            if (
              last?.role === "assistant" &&
              typeof last.content === "string"
            ) {
              return [
                ...current.slice(0, current.length - 1),
                {
                  ...last,
                  content: last.content + chunk.text,
                },
              ];
            } else {
              return [...current, { role: "assistant", content: chunk.text }];
            }
          });
        }
      },
      onFinish: (result) => {
        setPendingMessages([]);
        setMessages((current) => [...current, ...result.response.messages]);
      },
      messages: [
        ...messages,
        { role: "system", content: systemPrompt },
        message,
      ],
    });

    for await (const _ of result.fullStream) {
      // Just call this to prompt the stream to execute
    }
  };

  const allMessages = useMemo(
    () => [...messages, ...pendingMessages],
    [messages, pendingMessages]
  );

  return { messages: allMessages, run };
};

const useTools = () => {
  return useMemo(() => ({}), []);
};
