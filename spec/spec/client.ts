// Hardcoded for now
const SCHEMA = {
  item: {
    user: "user",
    timestamp: "timestamp",
    status: "text",
    __index: {
      timestamp: ["timestamp"],
      user_timestamp: ["user", "timestamp"],
    },
  },
  user: {
    first_name: "string",
    last_name: "string",
    date_of_birth: "timestamp",
    permissions: { array: "text" },
    __index: {
      name: ["first_name"],
    },
  },
};

type Query<T> = { eq: T } | { in: T[] } | { gt?: T; lt?: T; gte?: T; lte?: T };

type Uuid = string;

type Item = {
  user: Uuid;
  timestamp: Date;
  status: string;
};

type User = {
  firstName: string;
  lastName: string;
  dateOfBirth: Date;
  permissions: string[];
};

export const queryItems = (
  query: Partial<{
    [K in keyof Item]: Query<Item[K]>;
  }>,
  options?: { orderBy?: (keyof Item)[]; expand?: {} }
) => {};

queryItems(
  { timestamp: { lt: new Date() }, status: { eq: "active" } },
  {
    orderBy: ["status", "timestamp"],
  }
);
