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

type Get<T> = { eq: T } | { in: T[] } | { gt?: T; lt?: T; gte?: T; lte?: T };
type Update<T> =
  | { set: T }
  | { add?: T; remove?: T }
  | { inc?: number; dec?: number }
  | { lua: string }
  | Update<T>[];

type Uuid = string;
type Pointer<T> = { id: Uuid; data?: T };

type Item = {
  id: Uuid;
  user: Pointer<User>;
  timestamp: Date;
  status: string;
};

type User = {
  id: Uuid;
  firstName: string;
  lastName: string;
  dateOfBirth: Date;
  permissions: string[];
};

export const getItem = (
  query: Partial<{
    [K in keyof Item]: Get<Item[K]>;
  }>,
  options?: {
    orderBy?: (keyof Item)[];
    expand?: (keyof Item)[];
    then?: (items: Item[]) => Item[];
  }
): Promise<Item[]> => {
  throw new Error("Not implemented");
};

export const updateItem = (items: {
  [id: Uuid]: { [K in keyof Item]: Update<Item[K]> };
}) => {};

getItem(
  { timestamp: { lt: new Date() }, status: { eq: "active" } },
  {
    orderBy: ["status", "timestamp"],
  }
);

export const setItem = (items: { [id: Uuid]: Item | null }) => {};
