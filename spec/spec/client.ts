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
type Mutation<T> =
  | { set: T }
  | { add?: T; remove?: T }
  | { increment: number }
  | { decrement: number }
  | Mutation<T>[];

type Uuid = string;
type Pointer<T> = { id: Uuid; data?: T };

type Item = {
  id: Uuid;
  user: Pointer<User>;
  timestamp: Date;
  status: string;
};

type User = {
  firstName: string;
  lastName: string;
  dateOfBirth: Date;
  permissions: string[];
};

export const readItem = (
  query: Partial<{
    [K in keyof Item]: Query<Item[K]>;
  }>,
  options?: {
    orderBy?: (keyof Item)[];
    expand?: (keyof Item)[];
    then?: (items: Item[]) => Item[];
  }
): Promise<Item[]> => {
  throw new Error("Not implemented");
};

export const mutateItem = (items: {
  [id: Uuid]: { [K in keyof Item]: Mutation<Item[K]> };
}) => {};

readItem(
  { timestamp: { lt: new Date() }, status: { eq: "active" } },
  {
    orderBy: ["status", "timestamp"],
  }
);
