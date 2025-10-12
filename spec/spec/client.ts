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
