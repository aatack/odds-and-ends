#!/usr/bin/env node
// Runs Looper against the current directory. A Node launcher rather than a shell
// script, so the `looper` command npm makes from it works on Windows as well as
// everywhere else. Node 22.18+ runs the TypeScript directly, so there is nothing
// to build first.
import "../src/index.ts";
