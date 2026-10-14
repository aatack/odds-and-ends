# Walk benchmarks

This document gives the results of the walk stress benchmark.

## How to run

```
cargo run --release --example bench_walk -- 1000,10000,100000 /workspace/bench-data
```

The first argument is a list of graph sizes. The second argument is the directory for the database files. Do not commit the database files.

## Machine

- CPU: 8 vCPU, Intel Xeon Processor.
- Memory: 15 GB RAM. Approximately 2 GB was available during the runs.
- Build: release profile. SQLite is bundled (rusqlite). Journal mode is WAL.

## Graph

- The generator uses a seeded SplitMix64 RNG. Each run makes the same graph.
- Each node has 2 to 15 children. The generator fills the tree breadth-first.
- Each node has four values: `type` (list, task, note or section), `title`, `done` and `priority`.
- Approximately 5% of nodes have one cross link of the link type `ref`.
- The generator writes events in batches of 2000 operations.
- Fork scenario: the generator forks the branch `frozen` from main. Then it supersedes 10% of the titles on main.

## Method

- Each scenario runs 3 times. The tables show the median.
- "entities/s" is the number of visited entities divided by the median time.
- "loads per 1k" is the number of entity loads for each 1000 visited entities.
- Peak memory is VmHWM from `/proc/self/status`. It is for the full process, for all sizes.

## Write results

| nodes | events | write s | events/s | supersede events | file MB |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 1000 | 5056 | 0.02 | 283658 | 73 | 1.3 |
| 10000 | 50498 | 0.23 | 218659 | 997 | 11.4 |
| 100000 | 504992 | 2.79 | 181080 | 10021 | 63.0 |

The file size includes the WAL file. After a checkpoint, the 100k file is 53 MB.

## Walk results

| nodes | scenario | entities/s | loads per 1k |
| ---: | --- | ---: | ---: |
| 1000 | page 100 | 46818 | 125.0 |
| 1000 | page 1000 | 100569 | 91.0 |
| 1000 | one page | 101289 | 91.0 |
| 1000 | page 1000, read-ahead off | 37943 | 1000.0 |
| 1000 | page 1000, read-ahead width 256 | 99287 | 97.0 |
| 1000 | page 1000, return cache | 102863 | 91.0 |
| 1000 | tree + cross, visited set | 93201 | 107.0 |
| 1000 | page 1000, frozen fork | 82170 | 91.0 |
| 10000 | page 100 | 31459 | 141.0 |
| 10000 | page 1000 | 74954 | 100.0 |
| 10000 | one page | 72685 | 94.0 |
| 10000 | page 1000, read-ahead off | 39958 | 1000.0 |
| 10000 | page 1000, read-ahead width 256 | 68885 | 52.7 |
| 10000 | page 1000, return cache | 74925 | 100.0 |
| 10000 | tree + cross, visited set | 64046 | 135.8 |
| 10000 | page 1000, frozen fork | 62296 | 100.0 |
| 100000 | page 100 | 27489 | 150.8 |
| 100000 | page 1000 | 68468 | 101.1 |
| 100000 | one page | 65325 | 92.9 |
| 100000 | page 1000, read-ahead off | 40122 | 1000.0 |
| 100000 | page 1000, read-ahead width 256 | 61938 | 57.7 |
| 100000 | page 1000, return cache | 67201 | 101.1 |
| 100000 | tree + cross, visited set | 52445 | 139.0 |
| 100000 | page 1000, frozen fork | 55102 | 101.1 |

Peak RSS: 346 MB.

## Changes made during the benchmark

Two small changes in `src/walk.rs`. We measured before and after each change at 100k nodes.

1. The children memo is a stack, not a map that uses the path as a key. Memory for the memo is now O(depth).
2. The loader groups entities by their followed pairs, not by type. Types that follow the same pairs share one links query.

| scenario (100k) | before | after |
| --- | ---: | ---: |
| page 100 | 23k | 27k |
| page 1000 | 56k | 68k |
| one page | 45k | 65k |
| read-ahead off | 38k | 40k |
| tree + cross, visited set | 43k | 52k |
| frozen fork | 42k | 55k |

Peak RSS went from 393 MB to 346 MB.

## Observations and bottlenecks

- Read-ahead is necessary. Without it, the walk does one load for each entity and is approximately 1.7 times slower.
- A wider read-ahead (width 256) decreases the number of loads by half, but the walk is slower. The cost is in the rows that SQLite reads, not in the number of queries. Keep the default (depth 2, width 32).
- Small pages are slow. The walk is stateless, so each page loads the ancestors and the read-ahead again. Page 100 is approximately 2.5 times slower than page 1000. Possible fix: let the caller give the returned cache back to the next call.
- A frozen fork costs approximately 20% more. The read for a bounded range uses the current table UNION ALL the history table.
- Cross links with the visited set cost approximately 20% more. The walk loads the cross-link targets and keeps the visited set.
- Speed decreases slowly when the graph size increases (68k at 100k, 75k at 10k). The B-trees become deeper and the page cache hit rate decreases.
- Peak memory is mostly the SQLite page cache and the walk cache. It is not the memo.

## Extrapolation to 1M and 10M nodes

These numbers are estimates. We did not measure them.

| nodes | events | file size | write time | walk of all nodes (page 1000) |
| ---: | ---: | ---: | ---: | ---: |
| 1M | approximately 5M | approximately 550-650 MB | approximately 30-40 s | approximately 17-25 s |
| 10M | approximately 50M | approximately 5.5-6.5 GB | approximately 6-10 min | approximately 4-6 min |

- At 10M nodes, the database is larger than the available memory on this machine. Then disk reads control the speed, and the speed can be much lower.
- The generator streams the writes in batches, so it can make 1M and 10M graphs. A walk of a full 10M graph keeps a visited set of 10M IDs. Use pages and do not use the returned cache for such walks.
