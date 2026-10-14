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

## Cold cache

This section shows the walk speed when the database file is not in memory.

### Method

- Graph: 100k nodes. We used the existing graph (`walk-100000.db`, IDs `n0`, `n1`, ...). We also used a second graph with hashed IDs (`--random-ids`, `walk-100000-rand.db`, 53 MB). With hashed IDs, the key order has no relation to the tree order. This is like UUID keys.
- Before the runs, the benchmark does a WAL checkpoint (TRUNCATE).
- Warm: the file is in the OS page cache. The SQLite cache is 2000 KiB (the default). `mmap_size` is 0 for all runs.
- Cold (`--cold`): before each run, the benchmark opens a new connection (the SQLite cache is empty). Then it calls `posix_fadvise(POSIX_FADV_DONTNEED)` on the db and WAL files. `vmtouch` shows 0% of the file is resident after this.
- Limited: cold, `--cache-size 256` (256 KiB SQLite cache), and the process runs in a cgroup v2 group with `memory.max` (12M, 10M or 8M) and `memory.swap.max` 0. The limit applies to the process memory and to the page cache that the process uses. A test with sequential and random reads showed that the kernel reads the file again under the limit. The process itself uses approximately 6 MB.
- "MB read" is `read_bytes` from `/proc/self/io` for each run. Major faults are from `/proc/self/stat`. SQLite uses `pread`, not mmap, so major faults stay near 0.
- Each value is the median of 3 runs.
- Disk: virtio block device (`vda`). The kernel reports `rotational = 1`, but this is the default value for virtio. The device is virtual. A random 4 KiB read with `O_DIRECT` takes approximately 22 µs, and a sequential read is approximately 234 MB/s. The host possibly caches the disk. Thus "cold" is cold for this VM only, and the results are optimistic for real disks.

### Results (entities/s)

| graph | scenario | warm | cold | limited 12M | limited 10M | limited 8M |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| sequential IDs | page 1000 | 66308 | 55994 | 52348 | - | - |
| sequential IDs | one page | 60780 | 54125 | (OOM) | - | - |
| sequential IDs | read-ahead off | 35229 | 33950 | 31500 | - | - |
| sequential IDs | frozen fork | 54038 | 46794 | 45202 | - | - |
| hashed IDs | page 1000 | 62867 | 47993 | 49802 | 42971 | 28451 |
| hashed IDs | one page | 53579 | 47270 | (OOM) | - | - |
| hashed IDs | read-ahead off | 33585 | 27526 | 28247 | 27311 | 22549 |
| hashed IDs | frozen fork | 48488 | 45606 | 42080 | 36279 | 24752 |

MB read for each run (hashed IDs, page 1000): warm 0, cold 47, limited 12M 54, limited 10M 116, limited 8M 363.

### Observations

- When the file is cold, the walk reads the file one time. The walk is then 10% to 25% slower.
- The working set of the walk is small. With approximately 5 MB of page cache, the walk reads each page approximately one time. With less page cache, the walk reads the same pages again and again. At 8M, it reads 7 times the file size and the speed decreases by 55%.
- Each extra 4 KiB read costs approximately 20 µs on this machine. This agrees with the measured disk latency.
- A walk that is one page for all 100k nodes did not complete with 128 MB or 256 MB of memory. Inside one call, the walk keeps every loaded entity in its cache. Use pages for large walks. A possible fix: remove cache entries when the walk leaves a subtree.
- The tables that the walk reads (`value_current`, `link_current`, `entities` and their indexes) are approximately 55% of the file.

### What this means for 10M nodes

- The file is approximately 5.5 GB. The walk tables are approximately 3 GB. The working set for one walk is a small part of this, but it increases with the graph.
- If the page cache cannot hold the working set, each entity costs several random reads. On this VM (approximately 20 µs per read), a walk can go down to approximately 20k to 30k entities/s. On a local NVMe disk (approximately 80 to 100 µs per read), expect approximately 5k to 10k entities/s. On a network disk (approximately 0.5 to 1 ms per read), expect approximately 1k entities/s or less.
- Recommendation: give the page cache enough memory for the walk tables, or keep the walk tables on local SSD.

## External IDs only (branch `external-ids`, schema v2)

Schema v2 drops the `entities` table. Event tables, history tables and all indexes key on the external TEXT ID (`entity`, `source`, `destination`) instead of the integer `eid`, and reads no longer do an ID lookup query. We compared master (9970dd3) with this branch on the same machine, back to back. Both used the same binary flags, the same seed and a fresh hashed-ID graph (`--random-ids`, 100k nodes, 16-hex-digit IDs). Each value is the median of 3 runs. The cold and limited methods are the same as in the section above.

| metric (100k, hashed IDs) | master (int `eid`) | external IDs |
| --- | ---: | ---: |
| build: write s | 4.51 | 19.46 |
| build: events/s | 111982 | 25952 |
| file MB after build (with WAL) | 53.1 | 75.4 |
| file MB after checkpoint | 53.1 | 75.4 |
| queries per walk, page 1000 | 41105 | 21329 |
| queries per walk, read-ahead off | 411888 | 211888 |
| queries per walk, frozen fork | 62434 | 42658 |

Entities/s. Warm values are the three warm runs (build run, then two `--reuse` runs).

| scenario | master warm | ext warm | master cold | ext cold | master 12M | ext 12M | master 10M | ext 10M | master 8M | ext 8M |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| page 1000 | 63808 / 64171 / 67257 | 54200 / 55337 / 52517 | 51129 | 45359 | 45642 | 14670 | 41473 | 13316 | 29139 | 9760 |
| one page | 51879 / 56809 / 58578 | 45165 / 53859 / 51101 | 51642 | 43961 | - | - | - | - | - | - |
| read-ahead off | 36480 / 36204 / 33915 | 41305 / 44059 / 40109 | 27888 | 36664 | 26943 | 15931 | 26014 | 14747 | 21339 | 12533 |
| frozen fork | 48641 / 53700 / 53331 | 41582 / 43924 / 39717 | 42471 | 34785 | 38621 | 13140 | 33022 | 11282 | 25123 | 6729 |

MB read per run, page 1000: cold 47.0 vs 38.0; limited 12M 53 vs 955; 10M 121 vs 1136; 8M 393 vs 1793.

### Observations

- The query count halves, as expected: the two ID lookups per load go away. This helps only when a load is very small. With read-ahead off (about 1 ID per query), the branch is 10% to 30% faster.
- With read-ahead on, the branch is about 15% to 20% slower when the file is warm, and about 10% to 20% slower when it is cold. The lookup queries were cheap. The extra cost of the wider TEXT keys is larger: each key and index entry repeats the 16-character ID, and the rows that a walk reads are larger.
- Probable main cause (not measured directly): master gives `eid` values in write order, which is breadth-first tree order. So the rows of siblings sit together in the B-trees, even with hashed external IDs. With TEXT keys, the rows sort by the hash, and siblings are spread over the whole table.
- The cause of the large slowdown under the memory limits is the same: the walk's working set no longer fits. At 12M the branch reads 955 MB per walk (master: 53 MB), and it is 3 to 4 times slower.
- The build is 4.3 times slower and the file is 42% larger. Every insert writes the TEXT ID into the primary key and into each index, at random positions.
- Conclusion: with random (UUID-like) IDs, external-ID keys are slower. The integer dictionary pays for itself in locality and key size, not in query count.
