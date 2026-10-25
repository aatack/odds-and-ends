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

## Look up interned IDs once per load (commits 707a6be, 32491f4)

Before, a walk load round called `lookup` (external ID to `eid`) once in the values read and once in the links read, 20,226 lookup queries per 100k walk. Now:

- `707a6be`: the load does one lookup and passes the `eid`s to `values_with` and `links_with`. The public `values`, `links` and the rest are unchanged (they look up, then call the same internals).
- `32491f4`: the links read also returns the `eid` of every other entity it saw, and the walk keeps them for the whole walk. A load looks up only IDs it has not seen. In a tree, nearly every ID of a load is a child that the previous link read returned, so almost no lookup is left.

Same machine, back to back, hashed-ID graph (`--random-ids`, 100k nodes, same data file for both). "old" is 9970dd3. Each cell is the median of 3 runs (warm: 3 invocations, shown as the range). Cold and limited use the method of the cold-cache section.

| scenario (entities/s) | old | new |
| --- | ---: | ---: |
| page 1000, warm | 66640 - 67512 | 80855 - 81451 |
| one page, warm | 64161 - 66663 | 72977 - 78148 |
| read-ahead off, warm | 36650 - 37251 | 50607 - 51472 |
| frozen fork, warm | 54078 - 55385 | 62873 - 64481 |
| page 1000, cold | 55982 | 67772 |
| read-ahead off, cold | 30176 | 41544 |
| frozen fork, cold | 45317 | 50847 |
| page 1000, 12M | 51496 | 61030 |
| page 1000, 10M | 44135 | 61791 |
| page 1000, 8M | 29412 | 59799 |
| read-ahead off, 8M | 22803 | 40451 |
| frozen fork, 8M | 26200 | 47323 |

| SQL queries per full 100k walk | old | new, 707a6be only | new, 32491f4 |
| --- | ---: | ---: | ---: |
| page 1000 | 41105 | 31217 | 21429 |
| frozen fork | 62434 | 52546 | 42758 |
| read-ahead off | 411888 | 311888 | 211988 |

The build is unchanged (about 4 s, 53.1 MB). The big gain under a memory limit (8M: 29k to 60k) is because the walk no longer reads the `entities` table and its unique index, so it touches fewer pages: MB read per walk at 8M went from 395 to 66.

## Rich history and a branch stack (`examples/bench_rich.rs`)

Run: `cargo run --release --example bench_rich -- 100000 /workspace/bench-data --random-ids [--reuse] [--cold]`. Measured on 32491f4 (master), hashed IDs, 100k nodes, page 1000, read-ahead default, `child` links only.

Graph: the same tree as the simple benchmark (same seed and generator calls), then on `base`:

- 1 to 3 extra revisions per entity (2 to 4 in all) of 3 or 4 of the fields `title`, `done`, `priority`, `note`. `priority` and `note` are set to null in 15% of the revisions.
- 30% of the `child` links get an explicit sort key. Half of those are then removed and added again with another explicit sort key.

Branch stack (the same edits go to the frozen and the open variant):

- `mid_frozen` = fork of `base`; `mid_open` = open pull of `base`. Each has its own edits: 15% of titles and priorities overridden, 2% of entities archived (null type), 1% of `child` links removed.
- `top_frozen` = fork of `mid_frozen`; `top_open` = open pull of `mid_open`. Each has 5% titles overridden, 1% archived and 2% of `child` links with a new explicit sort key.
- After that `base` gets 10% more title edits, so only the open stack sees them.

Archived entities and removed links are not reached, so the mid and top walks visit fewer entities (78,495 and 74,490).

Build and size (one run):

| | simple | rich |
| --- | ---: | ---: |
| events written | 504,992 (+10,021 supersede) | 1,357,077 |
| build time | about 4 s | 14.6 s (tree part 4.8 s) |
| file MB | 53.1 | 143.1 |

Rows per table (rich): `value_current` 547,236; `value_history` 639,448; `link_current` 111,272; `link_history` 59,121.

Entities/s (warm: two runs; cold: one run, median of 3 inside; "visited" is the number of entities walked). Simple benchmark: the same tree on the simple file, page 1000, 3 warm runs 80855-81451, cold 67772.

| walk | visited | warm | cold | MB read cold | queries per walk (new) | queries per walk (9970dd3) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| base | 100000 | 73136 / 76067 | 62307 | 47.0 | 21452 | 41072 |
| mid_frozen | 78495 | 34430 / 34447 | 28351 | 108.0 | 34315 | 50052 |
| mid_open | 78495 | 62030 / 62020 | 49524 | 42.7 | 34315 | 50052 |
| top_frozen | 74490 | 30618 / 30801 | 24861 | 106.2 | 48942 | 63911 |
| top_open | 74490 | 54118 / 54265 | 44143 | 42.6 | 48942 | 63911 |
| top_open, read-ahead off | 74490 | 25007 / 25168 | 21904 | 36.9 | 474693 | 623598 |

The old commit 9970dd3 on the same rich file, one warm run: base 61351, mid_frozen 30948, mid_open 52198, top_frozen 27559, top_open 46847, top_open read-ahead off 20735.

### Observations

- `base` on the rich graph is 6% to 10% slower than on the simple graph (73-76k vs 81k), with history tables more than twice as large as the current tables. Reads on an unbounded branch use only the current tables, so history costs little.
- Open stacks are slower as the stack gets deeper: 76k (base), 62k (mid), 54k (top). Each layer in the stack is one more event source, so each read runs one more query for each layer.
- Frozen stacks are about 2 times slower than open ones (mid 34k vs 62k, top 31k vs 54k). A bounded range reads current UNION ALL history, and the cold run reads 2.5 times more data (106 MB vs 43 MB).
- The number of queries is the same for the frozen and the open stack (one query per layer for each read), so the difference is the cost of each query, not their number.

## SQL type filter, history half filter, path move (cbe4906 vs f8e2855)

Three changes, one commit each: the implicit type filter (R23) is a correlated
subquery in the link query instead of a second query (3f89156); the history half
of a frozen read skips rows superseded inside the range (97c90a0); the walk moves
the path into the emitted row instead of cloning it (cbe4906).

Method as before: hashed IDs, warm cache, page 1000, two paired runs each
(base, new, base, new) at `nice -20`, mean of the two runs. Machine shared with
other jobs; noise about +-3% warm (the two runs of one variant differ by up to
that, top_open base by 25%, so read that row with care). Cold-cache runs were within
the +-7% noise and are not tabulated.

| Scenario (entities/s, page 1000) | f8e2855 | cbe4906 | Change |
|---|---|---|---|
| tree 100k, one branch | 77,600 | 90,300 | +16% |
| tree 100k, frozen fork | 63,800 | 68,400 | +7% |
| rich base (no stack) | 72,900 | 80,800 | +11% |
| rich mid_open | 60,900 | 67,900 | +11% |
| rich mid_frozen | 32,600 | 49,800 | +53% |
| rich top_open | 46,800 | 57,900 | +24% |
| rich top_frozen | 29,500 | 40,000 | +36% |

The typed filter helps most on stacks (it removes one query per layer-set per
batch); the history filter helps frozen reads. Equivalence of the SQL type filter
with reading the type value is covered by
`the_sql_type_filter_equals_reading_the_type_for_stacks_forks_and_archives`.

## Scale: 100k to 20M entities (101M events) in one file

Tool: `examples/bench_scale.rs`. Same generator as `bench_walk` (tree, 4 values + 1
`child` link per node, `ref` link on 5%; 5.05 events per entity), hashed IDs, one
branch, batches of 2000 ops, default SQLite cache (2000 KiB), mmap off, page size 4096,
WAL, synchronous NORMAL. One growing file, measured at each milestone after a
checkpoint. Box: 8 cores, 15.6 GB RAM, virtio disk. During this run 9-13 GB of RAM was
free, so the OS page cache held the whole file up to about 10M entities and most of it at
20M. "Cold" = file dropped from the OS cache (`posix_fadvise DONTNEED`), new
connection, root read first. Cold numbers are therefore the worst case of "working
set larger than RAM", on a fast disk; they are not a measurement of a swapping box.

| Entities | Events | File | Bytes/event | Write rate since last milestone | Warm `values` p50/p99 | Warm `children` p50/p99 | Warm walk | Cold `values` p50/p99 | Cold `children` p50/p99 | Cold walk |
|---|---|---|---|---|---|---|---|---|---|---|
| 100k | 0.5M | 54 MB | 106 | 162,600/s | 14/24 us | 27/44 us | 97,700/s | 101/902 us | 297/444 us | 70,900/s |
| 1M | 5.0M | 544 MB | 108 | 105,800/s | 19/31 us | 36/61 us | 83,000/s | 121/1505 us | 347/608 us | 63,000/s |
| 5M | 25.2M | 2.78 GB | 110 | 84,600/s | 27/48 us | 53/93 us | 84,800/s | 145/742 us | 381/680 us | 60,300/s |
| 10M | 50.5M | 5.62 GB | 111 | 85,800/s | 30/57 us | 57/102 us | 83,600/s | 159/383 us | 389/1099 us | 58,500/s |
| 20M | 101.0M | 11.43 GB | 113 | about 88,000/s | 33/590 us | 63/977 us | 81,400/s | 276/2925 us | 601/3156 us | 44,900/s |

Write time for the whole 101M events: 1141 s (88,500 events/s overall, write calls
only). The walk is 1000-step pages from the root, so it does not slow down with size
while cached. Point reads grow slowly (log of the tree depth, and the 20M p99 shows
cache misses). The query plans use no statistics (no `ANALYZE`, no `sqlite_stat1`), so
they cannot change with size; walk and read latency stay flat, which agrees.

Where the bytes are (20M entities, 11.43 GB, 4096-byte pages; same shares at every size):

| Table / index | Share of file |
|---|---|
| `value_current` | 36.1% |
| `value_current_value` (the value index) | 19.5% |
| `value_current` UNIQUE(bid, seq) index | 16.7% |
| `link_current` | 9.1% |
| `link_current` UNIQUE(bid, seq) index | 5.1% |
| `entities` table + `ext_id` index | 4.6% + 5.0% |
| `link_current_in` | 4.0% |

The two secondary indexes on `value_current` (value index and UNIQUE seq) are 36% of the
file: indexes are about 46% of the file in total. Pages are 88-92% full.

Write throughput by batch size (1M entities, 5.05M events, warm):

| Ops per batch | Events/s | Bytes written to storage per event |
|---|---|---|
| 10 | 44,900 | 11,433 |
| 100 | 77,900 | 4,419 |
| 2,000 | 115,100 | 2,180 |
| 20,000 | 143,100 | 1,242 |

Write amplification: the final file is 108 bytes per event, but 2,180 bytes per event
reach storage at 2000 ops per batch (about 20x: each batch writes every touched page
to the WAL, and the checkpoint writes it again). It falls as batches get larger because
more events share each touched page. With sequential IDs the same load runs at 189,700
events/s and 519 bytes per event written (hashed IDs: 103,972 and 2,180): random keys
touch a different leaf in each table and index for every event.

Other settings (1M entities, same load):

| Setting | Events/s | Warm `values` | Warm walk | Cold `values` | Cold walk |
|---|---|---|---|---|---|
| default (page 4096, cache 2 MB) | 115,100 | 19 us | 86,500/s | 121 us | 63,000/s |
| page 16384 | 86,900 | 25 us | 81,600/s | 155 us | 69,300/s |
| page 65536 | 35,100 | 35 us | 87,500/s | 243 us | 71,000/s |
| cache 512 MB | 104,000 | - | - | - | - |
| mmap 4 GB | 110,700 | 17 us | 88,200/s | 505 us | 58,800/s |

Bigger pages cost write speed (5,200 and 9,970 bytes written per event) and give no
read gain here; the cache setting made no difference because the OS cache already held
the file. Cold numbers vary by about 20% between runs.

Verdict for 100M events (measured to 101M on this box): it works. The file is 11.4 GB
(113 bytes/event), writes run at 85-90k events/s with batches of 2000, warm reads and
walks are flat, and a cold point read costs 0.3-0.6 ms. Limits are far away (max
database size 281 TB at 64 KiB pages, 17.5 TB at 4 KiB pages; the 20M file has 2.79M of
4.29G pages). What gets worse: (1) write rate falls from 160k to 85k events/s as the
indexes outgrow the cache and stays near 85k while the OS cache holds the file; beyond
RAM it will fall further (not measured: estimate, a random-page write per index per
event, so disk IOPS bound; this box could not be run out of RAM); (2) cold reads have a
heavier tail (p99 up to 3 ms at 20M); (3) tiny batches are slow (45k events/s at 10
ops) and write 11 KB per event, so the app should batch writes; (4) history tables
grow with edits (not exercised here: this run has almost no history). At 1B events the
file would be about 113 GB (estimate); that needs sharding or a different store.
Mitigations that matter, in order: batch the writes; keep RAM at least as large as the
hot part of the file (value_current and its indexes); sequential or time-ordered IDs
(1.8x write speed, 4x less write traffic); do not raise page size; mmap and a large
SQLite cache gave nothing while the file was in the OS cache. Not measured: dropping
indexes (the value index is 19.5% of the file; `find` needs it), partitioning, and
sharding by branch.


## Find (`examples/bench_find.rs`)

```text
cargo run --release --example bench_find -- FILE N [--branches a,b] [--queries 0,3] \
    [--baseline] [--no-full] [--add-score BR,...] [--values] [--insertion]
```

### Data and method

- **Rich:** the 100k `bench_rich` file (random ids), branch stack base, mid and top, each open and frozen. `--add-score base,mid_frozen,top_frozen` first wrote `score = hash(i) % 10000` (an int) on those branches, so frozen views read it from history.
- **1M simple:** `bench_scale --milestones 1000000 --no-cold --no-dbstat` (one open branch `main`, 1M entities, 5.05M events), plus `score` on `main` (written twice, because a first run was stopped; the open view does not read the history copy).
- Queries: `type = task` (25%), `title =` one entity, `score > 9899` (1%), `score > 4999` (50%), `title contains "12345"` (rare), `title contains "9"` (40%), `type = task AND title contains "99"`, `priority = 1 OR priority = 2` (≈17%), `score > 9899 AND type = task`.
- Pages of 1000, ids only (`values = false`), entity-id order unless stated. "First page" is the median of 5 runs. "Full read" pages through the whole result. "Checked" is the number of candidates rolled up and checked (`Find::scanned`).
- **Naive** is the same query as a `Filter::Custom` closure with `include_archived` (no index can serve it): it reads the dictionary in order, rolls up every entity and checks it. Its numbers are from the first build; its code path did not change.
- Times are wall clock on the machine above, one run per cell (median of 5 for the first page). Other jobs ran on the box during some runs, so a ±15% spread between runs is normal.

### Before and after

**base** (first page ms / full read s / entities checked in the full read)

| query | naive | first build (3dee406) | final (da0b12e) |
| --- | ---: | ---: | ---: |
| type = task | 22.72 / 0.609 / 100906 | 8.47 / 0.220 / 25208 | 8.25 / 0.211 / 25208 |
| title = (1 entity) | 599.79 / 0.611 / 100000 | 0.04 / 0.000 / 1 | 0.02 / 0.000 / 1 |
| score > 9899 (1%) | 572.37 / 0.569 / 100000 | 7.07 / 0.008 / 1007 | 4.84 / 0.005 / 1007 |
| score > 4999 (50%) | 11.81 / 0.599 / 101375 | 8.76 / 0.471 / 49982 | 6.22 / 0.349 / 49982 |
| title contains "12345" (rare) | 601.93 / 0.605 / 100000 | 13.21 / 0.014 / 1 | 13.13 / 0.012 / 1 |
| title contains "9" (common) | 13.73 / 0.580 / 101184 | 11.38 / 0.474 / 40951 | 9.07 / 0.391 / 40951 |
| type = task AND title contains "99" | 567.78 / 0.560 / 100000 | 278.45 / 0.272 / 25208 | 28.58 / 0.029 / 972 |
| priority = 1 OR priority = 2 | 33.09 / 0.594 / 100537 | 21.26 / 0.334 / 16976 | 14.99 / 0.253 / 16976 |
| score > 9899 AND type = task | 617.87 / 0.626 / 100000 | 10.46 / 0.010 / 253 | 5.34 / 0.005 / 1007 |

**mid_open** (first page ms / full read s / entities checked in the full read)

| query | naive | first build (3dee406) | final (da0b12e) |
| --- | ---: | ---: | ---: |
| type = task | 27.08 / 0.686 / 100736 | 17.00 / 0.389 / 26210 | 12.81 / 0.305 / 26210 |
| title = (1 entity) | 701.21 / 0.702 / 100000 | 0.03 / 0.000 / 1 | 0.04 / 0.000 / 1 |
| score > 9899 (1%) | 679.95 / 0.688 / 100000 | 10.03 / 0.010 / 1007 | 7.01 / 0.007 / 1007 |
| score > 4999 (50%) | 14.25 / 0.709 / 101385 | 30.40 / 1.544 / 52036 | 10.35 / 0.549 / 101385 |
| title contains "12345" (rare) | 691.19 / 0.699 / 100000 | 15.32 / 0.017 / 1 | 15.24 / 0.016 / 1 |
| title contains "9" (common) | 16.65 / 0.679 / 100912 | 26.56 / 1.079 / 42682 | 17.38 / 0.644 / 100912 |
| type = task AND title contains "99" | 709.81 / 0.711 / 100000 | 383.04 / 0.399 / 25208 | 35.70 / 0.035 / 972 |
| priority = 1 OR priority = 2 | 37.47 / 0.707 / 100494 | 33.37 / 0.635 / 21546 | 24.62 / 0.446 / 21546 |
| score > 9899 AND type = task | 739.93 / 0.733 / 100000 | 18.12 / 0.016 / 253 | 7.41 / 0.007 / 1007 |

**mid_frozen** (first page ms / full read s / entities checked in the full read)

| query | naive | first build (3dee406) | final (da0b12e) |
| --- | ---: | ---: | ---: |
| type = task | 48.56 / 1.309 / 100736 | 38.93 / 1.003 / 26210 | 23.82 / 0.603 / 26210 |
| title = (1 entity) | 1242.35 / 1.246 / 100000 | 0.03 / 0.000 / 1 | 0.04 / 0.000 / 1 |
| score > 9899 (1%) | 1249.34 / 1.237 / 100000 | 18.94 / 0.017 / 1007 | 12.28 / 0.012 / 1007 |
| score > 4999 (50%) | 23.65 / 1.269 / 101385 | 98.68 / 4.890 / 52036 | 16.32 / 0.808 / 101385 |
| title contains "12345" (rare) | 1235.62 / 1.227 / 100000 | 241.17 / 0.233 / 1 | 61.40 / 0.061 / 1 |
| title contains "9" (common) | 33.02 / 1.384 / 100912 | 157.68 / 6.491 / 42682 | 29.20 / 1.180 / 100912 |
| type = task AND title contains "99" | 1270.11 / 1.288 / 100000 | 683.05 / 0.677 / 25208 | 104.42 / 0.105 / 972 |
| priority = 1 OR priority = 2 | 154.36 / 2.503 / 100494 | 113.71 / 2.163 / 21546 | 67.56 / 1.234 / 21546 |
| score > 9899 AND type = task | 1234.08 / 1.260 / 100000 | 47.86 / 0.044 / 253 | 11.84 / 0.012 / 1007 |

**top_open** (first page ms / full read s / entities checked in the full read)

| query | naive | first build (3dee406) | final (da0b12e) |
| --- | ---: | ---: | ---: |
| type = task | 31.46 / 0.797 / 100890 | 18.68 / 0.460 / 25953 | 16.37 / 0.353 / 25953 |
| title = (1 entity) | 767.78 / 0.792 / 100000 | 0.03 / 0.000 / 1 | 0.03 / 0.000 / 1 |
| score > 9899 (1%) | 763.81 / 0.782 / 100000 | 9.62 / 0.010 / 1007 | 7.43 / 0.007 / 1007 |
| score > 4999 (50%) | 15.52 / 0.805 / 101859 | 34.47 / 1.642 / 51578 | 11.59 / 0.606 / 101859 |
| title contains "12345" (rare) | 787.51 / 0.821 / 100000 | 15.48 / 0.015 / 1 | 14.91 / 0.015 / 1 |
| title contains "9" (common) | 19.27 / 0.837 / 101075 | 28.13 / 1.204 / 42249 | 18.61 / 0.738 / 101075 |
| type = task AND title contains "99" | 805.48 / 0.798 / 100000 | 485.39 / 0.476 / 25208 | 37.69 / 0.036 / 972 |
| priority = 1 OR priority = 2 | 48.11 / 0.830 / 100673 | 40.77 / 0.822 / 21499 | 29.05 / 0.554 / 21499 |
| score > 9899 AND type = task | 799.66 / 0.789 / 100000 | 17.28 / 0.016 / 253 | 9.03 / 0.008 / 1007 |

**top_frozen** (first page ms / full read s / entities checked in the full read)

| query | naive | first build (3dee406) | final (da0b12e) |
| --- | ---: | ---: | ---: |
| type = task | 58.14 / 1.554 / 100890 | 48.42 / 1.187 / 25953 | 31.69 / 0.773 / 25953 |
| title = (1 entity) | 1388.42 / 1.391 / 100000 | 0.05 / 0.000 / 1 | 0.06 / 0.000 / 1 |
| score > 9899 (1%) | 1425.35 / 1.407 / 100000 | 23.02 / 0.024 / 1007 | 16.37 / 0.015 / 1007 |
| score > 4999 (50%) | 27.96 / 1.491 / 101859 | 148.50 / 7.525 / 51578 | 19.81 / 1.014 / 101859 |
| title contains "12345" (rare) | 1431.76 / 1.419 / 100000 | 254.55 / 0.256 / 1 | 65.82 / 0.066 / 1 |
| title contains "9" (common) | 37.48 / 1.494 / 101075 | 175.94 / 6.822 / 42249 | 34.80 / 1.393 / 101075 |
| type = task AND title contains "99" | 1432.99 / 1.435 / 100000 | 847.75 / 0.880 / 25208 | 108.53 / 0.103 / 972 |
| priority = 1 OR priority = 2 | 80.90 / 1.537 / 100673 | 142.15 / 2.544 / 21499 | 83.70 / 1.547 / 21499 |
| score > 9899 AND type = task | 1413.45 / 1.407 / 100000 | 51.75 / 0.049 / 253 | 16.44 / 0.016 / 1007 |


**1M simple, main (one open layer)** (first page ms / full read s / checked)

| query | naive | 3f3549d | final (da0b12e) |
| --- | ---: | ---: | ---: |
| type = task | 27.14 / 6.367 / 1007576 | 15.19 / 4.176 / 249284 | 15.52 / 3.736 / 249284 |
| title = (1 entity) | 6383.94 / 6.626 / 1000000 | 0.02 / 0.000 / 1 | 0.02 / 0.000 / 1 |
| score > 9899 (1%) | 606.20 / 6.200 / 1000291 | 17.93 / 0.157 / 9779 | 19.51 / 0.184 / 9779 |
| score > 4999 (50%) | 11.95 / 6.825 / 1013878 | 12.29 / 6.519 / 500430 | 9.63 / 5.504 / 500430 |
| title contains "12345" (rare) | 6419.91 / 20.710 / 1000000 | 146.79 / 0.157 / 20 | 155.41 / 0.154 / 20 |
| title contains "9" (common) | 13.64 / 6.852 / 1016450 | 26.25 / 13.386 / 468559 | 16.71 / 8.032 / 468559 |
| type = task AND title contains "99" | 536.40 / 6.118 / 1000336 | 224.87 / 2.742 / 11335 | 315.46 / 3.624 / 11335 |
| priority = 1 OR priority = 2 | 17.49 / 6.199 / 1010227 | 19.46 / 7.177 / 334171 | 16.33 / 5.514 / 334171 |
| score > 9899 AND type = task | 2429.49 / 6.342 / 1000069 | 68.85 / 0.192 / 2420 | 68.02 / 0.201 / 2420 |

What changed between the first build (3dee406) and the final one:

| change | commit | measured effect |
| --- | --- | --- |
| `And` estimates its terms (cap 2048) and intersects two large terms in SQL instead of using the best-ranked term alone | 961ce27, da0b12e | `type = task AND contains "99"`, base: first page 278 → 29 ms, checked 25 208 → 972 |
| The implicit type filter alone (no index term) reads the dictionary in order instead of a `type NOT NULL` index plan | 961ce27 | `Custom` filter, `type = task` closure, first build: typed plan 27.7 ms / 0.73 s vs dictionary 22.7 ms / 0.61 s on base, 130.9 ms / 3.18 s vs 48.6 ms / 1.31 s on mid_frozen |
| Existence check (`any_event`) only with `include_archived` | 961ce27 | the type check after the roll-up already proves existence |
| Roll-up of only the keys the filter needs (several keys: `key IN json_each`) | 961ce27 | mid_frozen `score > 9899 AND type = task`, first page: all keys 16.7 ms, two keys 11.7 ms |
| Schema V2: `superseded_by` in `value_history_value` | b0943e8 | history arm of `contains "12345"` for bid 1 alone: 88.7 → 21.6 ms; mid_frozen rare contains, first page 241 → 102 ms (→ 59 ms after 3f3549d) |
| Dense plans on layered views read the dictionary instead of probing each entity | b0943e8 | mid_frozen `score > 4999` full read 4.89 → 0.81 s |
| Candidate estimate counts each layer separately and streams (no `UNION`, stops at the cap) | 3f3549d | the old `UNION` count cost 32 ms per page on mid_frozen; full read of `score > 4999` 4.42 → 0.74 s |
| Dense threshold `sqrt(limit × entities)` instead of 10% of entities | 5c0c0e4 | 1M `contains "9"` full read 13.4 → 8.0 s (counting 100k rows on each page was the cost) |

### Values, order

Base and top_frozen, final build (first page ms / full read s):

| branch | query | ids only | with values | insertion order |
| --- | --- | ---: | ---: | ---: |
| base | type = task | 8.25 / 0.211 | 11.76 / 0.313 | 6.56 / 0.17 |
| base | title = (one entity) | 0.02 / 0 | 0.02 / 0 | 0.02 / 0 |
| base | score > 9899 (1%) | 4.84 / 0.005 | 7.41 / 0.008 | 4.7 / 0.005 |
| base | score > 4999 (50%) | 6.22 / 0.349 | 9.06 / 0.491 | 5.39 / 0.278 |
| base | title contains "12345" (rare) | 13.13 / 0.012 | 13.16 / 0.013 | 13.65 / 0.013 |
| base | title contains "9" (common) | 9.07 / 0.391 | 11.78 / 0.504 | 11.58 / 0.34 |
| base | type = task AND title contains "99" | 28.58 / 0.029 | 31.16 / 0.032 | 29.26 / 0.029 |
| base | priority = 1 OR priority = 2 | 14.99 / 0.253 | 17.58 / 0.334 | 15.48 / 0.249 |
| base | score > 9899 AND type = task | 5.34 / 0.005 | 9.09 / 0.008 | 5.01 / 0.005 |
| top_frozen | type = task | 31.69 / 0.773 | 36.73 / 0.921 | 21.37 / 0.512 |
| top_frozen | title = (one entity) | 0.06 / 0 | 0.05 / 0 | 0.06 / 0 |
| top_frozen | score > 9899 (1%) | 16.37 / 0.015 | 21.05 / 0.021 | 13.39 / 0.013 |
| top_frozen | score > 4999 (50%) | 19.81 / 1.014 | 32.76 / 1.797 | 11.45 / 0.585 |
| top_frozen | title contains "12345" (rare) | 65.82 / 0.066 | 72.52 / 0.071 | 67.34 / 0.07 |
| top_frozen | title contains "9" (common) | 34.8 / 1.393 | 43.6 / 1.758 | 28.61 / 0.747 |
| top_frozen | type = task AND title contains "99" | 108.53 / 0.103 | 113.76 / 0.112 | 103.07 / 0.108 |
| top_frozen | priority = 1 OR priority = 2 | 83.7 / 1.547 | 87.47 / 1.613 | 31.4 / 0.606 |
| top_frozen | score > 9899 AND type = task | 16.44 / 0.016 | 22.93 / 0.021 | 13.53 / 0.013 |

- Values on cost 5–80% more (all keys are read and copied).
- Insertion order reads the dictionary by `eid`, with no probe. On frozen views this is faster than the entity-id order of the same plan: `priority = 1 OR 2` on top_frozen 0.61 s vs 1.55 s (settled in Q2 of round 2: locality of the roll-up; since cb24a30 Insertion pages also probe).

### Contains: instr scan vs FTS5 trigram

Measured with Python's sqlite3 (SQLite 3.46.1, the same minor version as the bundled one) on a copy of the rich file: a contentless FTS5 table (`tokenize='trigram'`, `contentless_delete=1`) over the 300 403 text rows of `value_current` (all branches; history not included), rowid `bid << 40 | seq`, joined back to `value_current` on `(bid, seq)`.

| | instr scan (value index range of `title`, base) | FTS5 trigram |
| --- | ---: | ---: |
| build | – | 2.73 s |
| size | – | +14.6 MB on 156.8 MB (+9%), after VACUUM |
| `contains "12345"` (1 row) | 9.57 ms | 0.04 ms |
| `contains "999"` (280 rows) | 11.08 ms | 0.53 ms |
| `contains "99"` (3 691 rows) | 12.22 ms | not possible (trigram needs 3+ characters) |
| `contains "9"` (40 951 rows) | 19.85 ms | not possible |

Decision: no FTS index for now. The scan of 100k titles costs 10–20 ms. FTS5 is 20–250× faster for rare strings of 3+ characters, but it costs 9% of the file, a second write on each text event (not measured), a history copy for frozen views, and a fallback for short strings. It becomes worth it when one key has millions of text values (1M: rare contains 155 ms per call).

### Schema V2 cost

On the rich file (639 448 history rows): the migration (drop and rebuild `value_history_value` with `superseded_by`) took 0.59 s, and the file grew from 156.8 to 159.4 MB (+1.7%, after VACUUM).

### Observations

- `Eq` is the fast path: 0.02–0.06 ms for one entity on every branch, at 100k and 1M (naive: 0.6–6.5 s).
- Rare terms (1% ranges, rare contains, `And` with a small term) are 19–110× faster than naive at 100k, 31–42× at 1M (full read).
- Dense terms (40–50%) are close to naive, because both must roll up about two entities per row. The index plan gives only rows that can match, so it checks half as many entities.
- Frozen views cost 2–5× open ones: each frozen layer adds a history arm, and the roll-up reads history.
- Weak spots of the build above (both settled in the next section): 1M `contains "9"` (47%) was slower than naive (8.0 s vs 6.9 s full read, wall clock), and 1M `type = task AND contains "99"` was slower than at 3f3549d.

## Find, round 2: open questions settled (CPU time)

The runs above are wall clock and moved ±15% (or more) with other jobs on the box. From here, `bench_find` also reports the CPU time of the thread (`/proc/thread-self/schedstat`), and a Python driver records the CPU time of each whole process (`getrusage(RUSAGE_CHILDREN)`: open, 5 first pages, full read), so old binaries without the CPU columns can be compared. Each cell is the minimum of 2–3 processes. Files: the rich and 1M files of the section above; the current build runs on copies migrated to schema V3.

### Before and after (process CPU s; first page = thread CPU ms, median of 5)

| branch | query | rows | before (31929d0) | after (04582c9) | naive | after, first page CPU ms |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| base | type = task | 25208 | 0.28 | 0.32 | 0.71 | 11.4 |
| base | title = (one entity) | 1 | 0.00 | 0.02 | 3.56 | 3.7 |
| base | score > 9899 (1%) | 1007 | 0.03 | 0.05 | 3.40 | 6.7 |
| base | score > 4999 (50%) | 49982 | 0.38 | 0.45 | 0.63 | 8.0 |
| base | title contains "12345" (rare) | 1 | 0.08 | 0.06 | 3.43 | 8.0 |
| base | title contains "9" (common) | 40951 | 0.42 | 0.40 | 0.64 | 8.0 |
| base | type = task AND title contains "99" | 972 | 0.18 | 0.17 | 3.51 | 26.7 |
| base | priority = 1 OR priority = 2 | 16976 | 0.33 | 0.40 | 0.76 | 19.8 |
| base | score > 9899 AND type = task | 253 | 0.04 | 0.08 | 3.69 | 12.7 |
| mid_frozen | type = task | 24659 | 0.78 | 0.81 | 1.57 | 27.9 |
| mid_frozen | title = (one entity) | 1 | 0.00 | 0.03 | 7.32 | 4.0 |
| mid_frozen | score > 9899 (1%) | 983 | 0.08 | 0.08 | 7.59 | 13.8 |
| mid_frozen | score > 4999 (50%) | 48944 | 0.94 | 0.99 | 1.36 | 19.8 |
| mid_frozen | title contains "12345" (rare) | 1 | 0.38 | 0.16 | 7.05 | 24.9 |
| mid_frozen | title contains "9" (common) | 40121 | 1.37 | 1.25 | 1.46 | 30.6 |
| mid_frozen | type = task AND title contains "99" | 953 | 0.63 | 0.34 | 7.38 | 57.8 |
| mid_frozen | priority = 1 OR priority = 2 | 18407 | 1.61 | 1.39 | 1.60 | 56.0 |
| mid_frozen | score > 9899 AND type = task | 244 | 0.08 | 0.13 | 7.77 | 20.6 |
| top_open | type = task | 24403 | 0.46 | 0.54 | 0.89 | 19.9 |
| top_open | title = (one entity) | 1 | 0.00 | 0.03 | 4.50 | 3.7 |
| top_open | score > 9899 (1%) | 977 | 0.05 | 0.07 | 4.40 | 11.6 |
| top_open | score > 4999 (50%) | 48494 | 0.68 | 0.76 | 0.82 | 14.7 |
| top_open | title contains "12345" (rare) | 1 | 0.09 | 0.07 | 4.50 | 12.0 |
| top_open | title contains "9" (common) | 39730 | 0.88 | 0.69 | 0.84 | 16.9 |
| top_open | type = task AND title contains "99" | 939 | 0.23 | 0.24 | 4.48 | 39.4 |
| top_open | priority = 1 OR priority = 2 | 18227 | 0.74 | 0.86 | 0.96 | 36.1 |
| top_open | score > 9899 AND type = task | 243 | 0.05 | 0.12 | 4.70 | 19.9 |
| top_frozen | type = task | 24403 | 1.07 | 1.08 | 1.68 | 34.7 |
| top_frozen | title = (one entity) | 1 | 0.00 | 0.04 | 7.98 | 5.0 |
| top_frozen | score > 9899 (1%) | 977 | 0.10 | 0.11 | 8.31 | 17.7 |
| top_frozen | score > 4999 (50%) | 48494 | 1.16 | 1.27 | 1.56 | 24.0 |
| top_frozen | title contains "12345" (rare) | 1 | 0.42 | 0.18 | 8.06 | 28.2 |
| top_frozen | title contains "9" (common) | 39730 | 1.68 | 1.52 | 1.57 | 33.9 |
| top_frozen | type = task AND title contains "99" | 939 | 0.65 | 0.38 | 8.34 | 63.6 |
| top_frozen | priority = 1 OR priority = 2 | 18227 | 2.06 | 1.75 | 1.85 | 75.8 |
| top_frozen | score > 9899 AND type = task | 243 | 0.11 | 0.16 | 8.40 | 27.9 |
| main | type = task | 249284 | 3.78 | 3.93 | 6.32 | 16.9 |
| main | title = (one entity) | 1 | 0.00 | 0.02 | 37.86 | 4.0 |
| main | score > 9899 (1%) | 9779 | 0.30 | 0.30 | 9.08 | 19.9 |
| main | score > 4999 (50%) | 500430 | 5.54 | 6.05 | 6.33 | 12.0 |
| main | title contains "12345" (rare) | 20 | 0.96 | 0.48 | 37.72 | 74.7 |
| main | title contains "9" (common) | 468559 | 8.27 | 5.81 | 6.46 | 13.1 |
| main | type = task AND title contains "99" | 11335 | 5.11 | 3.73 | 8.77 | 229.3 |
| main | priority = 1 OR priority = 2 | 334171 | 5.53 | 6.45 | 6.34 | 16.4 |
| main | score > 9899 AND type = task | 2420 | 0.54 | 0.62 | 18.47 | 79.4 |

- `after` is 04582c9. Two later commits change some cells: de66145 (selective `Eq`/range terms counted exactly instead of sampled: `title =` one entity, first page 3.6–4.0 → 0.0 ms CPU; base `score > 9899` 7.9 → 4.4 ms; base `score > 9899 AND type = task` 12.4–14.1 → 8.9–10.4 ms) and cb24a30 (Insertion order, below; entity-id order is unchanged).
- The small queries cost 10–40 ms more process CPU than before: the sampled estimate (2048 primary key lookups per layer) replaced exact counts that were wrong by up to 50× for `contains` (see Q3). de66145 takes back most of it for index-range terms.
- Frozen views: rare contains 0.38–0.42 → 0.16–0.18 s, `type = task AND contains "99"` 0.63–0.65 → 0.34–0.38 s, `priority = 1 OR 2` 1.61–2.06 → 1.39–1.75 s (now below naive 1.60–1.85 s).

### Q1. Dense contains and ranges on one open layer at 1M

Variants measured (full read, 1M `main`): a per-entity `EXISTS` probe in dictionary order (`--knob dense=1`), reading the dictionary and checking every entity after the roll-up (`dense=2`), and the default rule. Wall clock, two runs each, other jobs on the box:

| query | probe (dense=1) | check all (dense=2) | naive |
| --- | ---: | ---: | ---: |
| score > 4999 (50%) | 8.91 / 15.40 s | 11.66 / 9.84 s | 9.58 s |
| title contains "9" (47%) | 15.53 / 10.33 s | 15.70 / 16.95 s | 11.54 s |
| priority = 1 OR 2 (33%) | 7.46 / 8.60 s | 8.14 / 7.87 s | 14.61 s |

The wall clock could not separate them, so the decision used process CPU (3 runs each, current build):

| query | default (probe: the view has no history) | naive |
| --- | ---: | ---: |
| score > 4999 (50%) | 6.04–6.35 s | 6.48–6.52 s |
| title contains "9" (47%) | 5.97–6.08 s | 6.45–6.57 s |
| priority = 1 OR 2 (33%) | 6.32–6.69 s | 6.15–6.33 s |

Decision (9663e18, 04582c9): a dense plan probes each entity when the view reads no history (no frozen range) or the plan is one `Eq`; otherwise it reads the dictionary and checks every entity after the roll-up (a probe that must also search the history arms costs more than the roll-up: top_frozen `score > 4999` full read, wall clock, first build with the probe 7.53 s, da0b12e checking all 1.01 s). With CPU time, the dense 1M queries are 2–8% faster than naive, and the `Or` is up to 6% slower. The old 8.0 s vs 6.9 s gap was mostly the 10%-of-entities dense threshold (fixed in 5c0c0e4) and wall-clock noise.

### Q2. Insertion vs entity-id order

Full read, thread CPU s, ids only (minimum of 2; `--cache-kib` 2000 is the default cache, 65536 holds the hot part of the file):

| data | branch | query | cache KiB | EntityId | Insertion (04582c9) |
| --- | --- | --- | ---: | ---: | ---: |
| rich (random ids) | top_frozen | type = task | 2000 | 0.87 | 0.68 |
| rich | top_frozen | type = task | 65536 | 0.69 | 0.68 |
| rich | top_frozen | score > 4999 | 2000 | 1.15 | 0.76 |
| rich | top_frozen | contains "9" | 2000 | 1.39 | 0.83 |
| rich | top_frozen | priority = 1 OR 2 | 2000 | 1.37 | 0.80 |
| rich | mid_frozen | priority = 1 OR 2 | 2000 | 1.12 | 0.63 |
| 1M random ids | main | type = task | 2000 | 3.78 | 2.43 |
| 1M random ids | main | contains "9" | 2000 | 5.58 | 4.10 |
| 1M random ids | main | priority = 1 OR 2 | 2000 | 6.13 | 4.51 |
| 1M sequential ids | main | type = task | 2000 | 1.82 | 2.45 |
| 1M sequential ids | main | contains "9" | 2000 | 3.35 | 4.26 |
| 1M sequential ids | main | priority = 1 OR 2 | 2000 | 3.85 | 4.79 |

- The gap is locality, not the plan: the roll-up reads `value_current` by `(bid, eid, key)`, so pages in `eid` order read the table in order, and pages in random-id order jump. A larger cache closes part of it. With sequential ids (UUIDv7 and the like) the two orders are the same order, and entity-id order was *faster*, because Insertion pages never used the probe.
- Sorting a chunk by `eid` before the roll-up cannot help: 1000 random ids of 1M are on 1000 different pages either way.
- Fix (cb24a30): dense Insertion pages use the same `EXISTS` probe rule as entity-id pages. Full read, thread CPU s (minimum of 2):

| data | branch | query | Insertion before | Insertion after |
| --- | --- | --- | ---: | ---: |
| rich | base | type = task | 0.212 | 0.145 |
| rich | base | contains "9" | 0.345 | 0.248 |
| rich | base | priority = 1 OR 2 | 0.311 | 0.197 |
| rich | top_frozen | type = task | 0.654 | 0.417 |
| rich | top_frozen | priority = 1 OR 2 | 0.778 | 0.791 |
| 1M random | main | type = task | 2.412 | 1.774 |
| 1M random | main | score > 4999 | 4.121 | 3.357 |
| 1M random | main | contains "9" | 4.100 | 3.232 |
| 1M sequential | main | type = task | 2.355 | 1.739 |
| 1M sequential | main | contains "9" | 4.113 | 3.320 |

- Decision: the default stays `EntityId` (it is the order a caller can use). Recommendation: use `Insertion` for exports and bulk reads with random ids (1.3–1.9× less CPU), and prefer time-ordered ids for new entities, which make entity-id pages as fast as insertion pages.

### Q3. The 1M `And` regression

`type = task AND title contains "99"` (11 335 rows), 1M `main`, process CPU s, 3 runs each:

| build | process CPU s | first page |
| --- | ---: | ---: |
| 3f3549d | 4.01–4.57 | 235–270 ms wall (earlier bisect, 3 runs) |
| 5c0c0e4 | 6.28–8.16 | 351–417 ms wall |
| da0b12e | 5.07–5.65 | 289–400 ms wall |
| 31929d0 | 5.03–5.35 | – |
| now (cb24a30) | 3.72–4.06 | 226–232 ms thread CPU |

- It was real, from 5c0c0e4 (dense threshold `sqrt(limit × entities)`). The estimate counted the first `cap` rows of `title` with `instr`; with the larger cap that count read far more of the index on each page, and where it stopped depended on where matches fell in the index, so the cost and the plan choice moved between pages.
- Fix (6e48eb4, 7737652, 9663e18, 04582c9): estimates for large stores use a fixed hashed sample of 2048 entity numbers (primary key lookups, `+key` keeps SQLite off the value index), so the cost does not depend on the selectivity; `And` intersects its two smallest terms in SQL. Reading the smallest term and probing the others (6e48eb4) measured slower than the `INTERSECT` and was reverted in 04582c9.

### Q4. Order by a value (`FindOrder::ValueAsc(key)` / `ValueDesc(key)`)

Thread CPU, ids only, order key `score` (minimum of 2):

| data | query | rows | by score asc: first page ms / full s | desc | entity id |
| --- | --- | ---: | ---: | ---: | ---: |
| rich base | type = task | 25208 | 20.0 / 0.50 | 19.9 / 0.51 | 10.8 / 0.23 |
| rich base | score > 9899 | 1007 | 4.0 / 0.00 | 6.0 / 0.00 | 7.9 / 0.01 |
| rich base | score > 4999 | 49982 | 4.0 / 0.21 | 4.0 / 0.21 | 8.0 / 0.43 |
| rich top_frozen | type = task | 24403 | 193.0 / 2.98 | 198.9 / 3.10 | 38.8 / 0.88 |
| rich top_frozen | score > 9899 | 977 | 8.0 / 0.01 | 9.2 / 0.01 | 16.5 / 0.02 |
| rich top_frozen | score > 4999 | 48494 | 21.7 / 0.79 | 20.9 / 0.78 | 23.6 / 1.12 |
| 1M main | type = task | 249284 | 25.8 / 6.83 | 24.8 / 6.83 | 16.0 / 3.75 |
| 1M main | score > 9899 | 9779 | 5.0 / 0.06 | 4.0 / 0.06 | 19.8 / 0.18 |
| 1M main | score > 4999 | 500430 | 5.3 / 2.81 | 4.8 / 2.81 | 11.7 / 5.95 |

- Keyset on `(value, eid)`; the cursor is opaque. A filter on the order key is served by the merge scan itself and is 2× faster than entity-id order. A dense filter on another key (`type = task` by score) walks the `score` rows in order and checks the filter: 2× the entity-id cost on open views, 3.4× on top_frozen (the frozen layers merge history rows of `score`, which the rich file changes often).
- Schema V3 (04582c9) adds `seq` to `value_current_value`, so the "is this row the winner" check of the merge reads the index only. The filter's bound on the order key limits the merge scan (04582c9): top_frozen `score > 4999` by score, full read 0.79 s vs 1.12 s in entity-id order.
- Decision: added (ae705e1, 04582c9). The model test covers it (random filters, both directions, paging).

### Q5. FTS5 trigram for contains at 1M

Python sqlite3 on a copy of the 1M file: contentless FTS5 (`tokenize='trigram'`, `contentless_delete=1`) over the 2 000 000 text rows of `value_current`.

| | instr scan | FTS5 trigram |
| --- | ---: | ---: |
| build | – | 52.8 s (37 890 rows/s) |
| size | 564 MB | 686 MB (+122 MB, +21.6%) |
| contains "12345" (20 rows) | 108.6 ms CPU | 0.23 ms |
| contains "99999" (19 rows) | 107.7 ms | 0.29 ms |
| contains "999" (3 700 rows) | 109.0 ms | 6.21 ms |
| contains "123" (3 999 rows) | 109.3 ms | 7.41 ms |
| contains "9" (468 559 rows) | 238.8 ms | not possible (< 3 characters) |
| write of 100k text rows | 0.6 µs/row | 4.4 µs/row (+3.7 µs per text event) |

- The writer averages 14.4 µs per event at 1M (69 294 events/s, `bench_scale`), so FTS5 would add about 26% to each text event (estimate from these two numbers, not measured end to end).
- Branches and history: the index would also need the history rows (frozen views) and the same "winner at the range end" check as the value index, plus a fallback scan for strings under 3 characters.
- Decision: not added. Rare contains at 1M costs ~110 ms per call with the scan, and the cost (+22% file, +26% per text write, a second index path with history) is not worth it now. Revisit as an opt-in per key if one key reaches about 10M text values (the scan grows linearly: ~1 s per call, estimate).

### Schema V3 cost

Migration of the rich file (V2 → V3, 847 236 current value rows; rebuild of `value_current_value` with `seq`): 1.05 s for open + migration + one query (wall clock); a second open takes 6 ms. File after VACUUM: 159.4 → 162.7 MB (+2.1%). Test: `migrates_a_version_1_file_to_the_covering_history_index` migrates a V1 file through V2 and V3 and checks both index column lists.

### Extra optimisations found while profiling

| change | commit | measured |
| --- | --- | --- |
| Index-range terms (all but contains) are counted exactly up to 2048 rows before sampling | de66145 | `title =` one entity, first page 3.6–4.0 → 0.0 ms CPU (base and 1M); base `score > 9899` 7.9 → 4.4 ms |
| Dense Insertion pages probe | cb24a30 | table in Q2: 1M `type = task` 2.41 → 1.77 s |

### Tests

`cargo test --release`: 71 passed. Differential model test with `EGDB_DIFF_CASES=20000`: passed at 04582c9 (325 s) and again at cb24a30 (224 s), with random filters, paging, all four orders, and random planner knobs (probe/dense/exact count/always sample). `cargo clippy --all-targets --release -- -D warnings`: clean.

## Search: type equality and name text (`examples/bench_search.rs`)

```text
cargo run --release --example bench_search -- build N DIR       # DIR/search-N.db
cargo run --release --example bench_search -- run FILE [--branches base,top_open,top_frozen] \
    [--queries t1,c2,...] [--repeats 7] [--full-repeats 3] [--cold] [--knob probe=2]...
```

Measured on a5174c7 (the `find` code of b374eae), same box as above (8 vCPU, 15 GB RAM, about 3 GB free; other jobs ran on the box).

### Data

- Built by `bench_search build` at 10 000, 100 000 and 1 000 000 entities, hashed 16-hex ids, batches of 2000 ops.
- `type`: task 50%, note 25%, project 10%, person 9.9%, doc 5%, invoice 0.1%. `name`: 2 to 5 words from a list of about 300 words with Zipf-like frequencies, mixed case (60% Title Case, 25% sentence case, 10% lower, 5% UPPER), 10% with a number (`2026`, `#417`). Also `done`, `priority` and one `child` link per entity.
- History on `base`: 30% of entities renamed 1 or 2 times, 3% retyped, 20% `done` toggled. Branch stack as in `bench_rich`: `mid_*` renames 10%, archives 1%, retypes 0.5%; `top_*` renames 3%, archives 0.5%; then `base` renames 5% more. `top_open` = open pulls (3 layers, current tables only); `top_frozen` = forks (3 layers, frozen ranges read history).
- Size: 10k 60 025 events, 6.7 MB; 100k 602 330 events, 69.1 MB; 1M 6 026 678 events, 699.8 MB (build 100 s of write calls, 16.6 µs per event).

### Method

- Every query: ids only (`values = false`), entity-id order, limit 1000. "First page" = one `find` call; "all pages" = page with the cursor to the end. Warm: one warm-up call, then the median of 7 first pages (5 at 1M) and of 3 full reads. Wall time and thread CPU time (`CLOCK_THREAD_CPUTIME_ID`) are both medians. Warm runs are CPU bound, so wall and CPU agree within about 1% everywhere; other jobs ran on the box, so expect ±10–15% between runs (repeated cells moved by that much).
- "checked" = `Find::scanned` summed over all pages (candidates rolled up and checked).
- Fuzzy (not in the API) is done with the current API: `Custom` = some word of the name (ASCII-folded) within Levenshtein distance k of the query; "pieces" = `And([Or(ContainsIgnoreCase(piece)…), Custom])` with the query split into k + 1 disjoint pieces (a word within k edits contains one piece unchanged, so the pre-filter never loses a match); "trigrams" = `Or` over every trigram of the query, then the `Custom`.

### Results (current code)

All times in ms per query (whole result for "all pages"). Matches = rows returned by the full read.

#### Type equality (`Filter::Eq("type", ..)`)

| entities | query | matches (base) | base: first page wall / CPU ms | base: all pages wall / CPU ms | base: checked (all pages) | top_open: first / all pages CPU ms | top_frozen: first / all pages CPU ms | matches (top_frozen) |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 10,000 | type = "task" | 5,001 | 4.16 / 4.16 | 20.3 / 20.3 | 5,001 | 6.60 / 33.5 | 8.64 / 42.4 | 4,906 |
| 10,000 | type = "project" | 999 | 2.44 / 2.44 | 2.60 / 2.60 | 999 | 3.85 / 4.20 | 4.47 / 4.40 | 982 |
| 10,000 | type = "doc" | 480 | 1.11 / 1.11 | 1.00 / 1.00 | 480 | 1.42 / 1.40 | 2.20 / 2.40 | 472 |
| 10,000 | type = "invoice" | 9 | 0.03 / 0.03 | 0.00 / 0.00 | 9 | 0.06 / 0.10 | 0.06 / 0.10 | 10 |
| 10,000 | type = "spaceship" | 0 | 0.02 / 0.02 | 0.00 / 0.00 | 0 | 0.02 / 0.00 | 0.02 / 0.00 | 0 |
| 100,000 | type = "task" | 50,278 | 8.03 / 8.03 | 406 / 405 | 50,278 | 11.9 / 613 | 15.4 / 772 | 49,582 |
| 100,000 | type = "project" | 9,988 | 17.0 / 16.9 | 176 / 176 | 9,988 | 31.8 / 328 | 47.6 / 483 | 9,842 |
| 100,000 | type = "doc" | 4,800 | 7.84 / 7.84 | 39.6 / 39.6 | 4,800 | 13.7 / 67.1 | 15.8 / 71.1 | 4,728 |
| 100,000 | type = "invoice" | 120 | 0.31 / 0.31 | 0.20 / 0.20 | 120 | 0.37 / 0.40 | 1.01 / 1.10 | 122 |
| 100,000 | type = "spaceship" | 0 | 0.01 / 0.01 | 0.00 / 0.00 | 0 | 0.02 / 0.00 | 0.02 / 0.00 | 0 |
| 1,000,000 | type = "task" | 500,076 | 10.9 / 10.9 | 6010 / 6010 | 500,076 | 21.4 / 10518 | 25.8 / 13361 | 492,688 |
| 1,000,000 | type = "project" | 99,875 | 29.3 / 29.3 | 2942 / 2941 | 99,875 | 61.2 / 6537 | 86.5 / 8839 | 98,400 |
| 1,000,000 | type = "doc" | 49,466 | 58.8 / 58.8 | 3030 / 3030 | 49,466 | 134 / 5883 | 182 / 8666 | 48,755 |
| 1,000,000 | type = "invoice" | 1,006 | 4.87 / 4.87 | 5.70 / 5.70 | 1,006 | 7.96 / 7.90 | 10.4 / 10.0 | 998 |
| 1,000,000 | type = "spaceship" | 0 | 0.01 / 0.01 | 0.00 / 0.00 | 0 | 0.02 / 0.00 | 0.02 / 0.00 | 0 |

#### Name contains, case-sensitive (`Contains`)

| entities | query | matches (base) | base: first page wall / CPU ms | base: all pages wall / CPU ms | base: checked (all pages) | top_open: first / all pages CPU ms | top_frozen: first / all pages CPU ms | matches (top_frozen) |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 10,000 | name contains "Zeppelin" | 8 | 2.39 / 2.39 | 2.30 / 2.30 | 8 | 3.94 / 3.90 | 3.75 / 3.90 | 10 |
| 10,000 | name contains "Budget" | 175 | 2.93 / 2.92 | 3.00 / 3.00 | 175 | 4.42 / 4.30 | 5.14 / 4.90 | 176 |
| 10,000 | name contains "in" | 2,946 | 7.17 / 7.17 | 20.8 / 20.8 | 2,946 | 15.6 / 40.0 | 18.9 / 53.3 | 2,893 |
| 10,000 | name contains "e" | 9,042 | 4.66 / 4.66 | 45.5 / 45.5 | 9,042 | 6.73 / 60.8 | 8.00 / 72.3 | 8,897 |
| 100,000 | name contains "Zeppelin" | 188 | 11.5 / 11.5 | 11.3 / 11.3 | 188 | 14.2 / 15.1 | 19.0 / 18.4 | 192 |
| 100,000 | name contains "Budget" | 1,873 | 17.2 / 17.2 | 36.2 / 36.2 | 1,873 | 42.6 / 62.5 | 57.9 / 82.6 | 1,875 |
| 100,000 | name contains "in" | 30,023 | 10.5 / 10.5 | 328 / 328 | 30,023 | 18.8 / 528 | 28.7 / 859 | 29,573 |
| 100,000 | name contains "e" | 90,486 | 7.63 / 7.63 | 696 / 696 | 90,486 | 10.8 / 1008 | 13.5 / 1135 | 89,119 |
| 1,000,000 | name contains "Zeppelin" | 1,824 | 93.2 / 93.2 | 186 / 186 | 1,824 | 325 / 449 | 457 / 627 | 1,773 |
| 1,000,000 | name contains "Budget" | 19,198 | 116 / 116 | 2507 / 2507 | 19,198 | 396 / 7391 | 520 / 10238 | 19,019 |
| 1,000,000 | name contains "in" | 301,195 | 14.3 / 14.3 | 4875 / 4874 | 301,195 | 32.4 / 9218 | 42.7 / 13632 | 296,781 |
| 1,000,000 | name contains "e" | 904,331 | 10.4 / 10.4 | 9948 / 9945 | 904,331 | 15.9 / 16173 | 20.0 / 16685 | 891,185 |

#### Name contains, ignore case (`ContainsIgnoreCase`)

| entities | query | matches (base) | base: first page wall / CPU ms | base: all pages wall / CPU ms | base: checked (all pages) | top_open: first / all pages CPU ms | top_frozen: first / all pages CPU ms | matches (top_frozen) |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 10,000 | name contains-ignore-case "zeppelin" | 15 | 3.37 / 3.37 | 3.40 / 3.40 | 15 | 4.69 / 4.80 | 4.81 / 4.80 | 18 |
| 10,000 | name contains-ignore-case "budget" | 260 | 4.39 / 4.39 | 4.20 / 4.20 | 260 | 6.26 / 5.40 | 6.73 / 6.70 | 254 |
| 10,000 | name contains-ignore-case "re" | 5,045 | 5.47 / 5.47 | 28.6 / 28.6 | 5,045 | 8.36 / 42.4 | 12.6 / 70.4 | 5,004 |
| 10,000 | name contains-ignore-case "e" | 9,515 | 4.69 / 4.69 | 45.6 / 45.5 | 9,515 | 6.86 / 67.7 | 7.89 / 77.7 | 9,368 |
| 100,000 | name contains-ignore-case "zeppelin" | 265 | 17.7 / 17.7 | 17.6 / 17.6 | 265 | 20.9 / 21.0 | 29.4 / 33.5 | 270 |
| 100,000 | name contains-ignore-case "budget" | 2,792 | 24.7 / 24.7 | 77.1 / 77.1 | 2,792 | 64.8 / 156 | 92.7 / 238 | 2,790 |
| 100,000 | name contains-ignore-case "re" | 50,793 | 10.0 / 10.0 | 498 / 498 | 50,793 | 14.1 / 729 | 20.4 / 1009 | 50,001 |
| 100,000 | name contains-ignore-case "e" | 95,360 | 7.99 / 7.99 | 785 / 785 | 95,360 | 10.9 / 1084 | 11.4 / 1144 | 93,968 |
| 1,000,000 | name contains-ignore-case "zeppelin" | 2,713 | 155 / 155 | 465 / 465 | 2,713 | 549 / 1293 | 724 / 1708 | 2,626 |
| 1,000,000 | name contains-ignore-case "budget" | 28,544 | 96.6 / 96.6 | 2634 / 2634 | 28,544 | 214 / 5948 | 399 / 11234 | 28,284 |
| 1,000,000 | name contains-ignore-case "re" | 507,281 | 11.9 / 11.9 | 6631 / 6630 | 507,281 | 22.7 / 11843 | 27.1 / 14721 | 499,354 |
| 1,000,000 | name contains-ignore-case "e" | 952,347 | 9.61 / 9.61 | 10541 / 10538 | 952,347 | 18.3 / 16644 | 19.9 / 16907 | 938,478 |

#### Name prefix (`Prefix`)

| entities | query | matches (base) | base: first page wall / CPU ms | base: all pages wall / CPU ms | base: checked (all pages) | top_open: first / all pages CPU ms | top_frozen: first / all pages CPU ms | matches (top_frozen) |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 10,000 | name prefix "Zep" | 1 | 0.02 / 0.02 | 0.00 / 0.00 | 1 | 0.04 / 0.00 | 0.05 / 0.00 | 2 |
| 10,000 | name prefix "Weekly" | 164 | 0.44 / 0.44 | 0.50 / 0.50 | 164 | 0.74 / 0.90 | 1.18 / 1.20 | 165 |
| 10,000 | name prefix "R" | 1,755 | 4.16 / 4.16 | 6.70 / 6.70 | 1,755 | 7.86 / 14.2 | 9.72 / 15.4 | 1,740 |
| 100,000 | name prefix "Zep" | 57 | 0.17 / 0.17 | 0.20 / 0.20 | 57 | 0.32 / 0.30 | 0.55 / 0.50 | 59 |
| 100,000 | name prefix "Weekly" | 1,615 | 5.64 / 5.64 | 9.00 / 9.00 | 1,615 | 11.0 / 16.2 | 15.8 / 20.2 | 1,605 |
| 100,000 | name prefix "R" | 17,243 | 14.3 / 14.3 | 240 / 240 | 17,243 | 23.5 / 413 | 48.3 / 857 | 16,945 |
| 1,000,000 | name prefix "Zep" | 651 | 3.61 / 3.61 | 4.00 / 4.00 | 651 | 6.14 / 6.20 | 8.81 / 8.20 | 640 |
| 1,000,000 | name prefix "Weekly" | 16,008 | 29.3 / 29.3 | 468 / 468 | 16,008 | 70.3 / 1110 | 72.3 / 1183 | 15,933 |
| 1,000,000 | name prefix "R" | 174,382 | 19.5 / 19.5 | 3772 / 3771 | 174,382 | 42.0 / 7434 | 70.3 / 12917 | 171,572 |

#### Fuzzy name match (current API: `Custom`, optionally behind a `ContainsIgnoreCase` pre-filter)

| entities | query | matches (base) | base: first page wall / CPU ms | base: all pages wall / CPU ms | base: checked (all pages) | top_open: first / all pages CPU ms | top_frozen: first / all pages CPU ms | matches (top_frozen) |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 10,000 | name fuzzy "managment" <= 1 edits, Custom scan | 141 | 41.1 / 41.0 | 41.2 / 41.2 | 10,000 | 49.4 / 50.1 | 65.4 / 68.3 | 136 |
| 10,000 | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 141 | 6.63 / 6.62 | 6.50 / 6.50 | 141 | 10.1 / 9.90 | 11.1 / 10.9 | 136 |
| 10,000 | name fuzzy "managment" <= 1, pre-filter any of 7 trigrams | 141 | 40.7 / 40.7 | 40.3 / 40.3 | 1,320 | 50.3 / 50.2 | 68.5 / 67.5 | 136 |
| 10,000 | name fuzzy "mangment" <= 2 edits, Custom scan | 141 | 42.9 / 42.8 | 42.5 / 42.5 | 10,000 | 50.0 / 49.8 | 66.9 / 64.7 | 136 |
| 10,000 | name fuzzy "mangment" <= 2, pre-filter pieces ["ma", "ngm", "ent"] | 141 | 30.5 / 30.4 | 31.7 / 31.7 | 1,931 | 39.9 / 40.3 | 52.5 / 51.4 | 136 |
| 10,000 | name fuzzy "zepelin" <= 1 edits, Custom scan | 15 | 43.4 / 43.4 | 42.7 / 42.7 | 10,000 | 51.5 / 51.4 | 66.3 / 66.1 | 18 |
| 10,000 | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 15 | 6.67 / 6.67 | 7.10 / 7.10 | 137 | 9.35 / 8.90 | 11.2 / 12.6 | 18 |
| 10,000 | name fuzzy "zepelin" <= 1, pre-filter any of 5 trigrams | 15 | 15.8 / 15.8 | 15.7 / 15.7 | 349 | 22.2 / 25.0 | 28.0 / 28.3 | 18 |
| 100,000 | name fuzzy "managment" <= 1 edits, Custom scan | 1,574 | 294 / 294 | 452 / 452 | 100,016 | 361 / 604 | 490 / 752 | 1,567 |
| 100,000 | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 1,574 | 40.9 / 40.9 | 77.9 / 77.9 | 1,574 | 119 / 167 | 165 / 224 | 1,567 |
| 100,000 | name fuzzy "managment" <= 1, pre-filter any of 7 trigrams | 1,574 | 332 / 332 | 527 / 527 | 13,440 | 651 / 1053 | 541 / 846 | 1,567 |
| 100,000 | name fuzzy "mangment" <= 2 edits, Custom scan | 1,574 | 302 / 302 | 470 / 469 | 100,016 | 380 / 588 | 498 / 838 | 1,567 |
| 100,000 | name fuzzy "mangment" <= 2, pre-filter pieces ["ma", "ngm", "ent"] | 1,574 | 236 / 236 | 389 / 388 | 19,633 | 389 / 627 | 518 / 830 | 1,567 |
| 100,000 | name fuzzy "zepelin" <= 1 edits, Custom scan | 265 | 465 / 465 | 462 / 462 | 100,000 | 583 / 582 | 773 / 762 | 270 |
| 100,000 | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 265 | 74.3 / 74.3 | 74.1 / 74.1 | 1,353 | 91.7 / 89.8 | 129 / 130 | 270 |
| 100,000 | name fuzzy "zepelin" <= 1, pre-filter any of 5 trigrams | 265 | 356 / 356 | 368 / 368 | 3,572 | 517 / 520 | 737 / 747 | 270 |
| 1,000,000 | name fuzzy "managment" <= 1 edits, Custom scan | 15,488 | 359 / 359 | 5633 / 5622 | 1,000,485 | 552 / 8253 | 750 / 11462 | 15,221 |
| 1,000,000 | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 15,488 | 325 / 325 | 5573 / 5572 | 15,488 | 1125 / 16979 | 1571 / 24612 | 15,221 |
| 1,000,000 | name fuzzy "managment" <= 1, pre-filter any of 7 trigrams | 15,488 | 442 / 442 | 6916 / 6914 | 133,631 | 977 / 15042 | 818 / 12393 | 15,221 |
| 1,000,000 | name fuzzy "mangment" <= 2 edits, Custom scan | 15,488 | 356 / 356 | 5778 / 5775 | 1,000,485 | 546 / 8208 | 789 / 11612 | 15,221 |
| 1,000,000 | name fuzzy "mangment" <= 2, pre-filter pieces ["ma", "ngm", "ent"] | 15,488 | 322 / 320 | 5226 / 5224 | 194,551 | 634 / 10435 | 828 / 12229 | 15,221 |
| 1,000,000 | name fuzzy "zepelin" <= 1 edits, Custom scan | 2,713 | 1985 / 1985 | 5771 / 5766 | 1,000,080 | 3128 / 8524 | 4217 / 11528 | 2,626 |
| 1,000,000 | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 2,713 | 5821 / 5820 | 14185 / 14184 | 13,623 | 7836 / 18320 | 11160 / 26519 | 2,626 |
| 1,000,000 | name fuzzy "zepelin" <= 1, pre-filter any of 5 trigrams | 2,713 | 1824 / 1824 | 5208 / 5208 | 35,655 | 4039 / 11413 | 4227 / 11761 | 2,626 |

Observations:

- `type` equality: absent and rare types cost 0.01–10 ms at every size. Large types cost about 8–30 ms for the first page on `base`, and the full read grows with the number of matches (about 12 µs per row on `base` at 1M, 20–27 µs on the stacks).
- Contains is a scan of every text value of the key in each layer (the value index range, `instr`): a rare string costs ~2–3 ms at 10k, ~11–18 ms at 100k and ~95–155 ms at 1M on `base`, 3–5× that on the 1M stacks. Common strings are fast for the first page (dense plan: 10–20 ms) and pay per row for the full read.
- `ContainsIgnoreCase` is 1.3–1.7× the case-sensitive scan (`instr(lower(value), ..)` copies every value).
- `Prefix` is an index range: 0.02–9 ms for rare prefixes, and the cost follows the number of matches.
- Fuzzy with `Custom` is a full roll-up of every entity: ~4 µs per entity (41 ms at 10k, 450–760 ms at 100k, 5.6–11.6 s at 1M for all pages). The pigeonhole pre-filter helps when its pieces are rare (`managment`: 6× at 10k and 100k), but not at 1M on `base` (325 vs 359 ms first page, 5.6 s both for all pages: each of the 16 pages scans 1M names twice with `instr(lower())`); it hurts when a piece is common or short (`mangment` k=2 has the piece "ma"; `zepelin` has "elin", 13% of names: at 1M the dense probe plan of that `Or` took 5.8 s for the first page vs 2.0 s for the plain scan). The any-trigram `Or` is no better than the scan.
- Stacks: open stacks cost 1.5–3.5×, frozen stacks 2–5× `base`.

#### Cold (1M, one run per cell)

`--cold`: new connection and `posix_fadvise(DONTNEED)` on the file before the first page and again before the full read. One run per cell, so treat these as indicative. MB read covers both runs.

| branch | query | matches | first page wall / CPU ms | all pages wall / CPU ms | MB read | warm first / all pages wall ms |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| base | type = "task" | 500,076 | 2058 / 120 | 21399 / 7417 | 251.9 MB | 10.9 / 6010 |
| base | type = "project" | 99,875 | 292 / 136 | 5114 / 3998 | 279.8 MB | 29.3 / 2942 |
| base | type = "invoice" | 1,006 | 218 / 28.2 | 57.9 / 26.4 | 21.2 MB | 4.87 / 5.70 |
| base | name contains "Zeppelin" | 1,824 | 2430 / 350 | 810 / 466 | 150.9 MB | 93.2 / 186 |
| base | name contains "Budget" | 19,198 | 1384 / 443 | 3855 / 3130 | 251.6 MB | 116 / 2507 |
| base | name contains-ignore-case "zeppelin" | 2,713 | 565 / 320 | 890 / 659 | 159.8 MB | 155 / 465 |
| base | name prefix "Zep" | 651 | 39.7 / 18.0 | 34.9 / 15.8 | 14.6 MB | 3.61 / 4.00 |
| base | name prefix "Weekly" | 16,008 | 272 / 118 | 1035 / 710 | 135.6 MB | 29.3 / 468 |
| base | name fuzzy "managment" <= 1 edits, Custom scan | 15,488 | 1357 / 735 | 7756 / 6391 | 411.9 MB | 359 / 5633 |
| base | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 15,488 | 827 / 503 | 6107 / 5557 | 239.2 MB | 325 / 5573 |
| top_frozen | type = "task" | 492,688 | 1078 / 129 | 18437 / 14477 | 369.4 MB | 25.8 / 13372 |
| top_frozen | type = "project" | 98,400 | 464 / 238 | 11079 / 9630 | 403.5 MB | 86.5 / 8840 |
| top_frozen | type = "invoice" | 998 | 113 / 51.4 | 168 / 78.4 | 57.5 MB | 10.4 / 10.0 |
| top_frozen | name contains "Zeppelin" | 1,773 | 1448 / 717 | 1210 / 853 | 272.3 MB | 457 / 627 |
| top_frozen | name contains "Budget" | 19,019 | 1174 / 777 | 11640 / 10790 | 422.5 MB | 520 / 10239 |
| top_frozen | name contains-ignore-case "zeppelin" | 2,626 | 1316 / 956 | 2356 / 1971 | 287.1 MB | 724 / 1711 |
| top_frozen | name prefix "Zep" | 640 | 92.3 / 41.0 | 80.5 / 39.6 | 42.9 MB | 8.81 / 8.20 |
| top_frozen | name prefix "Weekly" | 15,933 | 355 / 184 | 2178 / 1601 | 234.4 MB | 72.3 / 1183 |
| top_frozen | name fuzzy "managment" <= 1 edits, Custom scan | 15,221 | 2008 / 1311 | 15189 / 12816 | 590.5 MB | 750 / 11462 |
| top_frozen | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 15,221 | 2236 / 1800 | 25306 / 24534 | 409.2 MB | 1575 / 24615 |

Cold first pages cost 0.04–2.4 s; the full scans read 150–590 MB (most of the 700 MB file). The first query after eviction also pays for the schema and the dictionary pages (t1 `base`: 2.06 s wall but 0.12 s CPU).

### Trigram index prototype (FTS5), not committed

A copy of the crate (outside the repository) added an optional index: a contentless FTS5 table (`tokenize='trigram'`, `contentless_delete=1`) over every text event of `name` (current and history, row id `bid << 40 | seq`), kept up to date by triggers on `value_current`. `Contains` and `ContainsIgnoreCase` of 3 or more characters take their candidates from it, with one arm per layer interval that mirrors the value-index arms (current row in range; history rows still newest at a frozen end), so the candidate set is still a superset and the roll-up decides; dense terms fall back to `instr`. Every query gave the same ids with and without the index (24 queries × 3 branches at 10k and 100k).

Size and build (dbstat, after a checkpoint, no VACUUM):

| entities | name events indexed | build s | file MB before → after | added MB |
| ---: | ---: | ---: | ---: | ---: |
| 10,000 | 17,420 | 0.15 | 6.7 → 8.4 | +1.7 MB (+25.0%) |
| 100,000 | 175,517 | 1.78 | 69.1 → 85.8 | +16.7 MB (+24.1%) |
| 1,000,000 | 1,757,571 | 18.41 (10.5 µs per event) | 699.8 → 866.1 | +166.4 MB (+23.8%) |

Write cost (`Store::write`, random existing entities on `base`, 40 000 events in batches of 2000, 4000 events in batches of 10; µs per event, wall / thread CPU; one run at 1M, two at 100k):

| entities | workload | no index | with trigram index | added |
| ---: | --- | ---: | ---: | ---: |
| 100,000 | renames, batch 2000 | 64–76 / 52–59 | 115 / 100 | +40–50 µs wall per name event |
| 100,000 | mixed (1 in 4 is a rename), batch 2000 | 70–82 / 59–61 | 98–99 / 80 | +16–28 µs per event |
| 100,000 | renames, batch 10 | 93–129 / 60–70 | 147–150 / 92–95 | +20–55 µs per name event |
| 100,000 | mixed, batch 10 | 94–124 / 61–74 | 122–136 / 76–77 | +10–30 µs per event |
| 1,000,000 | renames, batch 2000 | 151 / 97 | 221 / 181 | +70 µs wall, +84 µs CPU per name event |
| 1,000,000 | mixed, batch 2000 | 141 / 94 | 165 / 134 | +24 µs wall, +40 µs CPU per event |
| 1,000,000 | renames, batch 10 | 139 / 78 | 164 / 108 | +25 µs wall, +29 µs CPU per name event |
| 1,000,000 | mixed, batch 10 | 136 / 79 | 150 / 91 | +14 µs wall, +12 µs CPU per event |

(These random superseding updates cost far more than the sequential build, 10–17 µs per event, because each touches random pages of 6–8 B-trees with the default 2 MB cache. Plain `INSERT` triggers instead of `INSERT OR REPLACE` saved about 6 µs per name event.)

Query time, first page / all pages, CPU ms (wall within 1%), `instr` scan → trigram index:

| entities | query | matches (base) | base: instr → FTS, first page CPU ms | base: instr → FTS, all pages CPU ms | top_frozen: instr → FTS, first page CPU ms | top_frozen: instr → FTS, all pages CPU ms |
| ---: | --- | ---: | ---: | ---: | ---: | ---: |
| 10,000 | name contains "Zeppelin" | 8 | 2.39 → 0.14 | 2.30 → 0.10 | 3.75 → 0.37 | 3.90 → 0.30 |
| 10,000 | name contains "Budget" | 175 | 2.92 → 1.21 | 3.00 → 1.20 | 5.14 → 3.00 | 4.90 → 3.00 |
| 10,000 | name contains "in" | 2,946 | 7.17 → 7.53 | 20.8 → 26.5 | 18.9 → 20.3 | 53.3 → 52.6 |
| 10,000 | name contains-ignore-case "zeppelin" | 15 | 3.37 → 0.16 | 3.40 → 0.20 | 4.81 → 0.34 | 4.80 → 0.40 |
| 10,000 | name contains-ignore-case "budget" | 260 | 4.39 → 1.54 | 4.20 → 2.10 | 6.73 → 2.81 | 6.70 → 2.70 |
| 10,000 | name contains-ignore-case "re" | 5,045 | 5.47 → 10.2 | 28.6 → 42.3 | 12.6 → 13.2 | 70.4 → 70.8 |
| 10,000 | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 141 | 6.62 → 1.04 | 6.50 → 1.10 | 11.1 → 1.74 | 10.9 → 1.70 |
| 10,000 | name fuzzy "mangment" <= 2, pre-filter pieces ["ma", "ngm", "ent"] | 141 | 30.4 → 25.4 | 31.7 → 25.7 | 52.5 → 38.4 | 51.4 → 37.4 |
| 10,000 | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 15 | 6.67 → 0.88 | 7.10 → 0.80 | 11.2 → 1.54 | 12.6 → 2.20 |
| 100,000 | name contains "Zeppelin" | 188 | 11.5 → 3.04 | 11.3 → 3.00 | 19.0 → 6.19 | 18.4 → 6.00 |
| 100,000 | name contains "Budget" | 1,873 | 17.2 → 38.0 | 36.2 → 61.4 | 57.9 → 80.2 | 82.6 → 129 |
| 100,000 | name contains "in" | 30,023 | 10.5 → 10.5 | 328 → 334 | 28.7 → 30.7 | 859 → 910 |
| 100,000 | name contains-ignore-case "zeppelin" | 265 | 17.7 → 3.15 | 17.6 → 3.10 | 29.4 → 6.04 | 33.5 → 6.00 |
| 100,000 | name contains-ignore-case "budget" | 2,792 | 24.7 → 13.6 | 77.1 → 38.2 | 92.7 → 48.4 | 238 → 117 |
| 100,000 | name contains-ignore-case "re" | 50,793 | 10.0 → 9.02 | 498 → 495 | 20.4 → 19.4 | 1009 → 1000 |
| 100,000 | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 1,574 | 40.9 → 11.4 | 77.9 → 19.5 | 165 → 35.0 | 224 → 48.4 |
| 100,000 | name fuzzy "mangment" <= 2, pre-filter pieces ["ma", "ngm", "ent"] | 1,574 | 236 → 219 | 388 → 355 | 518 → 514 | 830 → 808 |
| 100,000 | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 265 | 74.3 → 18.5 | 74.1 → 18.4 | 129 → 30.3 | 130 → 31.7 |
| 1,000,000 | name contains "Zeppelin" | 1,824 | 93.2 → 79.0 | 186 → 113 | 457 → 174 | 627 → 241 |
| 1,000,000 | name contains "Budget" | 19,198 | 116 → 148 | 2507 → 3150 | 520 → 605 | 10238 → 11548 |
| 1,000,000 | name contains "in" | 301,195 | 14.3 → 16.0 | 4874 → 5510 | 42.7 → 45.8 | 13632 → 13215 |
| 1,000,000 | name contains-ignore-case "zeppelin" | 2,713 | 155 → 24.5 | 465 → 64.7 | 724 → 96.7 | 1708 → 231 |
| 1,000,000 | name contains-ignore-case "budget" | 28,544 | 96.6 → 110 | 2634 → 3046 | 399 → 406 | 11234 → 11488 |
| 1,000,000 | name contains-ignore-case "re" | 507,281 | 11.9 → 14.2 | 6630 → 7280 | 27.1 → 37.3 | 14721 → 15407 |
| 1,000,000 | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 15,488 | 325 → 79.7 | 5572 → 1296 | 1571 → 324 | 24612 → 4828 |
| 1,000,000 | name fuzzy "mangment" <= 2, pre-filter pieces ["ma", "ngm", "ent"] | 15,488 | 320 → 322 | 5224 → 5369 | 828 → 820 | 12229 → 12103 |
| 1,000,000 | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 2,713 | 5820 → 1274 | 14184 → 3434 | 11160 → 2136 | 26519 → 5068 |

- Rare strings: 15–20× faster at 10k, 3.5–6× at 100k, 1.2–7.5× at 1M (`base` "Zeppelin" 93 → 79 ms, "zeppelin" ignore case 155 → 25 ms; `top_frozen` 457 → 174 ms and 724 → 97 ms). Case-insensitive gains most; case-sensitive rare strings gain less, because the index is case-insensitive: the candidates include the other casings (2731 for 1824 rows of "Zeppelin"), and a page that rejects many candidates queries twice.
- Medium strings (2–3% of names: "Budget"): case-sensitive is slower with the index at every size from 100k (1M `base` 148 vs 116 ms first page); ignore case is 2× faster at 100k and 1.1–1.2× slower at 1M. Each hit costs an FTS row plus a lookup of its event, so 20–30k hits cost more than the sequential scan of the value index.
- Short strings (1–2 characters) and dense strings cannot use it.
- Fuzzy with pieces: `managment` at 1M 4.1× (first page) and 4.3× (all pages) faster on `base`, 5× on `top_frozen`; `zepelin` 4.6× / 4.1×. The q-gram count filter (names with at least `trigrams − 3k` query trigrams, FTS only) found candidates in 99 ms (`managment`) and 29 ms (`zepelin`) at 1M, not better than the pieces, and for k = 2 on an 8-letter word the threshold is 0 (no filter).

### Optimisations measured (committed later: 243bd73, ccc445f, 733a675, 6a7952b; see "Search: the committed find optimisations")

Two changes in a copy of `find.rs`, interface unchanged; `cargo test --release` passed (72 tests) and the differential model test passed with `EGDB_DIFF_CASES=3000`:

1. `ContainsIgnoreCase` uses a Rust SQL function `egdb_contains(value, needle, 1)` (ASCII fold, no copy of the value) in place of `instr(lower(value), needle) > 0`. Same result, also for values with NUL bytes. (`LIKE '%x%'` was 2× faster than `instr(lower())` but misses values that contain a NUL byte, checked; with a NUL guard the gain shrank to ~13%. A Rust function for case-sensitive contains was slower than `instr`, so `Contains` keeps `instr`.) Needs the `functions` feature of rusqlite.
2. Sparse plans fetch twice the chunk of candidates and keep the rest for the next chunk of the same call. A sparse plan scans and sorts all candidates on every query, so a page that rejected candidates (overridden names in a stack, the other casings) paid a whole second scan.

CPU ms per query, first page / all pages, now → both changes:

| entities | branch | query | matches | first page CPU ms: now → opt | all pages CPU ms: now → opt |
| ---: | --- | --- | ---: | ---: | ---: |
| 100,000 | base | name contains "Zeppelin" | 188 | 12.6 → 11.7 | 12.2 → 12.7 |
| 100,000 | base | name contains "Budget" | 1,873 | 16.9 → 18.6 | 32.7 → 34.1 |
| 100,000 | base | name contains-ignore-case "zeppelin" | 265 | 17.6 → 12.2 | 18.0 → 12.2 |
| 100,000 | base | name contains-ignore-case "budget" | 2,792 | 25.1 → 18.8 | 76.6 → 57.6 |
| 100,000 | base | name prefix "Weekly" | 1,615 | 5.39 → 6.06 | 9.80 → 9.20 |
| 100,000 | base | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 1,574 | 43.9 → 32.3 | 86.6 → 57.8 |
| 100,000 | base | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 265 | 80.9 → 33.4 | 84.4 → 33.3 |
| 100,000 | top_open | name contains "Zeppelin" | 187 | 14.9 → 14.6 | 15.0 → 14.3 |
| 100,000 | top_open | name contains "Budget" | 1,874 | 47.6 → 23.7 | 65.5 → 42.2 |
| 100,000 | top_open | name contains-ignore-case "zeppelin" | 261 | 22.6 → 16.1 | 24.1 → 15.7 |
| 100,000 | top_open | name contains-ignore-case "budget" | 2,791 | 80.4 → 24.9 | 167 → 67.8 |
| 100,000 | top_open | name prefix "Weekly" | 1,605 | 11.4 → 7.98 | 18.5 → 13.4 |
| 100,000 | top_open | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 1,563 | 124 → 41.0 | 172 → 74.6 |
| 100,000 | top_open | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 261 | 92.0 → 43.1 | 90.4 → 43.6 |
| 100,000 | top_frozen | name contains "Zeppelin" | 192 | 19.0 → 19.5 | 19.5 → 20.2 |
| 100,000 | top_frozen | name contains "Budget" | 1,875 | 60.5 → 29.4 | 90.0 → 55.5 |
| 100,000 | top_frozen | name contains-ignore-case "zeppelin" | 270 | 31.7 → 20.4 | 31.4 → 21.0 |
| 100,000 | top_frozen | name contains-ignore-case "budget" | 2,790 | 97.5 → 33.0 | 223 → 91.8 |
| 100,000 | top_frozen | name prefix "Weekly" | 1,605 | 13.8 → 11.2 | 20.7 → 17.4 |
| 100,000 | top_frozen | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 1,567 | 166 → 52.4 | 242 → 101 |
| 100,000 | top_frozen | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 270 | 129 → 57.2 | 126 → 60.0 |
| 1,000,000 | base | name contains "Zeppelin" | 1,824 | 107 → 94.9 | 210 → 194 |
| 1,000,000 | base | name contains "Budget" | 19,198 | 133 → 131 | 2638 → 2519 |
| 1,000,000 | base | name contains-ignore-case "zeppelin" | 2,713 | 163 → 104 | 525 → 301 |
| 1,000,000 | base | name contains-ignore-case "budget" | 28,544 | 103 → 87.2 | 2819 → 2600 |
| 1,000,000 | base | name prefix "Weekly" | 16,008 | 33.6 → 31.7 | 523 → 507 |
| 1,000,000 | base | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 15,488 | 349 → 222 | 5754 → 3549 |
| 1,000,000 | base | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 2,713 | 5969 → 1556 | 15079 → 3878 |
| 1,000,000 | top_open | name contains "Zeppelin" | 1,782 | 335 → 116 | 447 → 224 |
| 1,000,000 | top_open | name contains "Budget" | 18,976 | 403 → 136 | 7444 → 2583 |
| 1,000,000 | top_open | name contains-ignore-case "zeppelin" | 2,638 | 537 → 119 | 1301 → 353 |
| 1,000,000 | top_open | name contains-ignore-case "budget" | 28,237 | 214 → 194 | 5996 → 5430 |
| 1,000,000 | top_open | name prefix "Weekly" | 15,935 | 74.8 → 38.2 | 1176 → 568 |
| 1,000,000 | top_open | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 15,218 | 1138 → 257 | 17518 → 4110 |
| 1,000,000 | top_open | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 2,638 | 7822 → 2287 | 18284 → 5319 |
| 1,000,000 | top_frozen | name contains "Zeppelin" | 1,773 | 463 → 160 | 636 → 323 |
| 1,000,000 | top_frozen | name contains "Budget" | 19,019 | 536 → 186 | 10455 → 3754 |
| 1,000,000 | top_frozen | name contains-ignore-case "zeppelin" | 2,626 | 725 → 177 | 1663 → 536 |
| 1,000,000 | top_frozen | name contains-ignore-case "budget" | 28,284 | 392 → 412 | 10994 → 11285 |
| 1,000,000 | top_frozen | name prefix "Weekly" | 15,933 | 69.3 → 42.2 | 1120 → 708 |
| 1,000,000 | top_frozen | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 15,221 | 1482 → 401 | 23433 → 5831 |
| 1,000,000 | top_frozen | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 2,626 | 10765 → 3264 | 24659 → 7626 |

- Rare contains on the 1M stacks: 335 → 116 ms (`top_open`), 463 → 160 ms (`top_frozen`) for the first page; ignore case 537 → 119 ms and 725 → 177 ms. Medium contains on stacks 2.9× faster for the full read. Fuzzy with pieces 2.6–4.4× faster. Single-layer case-sensitive queries do not change (within noise). `top_frozen` ignore-case "budget" at 1M does not change: its plan is the dictionary scan.

A third, measured with the existing planner knobs only (no code written): the dense threshold for a lone `Eq` term. Today a plan is dense at `sqrt(limit × entities)` candidates (10 000 at 100k, 31 623 at 1M) and is then probed per entity. For `type = X` the sorted (sparse) plan stays cheaper up to about 2.5× that count:

| entities | branch | query | matches | default (probe): first page / all pages CPU ms | sparse (sorted in SQL, `probe=2`): first / all CPU ms | dictionary scan (`dense=2`): first / all CPU ms |
| ---: | --- | --- | ---: | ---: | ---: | ---: |
| 100,000 | base | type = "task" | 50,278 | 8.03 / 405 | 14.6 / 703 | 7.74 / 403 |
| 100,000 | base | type = "project" | 9,988 | 16.9 / 176 | 6.84 / 66.7 | 30.2 / 289 |
| 100,000 | base | type = "doc" | 4,800 | 7.84 / 39.6 | 5.23 / 24.7 | 7.79 / 37.5 |
| 100,000 | top_frozen | type = "task" | 49,582 | 15.4 / 772 | 41.1 / 1910 | 16.3 / 808 |
| 100,000 | top_frozen | type = "project" | 9,842 | 47.6 / 483 | 15.4 / 148 | 62.4 / 617 |
| 100,000 | top_frozen | type = "doc" | 4,728 | 15.8 / 71.1 | 11.2 / 51.7 | 15.1 / 76.5 |
| 1,000,000 | base | type = "task" | 500,076 | 10.9 / 6010 | 96.6 / 50204 | 13.0 / 5657 |
| 1,000,000 | base | type = "project" | 99,875 | 29.3 / 2941 | 34.0 / 3557 | 40.5 / 4638 |
| 1,000,000 | base | type = "doc" | 49,466 | 58.8 / 3030 | 22.4 / 1116 | 76.7 / 4419 |
| 1,000,000 | top_frozen | type = "task" | 492,688 | 25.8 / 13361 | 385 / 196545 | 24.8 / 12989 |
| 1,000,000 | top_frozen | type = "project" | 98,400 | 86.5 / 8839 | 105 / 10054 | 90.7 / 10071 |
| 1,000,000 | top_frozen | type = "doc" | 48,755 | 182 / 8666 | 63.3 / 3092 | 192 / 9748 |

`doc` (5%) at 1M: 2.6–2.8× faster all pages and first page if sorted in SQL; `project` (10%) at 100k: 2.5–3×; `project` at 1M and `task` (50%) at any size are faster with the probe. Raising the cap for a lone `Eq` to about `sqrt(6 × limit × entities)` would pick the faster plan in every measured case. The dictionary scan (`dense=2`) was never better than the probe for these queries.

### Recommendation

- Exact `type` and `Prefix` are fine at 1M (index seeks; cost follows matches). Contains at 1M costs about 0.1 s on one layer and 0.3–0.5 s on a 3-layer stack per call, and every further page of a sparse result scans again; that is the limit of the scan design.
- Fuzzy: do not add a fuzzy filter to the core yet. A caller can do typo-tolerant search now with `Custom` behind a pigeonhole `ContainsIgnoreCase` pre-filter: it never loses matches, and it is about 6× faster than `Custom` alone up to 100k entities when the pieces are 4+ letters and not common. At 1M it gains little today (1.6× with optimisations 1 and 2), and it is slower than the plain `Custom` scan when a piece is short or common, so a caller should only use pieces of 4+ letters that are not common words. Expect 0.4–0.6 s per first page and 6–12 s for a full fuzzy read at 1M without an index.
- If fuzzy or rare-substring search on millions of names becomes a product need, the trigram index is the right tool for rare strings (1.2–20× faster; ignore case 6–20×; fuzzy with pieces 4–5× faster at 1M), but it costs about +24% of the file, +25–85 µs per text event written (+15–40 µs per event of a mixed workload), a second candidate path with history, and it is slower than the scan for medium-frequency strings. If built: make it opt-in per key, index current rows and frozen-range history only, encode the entity in the index row (to drop the event lookup), and keep the scan for short and dense strings.
- Worth doing first, low risk: optimisation 1 and 2 above (up to 3–4× on stacks), and the `Eq` dense-threshold change (2.5–3× on 5–10% types). (Done: see the next section.)

## Search: the committed find optimisations (fe9ea95 → 6a7952b)

What changed (interfaces unchanged; each has a knob in `egdb::find_knobs` that restores the old path, and the differential test flips them):

1. 243bd73: `ContainsIgnoreCase` uses the Rust SQL function `egdb_contains(value, needle, 1)` in place of `instr(lower(value), needle) > 0` (`NO_CONTAINS_FN`). Same results: ASCII-only folding, as before (DESIGN.md, find semantics 4).
2. ccc445f + 6a7952b: sparse plans fetch two chunks per query when the check can reject candidates: several layers, history, or a compound filter (`NO_OVERFETCH`). One indexed term on one current layer fetches one chunk, as before.
3. 733a675: a lone `Eq` is dense from `sqrt(6 × limit × entities)` candidates, not `sqrt(limit × entities)` (`EQ_CAP`).

Method: `bench_search run` with the binary of fe9ea95 ("before") and of the new code ("after"), each with the same example (`--limit` and the query `t6 type = "person"` were added for this run). Same data files as above, warm, ids only. First page = median of 7 calls at 100k and 5 at 1M; all pages = median of 3 full reads. CPU = thread CPU time, ms per query. The two binaries ran one after the other on the same box, so the usual ±10–15% noise applies between cells. Single cells that moved by more than that with no code-path change (for example `base` prefix "Weekly" at 1M: 39.7 → 33.8 → 48.1 ms in three runs) are noise.

**Name text, pages of 1000** (`base` rows: `textbase` run of 6a7952b; stack rows: 544c9a2, same code path on stacks)

| entities | branch | query | matches | first page CPU ms: before → after | all pages CPU ms: before → after |
| ---: | --- | --- | ---: | ---: | ---: |
| 100,000 | base | name contains "Zeppelin" | 188 | 14.1 → 13.7 (-3%) | 16.2 → 13.0 (-20%) |
| 100,000 | base | name contains "Budget" | 1,873 | 21.4 → 20.1 (-6%) | 38.8 → 39.1 (+1%) |
| 100,000 | base | name contains-ignore-case "zeppelin" | 265 | 20.1 → 15.0 (-26%) | 21.3 → 14.1 (-34%) |
| 100,000 | base | name contains-ignore-case "budget" | 2,792 | 29.1 → 22.4 (-23%) | 82.6 → 65.1 (-21%) |
| 100,000 | base | name prefix "Weekly" | 1,615 | 6.09 → 6.14 (+1%) | 10.4 → 10.5 (+1%) |
| 100,000 | base | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 1,574 | 50.6 → 38.4 (-24%) | 94.6 → 71.3 (-25%) |
| 100,000 | base | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 265 | 98.4 → 42.8 (-57%) | 97.3 → 50.1 (-49%) |
| 100,000 | top_open | name contains "Zeppelin" | 187 | 18.7 → 19.7 (+5%) | 18.2 → 19.2 (+5%) |
| 100,000 | top_open | name contains "Budget" | 1,874 | 56.6 → 28.1 (-50%) | 87.6 → 50.2 (-43%) |
| 100,000 | top_open | name contains-ignore-case "zeppelin" | 261 | 28.1 → 18.3 (-35%) | 27.1 → 19.2 (-29%) |
| 100,000 | top_open | name contains-ignore-case "budget" | 2,791 | 83.4 → 30.4 (-64%) | 203 → 84.0 (-59%) |
| 100,000 | top_open | name prefix "Weekly" | 1,605 | 19.0 → 9.85 (-48%) | 30.9 → 15.2 (-51%) |
| 100,000 | top_open | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 1,563 | 144 → 47.4 (-67%) | 203 → 88.8 (-56%) |
| 100,000 | top_open | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 261 | 111 → 54.6 (-51%) | 112 → 53.7 (-52%) |
| 100,000 | top_frozen | name contains "Zeppelin" | 192 | 24.2 → 23.8 (-2%) | 23.9 → 22.5 (-6%) |
| 100,000 | top_frozen | name contains "Budget" | 1,875 | 73.0 → 34.1 (-53%) | 108 → 85.1 (-21%) |
| 100,000 | top_frozen | name contains-ignore-case "zeppelin" | 270 | 36.0 → 28.3 (-21%) | 36.4 → 25.9 (-29%) |
| 100,000 | top_frozen | name contains-ignore-case "budget" | 2,790 | 111 → 45.7 (-59%) | 260 → 121 (-53%) |
| 100,000 | top_frozen | name prefix "Weekly" | 1,605 | 15.9 → 15.6 (-2%) | 24.5 → 25.6 (+4%) |
| 100,000 | top_frozen | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 1,567 | 194 → 71.5 (-63%) | 261 → 146 (-44%) |
| 100,000 | top_frozen | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 270 | 144 → 73.0 (-49%) | 148 → 67.1 (-55%) |
| 1,000,000 | base | name contains "Zeppelin" | 1,824 | 113 → 109 (-4%) | 222 → 241 (+9%) |
| 1,000,000 | base | name contains "Budget" | 19,198 | 137 → 137 (-0%) | 2,766 → 2,667 (-4%) |
| 1,000,000 | base | name contains-ignore-case "zeppelin" | 2,713 | 174 → 119 (-31%) | 537 → 395 (-26%) |
| 1,000,000 | base | name contains-ignore-case "budget" | 28,544 | 102 → 108 (+5%) | 3,003 → 2,976 (-1%) |
| 1,000,000 | base | name prefix "Weekly" | 16,008 | 39.7 → 48.1 (+21%) | 575 → 529 (-8%) |
| 1,000,000 | base | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 15,488 | 419 → 240 (-43%) | 6,123 → 3,812 (-38%) |
| 1,000,000 | base | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 2,713 | 6,151 → 1,620 (-74%) | 15,359 → 4,133 (-73%) |
| 1,000,000 | top_open | name contains "Zeppelin" | 1,782 | 339 → 125 (-63%) | 473 → 246 (-48%) |
| 1,000,000 | top_open | name contains "Budget" | 18,976 | 417 → 147 (-65%) | 8,216 → 2,779 (-66%) |
| 1,000,000 | top_open | name contains-ignore-case "zeppelin" | 2,638 | 567 → 140 (-75%) | 1,318 → 393 (-70%) |
| 1,000,000 | top_open | name contains-ignore-case "budget" | 28,237 | 221 → 204 (-8%) | 6,366 → 5,879 (-8%) |
| 1,000,000 | top_open | name prefix "Weekly" | 15,935 | 79.3 → 40.2 (-49%) | 1,253 → 726 (-42%) |
| 1,000,000 | top_open | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 15,218 | 1,173 → 302 (-74%) | 18,698 → 4,536 (-76%) |
| 1,000,000 | top_open | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 2,638 | 8,146 → 2,443 (-70%) | 18,753 → 5,936 (-68%) |
| 1,000,000 | top_frozen | name contains "Zeppelin" | 1,773 | 464 → 184 (-60%) | 636 → 355 (-44%) |
| 1,000,000 | top_frozen | name contains "Budget" | 19,019 | 530 → 211 (-60%) | 11,793 → 4,130 (-65%) |
| 1,000,000 | top_frozen | name contains-ignore-case "zeppelin" | 2,626 | 755 → 194 (-74%) | 1,880 → 590 (-69%) |
| 1,000,000 | top_frozen | name contains-ignore-case "budget" | 28,284 | 525 → 458 (-13%) | 13,495 → 13,937 (+3%) |
| 1,000,000 | top_frozen | name prefix "Weekly" | 15,933 | 86.0 → 48.4 (-44%) | 1,354 → 771 (-43%) |
| 1,000,000 | top_frozen | name fuzzy "managment" <= 1, pre-filter pieces ["mana", "gment"] | 15,221 | 1,644 → 404 (-75%) | 27,906 → 6,308 (-77%) |
| 1,000,000 | top_frozen | name fuzzy "zepelin" <= 1, pre-filter pieces ["zep", "elin"] | 2,626 | 11,505 → 3,597 (-69%) | 26,812 → 7,933 (-70%) |

**Lone `type` Eq, pages of 1000** (544c9a2)

| entities | branch | query | matches | first page CPU ms: before → after | all pages CPU ms: before → after |
| ---: | --- | --- | ---: | ---: | ---: |
| 100,000 | base | type = "task" | 50,278 | 10.1 → 8.76 (-13%) | 529 → 445 (-16%) |
| 100,000 | base | type = "project" | 9,988 | 24.2 → 11.6 (-52%) | 207 → 116 (-44%) |
| 100,000 | base | type = "doc" | 4,800 | 10.2 → 9.51 (-7%) | 46.8 → 45.4 (-3%) |
| 100,000 | base | type = "person" | 9,975 | 22.1 → 18.4 (-17%) | 206 → 162 (-22%) |
| 100,000 | base | type = "invoice" | 120 | 0.29 → 0.32 (+10%) | 0.30 → 0.30 (+0%) |
| 100,000 | top_open | type = "task" | 49,582 | 14.0 → 15.5 (+10%) | 738 → 750 (+2%) |
| 100,000 | top_open | type = "project" | 9,842 | 37.9 → 17.3 (-54%) | 386 → 203 (-48%) |
| 100,000 | top_open | type = "doc" | 4,728 | 17.1 → 17.5 (+2%) | 76.9 → 72.7 (-5%) |
| 100,000 | top_open | type = "person" | 9,818 | 38.0 → 17.3 (-54%) | 376 → 159 (-58%) |
| 100,000 | top_open | type = "invoice" | 122 | 0.40 → 0.42 (+5%) | 0.40 → 0.40 (+0%) |
| 100,000 | top_frozen | type = "task" | 49,582 | 18.1 → 18.3 (+1%) | 945 → 896 (-5%) |
| 100,000 | top_frozen | type = "project" | 9,842 | 59.3 → 26.7 (-55%) | 577 → 273 (-53%) |
| 100,000 | top_frozen | type = "doc" | 4,728 | 17.2 → 23.5 (+36%) | 82.9 → 111 (+34%) |
| 100,000 | top_frozen | type = "person" | 9,818 | 57.8 → 18.4 (-68%) | 574 → 186 (-67%) |
| 100,000 | top_frozen | type = "invoice" | 122 | 1.11 → 1.11 (+0%) | 1.00 → 1.10 (+10%) |
| 1,000,000 | base | type = "task" | 500,076 | 11.0 → 10.6 (-4%) | 6,253 → 6,260 (+0%) |
| 1,000,000 | base | type = "project" | 99,875 | 31.4 → 29.4 (-6%) | 3,231 → 3,245 (+0%) |
| 1,000,000 | base | type = "doc" | 49,466 | 54.0 → 34.9 (-35%) | 2,779 → 1,623 (-42%) |
| 1,000,000 | base | type = "person" | 99,028 | 28.2 → 27.6 (-2%) | 3,261 → 3,099 (-5%) |
| 1,000,000 | base | type = "invoice" | 1,006 | 5.09 → 4.93 (-3%) | 6.20 → 6.00 (-3%) |
| 1,000,000 | top_open | type = "task" | 492,688 | 22.4 → 20.4 (-9%) | 10,251 → 10,877 (+6%) |
| 1,000,000 | top_open | type = "project" | 98,400 | 58.4 → 64.3 (+10%) | 6,271 → 6,376 (+2%) |
| 1,000,000 | top_open | type = "doc" | 48,755 | 118 → 46.2 (-61%) | 5,681 → 2,360 (-58%) |
| 1,000,000 | top_open | type = "person" | 97,520 | 61.6 → 61.9 (+0%) | 6,402 → 6,451 (+1%) |
| 1,000,000 | top_open | type = "invoice" | 998 | 8.36 → 7.27 (-13%) | 8.40 → 7.40 (-12%) |
| 1,000,000 | top_frozen | type = "task" | 492,688 | 27.7 → 26.9 (-3%) | 14,402 → 13,751 (-5%) |
| 1,000,000 | top_frozen | type = "project" | 98,400 | 86.3 → 93.7 (+9%) | 9,329 → 9,449 (+1%) |
| 1,000,000 | top_frozen | type = "doc" | 48,755 | 176 → 61.4 (-65%) | 8,588 → 2,658 (-69%) |
| 1,000,000 | top_frozen | type = "person" | 97,520 | 88.8 → 92.3 (+4%) | 8,961 → 9,307 (+4%) |
| 1,000,000 | top_frozen | type = "invoice" | 998 | 11.3 → 10.0 (-12%) | 10.8 → 10.1 (-6%) |

**Lone `type` Eq, pages of 100** (544c9a2)

| entities | branch | query | matches | first page CPU ms: before → after | all pages CPU ms: before → after |
| ---: | --- | --- | ---: | ---: | ---: |
| 100,000 | base | type = "project" | 9,988 | 4.81 → 4.37 (-9%) | 474 → 467 (-2%) |
| 100,000 | base | type = "doc" | 4,800 | 5.78 → 5.14 (-11%) | 302 → 264 (-13%) |
| 100,000 | base | type = "person" | 9,975 | 4.79 → 4.75 (-1%) | 559 → 469 (-16%) |
| 100,000 | top_open | type = "project" | 9,842 | 9.83 → 9.45 (-4%) | 953 → 907 (-5%) |
| 100,000 | top_open | type = "doc" | 4,728 | 15.0 → 8.09 (-46%) | 669 → 374 (-44%) |
| 100,000 | top_open | type = "person" | 9,818 | 9.81 → 9.86 (+1%) | 933 → 899 (-4%) |
| 100,000 | top_frozen | type = "project" | 9,842 | 12.7 → 11.7 (-8%) | 1,245 → 1,194 (-4%) |
| 100,000 | top_frozen | type = "doc" | 4,728 | 21.8 → 8.22 (-62%) | 967 → 403 (-58%) |
| 100,000 | top_frozen | type = "person" | 9,818 | 13.0 → 12.2 (-6%) | 1,249 → 1,193 (-4%) |
| 1,000,000 | base | type = "project" | – | 6.04 → 6.23 (+3%) | not run |
| 1,000,000 | base | type = "doc" | – | 8.25 → 8.38 (+2%) | not run |
| 1,000,000 | base | type = "person" | – | 5.56 → 5.99 (+8%) | not run |
| 1,000,000 | top_open | type = "project" | – | 14.9 → 14.5 (-3%) | not run |
| 1,000,000 | top_open | type = "doc" | – | 24.7 → 26.2 (+6%) | not run |
| 1,000,000 | top_open | type = "person" | – | 14.7 → 15.3 (+4%) | not run |
| 1,000,000 | top_frozen | type = "project" | – | 20.9 → 20.5 (-2%) | not run |
| 1,000,000 | top_frozen | type = "doc" | – | 34.9 → 33.4 (-4%) | not run |
| 1,000,000 | top_frozen | type = "person" | – | 19.7 → 19.9 (+1%) | not run |


**Small pages, lone `type` Eq** (the shape where a higher dense cap could hurt: 5–10% types on 3-layer stacks with history). First page CPU ms, 31 calls per median, three runs alternating before and after (6a7952b):

| entities | limit | branch | query | first page CPU ms, before (3 runs) | after (3 runs) | median change |
| ---: | ---: | --- | --- | --- | --- | ---: |
| 100,000 | 10 | base | type = "project" | 4.05 / 4.48 / 3.98 | 4.05 / 5.90 / 3.87 | +0% |
| 100,000 | 10 | base | type = "doc" | 4.84 / 4.91 / 4.88 | 5.08 / 6.81 / 4.55 | +4% |
| 100,000 | 10 | base | type = "person" | 3.91 / 4.77 / 3.92 | 4.02 / 4.06 / 3.96 | +3% |
| 100,000 | 10 | base | type = "invoice" | 0.17 / 0.20 / 0.19 | 0.21 / 0.19 / 0.20 | +5% |
| 100,000 | 10 | top_open | type = "project" | 6.67 / 7.05 / 6.18 | 7.00 / 6.30 / 6.17 | -6% |
| 100,000 | 10 | top_open | type = "doc" | 8.50 / 9.03 / 8.13 | 8.53 / 7.90 / 7.89 | -7% |
| 100,000 | 10 | top_open | type = "person" | 6.40 / 8.03 / 6.31 | 6.51 / 6.19 / 6.13 | -3% |
| 100,000 | 10 | top_open | type = "invoice" | 0.26 / 0.39 / 0.29 | 0.29 / 0.28 / 0.30 | +0% |
| 100,000 | 10 | top_frozen | type = "project" | 7.42 / 9.83 / 7.43 | 7.93 / 7.39 / 7.46 | +0% |
| 100,000 | 10 | top_frozen | type = "doc" | 10.7 / 13.0 / 10.4 | 10.5 / 10.1 / 10.3 | -4% |
| 100,000 | 10 | top_frozen | type = "person" | 8.10 / 9.31 / 7.87 | 7.48 / 7.63 / 7.39 | -8% |
| 100,000 | 10 | top_frozen | type = "invoice" | 0.34 / 0.45 / 0.31 | 0.34 / 0.34 / 0.33 | +0% |
| 100,000 | 100 | base | type = "project" | 4.46 / 5.12 / 4.37 | 4.35 / 4.80 / 4.13 | -2% |
| 100,000 | 100 | base | type = "doc" | 5.71 / 6.42 / 5.77 | 5.17 / 5.18 / 4.91 | -10% |
| 100,000 | 100 | base | type = "person" | 4.38 / 4.74 / 4.75 | 4.67 / 4.89 / 4.35 | -1% |
| 100,000 | 100 | base | type = "invoice" | 0.25 / 0.25 / 0.25 | 0.25 / 0.24 / 0.22 | -4% |
| 100,000 | 100 | top_open | type = "project" | 9.48 / 10.3 / 8.86 | 9.12 / 9.84 / 8.89 | -4% |
| 100,000 | 100 | top_open | type = "doc" | 14.1 / 14.8 / 13.8 | 8.03 / 7.95 / 7.68 | -44% |
| 100,000 | 100 | top_open | type = "person" | 9.33 / 9.52 / 9.87 | 9.96 / 9.50 / 9.27 | -0% |
| 100,000 | 100 | top_open | type = "invoice" | 0.34 / 0.37 / 0.56 | 0.40 / 0.35 / 0.38 | +3% |
| 100,000 | 100 | top_frozen | type = "project" | 12.5 / 13.0 / 12.7 | 13.0 / 12.1 / 11.6 | -5% |
| 100,000 | 100 | top_frozen | type = "doc" | 20.4 / 21.1 / 20.2 | 8.75 / 8.36 / 8.35 | -59% |
| 100,000 | 100 | top_frozen | type = "person" | 13.1 / 13.6 / 12.2 | 17.4 / 12.8 / 12.7 | -2% |
| 100,000 | 100 | top_frozen | type = "invoice" | 0.93 / 1.18 / 0.90 | 0.95 / 0.94 / 0.94 | +1% |
| 1,000,000 | 10 | base | type = "project" | 5.39 / 6.01 / 4.95 | 5.62 / 5.98 / 5.49 | +4% |
| 1,000,000 | 10 | base | type = "doc" | 6.85 / 7.56 / 6.59 | 6.99 / 7.56 / 6.83 | +2% |
| 1,000,000 | 10 | base | type = "person" | 4.94 / 4.98 / 5.19 | 5.00 / 5.05 / 4.84 | +0% |
| 1,000,000 | 10 | base | type = "invoice" | 1.50 / 1.45 / 1.38 | 1.51 / 1.40 / 1.50 | +3% |
| 1,000,000 | 10 | top_open | type = "project" | 10.6 / 10.6 / 10.1 | 10.7 / 10.1 / 12.4 | +1% |
| 1,000,000 | 10 | top_open | type = "doc" | 14.4 / 14.2 / 14.5 | 15.3 / 13.9 / 15.8 | +6% |
| 1,000,000 | 10 | top_open | type = "person" | 9.90 / 9.53 / 9.48 | 10.1 / 9.56 / 10.6 | +6% |
| 1,000,000 | 10 | top_open | type = "invoice" | 1.78 / 1.76 / 1.83 | 1.89 / 1.80 / 1.90 | +6% |
| 1,000,000 | 10 | top_frozen | type = "project" | 13.3 / 12.2 / 13.3 | 13.7 / 12.2 / 13.4 | +1% |
| 1,000,000 | 10 | top_frozen | type = "doc" | 19.1 / 18.8 / 18.5 | 18.5 / 17.4 / 18.8 | -2% |
| 1,000,000 | 10 | top_frozen | type = "person" | 11.8 / 11.4 / 11.0 | 11.6 / 12.2 / 11.7 | +3% |
| 1,000,000 | 10 | top_frozen | type = "invoice" | 2.13 / 2.09 / 2.02 | 2.05 / 2.11 / 2.05 | -2% |
| 1,000,000 | 100 | base | type = "project" | 6.28 / 6.46 / 5.98 | 7.13 / 6.22 / 6.08 | -1% |
| 1,000,000 | 100 | base | type = "doc" | 8.29 / 8.55 / 8.05 | 9.69 / 8.19 / 8.06 | -1% |
| 1,000,000 | 100 | base | type = "person" | 6.12 / 6.07 / 5.81 | 5.98 / 6.10 / 5.94 | -1% |
| 1,000,000 | 100 | base | type = "invoice" | 1.60 / 1.62 / 1.50 | 1.68 / 1.59 / 1.56 | -1% |
| 1,000,000 | 100 | top_open | type = "project" | 16.1 / 15.8 / 15.6 | 16.6 / 17.1 / 15.8 | +5% |
| 1,000,000 | 100 | top_open | type = "doc" | 26.0 / 25.5 / 25.5 | 26.3 / 29.1 / 25.7 | +3% |
| 1,000,000 | 100 | top_open | type = "person" | 16.4 / 15.7 / 16.2 | 15.6 / 17.7 / 16.1 | -1% |
| 1,000,000 | 100 | top_open | type = "invoice" | 1.97 / 1.91 / 2.00 | 2.12 / 2.12 / 2.19 | +8% |
| 1,000,000 | 100 | top_frozen | type = "project" | 22.2 / 20.9 / 21.6 | 21.1 / 22.1 / 22.3 | +2% |
| 1,000,000 | 100 | top_frozen | type = "doc" | 40.1 / 36.3 / 36.3 | 35.5 / 35.8 / 35.9 | -1% |
| 1,000,000 | 100 | top_frozen | type = "person" | 21.2 / 21.4 / 21.3 | 20.6 / 20.6 / 20.7 | -3% |
| 1,000,000 | 100 | top_frozen | type = "invoice" | 2.33 / 2.38 / 2.34 | 2.59 / 2.57 / 2.63 | +11% |

Findings:

- Ignore-case contains: 1.3–1.5× faster on one layer (1M `base` "zeppelin" 174 → 119 ms per first page) and up to 4× on stacks, where overfetch also helps (1M `top_frozen` "zeppelin" 755 → 194 ms). The medium-frequency "budget" on `top_frozen` at 1M is unchanged, because its plan is the dictionary scan.
- Overfetch on stacks: case-sensitive "Zeppelin" and "Budget" 2.5–3× faster at 1M (`top_open` 339 → 125 ms and 417 → 147 ms per first page). Fuzzy with pieces is 3–4× faster on stacks (1M `top_frozen` "managment" 1,644 → 404 ms per first page, 27.9 → 6.3 s for all pages). On `base`, one indexed term is unchanged within noise.
- Lone `Eq`: types of 5–10% that are now sparse are 2–3× faster: 100k `project` and `person` at pages of 1000 (`top_frozen` 59 → 27 ms and 58 → 18 ms), and 1M `doc` (`top_frozen` 176 → 61 ms; all pages 8.6 → 2.7 s). At pages of 100, 100k `doc` is 1.8–2.7× faster on stacks. Types that stay dense (`task` 50%, `project`/`person` 10% at 1M), rare types, and every page of 10 are unchanged within noise.
- Small measured cost: a sparse lone `Eq` on a stack now overfetches. 1M `invoice` (1k matches) at pages of 100 went 2.0 → 2.1 ms (`top_open`) and 2.3 → 2.6 ms (`top_frozen`), +8–11% in all three runs.
- A first version of the `Eq` change counted the candidates twice when the plan was dense (once against each cap): +50–80% CPU on pages of 10. It was fixed before the commit; the tables above are the fixed code.


## Prove (`examples/bench_prove.rs`)

`Store::prove` (DESIGN.md 11.3) on a permission graph. Question: how long does it take to prove that an item is reachable from a root, by strategy, scenario and size?

### How to run

```
cargo run --release --example bench_prove -- --size 1000000 [--reps 11] [--cold] [--only SCENARIO] [--strategy backward|bidirectional|walk]
```

The store is built once per size in `--data` (default `/workspace/egdb-perm/data`) and reused. Do not commit the database files.

### Graph

- Seeded SplitMix64; the graph is the same on every run. Types `org`, `team`, `folder`, `doc`, `group`, `user`. Relationships (keyed by the upper entity's type): `org/team/folder --child(Out)-->`, `group --child(Out)--> group`, `group --grant(Out)--> folder|doc`, `group --shared_with(In)--> doc` (stored `doc -> group`), `user --member(Out)--> group`.
- Shapes: a chain of 11 nested folders (proofs of 1, 3, 6 and 13 links); one "huge" org with thousands of teams directly below it (3000; 400 at 10k); a doc shared with 100 groups; a "popular" doc shared with 3000 groups (1500 at 10k), which gives the large ancestor set for the no-path cases; a forest of normal orgs (5–20 teams, folders with 3–10 children, depth 2–4); a few archived folders.
- A 3-branch stack on top: `grand` reads `child` open, and `child` reads `main` open. On it, a folder is renamed, doc D is moved from folder F to folder G (the link is removed and added elsewhere), and the `team -> F` link is removed and added again. A frozen fork is taken before the move. The bench checks that the answers differ: at the `grand` head, D is reachable from G and not from F; on the frozen fork, it is reachable from F and not from G.
- Build time (5000-op batches, WAL): 10k 0.0 s (reused file), 100k 1.7 s, 1M 29.1 s. Files: 3.2 MB, 33 MB, 337 MB.

### Method

- Each (size, scenario, strategy) runs 11 warm repeats or 5 cold repeats. The tables show the median. Times are **µs per proof**.
- CPU is `CLOCK_THREAD_CPUTIME_ID`. On this VM that clock ticks in about 1 ms steps, so a warm repeat calls `prove` in a loop until it has used at least 20 ms of CPU, and divides. The box is noisy, so **decisions use CPU time**. Warm wall time is about the same as CPU time (the work is CPU-bound).
- Cold: before each repeat, a new `Store` (empty SQLite cache) and `posix_fadvise(DONTNEED)` on the db and `-wal` files. The disk is a virtio device (about 20 µs per random 4 KiB read), so "cold" is optimistic compared with a real disk.
- The bench asserts every outcome (`Proved` / `NoPath` / `Unknown`). For Backward and Bidirectional it also asserts the exact shortest path length. A wrong answer fails the run.
- Counts: `nodes visited` = entities whose links were read. `link-read rounds` / `type-read rounds` = batch reads. `SQL queries` = roll-up statements (one per layer interval of the view).
- Scenarios: `depthN`: proof of N links up the folder chain (`depth10plus` = 13). `fanin100`: a doc shared with 100 groups, with one group as the root. `fanout_root`: an item under the huge org, with the org as the root. `nopath`: the popular doc with roots that cannot reach it, so the whole ancestor set (about 3000 groups) is read. `nopath_depth`: the same with `max_depth = 4`. `nopath_budget`: the same with `max_nodes = 1000`, which gives `Unknown`. `roots1` / `roots100`: a 13-link proof with 1 root or with 100 roots (99 of them other orgs). `stack_head` / `stack_frozen`: depth-1 proof on the stack, at the `grand` head and on the frozen fork. `*_nof` / `*_nog`: the root that must *not* prove access on that view (`NoPath`).

### Results

Machine: 8 vCPU Intel Xeon, 15 GB RAM, release build, bundled SQLite, WAL. Full tables per size, with wall times, are in the bench output.

| scenario | strategy | outcome | path len (links) | CPU µs 10k warm | CPU µs 100k warm | CPU µs 1M warm | CPU µs 1M cold | wall µs 1M cold | nodes visited (1M, count) | link-read rounds (1M) | type-read rounds (1M) | SQL queries (1M) |
| --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| depth1 | backward | Proved | 1 | 35.5 | 36.1 | 34.0 | 358.8 | 548.5 | 1 | 1 | 2 | 4 |
| depth1 | bidirectional | Proved | 1 | 43.2 | 43.0 | 44.4 | 387.1 | 598.9 | 1 | 1 | 3 | 5 |
| depth1 | walk | Proved | 1 | 91.2 | 89.5 | 101.5 | 549.6 | 772.6 | 1 | 3 | 5 | 11 |
| depth3 | backward | Proved | 3 | 83.8 | 77.0 | 94.4 | 493.3 | 723.2 | 3 | 3 | 4 | 10 |
| depth3 | bidirectional | Proved | 3 | 84.5 | 88.8 | 99.4 | 517.4 | 742.7 | 3 | 3 | 5 | 11 |
| depth3 | walk | Proved | 3 | 154.0 | 157.2 | 195.1 | 637.4 | 846.8 | 3 | 6 | 8 | 20 |
| depth6 | backward | Proved | 6 | 133.6 | 139.9 | 161.3 | 578.1 | 793.0 | 6 | 6 | 7 | 19 |
| depth6 | bidirectional | Proved | 6 | 147.5 | 156.8 | 180.6 | 641.6 | 900.0 | 6 | 6 | 8 | 20 |
| depth6 | walk | Proved | 6 | 229.1 | 227.7 | 261.1 | 733.1 | 952.4 | 6 | 9 | 11 | 29 |
| depth10plus | backward | Proved | 13 | 299.9 | 320.3 | 338.8 | 801.9 | 1020.7 | 13 | 13 | 14 | 40 |
| depth10plus | bidirectional | Proved | 13 | 337.4 | 308.5 | 355.9 | 832.0 | 1075.8 | 13 | 13 | 15 | 41 |
| depth10plus | walk | Proved | 13 | 332.2 | 358.4 | 396.8 | 893.8 | 1104.8 | 13 | 14 | 16 | 44 |
| fanin100 | backward | Proved | 1 | 311.3 | 341.1 | 379.3 | 825.2 | 1074.8 | 1 | 1 | 2 | 4 |
| fanin100 | bidirectional | Proved | 1 | 47.8 | 47.3 | 49.4 | 474.1 | 706.0 | 1 | 1 | 3 | 5 |
| fanin100 | walk | Proved | 1 | 834.4 | 841.2 | 907.0 | 1470.4 | 1714.1 | 25 | 4 | 6 | 14 |
| fanout_root | backward | Proved | 3 | 84.2 | 77.3 | 92.8 | 507.2 | 773.4 | 3 | 3 | 4 | 10 |
| fanout_root | bidirectional | Proved | 3 | 1344.7 | 10604.9 | 11357.3 | 12019.0 | 13707.1 | 3 | 3 | 5 | 11 |
| fanout_root | walk | Proved | 3 | 113.4 | 109.6 | 111.5 | 561.8 | 791.0 | 3 | 4 | 6 | 14 |
| nopath | backward | NoPath | - | 7567.4 | 16557.6 | 18767.5 | 17968.8 | 20165.7 | 3001 | 2 | 3 | 6 |
| nopath | bidirectional | NoPath | - | 15394.1 | 25144.0 | 26115.2 | 29156.7 | 33983.8 | 4562 | 8 | 10 | 24 |
| nopath | walk | NoPath | - | 12525.5 | 33500.8 | 34893.7 | 36783.0 | 38658.5 | 3001 | 3 | 5 | 11 |
| nopath_depth | backward | NoPath | - | 7331.1 | 16133.6 | 17128.4 | 17733.5 | 19222.9 | 3001 | 2 | 3 | 6 |
| nopath_depth | bidirectional | NoPath | - | 8779.3 | 18209.3 | 20088.6 | 21539.4 | 22831.1 | 3021 | 4 | 6 | 13 |
| nopath_depth | walk | NoPath | - | 12375.9 | 31388.5 | 33024.2 | 32679.7 | 33471.5 | 3001 | 3 | 5 | 11 |
| nopath_budget | backward | Unknown | - | 4277.2 | 9659.2 | 9469.7 | 11620.4 | 12421.2 | 1 | 1 | 2 | 4 |
| nopath_budget | bidirectional | Unknown | - | 4901.7 | 11094.9 | 10397.4 | 13264.3 | 15060.7 | 21 | 3 | 5 | 11 |
| nopath_budget | walk | Unknown | - | 12363.1 | 31074.6 | 33986.7 | 35175.7 | 35779.9 | 1000 | 3 | 5 | 11 |
| roots100 | backward | Proved | 13 | 297.6 | 294.8 | 297.6 | 823.4 | 1038.1 | 13 | 13 | 14 | 40 |
| roots100 | bidirectional | Proved | 13 | 45575.1 | 47919.0 | 51323.1 | 57339.8 | 64477.2 | 8368 | 13 | 15 | 41 |
| roots100 | walk | Proved | 13 | 352.6 | 351.8 | 391.1 | 845.0 | 1115.2 | 13 | 14 | 16 | 44 |
| roots1 | backward | Proved | 13 | 280.8 | 293.2 | 318.1 | 598.6 | 781.4 | 13 | 13 | 14 | 40 |
| roots1 | bidirectional | Proved | 13 | 328.3 | 315.3 | 314.1 | 691.9 | 904.8 | 13 | 13 | 15 | 41 |
| roots1 | walk | Proved | 13 | 345.2 | 345.2 | 362.6 | 757.4 | 954.1 | 13 | 14 | 16 | 44 |
| stack_head | backward | Proved | 1 | 79.8 | 75.6 | 77.6 | 499.9 | 759.1 | 1 | 1 | 2 | 12 |
| stack_head | bidirectional | Proved | 1 | 95.8 | 88.9 | 104.7 | 530.9 | 831.1 | 1 | 1 | 3 | 15 |
| stack_head | walk | Proved | 1 | 187.1 | 188.5 | 255.2 | 648.2 | 967.3 | 1 | 3 | 5 | 33 |
| stack_head_nof | backward | NoPath | - | 202.1 | 217.0 | 284.6 | 659.1 | 884.6 | 4 | 4 | 5 | 36 |
| stack_head_nof | bidirectional | NoPath | - | 258.2 | 277.1 | 318.9 | 731.7 | 986.0 | 5 | 5 | 7 | 45 |
| stack_head_nof | walk | NoPath | - | 242.4 | 242.5 | 274.4 | 815.2 | 1059.2 | 4 | 4 | 6 | 42 |
| stack_frozen | backward | Proved | 1 | 115.3 | 110.9 | 137.0 | 916.0 | 1129.3 | 1 | 1 | 2 | 12 |
| stack_frozen | bidirectional | Proved | 1 | 137.9 | 136.6 | 156.0 | 1339.2 | 1570.7 | 1 | 1 | 3 | 15 |
| stack_frozen | walk | Proved | 1 | 301.1 | 284.7 | 358.1 | 1229.9 | 1546.2 | 1 | 3 | 5 | 33 |
| stack_frozen_nog | backward | NoPath | - | 325.7 | 328.8 | 397.9 | 1212.2 | 1531.4 | 4 | 4 | 5 | 36 |
| stack_frozen_nog | bidirectional | NoPath | - | 409.9 | 419.6 | 463.0 | 1644.1 | 2095.3 | 5 | 5 | 7 | 45 |
| stack_frozen_nog | walk | NoPath | - | 352.1 | 378.0 | 362.6 | 1800.9 | 2164.7 | 4 | 4 | 6 | 42 |

At 10k, the huge org has 400 teams and the popular doc has 1500 groups, so the 10k `fanout_root`, `nopath*` rows are smaller cases than at 100k and 1M.

### Strategy decision: Backward (the default)

- **Backward is fastest by CPU time in every scenario but one, at every size, warm and cold.** The ranking does not change with size, so no size-dependent heuristic is needed.
- **Bidirectional** is slow when the root side is large. It must expand the forward side at least once. At 1M: `fanout_root` takes 11,357 µs, against 93 µs for Backward (3000 teams below the root). `roots100` takes 51,323 µs, against 298 µs (8,368 nodes expanded, against 13). Its only win is `fanin100` (49 against 379 µs): the group root reaches the doc in one forward hop, but Backward must read the type of all 100 candidate groups. That case is below a millisecond either way.
- **Walk** (the walk loader with read-ahead) is never fastest. On short proofs its read-ahead issues 2–3× the rounds and SQL of Backward (depth1: 11 SQL against 4; 102 against 34 µs). On large no-path searches it is about 2× Backward (`nopath` 34.9 against 18.8 ms). With a budget, it uses the whole budget one node at a time (`nopath_budget` 34.0 ms). Backward refuses the 3000-node layer before it reads it (9.5 ms, all of it the type read of the candidates). Walk also does not return the shortest proof.
- **Cost model of Backward:** one link round plus one type round per proof link. The cost is about 25 µs per link warm, and it does not depend on graph size (10k ≈ 100k ≈ 1M). Cold adds about 300–500 µs per proof at 1M (a few random page reads). The worst case is a large ancestor set with no path. At 1M, 3001 ancestors cost about 18 ms (about 6 µs per ancestor), warm or cold. `max_depth` does not help when the large layer is near the item. `max_nodes` stops the search early and answers `Unknown`.
- Frozen views and stacks cost more SQL per round (12 statements for a depth-1 proof, against 4 on `main`, because the view has more layers). They stay below 0.4 ms warm and at most about 1.2 ms cold (1M).

### Optimisations considered, not built

- Skip the type read of the candidates when a reversed pair can only come from one type. This would help only `fanin100`-like cases, and in this map `child` is valid from three types. Not measured.
- Split very large frontiers into smaller `json_each` batches. This reads the same rows, so it can only lower peak memory. Not measured.

## Write middleware (`examples/bench_write.rs`)

Run: `cargo run --release --example bench_write -- 5000 10 --runs 5`. It writes 5000 batches of 10 ops (a mix of values and links) on a fresh temp file, for 0, 1 and 3 identity middleware. Each figure is the median of 5 runs. The question is whether the middleware hook slows writes when it is unused or does nothing.

The run was repeated three times, interleaved with `origin/master` (which has no middleware code), to show the noise. The numbers are process CPU seconds for the 5000 batches.

| build | round 1 | round 2 | round 3 |
| --- | ---: | ---: | ---: |
| `origin/master` (no middleware code) | 1.151 | 1.058 | 1.215 |
| this branch, 0 middleware | 1.177 | 1.238 | 1.086 |
| this branch, 1 identity middleware | 1.102 | 1.273 | 1.071 |
| this branch, 3 identity middleware | 1.098 | 1.073 | 1.081 |

There is no measurable difference. The noise between rounds is about ±10%, larger than any gap between the builds. The fast path (no middleware) builds no context and does not touch the ops, so it matches `origin/master`; one or three identity middleware add no visible cost.
