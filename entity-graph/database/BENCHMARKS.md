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
- Insertion order reads the dictionary by `eid`, with no probe. On frozen views this is faster than the entity-id order of the same plan: `priority = 1 OR 2` on top_frozen 0.61 s vs 1.55 s (open question below).

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

Decision (9663e18, 04582c9): a dense plan probes each entity when the view reads no history (no frozen range) or the plan is one `Eq`; otherwise it reads the dictionary and checks every entity after the roll-up (a probe that must also search the history arms costs more than the roll-up: top_frozen `score > 4999` probe vs check all, first build: 7.5 vs 1.0 s). With CPU time, the dense 1M queries are 2–8% faster than naive, and the `Or` is up to 6% slower. The old 8.0 s vs 6.9 s gap was mostly the 10%-of-entities dense threshold (fixed in 5c0c0e4) and wall-clock noise.

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

- It was real, from 5c0c0e4 (dense threshold `sqrt(limit × entities)`). The estimate counted the first `cap` rows of `title` with `instr`, and that count stopped early or late depending on where matches fell in the index: the estimate for `contains "99"` was 50× off, so the planner switched between plans from page to page.
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
- Schema V3 (04582c9) adds `seq` to `value_current_value`, so the "is this row the winner" check of the merge reads the index only. Pushing the filter's bound on the order key into the scan (04582c9): top_frozen `type = task` by score, full read 38.1 → 0.54 s CPU when the filter has a bound on `score`.
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
| Dense probe also for one `Eq` on frozen views | 04582c9 | top_frozen `type = task` entity-id order, full read 0.88 s (check all: 1.07 s process CPU before) |

### Tests

`cargo test --release`: 71 passed. Differential model test with `EGDB_DIFF_CASES=20000`: passed (325 s) at 04582c9, with random filters, paging, all four orders, and random planner knobs (probe/dense/exact count/always sample). `cargo clippy --all-targets --release -- -D warnings`: clean.
