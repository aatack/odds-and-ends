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

- Values on cost 10–80% more (all keys are read and copied).
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
- Rare terms (1% ranges, rare contains, `And` with a small term) are 30–100× faster than naive at 100k, 30–40× at 1M (full read).
- Dense terms (40–50%) are close to naive, because both must roll up about two entities per row. The index plan gives only rows that can match, so it checks half as many entities.
- Frozen views cost 2–5× open ones: each frozen layer adds a history arm, and the roll-up reads history.
- Weak spots: 1M `contains "9"` (47%) is slower than naive (8.0 s vs 6.9 s full read; the per-entity probe on one open layer). 1M `type = task AND contains "99"`: 0.30 s per page with the SQL intersection (the estimate before 5c0c0e4 measured 0.24 s; not explained yet).
