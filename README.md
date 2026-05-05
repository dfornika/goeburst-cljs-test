# goeBURST viewer

A browser-based implementation of the **goeBURST** algorithm for visualising pairwise allelic-distance data as a minimum spanning forest.

The algorithm is described in:

> Francisco AP, Bugalho M, Ramirez M, Carriço JA (2009).
> *Global optimal eBURST analysis of multilocus typing data using a graphic matroid approach.*
> BMC Bioinformatics 10:152. https://doi.org/10.1186/1471-2105-10-152

## What it does

1. **Parses** a pairwise allelic-distance matrix (CSV or TSV, with ST identifiers in the first row and column).
2. **Runs goeBURST**: applies Kruskal's algorithm with the goeBURST edge-priority ordering (SLV → DLV → TLV → occurrence frequency → ST ID) to produce a globally optimal minimum spanning forest.
3. **Visualises** the result as an interactive D3 force-directed graph.

Each edge carries a `:level` field indicating how it was distinguished from the next candidate edge: `nil` means there was no successor edge to compare against, `0` means distance alone separated them, and `1–6` encode the goeBURST tiebreak level (1–2 = SLV, 3–4 = DLV, 5–6 = TLV), matching the link-confidence colouring described in the paper.

## Input format

A square, symmetric matrix of non-negative integer allelic distances. The first row and first column must contain ST identifiers; diagonal values must be zero.

**TSV example:**

```
	ST1	ST2	ST3
ST1	0	1	2
ST2	1	0	1
ST3	2	1	0
```

**CSV example:**

```
,ST1,ST2,ST3
ST1,0,1,2
ST2,1,0,1
ST3,2,1,0
```

## Getting started

**Prerequisites:** Node.js and a Java runtime (required by shadow-cljs).

```bash
npm install
```

**Development server** (live reloading on http://localhost:9500):

```bash
npm run dev
# or: npx shadow-cljs watch app
```

**Production build:**

```bash
npm run release
# output written to public/js/
```

**Run tests:**

```bash
npx shadow-cljs compile test && node target/test/test.js
```

## UI controls

| Control | Description |
|---|---|
| **File picker** | Upload a CSV or TSV distance matrix |
| **Max distance** | Maximum allelic distance to include as an edge (increase to connect more nodes) |
| **Show edge distances** | Toggle distance labels on edges |
| **Link distance** | Preferred edge length in the force layout |
| **Repulsion** | Node-to-node repulsion strength |
| **Gravity** | Strength of the force pulling nodes toward the centre |

The graph supports **scroll to zoom** and **click-and-drag to pan**. Individual nodes can also be dragged to adjust the layout.

## Project structure

```
src/
  main/goeburst/
    parse.cljs      – TSV/CSV parser → {:ids [...] :matrix [[...]]}
    algorithm.cljs  – goeBURST algorithm (union-find, CC counts, Kruskal's)
    graph.cljs      – D3 force-directed visualisation (Reagent component)
    app.cljs        – UI shell (sidebar, state, file upload)
  test/goeburst/
    parse_test.cljs
    algorithm_test.cljs
public/
  index.html        – single-page entry point
```

## License

GNU General Public License v3.0 — see the algorithm reference above for the original Java implementation this is based on.
