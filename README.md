# scratchfn

`scratchfn` is a small Clojure DSL for generating Scratch 3 (`.sb3`) projects.

It is designed around building block trees as plain Clojure maps, flattening them
into Scratch's `project.json` shape, and then packaging that JSON plus SVG/WAV
assets into an `.sb3` zip archive.

## Mental Model

There are three levels to keep in mind:

1. Block constructors
Examples: `event-when-flag-clicked`, `motion-move-steps`, `data-set-variable`.

2. Script composition
Use `do-block` to chain blocks with `:next`.
Use `top-level-block` to flatten one or more top-level scripts into a target's
`:blocks` map.

3. Project assembly
Build target maps (`Stage` or sprites), then call `generate-sb3`.

## Common Workflow

Typical generator flow:

```clojure
(binding [$/*block-counter* 0]
  (let [ctx (merge ($/make-stage-variables {:score 0})
                   ($/make-broadcasts [:reset]))
        costume ($/create-costume some-svg "sprite")
        blocks ($/top-level-block
                ($/event-when-flag-clicked :x 0 :y 0)
                ($/data-set-variable (:score ctx) 0))
        builds [{:target {... :blocks blocks :costumes [(:costume costume)]}
                 :assets [costume]}]]
    ($/generate-sb3 "out.sb3" builds)))
```

Notes:

- Bind `*block-counter*` to `0` for deterministic block ids.
- `make-stage-variables`, `make-variables`, `make-stage-lists`, `make-lists`, and
  `make-broadcasts` return helper objects you pass back into block constructors.
- A generated project is assembled from `builds`, where each build may contribute:
  - `:target`
  - `:assets`
  - `:monitor`

## Most Useful Helpers

### Variables, Lists, Broadcasts

- `make-stage-variables`
- `make-variables`
- `make-stage-lists`
- `make-lists`
- `make-broadcasts`

The returned values are not raw ids; they are small helper maps carrying things
like `:as-variable`, `:as-list`, and `:as-input` so block constructors can use
them directly.

### Script Composition

- `do-block`
Chains blocks with `:next`.

- `top-level-block`
Flattens one or more top-level scripts into the `:blocks` map Scratch expects.

- `free-block`
Turns a normal command block into a free-floating top-level stack at a given
`x/y`. Useful for teacher-only helper stacks that do not start with an event hat.

Example:

```clojure
($/top-level-block
 ($/free-block ($/data-set-variable robot-mode "real") 580 570))
```

### Assets

- `create-costume`
Creates a standard SVG costume with default rotation center `(45,30)`.

- `create-costume-centered`
Creates an SVG costume with explicit rotation center.
Use this for:
  - stage backdrops
  - asymmetric sprites
  - any costume whose logical origin is not the center of a `90x60` canvas

Example:

```clojure
($/create-costume-centered svg "backdrop1" 240 180)
```

### Project Emission

- `generate-project-json`
Returns the `project.json` string for a collection of builds.

- `generate-sb3`
Writes the actual `.sb3` archive.

`generate-sb3` accepts optional extensions:

```clojure
($/generate-sb3 "robot.sb3" builds :extensions ["ev3"])
```

## Custom Blocks

Custom blocks are supported with:

- `proc-handle`
- `define-proc`
- `call-proc`

Important detail:

- `define-proc` returns a linked block tree, not a ready-to-merge `:blocks` map.
- To put a procedure definition into a target, flatten it with `flatten-block` or
  wrap it in a helper that does the flattening.
- By contrast, `top-level-block` already returns a fully flattened block map.

If you merge raw `define-proc` output directly into a target's `:blocks`, you will
produce invalid Scratch JSON.

## Useful Patterns

## What This Library Does Not Abstract Much

- Extension-specific opcodes are still mostly raw maps in project generators.
- There is no high-level target builder; generators usually assemble `:target`
  maps directly.
- Layout (`x/y` for stacks) is manual.

## Practical Advice For Future Agents

1. Read the generator that already exists before inventing a new pattern.
2. When a project fails to load in Scratch, inspect `project.json` first.
3. If you use custom blocks, flatten them explicitly before merging into a
   target's `:blocks`.
