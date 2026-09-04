# Global preferences

Project-local agent rules and per-project memory may refine/override these.

## Comments & docstrings

- **Describe the current state, never a transition.** No "was X", "supersedes X",
  "Legacy:", "for now", "previously… now…", or parentheticals naming a removed thing.
  If deleting the sentence loses nothing a fresh reader needs, it's transitional — cut it.
- **Define a concept once, at its owning symbol** (the field/function/type it describes).
  Everywhere else write `(see X)` — never re-explain or paraphrase it. After a multi-file
  change, sweep the diff for the same explanation appearing twice.
- **Terse. Prefer one concrete example over prose.** Delete any clause a reader could
  recover from `git log`, the code itself, or user docs.
- **Don't prefix a docstring with the module/file name** inside that module's own file —
  the file context already says it. Keep such prefixes only at cross-module call sites.
- **Backticks for inline code refs** (params, function/identifier names) in docstrings —
  not "double-quotes".
- **Doc descriptions start Titlecase and end with a period** (C `@param`/`@return`/`///<`
  especially). Rephrase identifier-led sentences ("True when …", "The …") rather than
  capitalizing an identifier.
- **Don't wrap comment/doc prose short** (e.g. at 80). Fill toward the project's line
  limit (commonly 100).

## Naming

- **No UPPERCASE constants.** Lowercase `snake_case` for every constant/local, even
  file-scope lookup tables and thresholds. `ALL_CAPS` is ONLY ever for C `#define` macros.
- **(Lua) callbacks are the last positional param, named `on_<verb>`** (`on_done`,
  `on_exit`, `on_stdout`), never `cb`.

## Code structure & style

- **Brace every control-flow block** (`if`/`else`/`for`/`while`), even single-statement
  bodies. No one-line braceless `if (x) doThing();`.
- **Keep control flow flat.** Guard clauses / early returns; fold a branch that is itself
  a binary decision *into* the enclosing `if/elseif` chain rather than nesting. Hoist
  shared computation above the branches. Aim for one flat list of `condition → action`.
- **Calculate, don't track.** Derive a condition from existing sources of truth (list
  sizes, timestamps, buffer content + cursor, extmark namespaces, transition points)
  instead of adding a parallel flag, cache, or side-table that must be kept in sync.
  Only cache when a real, measured cost demands it, and key the cache on the underlying
  state. Parallel state is a desync-bug surface.
- **(C) State at the top.** Group all type defs (struct/enum/typedef) and file-scope
  `static` module state at the top of the file, above the functions — never scattered
  next to first use. (Function-local `static` is not module state; leave it.)
- **(Lua) build strings with `('%s %s'):format(a, b)`**, not `..` concatenation.

## Testing

- **Extend an existing relevant test** (add assertions to it) before authoring a brand-new
  case; group assertions rather than spawning near-duplicate cases. Keep tests non-verbose.

## Workflow & communication

- **Never mutate git history without asking me each turn (and don't ask unless the task
  inherently requires git operations, e.g. if I asked you to rebase a branch)** — no `git add`,
  `commit`, `push`, `rebase`, `reset`, amend, or force-push, even if approved in a prior turn.
  Workspace edits (Edit/Write to tracked files) are fine.
- **End-of-task reports = non-obvious callouts only.** Surprises found, behavior changes,
  verification results, deliberate deviations, follow-ups. Don't recap the diff or restate
  "updated X to say Y" — I review diffs myself.
