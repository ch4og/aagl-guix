---
name: update-aagl-launcher
description: Update an AAGL launcher to an upstream release, refresh its Cargo closure, handle new Git/workspace dependencies, and verify the Guix build. Use when asked to bump or update a launcher made with make-aagl.
---

# Update an AAGL launcher

Use this skill for the complete, review-first update process for a launcher
packaged with `make-aagl`.  It is the repository's authoritative launcher
update guide.

The invoking request must identify the launcher to update and may specify a
target version.

`honkers-launcher` is used as the example below, but the process applies to the
other AAGL launchers as well.

## What an update includes

A launcher update has three parts:

1. update the launcher's Git tag and Guix source hash;
2. import its Rust dependency closure from a freshly generated `Cargo.lock`;
3. package every newly introduced Git/workspace crate as a standalone Cargo
   package.

The third part is essential.  `guix import crate` can initially render a Git
dependency as an origin, but a workspace dependency must have its own Cargo
package and dependency inputs before a dependent launcher can build.

## Scope and safety

- Extract the launcher name and optional target version from the request.  Ask
  for the launcher if absent.  If no target version is supplied, discover the
  newest stable upstream release tag.
- Start with `git status --short`.  Preserve unrelated user changes; do not
  reset, clean, restore, or overwrite them.
- Never overwrite an existing temporary checkout.  Create every checkout with
  `mktemp -d`; retain its generated path for the rest of the update.
- Leave checkouts created by this process in place for inspection unless the
  user explicitly asks to remove them.
- Use only declarative Guix tooling.  Never install Rust, Cargo, or other
  dependencies imperatively.
- Do not stage, commit, amend, or discard changes.
- Fetching source is not enough to trust it for execution.  Before running
  `cargo generate-lockfile` or `guix build` against external source, name the
  relevant upstream URLs and obtain explicit authorization if it was not
  already granted for this request.  The confirmation gate after release
  discovery defines how to obtain that authorization.
- Never run commands on remote hosts, Docker daemons, or Kubernetes clusters.

## Confirmation gate

Release discovery is read-only.  After identifying the target release, present
an update proposal containing:

- the launcher and its current and proposed versions;
- the package definition and other source files expected to change;
- every upstream URL that will be cloned or whose source will be executed;
- the commands that execute upstream source: `cargo generate-lockfile` and the
  final `guix build -L. LAUNCHER-NAME`.

Then ask: `Type CONFIRM to proceed with the update.`  Do not clone source,
create temporary checkouts, modify tracked files, run `cargo generate-lockfile`,
run `guix import`, or run `guix build` until the user replies with the exact
standalone keyword `CONFIRM`.  Ordinary affirmative replies do not authorize
the update.

The `CONFIRM` reply authorizes the complete update, including lockfile
generation and the final Guix build, against the URLs listed in that proposal.
Do not request a separate build confirmation.  If the target version or any
URL changes afterwards, present a revised proposal and require `CONFIRM` again.

## 1. Discover the target release

Locate the package definition and confirm the requested package is made with
`make-aagl`:

```sh
grep -n -A3 'define honkers-launcher-real' aagl/packages/honkers.scm
```

Record the current `#:version`.  The default launcher repository follows this
convention:

```text
https://github.com/an-anime-team/LAUNCHER-NAME
```

List the release tags:

```sh
git ls-remote --tags --refs \
  https://github.com/an-anime-team/honkers-launcher.git
```

If the user supplied a target version, verify its exact tag exists.  Otherwise,
choose the highest stable semantic-version tag newer than the packaged version.
Do not silently select a pre-release, branch head, or untagged commit.  Report
the proposed version transition, then follow the confirmation gate before
starting the update.

## 2. Fetch and hash the launcher source

Create a unique directory for the exact release, and retain the generated path
as `LAUNCHER_DIR` for the remaining launcher commands:

```sh
LAUNCHER_DIR=$(mktemp -d -t honkers-launcher-1.14.0.XXXXXX)
git clone --depth 1 --branch 1.14.0 \
  https://github.com/an-anime-team/honkers-launcher \
  "$LAUNCHER_DIR"
```

Verify both the tag and the checkout state:

```sh
git -C "$LAUNCHER_DIR" describe --tags --exact-match
git -C "$LAUNCHER_DIR" status --short
```

Calculate the source hash used by Guix's `git-fetch`:

```sh
guix hash -rx "$LAUNCHER_DIR"
```

`-x` excludes VCS metadata, so the hash matches the tree Guix fetches.  Update
only the relevant `#:version` and `#:hash` in the launcher's package definition.
The hash is Guix base32, not the Git commit hash.

## 3. Generate and preview the launcher lockfile

Regenerate the lockfile with Guix-provided Rust and Cargo:

```sh
guix shell rust rust:cargo -- sh -c \
  'cd "$1" && cargo generate-lockfile' sh "$LAUNCHER_DIR"
```

This can resolve newer compatible crate versions than the lockfile committed
upstream.  That is expected: import the generated lockfile that Guix will
actually use.

Generate an import preview first, without changing the repository.  Use a
fresh file owned by this update rather than a fixed temporary filename:

```sh
PREVIEW=$(mktemp -t honkers-launcher-crates.XXXXXX)
guix import crate \
  --lockfile="$LAUNCHER_DIR/Cargo.lock" \
  honkers-launcher > "$PREVIEW"
```

Review the preview, particularly definitions containing `git-fetch` or:

```text
TODO REVIEW: Define standalone package if this is a workspace.
```

These are Git/workspace dependencies that require the next section.  When the
preview is understood, import the normal dependency closure into the managed
crate file:

```sh
guix import --insert=aagl/packages/rust-crates.scm crate \
  --lockfile="$LAUNCHER_DIR/Cargo.lock" \
  honkers-launcher
```

`aagl/packages/rust-crates.scm` is managed by `guix import`.  Do not manually
write ordinary crates there; rerun the importer whenever a lockfile changes.

## 4. Resolve Git and workspace dependencies

Find new special sources inserted into `rust-crates.scm`:

```sh
grep -n -E 'rust-(anime-game-core|anime-launcher-sdk|sophon-lib)' \
  aagl/packages/rust-crates.scm
```

For each raw `(origin ...)` produced from a Git dependency, record its:

- crate name;
- release version;
- repository URL;
- exact commit;
- source hash.

Then search `aagl/packages/rust-sources.scm` for an existing matching
`make-*` helper.  Current known examples include:

- `make-anime-game-core`;
- `make-anime-launcher-sdk`;
- `make-sophon-lib`.

A raw source such as:

```scheme
(define rust-anime-launcher-sdk-1.36.7.c682598
  (origin ...))
```

must become an alias to a package exported by `rust-sources.scm`:

```scheme
(define rust-anime-launcher-sdk-1.36.7.c682598
  package:rust-anime-launcher-sdk-1.36.7.c682598)
```

Add the corresponding standalone source package, matching surrounding style:

```scheme
(define-public rust-anime-launcher-sdk-1.36.7.c682598
  (make-anime-launcher-sdk
   #:version "1.36.7"
   #:commit "c6825980b24a7c465fb41855db650a650cee0141"
   #:sha "19215m6nv2r203zjnwzdhi3bbwphhbmjsrfssbdy4r6rxq98qviv"))
```

Independently verify the source hash by cloning the exact tag or commit and
running `guix hash -rx` on the checkout.

### Import the special package's own closure

Every new standalone Git/workspace package needs its own generated lockfile and
import.  For each one:

1. create a fresh directory with `mktemp -d`, then clone the referenced tag or
   commit into it;
2. run `cargo generate-lockfile` in that directory through `guix shell rust
   rust:cargo`;
3. import the crate package from that lockfile;
4. create the source package and replace the raw origin with its alias;
5. repeat for any further raw Git/workspace origins revealed by the import.

For example:

```sh
DEPENDENCY_DIR=$(mktemp -d -t anime-launcher-sdk-1.36.7.XXXXXX)
git clone --depth 1 --branch 1.36.7 \
  https://github.com/an-anime-team/anime-launcher-sdk \
  "$DEPENDENCY_DIR"

guix shell rust rust:cargo -- sh -c \
  'cd "$1" && cargo generate-lockfile' sh "$DEPENDENCY_DIR"

guix import --insert=aagl/packages/rust-crates.scm crate \
  --lockfile="$DEPENDENCY_DIR/Cargo.lock" \
  anime-launcher-sdk-1.36.7
```

Continue until every new Git/workspace dependency is a package alias backed by
a matching export from `rust-sources.scm`.

If no appropriate `make-*` helper exists, stop before inventing a generic
factory.  Show the source repository layout and explain what a new helper
would need to package.  That is a packaging design decision requiring review.

## 5. Validate the update

Load the modified Scheme modules.  Substitute the actual launcher module for
`PACKAGE-MODULE`:

```sh
printf '%s\n' \
  '(use-modules (aagl packages rust-sources) (aagl packages rust-crates) (aagl packages PACKAGE-MODULE)) (display "ok") (newline)' \
  | guix repl -L.
```

Messages saying a source file is newer than a cached `.go` file are harmless.
Scheme errors, undefined variables, and unmatched parentheses are not.

Review the patch and reject unrelated churn:

```sh
git diff --check
git diff -- PACKAGE-FILE aagl/packages/rust-sources.scm
git diff --stat -- aagl/packages/rust-crates.scm
```

Finally, build the local package:

```sh
guix build -L. LAUNCHER-NAME
```

If Guix reports a fixed-output hash mismatch, first verify that the selected
tag or commit is correct.  Only then use the reported `got:` hash to correct
the package definition.

A missing Cargo input generally means a Git/workspace package was not converted
to a standalone source package, or its own lockfile has not been imported.
Trace that root cause instead of adding a guessed input manually.

## Report completion

State all of the following:

- old and new launcher versions;
- new standalone Git/workspace source packages and commits;
- source files changed;
- results of module loading, `git diff --check`, and the Guix build;
- any remaining blocker, including the exact failed command and its output.

Do not commit temporary checkouts or generated lockfiles.  Only the launcher
definition, `rust-sources.scm`, and importer-managed changes to
`rust-crates.scm` belong in this repository.
