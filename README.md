# eglot-helpers-java

LSP-driven Java helpers for Emacs, built on top of [Eglot](https://github.com/joaotavora/eglot) and [JDTLS](https://github.com/eclipse-jdtls/eclipse.jdt.ls).

Replaces shell-based Maven test invocations with LSP commands, uses [dape](https://github.com/svaante/dape) (DAP protocol) for debugging instead of `gud`/`jdb`, and auto-manages the two JDTLS plugin JARs required for test running and debugging.

## Features

- **Auto-installs JDTLS plugins** — downloads `com.microsoft.java.debug.plugin` and `com.microsoft.java.test.plugin` on first use; upgrades on demand
- **LSP-driven test running** — uses `vscode.java.test.junit.argument` to resolve classpath and launch args; falls back to `./mvnw -Dtest=...` transparently if the test plugin is not active
- **DAP debugging via dape** — uses `vscode.java.startDebugSession` to get a DAP port, then attaches dape; falls back to Maven Surefire debug (`-Dmaven.surefire.debug`) with port-polling
- **Auto-detects Lombok** — parses `pom.xml`, resolves the JAR from `~/.m2`, downloads from Maven Central if missing
- **Curated JDTLS settings** — ships a settings plist (JavaSE-21 runtime, favorite static members, import order, parameter-name inlay hints, decompiled-source references, auto-build, Maven source download…) installed both via `initializationOptions.settings` at startup *and* via `eglot-workspace-configuration` so JDTLS honors it on every runtime `workspace/configuration` request
- **Branch-scoped diagnostics** — Flymake filter showing only errors in files changed relative to a base branch
- **OSGi cache management** — `restart-server-clean` flag clears the JDTLS plugin cache to recover from bundle activation failures

## Requirements

- Emacs 29.1+
- [Eglot](https://github.com/joaotavora/eglot) 1.9+
- [dape](https://github.com/svaante/dape) 0.1+
- [JDTLS](https://github.com/eclipse-jdtls/eclipse.jdt.ls) installed and on `$PATH` as `jdtls`
- `unzip` available on `$PATH` (for extracting the test plugin from its `.vsix`)
- `lsof` available on `$PATH` (for port detection during Maven debug fallback)

## Installation

### Manual

Clone the repo and add it to your load path:

```elisp
(add-to-list 'load-path "/path/to/eglot-helpers-java")
(require 'eglot-helpers-java)
```

### use-package

```elisp
(use-package eglot-helpers-java
  :load-path "/path/to/eglot-helpers-java"
  :after eglot
  :demand t)
```

On first use, the package will automatically download the required JDTLS plugin JARs when Eglot starts on a Java file.

## Configuration

All settings are in the `eglot-helpers-java` customization group (`M-x customize-group RET eglot-helpers-java`).

| Variable | Default | Description |
|---|---|---|
| `eglot-helpers-java-bundles-dir` | `~/.emacs.d/java-bundles/` | Where plugin JARs are stored |
| `eglot-helpers-java-lombok-jar-path` | `nil` | Override Lombok JAR path (auto-detected from `pom.xml` when nil) |
| `eglot-helpers-java-debug-port` | `5005` | JDWP port for Maven Surefire debug fallback |
| `eglot-helpers-java-base-branch` | `"develop"` | Base branch for `flymake-branch-diagnostics` |
| `eglot-helpers-java-shutdown-timeout` | `20` | Seconds to wait for JDTLS `shutdown` (flush `.metadata/`) before force-kill |
| `eglot-helpers-java-connect-timeout` | `90` | Buffer-local override of `eglot-connect-timeout` for Java buffers |
| `eglot-helpers-java-read-process-output-max` | `4 MiB` | Buffer-local override of `read-process-output-max` for Java buffers |
| `eglot-helpers-java-heap-dump-dir` | `~/Library/Logs/jdtls/` (macOS) | Where the JVM writes a heap dump on OOM |

## Commands

### Bundle management

| Command | Description |
|---|---|
| `eglot-helpers-java-ensure-bundles` | Download debug and test plugin JARs if missing |
| `eglot-helpers-java-upgrade-bundles` | Upgrade JARs to latest versions |
| `eglot-helpers-java-reload-bundles` | Hot-reload plugins into running JDTLS (no restart) |

### Test running

| Command | Description |
|---|---|
| `eglot-helpers-java-run-test-class` | Run all tests in the class at point |
| `eglot-helpers-java-run-test-method` | Run the test method at point |

Output appears in the `*compilation*` buffer.

### Debugging

| Command | Description |
|---|---|
| `eglot-helpers-java-debug-test-method` | Debug the test method at point via dape/DAP |

Uses `vscode.java.startDebugSession` when available. If the test plugin is not loaded, falls back to Maven Surefire debug: Maven starts the JVM with JDWP listening on `eglot-helpers-java-debug-port`, then dape attaches once the port is ready.

To set breakpoints, use dape's native `dape-breakpoint-toggle` (`C-x C-a C-b` by default).

### Build

| Command | Description |
|---|---|
| `eglot-helpers-java-mvn-build-project-skiptests` | `./mvnw clean package -DskipTests -U` |

### Server management

| Command | Description |
|---|---|
| `eglot-helpers-java-restart-server` | Restart JDTLS |
| `eglot-helpers-java-restart-server-clean` | Restart JDTLS clearing the OSGi plugin cache |
| `eglot-helpers-java-force-rebuild` | Force a full JDTLS workspace rebuild |
| `eglot-helpers-java-wipe-workspace` | Delete the JDTLS workspace cache for the current project and restart (recovery for `ObjectNotFoundException` and friends) |
| `eglot-helpers-java-list-java-commands` | List all `executeCommand` handlers registered with JDTLS |

### Diagnostics

| Command | Description |
|---|---|
| `eglot-helpers-java-flymake-branch-diagnostics` | Show Flymake errors only for files changed vs `eglot-helpers-java-base-branch` |

## JDTLS settings

The package defines `eglot-helpers-java--jdtls-settings`, a plist of JDTLS configuration covering formatting, completion, code lenses, inlay hints, Maven/Gradle import, decompilation, code generation, and the Java runtime. Those settings are delivered to JDTLS in two ways:

1. **At server startup** as `initializationOptions.settings` (read once when JDTLS launches).
2. **At runtime** as `eglot-workspace-configuration` (Eglot answers JDTLS's `workspace/configuration` requests with the same plist).

The second channel matters: JDTLS pulls many settings (`java.configuration.runtimes`, `java.inlayHints`, `java.completion.favoriteStaticMembers`, `java.format.*`, …) on demand after startup. Without it, those keys silently fall back to JDTLS defaults regardless of what `initializationOptions.settings` contained.

To tweak the defaults, edit the `eglot-helpers-java--jdtls-settings` defconst and restart JDTLS (`M-x eglot-helpers-java-restart-server`).

## FQCN / FQMN resolution

The package resolves the fully-qualified class name (FQCN) and method name (FQMN) at point by querying JDTLS via `textDocument/hover`, so test commands work without any manual configuration.

## Test plugin activation

If `vscode.java.test.junit.argument` reports "no delegateCommandHandler", try:

1. `M-x eglot-helpers-java-reload-bundles` — hot-reload without restarting
2. `M-x eglot-helpers-java-restart-server-clean` — clears the OSGi bundle cache and restarts

If neither resolves it (usually a version mismatch between the test plugin JAR and the installed JDTLS), test running automatically falls back to `./mvnw -Dtest=FQCN test`. You'll see `vscode.java.test plugin not loaded — falling back to Maven...` in the echo area. This is normal and test output still appears in `*compilation*`.

## Stability

Several knobs are tuned to prevent JDTLS workspace corruption (the kind that surfaces as `ObjectNotFoundException` in `.metadata/.log` and is otherwise only recoverable with `eglot-helpers-java-wipe-workspace`):

- **File-notify auto-revert in Java buffers.** Doom disables `auto-revert-use-notify` globally and falls back to a lazy revert that only fires on buffer/window/frame switches and saves. External edits (Claude running in a terminal, formatters, branch switches) therefore stay stale in the Emacs buffer, Eglot never sends `didChange`, and JDTLS's in-memory model drifts from disk until the next build corrupts `.metadata/`. The package re-enables `auto-revert-mode` with file-notify buffer-locally in Java buffers so external edits propagate to JDTLS immediately. (Only covers files you have open in a buffer — for batches of edits to unvisited files, follow up with `M-x eglot-helpers-java-force-rebuild`.)
- **Graceful shutdown on Emacs quit.** A `kill-emacs-hook` calls `eglot-shutdown` on every JDTLS server with `eglot-helpers-java-shutdown-timeout` seconds to flush state. Eglot's stock 1.5s default is not enough on large projects — JDTLS gets force-killed mid-write, leaving `.metadata/` half-flushed.
- **Exit-on-OOM JVM flags.** `-XX:+ExitOnOutOfMemoryError` makes the JVM exit immediately on OOM rather than thrashing while workspace writes are in flight. `-XX:+HeapDumpOnOutOfMemoryError` and `-XX:HeapDumpPath=eglot-helpers-java-heap-dump-dir` capture a dump for diagnosis.
- **Larger JSON-RPC read buffer.** `read-process-output-max` is bumped to 4 MiB in Java buffers (Emacs default ~4 KiB is far too small for LSP payloads), and `process-adaptive-read-buffering` is disabled.
- **Serialized incremental builds.** `:maxConcurrentBuilds 1` in the JDTLS settings reduces concurrent `.metadata/` writers.
- **No file watcher registration.** `workspace/didChangeWatchedFiles` is suppressed because on large projects JDTLS tries to watch thousands of files and exhausts file descriptors.

If a workspace still ends up corrupted, `eglot-helpers-java-wipe-workspace` clears the cache and rebuilds. Check `eglot-helpers-java-heap-dump-dir` for a heap dump — its presence confirms OOM was the trigger, and bumping `-Xmx` in `eglot-helpers-java--server-contact` is the next step.

## License

GPL-3.0-or-later. See the [GNU General Public License](https://www.gnu.org/licenses/gpl-3.0.html) for details.
