# ls-compat technical documentation

## Design boundary

`ls-compat` is a narrow facade over implementation-specific operations used by
its client projects. It is not a replacement for portable libraries:

- projects use UIOP directly for paths, processes, and environment variables;
- projects use Bordeaux Threads directly for threads and synchronization;
- Babel supplies UTF-8 codecs;
- SBCL's bundled SB-POSIX interface implements the optional POSIX system;
- USOCKET implements the optional TCP system.

The core system depends on Babel and, except under ECL, Serapeum for public
function type declarations. ECL elides those declarations because the current
Serapeum release does not compile there. POSIX support is currently SBCL-only.
POSIX and TCP support are split into
`ls-compat/posix` and `ls-compat/tcp` so applications do not load native
facilities they do not use.

`finite-float-p` uses Common Lisp's standardized finite `long-float` bounds and
NaN self-inequality. Arithmetic failures while inspecting non-finite input are
classified as non-finite instead of exposing an implementation-specific
predicate.

## UTF-8 measurement

`utf8-string-length` and `utf8-string-prefix-end` share a scalar-value scan.
They count one to four bytes per character and reject surrogates and values
above U+10FFFF. Invalid input goes through the existing encoder to preserve its
portable condition and native cause. A prefix scan validates the entire selected
range even after the byte budget is exhausted.

`internal-real-time-seconds` divides `get-internal-real-time` by
`internal-time-units-per-second` coerced to `double-float`. It introduces no clock
state or deadline policy.

## Timeout semantics

`call-with-timeout` accepts an absolute duration in seconds. A `nil` duration
runs the thunk directly. The supported implementations intentionally have
implementation-specific interruption models:

- SBCL uses `sb-ext:with-timeout` in the calling thread.
- CCL starts a worker process, waits on a semaphore, and kills that worker if
the deadline elapses.

Both translate expiration to `timeout-expired`. On CCL, a timed operation must
therefore tolerate termination at arbitrary points, and cleanup that belongs to
the caller should use `unwind-protect`. Neither implementation can make an
arbitrary foreign call safely interruptible.

An unsupported implementation signals `unsupported-operation`; it never runs
the thunk without enforcing the requested deadline. This keeps a deadline from
being silently weakened. ECL currently loads the core system but has no timeout
backend.

## POSIX semantics

The POSIX system normalizes path designators through `uiop:native-namestring`
before passing them to SB-POSIX. It intentionally exposes raw permission bits,
not a platform-neutral file-permission model.

`process-alive-p` treats an SB-POSIX `EPERM` result as evidence that the target
PID exists. Other POSIX failures produce `nil`. This is consistent with POSIX
`kill(pid, 0)` but cannot distinguish a reused PID from the original process.
`process-group-alive-p` has the same semantics through `kill(-pgid, 0)`.

`signal-process-group` maps `:terminate` and `:kill` to SB-POSIX signal
constants and signals the negative group identifier, thereby targeting every
current group member. It deliberately propagates SB-POSIX errors rather than
silently degrading process-tree cleanup to direct-process termination.

`make-directory-exclusively` relies on the single `mkdir` system call rather
than a check-then-create sequence. It therefore retains atomic already-exists
failure behavior.

## TCP semantics

The TCP system delegates connection and listener creation to USOCKET. Sockets
are opaque external values; clients receive them only to pass to the package's
stream, metadata, and close operations. The default streams are binary octet
streams. A protocol needing character I/O must apply its own encoding, normally
with the core UTF-8 functions.

## Test matrix

`ls-compat/tests` exercises UTF-8 round trips, expiry behavior, POSIX directory
and mode operations, and a loopback TCP connection. The test system is run on:

| Implementation | Core | POSIX | TCP | Timeout |
| --- | --- | --- | --- | --- |
| SBCL | supported | supported on Unix | supported | supported |
| CCL | supported | signals `unsupported-operation` | supported | supported |
| ECL | loadable | signals `unsupported-operation` | dependency/platform dependent | signals `unsupported-operation` |

The POSIX system loads on other implementations so calls can signal the explicit
`unsupported-operation` boundary.
The matrix is a statement of tested library behavior, not a claim that every
client application is portable.
