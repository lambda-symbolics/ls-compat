# ls-compat user documentation

## Core system

Load `ls-compat` for UTF-8 conversion and deadline handling.

```lisp
(ql:quickload :ls-compat)

(ls-compat:utf8-string-to-octets "Příliš žluťoučký kůň")
(ls-compat:utf8-octets-to-string #(104 101 108 108 111))

(ls-compat:with-timeout 2
  (perform-operation))
```

`utf8-string-to-octets` and `utf8-octets-to-string` accept optional `:start`
and `:end` bounds. They always use UTF-8 and do not depend on the implementation
external format.

`finite-float-p` returns true only for a finite float. It rejects NaN and
infinite values before a protocol serializes them as JSON numbers.

`call-with-timeout` receives a number of seconds, or `nil`, and a nullary
function. `with-timeout` is its body form. A completed body returns all of its
values. A deadline signals `ls-compat:timeout-expired`; inspect its requested
duration with `ls-compat:timeout-expired-seconds`.

A `nil` deadline does not install a timeout. An implementation that cannot
provide a safe timeout signals `ls-compat:unsupported-operation`. Its operation
name is available through `ls-compat:unsupported-operation-name`.

## POSIX system

Load `ls-compat/posix` only for Unix process and permission APIs.

```lisp
(ql:quickload :ls-compat/posix)

(ls-compat.posix:current-process-id)
(ls-compat.posix:process-alive-p process-id)
(ls-compat.posix:process-group-id process-id)
(ls-compat.posix:process-group-alive-p process-group-id)
(ls-compat.posix:signal-process-group process-group-id :terminate)
(ls-compat.posix:make-directory-exclusively #p"/tmp/example" :mode #o700)
(setf (ls-compat.posix:file-mode #p"/tmp/example") #o700)
```

`make-directory-exclusively` is atomic. On SBCL it signals the underlying
SB-POSIX condition if the directory already exists or cannot be created.
`file-mode`
reads and writes raw POSIX mode bits. The operating-system umask may restrict
the mode supplied when a directory is created.

`make-temporary-directory` takes a parent directory and name prefix and returns
an exclusively created private directory. The caller chooses when to remove it.
`replace-file` renames an existing source over a target; `remove-file` unlinks
one exact native path. On Windows, deletion restores the original attributes on
failure, while replacement clears a target read-only attribute without restoring
it on failure.

`delete-directory-tree` takes a physical, non-wild directory pathname and an
explicit `:validate` predicate (or `t`). Its missing-path policy is
`:if-does-not-exist :error` or `:ignore`. On Windows, native enumeration includes
dot entries, reparse points are removed without traversal, and transient file
failures receive bounded retries. These mutation APIs require SBCL, including
its native Windows backend. Path authorization and lifecycle belong to callers.

`process-alive-p` uses `kill(pid, 0)`. It is a snapshot, not process identity:
a PID can be reused. A process that exists but cannot be signaled due to
permissions counts as alive.

`process-group-id` returns a process's POSIX group identifier.
`process-group-alive-p` uses `kill(-pgid, 0)` and has the same snapshot and
permission semantics. `signal-process-group` sends `:terminate` or `:kill` to
every member of the group. On SBCL it propagates an SB-POSIX condition when the
operation cannot be completed.

The POSIX backend currently supports SBCL on Unix. On other implementations,
these operations signal `ls-compat:unsupported-operation`. The core `ls-compat`
system remains loadable on ECL.

## TCP system

Load `ls-compat/tcp` for simple client and listener lifecycle operations.

```lisp
(ql:quickload :ls-compat/tcp)

(let ((listener (ls-compat.tcp:tcp-listen "127.0.0.1" 0)))
  (unwind-protect
       (let ((port (ls-compat.tcp:tcp-local-port listener)))
         ;; Pass PORT to a client, then accept its connection.
         (multiple-value-bind (socket address port)
             (ls-compat.tcp:tcp-accept listener)
           (declare (ignore address port))
           (unwind-protect
                (ls-compat.tcp:tcp-stream socket)
             (ls-compat.tcp:close-tcp socket))))
    (ls-compat.tcp:close-tcp listener)))
```

`tcp-connect` returns a USOCKET socket. `tcp-listen` returns a listener and
uses a backlog of 16, address reuse, and `(unsigned-byte 8)` streams by default.
Pass `:element-type` to request another stream element type. `tcp-accept`
returns the accepted socket, peer address, and peer port. `tcp-stream` returns
the socket's bidirectional stream. `tcp-local-port` returns the bound local
port. Always release sockets and listeners with `close-tcp`.

The TCP APIs deliberately expose the underlying socket object as an opaque
value. Do not rely on its representation; pass it only to this system or close
it with `close-tcp`.
