# ls-compat

`ls-compat` is a small Common Lisp portability facade for applications that
need to run on SBCL and Clozure CL, with ECL kept loadable where practical.

It deliberately does not replace portable libraries that are already pleasant
to use directly:

- use `uiop` for paths, environment, processes, and command-line behavior;
- use `bordeaux-threads` for threads, locks, and condition variables;
- use `ls-compat` for UTF-8 octets and timeouts;
- use `ls-compat/posix` for the deliberately Unix-specific file-mode,
  process-ID, process-descendant, and exclusive-directory operations;
- use `ls-compat/files` for atomic file publication through a temporary
  sibling, replacing or refusing to replace an existing target;
- use `ls-compat/tcp` for basic TCP lifecycle.

The public API does not expose SBCL, CCL, or dependency-specific values.
`ls-compat/posix` and `ls-compat/tcp` are optional ASDF systems so projects
only take the native dependencies they need.

## Loading

```lisp
(ql:quickload :ls-compat)
(ql:quickload :ls-compat/posix)
(ql:quickload :ls-compat/files)
(ql:quickload :ls-compat/tcp)
```

## Compatibility

The core system supports UTF-8 conversion on all Common Lisp implementations
supported by Babel. Its timeout implementation supports SBCL and CCL. On an
implementation without a safe implementation hook, attempting a timeout
signals `ls-compat:unsupported-operation` rather than silently ignoring the
deadline.

The POSIX and TCP systems are intended for Unix-like systems. The POSIX
system uses SBCL's bundled SB-POSIX interface and signals
`ls-compat:unsupported-operation` on other implementations. The TCP system
uses USOCKET.

See `docs/USER_DOC.md` for the API and `docs/TECHNICAL_DOC.md` for semantics
and implementation constraints.
