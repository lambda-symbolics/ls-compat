(defpackage #:ls-compat
  (:use #:cl)
  (:documentation "UTF-8 conversion, float checks, and timeouts that behave alike on every supported implementation.")
  (:export #:utf8-string-to-octets
           #:utf8-octets-to-string
           #:utf8-conversion-failed
           #:utf8-conversion-failed-direction
           #:utf8-conversion-failed-cause
           #:finite-float-p
           #:call-with-timeout
           #:with-timeout
           #:timeout-expired
           #:timeout-expired-seconds
           #:unsupported-operation
           #:unsupported-operation-name))

(defpackage #:ls-compat.posix
  (:use #:cl)
  (:documentation "Process, file mode, file information, and path operations for POSIX hosts, with a Win32 backend on SBCL.")
  (:export #:current-process-id
           #:process-state
           #:process-alive-p
           #:process-groups-supported-p
           #:process-group-id
           #:process-group-alive-p
           #:signal-process-group
           #:make-directory-exclusively
           #:file-mode
           #:link-file
           #:link-target-exists
           #:link-failed
           #:link-failed-message
           #:mode-failed
           #:mode-failed-message
           #:failure-reason
           #:file-operation-failed
           #:file-operation-failed-operation
           #:file-operation-failed-message
           #:file-operation-failed-reason
           #:file-operation-failed-code
           #:not-regular-file
           #:not-regular-file-kind
           #:file-kind
           #:file-information
           #:make-file-information
           #:file-information-p
           #:file-information-kind
           #:file-information-identity
           #:file-information-size
           #:file-information-modification-time
           #:file-information-change-time
           #:file-information-owned-p
           #:file-information-private-p
           #:file-information-read-only-p
           #:file-information-same-object-p
           #:file-information-unchanged-p
           #:stream-file-information
           #:open-regular-file
           #:directory-entries
           #:directory-names
           #:resolve-pathname
           #:canonical-pathname
           #:pathname-within-p
           #:signal-process
           #:descendant-process-ids))

(defpackage #:ls-compat.files
  (:use #:cl)
  (:documentation "Atomic file publication through temporary siblings, and bounded UTF-8 file reading.")
  (:export #:publish-pathname
           #:publish-file
           #:read-file-text
           #:file-too-large
           #:file-too-large-size
           #:file-too-large-limit
           #:file-changed
           #:file-not-utf-8
           #:temporary-file-unavailable
           #:temporary-file-unavailable-attempts))

(defpackage #:ls-compat.tcp
  (:use #:cl)
  (:documentation "TCP connections, listeners, and accepted streams backed by USOCKET.")
  (:export #:tcp-connect
           #:tcp-listen
           #:tcp-accept
           #:tcp-stream
           #:tcp-local-port
           #:close-tcp))
