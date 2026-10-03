(defpackage #:ls-compat
  (:use #:cl)
  (:export #:utf8-string-to-octets
           #:utf8-octets-to-string
           #:finite-float-p
           #:call-with-timeout
           #:with-timeout
           #:timeout-expired
           #:timeout-expired-seconds
           #:unsupported-operation
           #:unsupported-operation-name))

(defpackage #:ls-compat.posix
  (:use #:cl)
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
  (:export #:publish-pathname
           #:publish-file))

(defpackage #:ls-compat.tcp
  (:use #:cl)
  (:export #:tcp-connect
           #:tcp-listen
           #:tcp-accept
           #:tcp-stream
           #:tcp-local-port
           #:close-tcp))
