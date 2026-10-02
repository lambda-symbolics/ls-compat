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
           #:file-operation-failed
           #:file-operation-failed-operation
           #:file-operation-failed-message
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
           #:stream-file-information
           #:open-regular-file
           #:directory-entries
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
