(defpackage #:ls-compat/tests
  (:use #:cl)
  (:import-from #:ls-compat
                #:utf8-string-to-octets
                #:utf8-octets-to-string
                #:utf8-conversion-failed
                #:utf8-conversion-failed-direction
                #:finite-float-p
                #:with-timeout
                #:timeout-expired
                #:unsupported-operation
                #:unsupported-operation-name)
  (:import-from #:ls-compat.posix
                #:current-process-id
                #:process-alive-p
                #:process-group-id
                #:process-group-alive-p
                #:make-directory-exclusively
                #:file-mode
                #:link-target-exists
                #:descendant-process-ids
                #:file-information
                #:file-information-kind
                #:file-information-owned-p
                #:file-information-private-p
                #:file-information-read-only-p
                #:file-information-same-object-p
                #:open-regular-file
                #:stream-file-information
                #:file-operation-failed
                #:file-operation-failed-reason
                #:not-regular-file
                #:directory-entries
                #:directory-names
                #:resolve-pathname
                #:canonical-pathname
                #:pathname-within-p
                #:signal-process)
  (:import-from #:ls-compat.files
                #:publish-pathname
                #:publish-file
                #:read-file-text
                #:file-too-large
                #:file-changed
                #:file-not-utf-8)
  (:import-from #:ls-compat.tcp
                #:tcp-connect
                #:tcp-listen
                #:tcp-accept
                #:tcp-stream
                #:tcp-local-port
                #:close-tcp))
