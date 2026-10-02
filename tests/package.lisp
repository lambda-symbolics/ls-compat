(defpackage #:ls-compat/tests
  (:use #:cl)
  (:import-from #:ls-compat
                #:utf8-string-to-octets
                #:utf8-octets-to-string
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
                #:link-target-exists)
  (:import-from #:ls-compat.files
                #:publish-pathname
                #:publish-file)
  (:import-from #:ls-compat.tcp
                #:tcp-connect
                #:tcp-listen
                #:tcp-accept
                #:tcp-stream
                #:tcp-local-port
                #:close-tcp))
