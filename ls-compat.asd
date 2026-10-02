(asdf:defsystem #:ls-compat
  :description "Focused Common Lisp portability utilities for LS projects."
  :author "Lukáš Hozda"
  :license "COLL-Attribution"
  :version "0.1.0"
  :depends-on (#:babel
               #-ecl #:serapeum)
  :serial t
  :components ((:file "src/package")
               (:file "src/core"))
  :in-order-to ((asdf:test-op (asdf:test-op #:ls-compat/tests))))

(asdf:defsystem #:ls-compat/posix
  :description "Process and file operations backed by SB-POSIX, with a Win32 backend."
  :depends-on (#:ls-compat #+sbcl #:sb-posix)
  :serial t
  :components ((:file "src/win32" :if-feature (:and :sbcl :win32))
               (:file "src/posix")))

(asdf:defsystem #:ls-compat/files
  :description "Atomic file publication through temporary siblings."
  :depends-on (#:ls-compat/posix)
  :serial t
  :components ((:file "src/files")))

(asdf:defsystem #:ls-compat/tcp
  :description "TCP lifecycle operations backed by USOCKET."
  :depends-on (#:ls-compat #:usocket)
  :serial t
  :components ((:file "src/tcp")))

(asdf:defsystem #:ls-compat/tests
  :description "Regression tests for ls-compat."
  :depends-on (#:ls-compat/posix #:ls-compat/files #:ls-compat/tcp)
  :serial t
  :components ((:file "tests/package")
               (:file "tests/tests"))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:ls-compat/tests '#:run-tests)))
