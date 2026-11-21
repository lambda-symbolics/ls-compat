;;; Native Windows owning-library gate, with the same distribution as qlfile.

(require :asdf)
(assert (and (member :win32 *features*) (member :x86-64 *features*)))
(assert (string= "2.6.6" (lisp-implementation-version)))
(format t "Native gate: ~A ~A, ~A ~A~%"
        (lisp-implementation-type) (lisp-implementation-version)
        (software-type) (software-version))
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

(let ((releases
        (with-open-file (stream "qlfile")
          (loop for line = (read-line stream nil)
                while line
                for fields = (remove "" (uiop:split-string line
                                                         :separator '(#\Space #\Tab #\Return))
                                     :test #'string=)
                when (equal (first fields) "ql") collect (third fields)))))
  (assert releases)
  (assert (every (lambda (release) (equal release (first releases))) releases))
  (ql-dist:install-dist
   (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/distinfo.txt" (first releases))
   :replace t :prompt nil))

(let ((root (uiop:getcwd)))
  (asdf:initialize-source-registry
   `(:source-registry (:directory ,root) :ignore-inherited-configuration))
  (asdf:load-asd (merge-pathnames "ls-compat.asd" root))
  (assert (equal root (asdf:system-source-directory "ls-compat")))
  (ql:quickload "ls-compat/tests")
  (assert (equal root (asdf:system-source-directory "ls-compat")))
  (assert (equal root (asdf:system-source-directory "ls-compat/tests")))
  (assert (plusp (uiop:symbol-call :ls-compat.posix :current-process-id)))
  (asdf:test-system "ls-compat/tests"))
