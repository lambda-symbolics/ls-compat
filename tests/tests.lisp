(in-package #:ls-compat/tests)

(defvar *test-failures* nil
  "Descriptions of failed ls-compat regression checks.")


;;;; -- Test helpers --

(defun tests--check (value description)
  "Record DESCRIPTION unless VALUE is true, then return VALUE."
  (unless value
    (push description *test-failures*))
  value)

(defvar *tests--random-state* (make-random-state t)
  "A fresh random state naming temporary directories on every implementation.")

(defun tests--temporary-directory ()
  "Return a unique temporary directory pathname."
  (merge-pathnames
   (format nil "ls-compat-~D-~36R/"
           (get-universal-time)
           (random most-positive-fixnum *tests--random-state*))
   (uiop:temporary-directory)))


;;;; -- Core --

(defun tests--utf8-round-trip ()
  "Check portable UTF-8 encoding and decoding."
  (let* ((text "Příliš žluťoučký kůň")
         (octets (utf8-string-to-octets text)))
    (tests--check
     (equalp octets
             #(80 197 153 195 173 108 105 197 161 32 197 190 108 117 197 165 111 117 196 141 107 195 189 32 107 197 175 197 136))
     "UTF-8 encoding produced unexpected octets.")
    (tests--check (string= text (utf8-octets-to-string octets))
                  "UTF-8 decoding did not recover the original string.")))

(defun tests--utf8-failures ()
  "Check unencodable strings and invalid octets signal the portable condition."
  (let ((surrogate (let ((character (code-char #xD800)))
                     (and (characterp character) (string character)))))
    (when surrogate
      (tests--check
       (handler-case (progn (utf8-string-to-octets surrogate) nil)
         (utf8-conversion-failed (condition)
           (eq :encode (utf8-conversion-failed-direction condition))))
       "A lone surrogate encoded instead of signaling.")))
  (dolist (octets '((104 #xC3 #x28 105) (#xC0 #xAF) (104 #xED #xA0 #x80 105)))
    (tests--check
     (handler-case
         (progn (utf8-octets-to-string (coerce octets '(vector (unsigned-byte 8)))) nil)
       (utf8-conversion-failed (condition)
         (eq :decode (utf8-conversion-failed-direction condition))))
     (format nil "Invalid octets ~S decoded instead of signaling." octets)))
  (tests--check (string= "žluť" (utf8-octets-to-string
                                 (utf8-string-to-octets "Příliš žluťoučký" :start 7 :end 11)))
                "Delimited conversion did not round-trip."))

(defun tests--finite-floats ()
  "Check that ordinary and extreme finite floats are accepted."
  (tests--check (finite-float-p 1.0d0)
                "An ordinary double float was not finite.")
  (tests--check (finite-float-p (- most-positive-double-float))
                "The largest negative double float was not finite."))

(defun tests--timeout-signals-condition ()
  "Check that a deadline expires or declares the capability unsupported."
  (handler-case
      (progn
        (with-timeout 0.01
          (sleep 1))
        (tests--check nil "A timeout did not signal a public condition."))
    (timeout-expired ()
      t)
    (unsupported-operation (condition)
      (tests--check (eq 'ls-compat:call-with-timeout
                        (unsupported-operation-name condition))
                    "Unsupported timeout named the wrong operation."))))


;;;; -- POSIX --

(defun tests--posix-supported-p ()
  "Return whether this implementation provides the POSIX backend."
  (handler-case
      (progn
        (current-process-id)
        t)
    (unsupported-operation (condition)
      (tests--check
       (eq 'ls-compat.posix:current-process-id
           (unsupported-operation-name condition))
       "Unsupported POSIX backend named the wrong operation.")
      nil)))

(defun tests--current-process-group ()
  "Check current process group lookup and liveness."
  (let* ((process-id (current-process-id))
         (process-group-id (process-group-id process-id)))
    (tests--check (process-alive-p process-id)
                  "Current process is not alive.")
    (tests--check (plusp process-group-id)
                  "Current process group identifier is not positive.")
    (tests--check (process-group-alive-p process-group-id)
                  "Current process group is not alive.")))

(defun tests--exclusive-directory-and-mode ()
  "Check atomic directory creation and permission mode access."
  (let ((directory (tests--temporary-directory)))
    (unwind-protect
         (progn
           (tests--check
            (pathnamep (make-directory-exclusively directory :mode #o700))
            "Exclusive directory creation did not return a pathname.")
           (tests--check (= #o700 (logand #o777 (file-mode directory)))
                          "New directory mode is not 0700.")
           (setf (file-mode directory) #o755)
           (tests--check (= #o755 (logand #o777 (file-mode directory)))
                          "Updated directory mode is not 0755."))
      (ignore-errors
        (uiop:delete-directory-tree directory :validate t)))))


;;;; -- Files --

(defun tests--read-text (pathname)
  "Return PATHNAME's complete UTF-8 text."
  (with-open-file (stream pathname :external-format :utf-8)
    (let ((text (make-string (file-length stream))))
      (subseq text 0 (read-sequence text stream)))))

(defun tests--stray-siblings (pathname)
  "Return temporary siblings PUBLISH-FILE may have left beside PATHNAME."
  (directory (merge-pathnames (make-pathname :name :wild :type "tmp") pathname)))

(defun tests--publish-file-replaces ()
  "Check text and octet publication replaces existing targets without leftovers."
  (let* ((directory (tests--temporary-directory))
         (target (merge-pathnames "published.txt" directory)))
    (unwind-protect
         (progn
           (tests--check (equal target (publish-file target "first"))
                         "Publication did not return its target.")
           (tests--check (string= "first" (tests--read-text target))
                         "First publication did not write its text.")
           (publish-file target (lambda (stream) (write-string "second" stream)))
           (tests--check (string= "second" (tests--read-text target))
                         "Writer publication did not replace the text.")
           (publish-file target (make-array 3 :element-type '(unsigned-byte 8)
                                              :initial-contents '(104 105 33)))
           (tests--check (string= "hi!" (tests--read-text target))
                         "Octet publication did not replace the content.")
           (tests--check (null (tests--stray-siblings target))
                         "Publication left temporary siblings behind."))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(defun tests--publish-file-cleans-up-failures ()
  "Check a failing producer leaves neither a target nor a temporary."
  (let* ((directory (tests--temporary-directory))
         (target (merge-pathnames "never.txt" directory)))
    (unwind-protect
         (progn
           (tests--check
            (handler-case
                (progn
                  (publish-file target (lambda (stream)
                                         (declare (ignore stream))
                                         (error "producer failed")))
                  nil)
              (error () t))
            "A failing producer did not propagate its error.")
           (tests--check (null (probe-file target))
                         "A failing producer still published a target.")
           (tests--check (null (tests--stray-siblings target))
                         "A failing producer left a temporary sibling."))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(defun tests--publish-pathname-hooks ()
  "Check the prepare and publish hooks see the temporary before the target exists."
  (let* ((directory (tests--temporary-directory))
         (target (merge-pathnames "hooked.txt" directory))
         (prepared nil)
         (published nil))
    (unwind-protect
         (progn
           (publish-pathname
            target
            (lambda (temporary)
              (with-open-file (stream temporary :direction :output :if-exists :supersede)
                (write-string "hooked" stream)))
            :prepare-function (lambda (temporary)
                                (setf prepared (and (probe-file temporary)
                                                    (not (probe-file target)))))
            :publish-function (lambda (temporary final)
                                (setf published (equal final target))
                                (rename-file temporary final)))
           (tests--check prepared
                         "The prepare hook did not see a written temporary before publication.")
           (tests--check published
                         "The publish hook did not receive the target pathname.")
           (tests--check (string= "hooked" (tests--read-text target))
                         "A custom publish hook did not publish the content."))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(defun tests--publish-file-refuses-existing ()
  "Check no-clobber publication keeps an occupied target or reports unsupport."
  (let* ((directory (tests--temporary-directory))
         (target (merge-pathnames "exclusive.txt" directory)))
    (unwind-protect
         (progn
           (publish-file target "original")
           (handler-case
               (progn
                 (publish-file target "intruder" :if-exists :error)
                 (tests--check nil "No-clobber publication replaced an existing target."))
             (link-target-exists (condition)
               (tests--check (equal target (pathname (file-error-pathname condition)))
                             "LINK-TARGET-EXISTS named the wrong pathname."))
             (unsupported-operation ()
               nil))
           (tests--check (string= "original" (tests--read-text target))
                         "No-clobber publication changed the existing content.")
           (tests--check (null (tests--stray-siblings target))
                         "No-clobber publication left a temporary sibling.")
           (let ((fresh (merge-pathnames "fresh.txt" directory)))
             (handler-case
                 (progn
                   (publish-file fresh "fresh" :if-exists :error)
                   (tests--check (string= "fresh" (tests--read-text fresh))
                                 "No-clobber publication did not create an absent target."))
               (unsupported-operation ()
                 nil))))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(defun tests--descendant-processes ()
  "Check a launched child appears among this process's descendants."
  (let ((child (uiop:launch-program '("sleep" "30") :output nil :error-output nil)))
    (unwind-protect
         (let ((pid (uiop:process-info-pid child)))
           (tests--check (member pid (descendant-process-ids (current-process-id)))
                         "A launched child was missing from the descendant snapshot.")
           (tests--check (not (member (current-process-id)
                                      (descendant-process-ids (current-process-id))))
                         "A process listed itself among its descendants."))
      (ignore-errors (uiop:terminate-process child :urgent t))
      (ignore-errors (uiop:wait-process child)))))


;;;; -- File information and paths --

(defun tests--write-octets (pathname octets)
  "Write the octet list OCTETS to PATHNAME."
  (with-open-file (stream pathname :direction :output :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
    (write-sequence (coerce octets '(vector (unsigned-byte 8))) stream)))

(defun tests--file-information-and-reasons ()
  "Check kinds, privacy, read-only state, and failure reasons."
  (let* ((directory (tests--temporary-directory))
         (file (merge-pathnames "plain.txt" directory))
         (missing (merge-pathnames "missing/below.txt" directory)))
    (unwind-protect
         (progn
           (publish-file file "plain")
           (setf (file-mode file) #o600)
           (let ((information (file-information file)))
             (tests--check (eq ':file (file-information-kind information))
                           "A regular file was not observed as :FILE.")
             (tests--check (file-information-owned-p information)
                           "A file this process created was not owned.")
             (tests--check (file-information-private-p information)
                           "A mode 0600 file was not private.")
             (tests--check (not (file-information-read-only-p information))
                           "A writable file was reported read-only."))
           (setf (file-mode file) #o444)
           (let ((information (file-information file)))
             (tests--check (file-information-read-only-p information)
                           "A mode 0444 file was not reported read-only.")
             (tests--check (not (file-information-private-p information))
                           "A world-readable file was reported private."))
           (setf (file-mode file) #o600)
           (handler-case
               (progn
                 (file-information missing)
                 (tests--check nil "A missing path was observed."))
             (file-operation-failed (condition)
               (tests--check (eq ':missing (file-operation-failed-reason condition))
                             "A missing path did not report :MISSING.")))
           (handler-case
               (progn
                 (file-information (merge-pathnames "plain.txt/below" directory))
                 (tests--check nil "A path through a file was observed."))
             (file-operation-failed (condition)
               (tests--check (eq ':not-directory (file-operation-failed-reason condition))
                             "A path through a file did not report :NOT-DIRECTORY."))))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(defun tests--directory-names ()
  "Check name-only listing agrees with classified listing and honors the limit."
  (let ((directory (tests--temporary-directory)))
    (unwind-protect
         (progn
           (dolist (name '("a.txt" "b.txt" "c.txt"))
             (publish-file (merge-pathnames name directory) name))
           (tests--check (equal (sort (directory-names directory) #'string<)
                                (sort (mapcar #'first (directory-entries directory))
                                      #'string<))
                         "Name-only listing disagreed with classified listing.")
           (multiple-value-bind (names more-p) (directory-names directory :limit 2)
             (tests--check (and (= 2 (length names)) more-p)
                           "Name-only listing ignored its limit.")))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(defun tests--canonical-pathnames ()
  "Check link resolution, missing tails, and containment through links."
  (let* ((directory (tests--temporary-directory))
         (inside (merge-pathnames "inside/" directory))
         (outside (merge-pathnames "outside/" directory))
         (escape (merge-pathnames "inside/escape" directory)))
    (unwind-protect
         (progn
           (ensure-directories-exist inside)
           (ensure-directories-exist outside)
           (uiop:run-program (list "ln" "-s"
                                   (uiop:native-namestring outside)
                                   (uiop:native-namestring escape)))
           (tests--check (uiop:pathname-equal (resolve-pathname inside) (truename inside))
                         "An existing directory did not resolve to its truename.")
           (handler-case
               (progn
                 (resolve-pathname (merge-pathnames "absent.txt" inside))
                 (tests--check nil "A missing path resolved."))
             (file-operation-failed (condition)
               (tests--check (eq ':missing (file-operation-failed-reason condition))
                             "A missing path did not resolve as :MISSING.")))
           (tests--check
            (uiop:pathname-equal
             (canonical-pathname (merge-pathnames "new/deeper/file.txt" inside))
             (merge-pathnames "new/deeper/file.txt" (truename inside)))
            "A missing tail was not kept under its resolved ancestor.")
           (tests--check (pathname-within-p (merge-pathnames "new/file.txt" inside) inside)
                         "A missing child was not within its root.")
           (tests--check (not (pathname-within-p (merge-pathnames "escape/x.txt" inside)
                                                 inside))
                         "A link out of the root still counted as within it.")
           (tests--check (pathname-within-p inside inside)
                         "A root did not count as within itself."))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(defun tests--signal-process ()
  "Check a single child process can be terminated."
  (let ((child (uiop:launch-program '("sleep" "30") :output nil :error-output nil)))
    (unwind-protect
         (let ((pid (uiop:process-info-pid child)))
           (tests--check (= pid (signal-process pid :terminate))
                         "SIGNAL-PROCESS did not return the process identifier.")
           (uiop:wait-process child)
           (tests--check (not (uiop:process-alive-p child))
                         "A terminated child stayed alive."))
      (ignore-errors (uiop:terminate-process child :urgent t))
      (ignore-errors (uiop:wait-process child)))))

(defun tests--read-file-text ()
  "Check bounded reading, size limits, link refusal, non-files, and UTF-8 checks."
  (let* ((directory (tests--temporary-directory))
         (file (merge-pathnames "text.txt" directory))
         (link (merge-pathnames "link.txt" directory))
         (binary (merge-pathnames "binary.bin" directory)))
    (unwind-protect
         (progn
           (publish-file file "Příliš žluťoučký kůň")
           (tests--check (string= "Příliš žluťoučký kůň" (read-file-text file))
                         "A UTF-8 file did not read back exactly.")
           (tests--check (handler-case (progn (read-file-text file :maximum-octets 4) nil)
                           (file-too-large () t))
                         "An oversized file did not signal FILE-TOO-LARGE.")
           (tests--check (handler-case
                             (progn
                               (read-file-text file :validation-function
                                               (lambda ()
                                                 (publish-file file "replaced")))
                               nil)
                           (file-changed () t))
                         "A file replaced during the read did not signal FILE-CHANGED.")
           (tests--write-octets binary '(104 #xC3 #x28 105))
           (tests--check (handler-case (progn (read-file-text binary) nil)
                           (file-not-utf-8 () t))
                         "Invalid UTF-8 did not signal FILE-NOT-UTF-8.")
           (tests--check (handler-case (progn (read-file-text directory) nil)
                           (not-regular-file () t)
                           (file-operation-failed () t))
                         "A directory was read as text.")
           (uiop:run-program (list "ln" "-s" (uiop:native-namestring file)
                                   (uiop:native-namestring link)))
           (tests--check (handler-case (progn (read-file-text link) nil)
                           (file-operation-failed (condition)
                             (eq ':symbolic-link (file-operation-failed-reason condition))))
                         "A link was followed without FOLLOW-LINKS-P.")
           (tests--check (string= "replaced" (read-file-text link :follow-links-p t))
                         "A link was not followed with FOLLOW-LINKS-P."))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))


;;;; -- TCP --

(defun tests--tcp-lifecycle ()
  "Check listener creation, client connection, acceptance, and streams."
  (let ((listener (tcp-listen "127.0.0.1" 0))
        (client nil)
        (server nil))
    (unwind-protect
         (let ((port (tcp-local-port listener)))
           (setf client (tcp-connect "127.0.0.1" port)
                 server (tcp-accept listener))
           (tests--check (streamp (tcp-stream client))
                          "TCP client has no stream.")
           (tests--check (streamp (tcp-stream server))
                          "Accepted TCP connection has no stream.")
           (let ((payload #(0 1 2 253 254 255))
                 (received (make-array 6 :element-type '(unsigned-byte 8))))
             (write-sequence payload (tcp-stream client))
             (finish-output (tcp-stream client))
             (tests--check (= (length payload)
                              (read-sequence received (tcp-stream server)))
                            "Accepted TCP connection did not receive every octet.")
             (tests--check (equalp payload received)
                            "TCP octets changed during transfer.")))
      (dolist (socket (list server client listener))
        (when socket
          (ignore-errors
            (close-tcp socket)))))))


;;;; -- Runner --

(defun run-tests ()
  "Run ls-compat regression tests and signal an error on any failure."
  (let ((*test-failures* nil))
    (dolist (test '(tests--utf8-round-trip
                    tests--utf8-failures
                    tests--finite-floats
                    tests--timeout-signals-condition))
      (funcall test))
    (when (tests--posix-supported-p)
      (dolist (test '(tests--current-process-group
                      tests--exclusive-directory-and-mode
                      tests--descendant-processes
                      tests--file-information-and-reasons
                      tests--directory-names
                      tests--canonical-pathnames
                      tests--signal-process
                      tests--read-file-text))
        (funcall test)))
    (dolist (test '(tests--publish-file-replaces
                    tests--publish-file-cleans-up-failures
                    tests--publish-pathname-hooks
                    tests--publish-file-refuses-existing))
      (funcall test))
    (tests--tcp-lifecycle)
    (when *test-failures*
      (error "ls-compat test failures:~%~{~A~%~}"
             (nreverse *test-failures*)))
    t))
