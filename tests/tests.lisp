(in-package #:ls-compat/tests)

(defvar *test-failures* nil
  "Descriptions of failed ls-compat regression checks.")

;;;; -- Test helpers --

(ls-compat::-> tests--check (t string) t)
(defun tests--check (value description)
  "Record DESCRIPTION unless VALUE is true, then return VALUE."
  (unless value
    (push description *test-failures*))
  value)

(defvar *tests--random-state* (make-random-state t)
  "A fresh random state naming temporary directories on every implementation.")

(ls-compat::-> tests--temporary-directory () pathname)
(defun tests--temporary-directory ()
  "Return a unique temporary directory pathname."
  (merge-pathnames
   (format nil "ls-compat-~D-~36R/"
           (get-universal-time)
           (random most-positive-fixnum *tests--random-state*))
   (uiop:temporary-directory)))


;;;; -- Core --

(ls-compat::-> tests--utf8-round-trip () t)
(defun tests--utf8-round-trip ()
  "Check portable UTF-8 encoding and decoding."
  (let* ((text   "Příliš žluťoučký kůň")
         (octets (utf8-string-to-octets text)))
    (tests--check
     (equalp octets
             #(80 197 153 195 173 108 105 197 161 32 197 190 108 117 197 165 111 117 196 141 107 195 189 32 107 197 175 197 136))
     "UTF-8 encoding produced unexpected octets.")
    (tests--check (string= text (utf8-octets-to-string octets))
                  "UTF-8 decoding did not recover the original string.")))

(ls-compat::-> tests--utf8-failures () t)
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

(ls-compat::-> tests--utf8-measurements () t)
(defun tests--utf8-measurements ()
  "Compare allocation-free measurements with the strict encoder at every bound."
  (let* ((scalars (coerce (mapcar #'code-char
                                '(0 #x7F #x80 #x7FF #x800 #xD7FF #xE000
                                  #xFFFF #x10000 #x10FFFF))
                          'string))
         (texts   (list "" "hello" scalars
                        (make-array 3 :element-type 'character
                                      :displaced-to scalars :displaced-index-offset 2)
                        (make-array 8 :element-type 'character :initial-element #\λ
                                      :fill-pointer 4))))
    (dolist (text texts)
      (loop for start from 0 to (length text)
            do (loop for end from start to (length text)
                     for bytes = (length (utf8-string-to-octets text :start start :end end))
                     do (tests--check (= bytes (utf8-string-length text :start start :end end))
                                      "UTF-8 length differed from the encoder.")
                        (loop for budget from 0 to (1+ bytes)
                              for expected = (loop for index from start to end
                                                   when (<= (length (utf8-string-to-octets
                                                                     text :start start :end index))
                                                            budget)
                                                     maximize index)
                              do (tests--check
                                  (= expected (utf8-string-prefix-end
                                               text budget :start start :end end))
                                  "A UTF-8 prefix did not end at the longest fitting character."))))))
  #+sbcl
  (let* ((text   (make-string 4096 :initial-element #\λ))
         (before (sb-ext:get-bytes-consed)))
    (dotimes (index 100)
      (utf8-string-length text)
      (utf8-string-prefix-end text index))
    (tests--check (= before (sb-ext:get-bytes-consed))
                  "Valid UTF-8 measurement allocated heap storage.")))

(ls-compat::-> tests--utf8-measurement-failures () t)
(defun tests--utf8-measurement-failures ()
  "Check rejected scalar values, invalid tails, and selected-range validation."
  (dolist (code '(#xD800 #xDFFF #x110000))
    (let ((character (ignore-errors (code-char code))))
      (when character
        (let* ((text     (concatenate 'string "a" (string character) "b"))
               (original (handler-case (progn (utf8-string-to-octets text) nil)
                           (utf8-conversion-failed (condition) condition))))
          (dolist (operation (list (lambda () (utf8-string-length text))
                                   (lambda () (utf8-string-prefix-end text 0))
                                   (lambda () (utf8-string-prefix-end text 1))
                                   (lambda () (utf8-string-prefix-end text 100))))
            (tests--check
             (handler-case (progn (funcall operation) nil)
               (utf8-conversion-failed (condition)
                 (and (eq :encode (utf8-conversion-failed-direction condition))
                      (or (null original)
                          (eq (type-of (utf8-conversion-failed-cause original))
                              (type-of (utf8-conversion-failed-cause condition)))))))
             "An invalid UTF-8 tail did not retain the encoder's failure."))
          (tests--check (= 1 (utf8-string-length text :end 1))
                        "UTF-8 length inspected text outside the selected range.")
          (tests--check (= 3 (utf8-string-prefix-end text 1 :start 2))
                        "UTF-8 prefix inspected text outside the selected range.")))))
  (tests--check
   (handler-case (progn (funcall (symbol-function 'utf8-string-prefix-end) "text" -1) nil)
     (type-error () t))
   "A negative byte budget did not signal TYPE-ERROR."))

(ls-compat::-> tests--internal-real-time-seconds () t)
(defun tests--internal-real-time-seconds ()
  "Check the shared conversion's representation and bounds against raw ticks."
  (let* ((units  (coerce internal-time-units-per-second 'double-float))
         (before (/ (get-internal-real-time) units))
         (actual (internal-real-time-seconds))
         (after  (/ (get-internal-real-time) units)))
    (tests--check (and (typep actual 'double-float) (<= before actual after))
                  "Internal real time was not converted to double-float seconds.")))

(ls-compat::-> tests--finite-floats () t)
(defun tests--finite-floats ()
  "Check that ordinary and extreme finite floats are accepted."
  (tests--check (finite-float-p 1.0d0)
                "An ordinary double float was not finite.")
  (tests--check (finite-float-p (- most-positive-double-float))
                "The largest negative double float was not finite."))

(ls-compat::-> tests--timeout-signals-condition () t)
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

(ls-compat::-> tests--posix-supported-p () boolean)
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

(ls-compat::-> tests--current-process-group () t)
(defun tests--current-process-group ()
  "Check current process group lookup and liveness."
  (let* ((process-id       (current-process-id))
         (process-group-id (process-group-id process-id)))
    (tests--check (process-alive-p process-id)
                  "Current process is not alive.")
    (tests--check (plusp process-group-id)
                  "Current process group identifier is not positive.")
    (tests--check (process-group-alive-p process-group-id)
                  "Current process group is not alive.")))

(ls-compat::-> tests--exclusive-directory-and-mode () t)
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

(ls-compat::-> tests--read-text (pathname) string)
(defun tests--read-text (pathname)
  "Return PATHNAME's complete UTF-8 text."
  (with-open-file (stream pathname :external-format :utf-8)
    (let ((text (make-string (file-length stream))))
      (subseq text 0 (read-sequence text stream)))))

(ls-compat::-> tests--stray-siblings (pathname) list)
(defun tests--stray-siblings (pathname)
  "Return temporary siblings PUBLISH-FILE may have left beside PATHNAME."
  (directory (merge-pathnames (make-pathname :name :wild :type "tmp") pathname)))

(ls-compat::-> tests--publish-file-replaces () t)
(defun tests--publish-file-replaces ()
  "Check text and octet publication replaces existing targets without leftovers."
  (let* ((directory (tests--temporary-directory))
         (target    (merge-pathnames "published.txt" directory)))
    (unwind-protect
         (progn
           (tests--check (equal target (publish-file target "first"))
                         "Publication did not return its target.")
           (tests--check (string= "first" (tests--read-text target))
                         "First publication did not write its text.")
           (publish-file target (lambda (stream) (write-string "second" stream)))
           (tests--check (string= "second" (tests--read-text target))
                         "Writer publication did not replace the text.")
           (publish-file target (make-array 3 :element-type     '(unsigned-byte 8)
                                              :initial-contents '(104 105 33)))
           (tests--check (string= "hi!" (tests--read-text target))
                         "Octet publication did not replace the content.")
           (tests--check (null (tests--stray-siblings target))
                         "Publication left temporary siblings behind."))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(ls-compat::-> tests--publish-file-cleans-up-failures () t)
(defun tests--publish-file-cleans-up-failures ()
  "Check a failing producer leaves neither a target nor a temporary."
  (let* ((directory (tests--temporary-directory))
         (target    (merge-pathnames "never.txt" directory)))
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

(ls-compat::-> tests--publish-pathname-hooks () t)
(defun tests--publish-pathname-hooks ()
  "Check the prepare and publish hooks see the temporary before the target exists."
  (let* ((directory (tests--temporary-directory))
         (target    (merge-pathnames "hooked.txt" directory))
         (prepared  nil)
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

(ls-compat::-> tests--publish-file-refuses-existing () t)
(defun tests--publish-file-refuses-existing ()
  "Check no-clobber publication keeps an occupied target or reports unsupport."
  (let* ((directory (tests--temporary-directory))
         (target    (merge-pathnames "exclusive.txt" directory)))
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

(ls-compat::-> tests--publish-file-reports-unavailable-temporary () t)
(defun tests--publish-file-reports-unavailable-temporary ()
  "Check publication signals TEMPORARY-FILE-UNAVAILABLE when no temporary name is free."
  (let* ((directory (tests--temporary-directory))
         (target    (merge-pathnames "unavailable.txt" directory)))
    (unwind-protect
         (tests--check
          (handler-case
              (let ((ls-compat.files::*files--temporary-attempts* 0))
                (publish-file target "content")
                nil)
            (temporary-file-unavailable (condition)
              (equal target (pathname (file-error-pathname condition)))))
          "Exhausted temporary names did not signal TEMPORARY-FILE-UNAVAILABLE.")
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(ls-compat::-> tests--descendant-processes () t)
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

(ls-compat::-> tests--write-octets (pathname sequence) (vector (unsigned-byte 8)))
(defun tests--write-octets (pathname octets)
  "Write the octet sequence OCTETS to PATHNAME."
  (with-open-file (stream pathname :direction :output :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
    (write-sequence (coerce octets '(vector (unsigned-byte 8))) stream)))

(ls-compat::-> tests--file-information-and-reasons () t)
(defun tests--file-information-and-reasons ()
  "Check kinds, privacy, read-only state, and failure reasons."
  (let* ((directory (tests--temporary-directory))
         (file      (merge-pathnames "plain.txt" directory))
         (missing   (merge-pathnames "missing/below.txt" directory)))
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

(ls-compat::-> tests--regular-file-ranges () t)
(defun tests--regular-file-ranges ()
  "Check standard byte lengths and bounded seeks retain the opened file identity."
  (let* ((directory (tests--temporary-directory))
         (file      (merge-pathnames "ranges.bin" directory))
         (original  (merge-pathnames "original.bin" directory))
         (octets    (make-array 65537 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           (ensure-directories-exist file)
           (dotimes (index (length octets))
             (setf (aref octets index) (mod index 256)))
           (tests--write-octets file octets)
           (setf (file-mode file) #o600)
           (multiple-value-bind (stream information) (open-regular-file file)
             (unwind-protect
                  (progn
                    (tests--check (= (length octets) (file-length stream))
                                  "The secure stream did not report its byte length.")
                    (tests--check (zerop (file-position stream))
                                  "The secure stream did not start at byte zero.")
                    (rename-file file original)
                    (tests--write-octets file '(42))
                    (tests--check
                     (file-information-same-object-p
                      information (stream-file-information stream))
                     "Replacing the pathname changed the opened stream identity.")
                    (tests--check (= (length octets) (file-length stream))
                                  "The stream length followed the replaced pathname.")
                    (dolist (start '(0 32761 65520))
                      (tests--check (file-position stream start)
                                    "Seeking to a byte range failed.")
                      (let ((range (make-array 17 :element-type '(unsigned-byte 8))))
                        (tests--check (= 17 (read-sequence range stream))
                                      "A bounded byte range was incomplete.")
                        (tests--check (equalp range (subseq octets start (+ start 17)))
                                      "A byte range did not preserve raw octets.")
                        (tests--check (= (+ start 17) (file-position stream))
                                      "Reading a range reported the wrong byte position.")))
                    (tests--check (file-position stream :end)
                                  "Seeking to the end failed.")
                    (tests--check (and (= (length octets) (file-position stream))
                                      (eq ':eof (read-byte stream nil ':eof)))
                                  "The stream end was not its original file end.")
                    (tests--check (file-position stream :start)
                                  "Seeking back to the start failed.")
                    (tests--check (= 0 (read-byte stream))
                                  "The start seek did not read the original first byte."))
               (close stream))))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(ls-compat::-> tests--directory-names () t)
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

(ls-compat::-> tests--canonical-pathnames () t)
(defun tests--canonical-pathnames ()
  "Check link resolution, missing tails, and containment through links."
  (let* ((directory (tests--temporary-directory))
         (inside    (merge-pathnames "inside/" directory))
         (outside   (merge-pathnames "outside/" directory))
         (escape    (merge-pathnames "inside/escape" directory)))
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

(ls-compat::-> tests--signal-process () t)
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

(ls-compat::-> tests--read-file-text () t)
(defun tests--read-file-text ()
  "Check bounded reading, size limits, link refusal, non-files, and UTF-8 checks."
  (let* ((directory (tests--temporary-directory))
         (file      (merge-pathnames "text.txt" directory))
         (link      (merge-pathnames "link.txt" directory))
         (binary    (merge-pathnames "binary.bin" directory)))
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

(ls-compat::-> tests--stable-file-octets () t)
(defun tests--stable-file-octets ()
  "Check complete no-following-links reads and stable identity validation."
  (let* ((directory (tests--temporary-directory))
         (file      (merge-pathnames "stable.bin" directory)))
    (unwind-protect
         (progn
           (tests--write-octets file '(0 1 2 253 254 255))
           (tests--check
            (equalp #(0 1 2 253 254 255)
                    (ls-compat.files:read-stable-file-octets file
                      :validation-function (lambda ()
                                             (tests--check t "Stable read validation did not run."))))
            "Stable read changed or truncated the octets.")
           (tests--check
            (handler-case
                (progn (ls-compat.files:read-stable-file-octets file :maximum-octets 2) nil)
              (ls-compat.files:file-too-large () t))
            "Stable read ignored its maximum octet budget.")
           (tests--check
            (handler-case
                (progn
                  (ls-compat.files:read-stable-file-octets
                   file
                   :validation-function
                   (lambda ()
                     (publish-file file (make-array 6 :element-type '(unsigned-byte 8) :initial-element 9))))
                  nil)
              (ls-compat.files:file-changed () t))
            "Stable read accepted replacement of the opened pathname."))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

(ls-compat::-> tests--confined-path-inspection () t)
(defun tests--confined-path-inspection ()
  "Check confined traversal, first failure reporting, and literal component handling."
  (let* ((directory (tests--temporary-directory))
         (root      (merge-pathnames "root/" directory))
         (file      (merge-pathnames "nested/plain.txt" root))
         (literal   (merge-pathnames (uiop:parse-native-namestring "nested/a\\b*.txt") root))
         (link      (merge-pathnames "link" root))
         (fifo      (merge-pathnames "fifo" root)))
    (unwind-protect
         (progn
           (publish-file file "regular")
           (publish-file literal "literal")
           (uiop:run-program (list "ln" "-s" "nested" (uiop:native-namestring link)))
           (uiop:run-program (list "mkfifo" (uiop:native-namestring fifo)))
           (loop for (target status first-failure) in
                 '(("nested/plain.txt" :file nil)
                   ("nested/" :directory nil)
                   ("nested/missing" :missing "nested/missing")
                   ("absent/missing" :missing "absent")
                   ("nested/plain.txt/below" :not-directory "nested/plain.txt")
                   ("link/plain.txt" :symbolic-link "link")
                   ("fifo" :special "fifo")
                   ("../outside" :failed "../outside"))
                 for pathname = (merge-pathnames (uiop:parse-native-namestring target) root)
                 for inspection = (ls-compat.files:confined-path-inspect pathname root)
                 do (tests--check
                     (and (eq status (ls-compat.files:confined-path-inspection-status inspection))
                          (equal (and first-failure
                                      (merge-pathnames (uiop:parse-native-namestring first-failure) root))
                                 (ls-compat.files:confined-path-inspection-failure-pathname inspection)))
                     (format nil "Wrong confined observation for ~A: ~S" target inspection)))
           (let ((inspection (ls-compat.files:confined-path-inspect literal root)))
             (tests--check
              (and (eq ':file (ls-compat.files:confined-path-inspection-status inspection))
                   (equal '("nested" "a\\b*.txt")
                          (ls-compat.files:confined-path-inspection-components inspection)))
              "Native backslash or wildcard characters did not survive confined inspection."))
           (let ((linked-root (merge-pathnames "link/" root)))
             (tests--check
              (eq ':symbolic-link
                  (ls-compat.files:confined-path-inspection-status
                   (ls-compat.files:confined-path-inspect
                    (merge-pathnames "plain.txt" linked-root) linked-root :root-policy ':root-inclusive)))
              "Root-inclusive inspection followed the root link.")
             (tests--check
              (eq ':file
                  (ls-compat.files:confined-path-inspection-status
                   (ls-compat.files:confined-path-inspect
                    (merge-pathnames "plain.txt" linked-root) linked-root :root-policy ':below-root)))
              "Below-root inspection rechecked the caller's trusted root.")))
      (ignore-errors (delete-file link))
      (ignore-errors (delete-file fifo))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))


;;;; -- TCP --

(ls-compat::-> tests--tcp-lifecycle () t)
(defun tests--tcp-lifecycle ()
  "Check listener creation, client connection, acceptance, and streams."
  (let ((listener (tcp-listen "127.0.0.1" 0))
        (client   nil)
        (server   nil))
    (unwind-protect
         (let ((port (tcp-local-port listener)))
           (setf client (tcp-connect "127.0.0.1" port)
                 server (tcp-accept listener))
           (tests--check (streamp (tcp-stream client))
                         "TCP client has no stream.")
           (tests--check (streamp (tcp-stream server))
                         "Accepted TCP connection has no stream.")
           (let ((payload  #(0 1 2 253 254 255))
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

(ls-compat::-> run-tests () boolean)
(defun run-tests ()
  "Run ls-compat regression tests and signal an error on any failure."
  (let ((*test-failures* nil))
    (dolist (test '(tests--utf8-round-trip
                    tests--utf8-failures
                    tests--utf8-measurements
                    tests--utf8-measurement-failures
                    tests--internal-real-time-seconds
                    tests--finite-floats
                    tests--timeout-signals-condition))
      (funcall test))
    (when (tests--posix-supported-p)
      (dolist (test '(tests--current-process-group
                      tests--exclusive-directory-and-mode
                      tests--descendant-processes
                      tests--file-information-and-reasons
                      tests--regular-file-ranges
                      tests--directory-names
                      tests--canonical-pathnames
                      tests--signal-process
                      tests--confined-path-inspection
                      tests--native-files
                      #+(and sbcl win32) tests--native-read-only-failures
                      tests--read-file-text))
        (funcall test)))
    (dolist (test '(tests--publish-file-replaces
                    tests--publish-file-cleans-up-failures
                    tests--publish-pathname-hooks
                    tests--publish-file-refuses-existing
                    tests--publish-file-reports-unavailable-temporary))
      (funcall test))
    (tests--tcp-lifecycle)
    (when *test-failures*
      (error "ls-compat test failures:~%~{~A~%~}"
             (nreverse *test-failures*)))
    t))
