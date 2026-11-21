(in-package #:ls-compat.files)

;;;; -- Types --

(deftype pathname-designator ()
  "A pathname or namestring accepted by ls-compat file operations."
  '(or pathname string))

(deftype file-content ()
  "Content PUBLISH-FILE writes: text, octets, or a function of the output stream."
  '(or string ls-compat::octet-vector function))

(deftype publication-policy ()
  "What publication does when the target already exists."
  '(member :replace :error))

(deftype confined-path-root-policy ()
  "Whether CONFINED-PATH-INSPECT includes ROOT in its traversal."
  '(member :below-root :root-inclusive))

(defstruct (confined-path-inspection
            (:constructor make-confined-path-inspection
                (status pathname components failure-pathname message)))
  "The result of inspecting a literal path without following links."
  (status :failed :type keyword)
  (pathname nil :type pathname)
  (components nil :type list)
  (failure-pathname nil :type (or null pathname))
  (message nil :type (or null string)))


;;;; -- Confined path inspection --

(ls-compat::-> confined-path-inspect
  (pathname-designator pathname-designator &key (:root-policy confined-path-root-policy))
  confined-path-inspection)
(defun confined-path-inspect (pathname root &key (root-policy :below-root))
  "Inspect an absolute literal PATHNAME at or below absolute directory ROOT.

The status is :DIRECTORY or :FILE on success, :MISSING on absence, :NOT-DIRECTORY
when traversal reaches a non-directory, :SYMBOLIC-LINK or :SPECIAL when that
kind of object is encountered, or :FAILED for an invalid or uninspectable path.
The result names the first failing prefix. ROOT-POLICY :ROOT-INCLUSIVE checks
ROOT itself; :BELOW-ROOT assumes it is a trusted directory. Both assume the
ancestors of ROOT are trusted. This is an observation, not an open, an
authorization decision, or a guarantee against concurrent path replacement."
  (check-type root-policy confined-path-root-policy)
  (let* ((pathname (if (stringp pathname) (uiop:parse-native-namestring pathname) pathname))
         (root     (uiop:ensure-directory-pathname
                    (if (stringp root) (uiop:parse-native-namestring root) root)))
         (relative (and (uiop:absolute-pathname-p pathname)
                        (uiop:absolute-pathname-p root)
                        (uiop:enough-pathname pathname root))))
    (unless (and relative (not (uiop:absolute-pathname-p relative))
                 (not (wild-pathname-p relative)))
      (return-from confined-path-inspect
        (make-confined-path-inspection
         :failed pathname nil pathname "Path is not literal and below the trusted root.")))
    (let* ((file       (uiop:native-namestring
                        (make-pathname :host nil :device nil :directory nil :defaults relative)))
           (components (append (rest (pathname-directory relative))
                               (unless (string= file "") (list file))))
           (prefix     (uiop:native-namestring root))
           (windows-p  (uiop:os-windows-p))
           (separator  (if windows-p "\\" "/"))
           (last-kind  :directory))
      (unless (and (not (wild-pathname-p root))
                   (every (lambda (component)
                            (and (stringp component) (plusp (length component))
                                 (not (member component '("." "..") :test #'string=))
                                 (not (find #\/ component)) (not (find #\Null component))
                                 (or (not windows-p) (not (find #\\ component)))))
                          (append (rest (pathname-directory root)) components)))
        (return-from confined-path-inspect
          (make-confined-path-inspection
           :failed pathname nil pathname "Path has a nonliteral component.")))
      (labels ((fail (status path message)
                 (return-from confined-path-inspect
                   (make-confined-path-inspection status pathname components path message)))

               (inspect-prefix (path needs-directory-p)
                 (handler-case
                     (setf last-kind
                           (ls-compat.posix:file-information-kind
                            (ls-compat.posix:file-information path :follow-links-p nil)))
                   (ls-compat.posix:file-operation-failed (condition)
                     (let ((reason (ls-compat.posix:file-operation-failed-reason condition)))
                       (fail (if (member reason '(:missing :not-directory :symbolic-link))
                                 reason
                                 :failed)
                             path (ls-compat.posix:file-operation-failed-message condition)))))
                 (cond
                   ((eq last-kind :symbolic-link)
                    (fail :symbolic-link path "A symbolic link is not permitted."))
                   ((not (member last-kind '(:file :directory)))
                    (fail :special path "A special file is not permitted."))
                   ((and needs-directory-p (not (eq last-kind :directory)))
                    (fail :not-directory path "A path component is not a directory.")))))
        (when (eq root-policy :root-inclusive)
          ;; A trailing separator can make lstat follow a directory symlink.
          (inspect-prefix (if (rest (pathname-directory root))
                              (uiop:parse-native-namestring (subseq prefix 0 (1- (length prefix))))
                              root)
                          t))
        (loop for remaining on components
              do (setf prefix (concatenate 'string prefix (first remaining)))
                 (inspect-prefix (uiop:parse-native-namestring prefix)
                                 (or (rest remaining) (uiop:directory-pathname-p pathname)))
                 (setf prefix (concatenate 'string prefix separator))))
      (make-confined-path-inspection last-kind pathname components nil nil))))


;;;; -- Conditions --

(define-condition file-too-large (file-error)
  ((size
    :initarg       :size
    :reader        file-too-large-size
    :type          (integer 0)
    :documentation "The file's size in octets.")
   (limit
    :initarg       :limit
    :reader        file-too-large-limit
    :type          (integer 0)
    :documentation "The largest size the reader accepted."))
  (:report (lambda (condition stream)
             (format stream "~A is ~:D octets, more than the ~:D a read accepts."
                     (file-error-pathname condition)
                     (file-too-large-size condition)
                     (file-too-large-limit condition))))
  (:documentation "READ-FILE-TEXT found a file larger than its limit."))

(define-condition file-changed (file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~A changed while it was being read."
                     (file-error-pathname condition))))
  (:documentation "READ-FILE-TEXT saw the path or the opened file change during the read."))

(define-condition file-not-utf-8 (file-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~A is not valid UTF-8 text."
                     (file-error-pathname condition))))
  (:documentation "READ-FILE-TEXT read octets that do not decode as UTF-8."))

(define-condition temporary-file-unavailable (file-error)
  ((attempts
    :initarg       :attempts
    :reader        temporary-file-unavailable-attempts
    :type          (integer 0)
    :documentation "How many temporary names publication tried."))
  (:report (lambda (condition stream)
             (format stream "No temporary sibling of ~A could be created in ~D attempts."
                     (file-error-pathname condition)
                     (temporary-file-unavailable-attempts condition))))
  (:documentation "Publication could not create a temporary sibling of its target."))


;;;; -- Bounded reading --

(ls-compat::-> read-stable-file-octets
  (pathname-designator
   &key (:maximum-octets (integer 0))
        (:validation-function (or null function)))
  (vector (unsigned-byte 8)))
(defun read-stable-file-octets (pathname &key (maximum-octets most-positive-fixnum)
                                                   validation-function)
  "Read PATHNAME as one stable regular-file snapshot of raw octets.

The file is opened without following links.  MAXIMUM-OCTETS is checked from
its opened size, VALIDATION-FUNCTION runs after opening and before reading,
and the descriptor must produce the complete announced size followed by EOF.
A final no-following-links status check must find the same object and size;
FILE-TOO-LARGE or FILE-CHANGED signals failures."
  (let ((path (pathname pathname))
        (stream nil))
    (unwind-protect
         (multiple-value-bind (opened information)
             (ls-compat.posix:open-regular-file path :follow-links-p nil)
           (setf stream opened)
           (let ((size (ls-compat.posix:file-information-size information)))
             (when (> size maximum-octets)
               (error 'file-too-large :pathname path :size size :limit maximum-octets))
             (when validation-function
               (funcall validation-function))
             (let ((octets (make-array size :element-type '(unsigned-byte 8))))
               (unless (= (read-sequence octets stream) size)
                 (error 'file-changed :pathname path))
               (unless (eq (read-byte stream nil :eof) :eof)
                 (error 'file-changed :pathname path))
               (let ((current (handler-case
                                  (ls-compat.posix:file-information
                                   path :follow-links-p nil)
                                (ls-compat.posix:file-operation-failed () nil))))
                 (unless (and current
                              (ls-compat.posix:file-information-same-object-p
                               information current)
                              (= size (ls-compat.posix:file-information-size current)))
                   (error 'file-changed :pathname path)))
               octets)))
      (when stream
        (close stream)))))

(ls-compat::-> read-file-text
  (pathname-designator
   &key (:maximum-octets (integer 0))
        (:follow-links-p boolean)
        (:validation-function (or null function)))
  string)
(defun read-file-text (pathname &key (maximum-octets most-positive-fixnum)
                                     follow-links-p validation-function)
  "Return regular file PATHNAME's complete content as UTF-8 text, read exactly once.

The file is opened without following a symbolic link unless FOLLOW-LINKS-P,
and its size must not exceed MAXIMUM-OCTETS (FILE-TOO-LARGE). After opening,
VALIDATION-FUNCTION runs when given; PATHNAME must then still name the opened
object, every octet must arrive, and the object's size and times must not
change during the read (FILE-CHANGED). Octets that are not UTF-8 signal
FILE-NOT-UTF-8. Opening failures propagate from OPEN-REGULAR-FILE."
  (let ((path   (pathname pathname))
        (stream nil))
    (unwind-protect
         (multiple-value-bind (opened information)
             (ls-compat.posix:open-regular-file path :follow-links-p follow-links-p)
           (setf stream opened)
           (let ((size (ls-compat.posix:file-information-size information)))
             (when (> size maximum-octets)
               (error 'file-too-large :pathname path :size size :limit maximum-octets))
             (when validation-function
               (funcall validation-function))
             (let ((current (handler-case
                                (ls-compat.posix:file-information path :follow-links-p t)
                              (ls-compat.posix:file-operation-failed ()
                                nil))))
               (unless (and current
                            (ls-compat.posix:file-information-same-object-p
                             information current))
                 (error 'file-changed :pathname path)))
             (let ((octets (make-array size :element-type '(unsigned-byte 8))))
               (unless (= (read-sequence octets stream) size)
                 (error 'file-changed :pathname path))
               (unless (ls-compat.posix:file-information-unchanged-p
                        information
                        (ls-compat.posix:stream-file-information stream))
                 (error 'file-changed :pathname path))
               (handler-case
                   (ls-compat:utf8-octets-to-string octets)
                 (error ()
                   (error 'file-not-utf-8 :pathname path))))))
      (when stream
        (close stream)))))


;;;; -- Temporary siblings --

(defvar *files--random-state* (make-random-state t)
  "The random state naming temporary siblings so parallel writers never collide.")

(defparameter *files--temporary-attempts* 32
  "How many fresh temporary names are tried before giving up.")

(ls-compat::-> files--temporary-sibling (pathname) pathname)
(defun files--temporary-sibling (target)
  "Return a hidden temporary pathname beside TARGET with a fresh random suffix."
  (make-pathname
   :name     (format nil ".~A.~36R"
                     (or (pathname-name target) "file")
                     (random (expt 2 64) *files--random-state*))
   :type     "tmp"
   :version  nil
   :defaults target))

(ls-compat::-> files--claim-temporary (pathname) pathname)
(defun files--claim-temporary (target)
  "Create and return an empty temporary sibling of TARGET that did not exist before."
  (loop for candidate = (files--temporary-sibling target)
        repeat *files--temporary-attempts*
        do (handler-case
               (progn
                 (with-open-file (stream candidate
                                         :direction         :output
                                         :if-exists         :error
                                         :if-does-not-exist :create)
                   (declare (ignorable stream)))
                 (return candidate))
             (file-error ()
               nil))
        finally (error 'temporary-file-unavailable
                       :pathname target
                       :attempts *files--temporary-attempts*)))


;;;; -- Publication --

(ls-compat::-> files--publish (pathname pathname publication-policy) pathname)
(defun files--publish (temporary target if-exists)
  "Publish TEMPORARY at TARGET according to IF-EXISTS."
  (ecase if-exists
    (:replace
     (uiop:rename-file-overwriting-target temporary target))
    (:error
     (ls-compat.posix:link-file temporary target)))
  target)

(ls-compat::-> publish-pathname
  (pathname-designator function
   &key (:if-exists publication-policy)
        (:prepare-function (or null function))
        (:publish-function (or null function)))
  pathname)
(defun publish-pathname (target produce-function
                         &key (if-exists :replace) prepare-function publish-function)
  "Publish a file at TARGET that PRODUCE-FUNCTION writes to a temporary sibling.

PRODUCE-FUNCTION receives the pathname of an empty temporary file in TARGET's
directory and must leave the complete content there. PREPARE-FUNCTION, when
given, then receives that pathname to set permissions or verify the content.
Publication is atomic: with IF-EXISTS :REPLACE the temporary renames over any
existing TARGET, and with :ERROR the temporary is hard-linked to TARGET, so an
occupied TARGET signals LS-COMPAT.POSIX:LINK-TARGET-EXISTS and stays untouched.
PUBLISH-FUNCTION replaces that final step with a caller-supplied function of
the temporary and target pathnames. The temporary never outlives the call,
whether publication succeeded or anything failed. Signals
TEMPORARY-FILE-UNAVAILABLE when no temporary sibling can be created. Returns
TARGET as a pathname."
  (check-type if-exists publication-policy)
  (let ((target    (pathname target))
        (temporary nil))
    (ensure-directories-exist target)
    (unwind-protect
         (progn
           (setf temporary (files--claim-temporary target))
           (funcall produce-function temporary)
           (when prepare-function
             (funcall prepare-function temporary))
           (if publish-function
               (funcall publish-function temporary target)
               (files--publish temporary target if-exists))
           target)
      (when (and temporary (probe-file temporary))
        (handler-case (delete-file temporary)
          (file-error ()
            nil))))))

(ls-compat::-> publish-file
  (pathname-designator file-content
   &key (:if-exists publication-policy)
        (:external-format t)
        (:prepare-function (or null function))
        (:publish-function (or null function)))
  pathname)
(defun publish-file (target content
                     &key (if-exists :replace) (external-format :utf-8)
                          prepare-function publish-function)
  "Atomically publish CONTENT at TARGET through a temporary sibling.

CONTENT is a string written with EXTERNAL-FORMAT, an octet vector written as
is, or a function receiving a character output stream in EXTERNAL-FORMAT. The
stream is flushed before publication. IF-EXISTS, PREPARE-FUNCTION and
PUBLISH-FUNCTION have their PUBLISH-PATHNAME meanings. Returns TARGET."
  (check-type content file-content)
  (publish-pathname
   target
   (lambda (temporary)
     (etypecase content
       (ls-compat::octet-vector
        (with-open-file (stream temporary
                                :direction    :output
                                :if-exists    :supersede
                                :element-type '(unsigned-byte 8))
          (write-sequence content stream)
          (finish-output stream)))
       ((or string function)
        (with-open-file (stream temporary
                                :direction       :output
                                :if-exists       :supersede
                                :external-format external-format)
          (if (stringp content)
              (write-string content stream)
              (funcall content stream))
          (finish-output stream)))))
   :if-exists        if-exists
   :prepare-function prepare-function
   :publish-function publish-function))
