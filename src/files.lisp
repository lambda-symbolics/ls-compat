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


;;;; -- Conditions --

(define-condition file-too-large (file-error)
  ((size
    :initarg :size
    :reader file-too-large-size
    :type (integer 0)
    :documentation "The file's size in octets.")
   (limit
    :initarg :limit
    :reader file-too-large-limit
    :type (integer 0)
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


;;;; -- Bounded reading --

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
  (let ((path (pathname pathname))
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
   :name (format nil ".~A.~36R"
                 (or (pathname-name target) "file")
                 (random (expt 2 64) *files--random-state*))
   :type "tmp"
   :version nil
   :defaults target))

(ls-compat::-> files--claim-temporary (pathname) pathname)
(defun files--claim-temporary (target)
  "Create and return an empty temporary sibling of TARGET that did not exist before."
  (loop repeat *files--temporary-attempts*
        for candidate = (files--temporary-sibling target)
        do (handler-case
               (progn
                 (with-open-file (stream candidate
                                         :direction :output
                                         :if-exists :error
                                         :if-does-not-exist :create)
                   (declare (ignorable stream)))
                 (return candidate))
             (file-error ()
               nil))
        finally (error 'file-error :pathname (files--temporary-sibling target))))


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
whether publication succeeded or anything failed. Returns TARGET as a pathname."
  (check-type if-exists publication-policy)
  (let ((target (pathname target))
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
        (ignore-errors (delete-file temporary))))))

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
                                :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
          (write-sequence content stream)
          (finish-output stream)))
       ((or string function)
        (with-open-file (stream temporary
                                :direction :output
                                :if-exists :supersede
                                :external-format external-format)
          (if (stringp content)
              (write-string content stream)
              (funcall content stream))
          (finish-output stream)))))
   :if-exists if-exists
   :prepare-function prepare-function
   :publish-function publish-function))
