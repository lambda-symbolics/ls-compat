(in-package #:ls-compat.posix)

;;;; -- Types --

(deftype pathname-designator ()
  "A pathname or namestring accepted by ls-compat POSIX operations."
  '(or pathname string))


;;;; -- Conditions --

(define-condition link-target-exists (file-error)
  ()
  (:documentation "LINK-FILE found its target name already occupied."))

(define-condition link-failed (file-error)
  ((message
    :initarg :message
    :reader link-failed-message
    :type string
    :documentation "The operating system's explanation of the failure."))
  (:report
   (lambda (condition stream)
     (format stream "Could not link ~A: ~A"
             (file-error-pathname condition)
             (link-failed-message condition))))
  (:documentation "LINK-FILE failed for a reason other than an occupied target."))

(define-condition mode-failed (file-error)
  ((message
    :initarg :message
    :reader mode-failed-message
    :type string
    :documentation "The operating system's explanation of the failure."))
  (:report
   (lambda (condition stream)
     (format stream "Could not read or set the mode of ~A: ~A"
             (file-error-pathname condition)
             (mode-failed-message condition))))
  (:documentation "FILE-MODE could not read or apply permissions on a Windows host."))

(deftype failure-reason ()
  "The portable discriminator a failed file operation carries."
  '(member :missing :exists :not-directory :symbolic-link :failed))

(define-condition file-operation-failed (file-error)
  ((operation
    :initarg :operation
    :reader file-operation-failed-operation
    :type keyword
    :documentation "The operation that failed: :INSPECT, :OPEN, :LIST, or :RESOLVE.")
   (message
    :initarg :message
    :reader file-operation-failed-message
    :type string
    :documentation "The operating system's explanation of the failure.")
   (reason
    :initarg :reason
    :initform ':failed
    :reader file-operation-failed-reason
    :type failure-reason
    :documentation
    "Why the operation failed: :MISSING when nothing exists there, :EXISTS,
:NOT-DIRECTORY for a path through a non-directory, :SYMBOLIC-LINK when a link
was refused, and :FAILED otherwise.")
   (code
    :initarg :code
    :initform nil
    :reader file-operation-failed-code
    :type (or null integer)
    :documentation "The host error number, an errno or a Windows error code, when known."))
  (:report
   (lambda (condition stream)
     (format stream "Could not ~(~A~) ~A: ~A"
             (file-operation-failed-operation condition)
             (file-error-pathname condition)
             (file-operation-failed-message condition))))
  (:documentation "A file inspection, opening, or listing failed."))

(define-condition not-regular-file (file-error)
  ((kind
    :initarg :kind
    :reader not-regular-file-kind
    :type keyword
    :documentation "The kind of object found instead of a regular file."))
  (:report
   (lambda (condition stream)
     (format stream "~A is not a regular file but ~(~A~)."
             (file-error-pathname condition)
             (not-regular-file-kind condition))))
  (:documentation "OPEN-REGULAR-FILE found something other than a regular file."))


;;;; -- Implementation boundary --

(ls-compat::-> posix--native-namestring (pathname-designator) string)
(defun posix--native-namestring (pathname)
  "Return PATHNAME as a native namestring for the POSIX backend."
  (uiop:native-namestring (pathname pathname)))

(defun posix--unsupported (operation)
  "Signal that OPERATION has no backend on this implementation or host."
  (error 'ls-compat:unsupported-operation :name operation))


;;;; -- Processes --

(ls-compat::-> current-process-id () (integer 1 *))
(defun current-process-id ()
  "Return the current process ID.

The POSIX system currently supports SBCL. Other implementations signal
LS-COMPAT:UNSUPPORTED-OPERATION."
  #+sbcl
  (sb-posix:getpid)
  #-sbcl
  (posix--unsupported 'current-process-id))

(ls-compat::-> process-groups-supported-p () boolean)
(defun process-groups-supported-p ()
  "Return whether this host has POSIX process groups.

Windows has no process groups, so PROCESS-GROUP-ID, PROCESS-GROUP-ALIVE-P, and
SIGNAL-PROCESS-GROUP signal LS-COMPAT:UNSUPPORTED-OPERATION there."
  #+(and sbcl (not win32)) t
  #-(and sbcl (not win32)) nil)

#+(and sbcl (not win32))
(defun posix--process-target-alive-p (target)
  "Return whether POSIX kill target TARGET exists or cannot be signaled."
  (handler-case
      (progn
        (sb-posix:kill target 0)
        t)
    (sb-posix:syscall-error (condition)
      (= (sb-posix:syscall-errno condition) sb-posix:eperm))))

(ls-compat::-> process-state ((integer 1 *)) (member :alive :dead :unknown))
(defun process-state (process-id)
  "Return :ALIVE, :DEAD, or :UNKNOWN for PROCESS-ID.

A process that exists but may not be inspected by the current user is :ALIVE.
A process identifier that names nothing is :DEAD. Any other failure to inspect
the process is :UNKNOWN, so callers can keep treating it as owned. PID reuse
means no answer proves process identity. The POSIX system currently supports
SBCL."
  (declare (ignorable process-id))
  #+(and sbcl (not win32))
  (handler-case
      (progn
        (sb-posix:kill process-id 0)
        ':alive)
    (sb-posix:syscall-error (condition)
      (let ((errno (sb-posix:syscall-errno condition)))
        (cond
          ((= errno sb-posix:esrch) ':dead)
          ((= errno sb-posix:eperm) ':alive)
          (t ':unknown))))
    (error ()
      ':unknown))
  #+(and sbcl win32)
  (win32--process-state process-id)
  #-sbcl
  (posix--unsupported 'process-state))

(ls-compat::-> process-alive-p ((integer 1 *)) boolean)
(defun process-alive-p (process-id)
  "Return whether PROCESS-ID currently exists or cannot be signaled.

On POSIX this is a kill-with-signal-zero snapshot, where an EPERM response
means that a process appears to exist but cannot be signaled by the current
user. Windows opens the process and reads its exit state instead. PID reuse
means the predicate cannot prove process identity. The POSIX system currently
supports SBCL."
  (declare (ignorable process-id))
  #+(and sbcl (not win32))
  (posix--process-target-alive-p process-id)
  #+(and sbcl win32)
  (eq (win32--process-state process-id) ':alive)
  #-sbcl
  (posix--unsupported 'process-alive-p))

(ls-compat::-> process-group-id ((integer 1 *)) (integer 1 *))
(defun process-group-id (process-id)
  "Return the POSIX process group identifier of PROCESS-ID.

The POSIX system currently supports SBCL on hosts with process groups."
  (declare (ignorable process-id))
  #+(and sbcl (not win32))
  (sb-posix:getpgid process-id)
  #-(and sbcl (not win32))
  (posix--unsupported 'process-group-id))

(ls-compat::-> process-group-alive-p ((integer 1 *)) boolean)
(defun process-group-alive-p (process-group-id)
  "Return whether PROCESS-GROUP-ID currently has members.

This is a POSIX kill-with-signal-zero snapshot. An EPERM response means that a
process group appears to exist but cannot be signaled by the current user. The
POSIX system currently supports SBCL on hosts with process groups."
  (declare (ignorable process-group-id))
  #+(and sbcl (not win32))
  (posix--process-target-alive-p (- process-group-id))
  #-(and sbcl (not win32))
  (posix--unsupported 'process-group-alive-p))

(ls-compat::-> signal-process-group
  ((integer 1 *) (member :terminate :kill))
  (integer 1 *))
(defun signal-process-group (process-group-id signal)
  "Send SIGNAL to every member of PROCESS-GROUP-ID and return its identifier.

SIGNAL is either :TERMINATE or :KILL. POSIX errors, including a missing process
group, propagate as backend conditions. The POSIX system currently supports
SBCL on hosts with process groups."
  (declare (ignorable process-group-id signal))
  #+(and sbcl (not win32))
  (progn
    (sb-posix:kill
     (- process-group-id)
     (ecase signal
       (:terminate sb-posix:sigterm)
       (:kill sb-posix:sigkill)))
    process-group-id)
  #-(and sbcl (not win32))
  (posix--unsupported 'signal-process-group))

(ls-compat::-> signal-process ((integer 1 *) (member :terminate :kill)) (integer 1 *))
(defun signal-process (process-id signal)
  "Send SIGNAL, :TERMINATE or :KILL, to PROCESS-ID and return its identifier.

POSIX errors, including a missing process, propagate as backend conditions,
as SIGNAL-PROCESS-GROUP's do. Windows has no signals, so it signals
LS-COMPAT:UNSUPPORTED-OPERATION there; terminate a Windows process through its
process object instead."
  (declare (ignorable process-id signal))
  #+(and sbcl (not win32))
  (progn
    (sb-posix:kill process-id
                   (ecase signal
                     (:terminate sb-posix:sigterm)
                     (:kill sb-posix:sigkill)))
    process-id)
  #-(and sbcl (not win32))
  (posix--unsupported 'signal-process))

(ls-compat::-> descendant-process-ids ((integer 1 *)) list)
(defun descendant-process-ids (process-id)
  "Return the live descendants of PROCESS-ID, deepest first, as a best-effort snapshot.

The snapshot comes from the host's ps listing of every process with its parent,
so processes that fork between the listing and the caller's use are missed, and
a host without ps yields NIL. Each identifier appears once."
  (let ((pairs
          (handler-case
              (let ((output
                      (uiop:run-program '("ps" "-ax" "-o" "pid=" "-o" "ppid=")
                                        :output ':string
                                        :ignore-error-status t)))
                (loop for line in (uiop:split-string output :separator '(#\Newline))
                      for fields = (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
                                           :test #'string=)
                      when (and (= (length fields) 2)
                                (every #'digit-char-p (first fields))
                                (every #'digit-char-p (second fields)))
                        collect (cons (parse-integer (first fields))
                                      (parse-integer (second fields)))))
            (error ()
              nil))))
    (labels ((descendants (parent)
               "Return PARENT's recursive descendants with children before parents."
               (loop for (pid . parent-pid) in pairs
                     when (and (= parent-pid parent) (/= pid parent))
                       append (append (descendants pid) (list pid)))))
      (remove-duplicates (descendants process-id) :test #'=))))


;;;; -- Filesystem modes --

(ls-compat::-> make-directory-exclusively
  (pathname-designator &key (:mode (integer 0 #o777)))
  pathname)
(defun make-directory-exclusively (pathname &key (mode #o700))
  "Create PATHNAME atomically with MODE and return its pathname.

Signals the backend's POSIX condition when PATHNAME already exists or creation
fails. The operating system umask can further restrict MODE, and Windows
ignores it. The POSIX system currently supports SBCL."
  (declare (ignorable pathname))
  (check-type mode (integer 0 #o777))
  #+(and sbcl win32)
  (progn
    (sb-posix:mkdir (posix--native-namestring pathname) mode)
    (win32--set-file-mode pathname mode)
    (pathname pathname))
  #+(and sbcl (not win32))
  (progn
    (sb-posix:mkdir (posix--native-namestring pathname) mode)
    (pathname pathname))
  #-sbcl
  (posix--unsupported 'make-directory-exclusively))

(ls-compat::-> file-mode (pathname-designator) (integer 0 *))
(defun file-mode (pathname)
  "Return PATHNAME's raw POSIX mode bits.

Windows has no mode bits, so it reports the permission bits its access control
list expresses: #o600 or #o700 for an object private to the owner, #o644 or
#o755 otherwise, with the write bits cleared when the owner may not write.
The POSIX system currently supports SBCL."
  (declare (ignorable pathname))
  #+(and sbcl win32)
  (win32--file-mode pathname)
  #+(and sbcl (not win32))
  (sb-posix:stat-mode
   (sb-posix:stat (posix--native-namestring pathname)))
  #-sbcl
  (posix--unsupported 'file-mode))

(ls-compat::-> (setf file-mode) ((integer 0 #o777) pathname-designator) (integer 0 #o777))
(defun (setf file-mode) (mode pathname)
  "Set PATHNAME's permission MODE and return MODE.

Windows expresses MODE through the access control list: a mode without group
or other bits, or without the owner write bit, grants the owner and SYSTEM
alone, withholding write access when the owner write bit is clear, while any
other mode inherits the parent's list. The read-only attribute is never set,
so the file stays deletable and replaceable. Signals MODE-FAILED when Windows
refuses. The POSIX system currently supports SBCL."
  (declare (ignorable pathname))
  (check-type mode (integer 0 #o777))
  #+(and sbcl win32)
  (win32--set-file-mode pathname mode)
  #+(and sbcl (not win32))
  (progn
    (sb-posix:chmod (posix--native-namestring pathname) mode)
    mode)
  #-sbcl
  (posix--unsupported 'file-mode))


;;;; -- Hard links --

(ls-compat::-> link-file (pathname-designator pathname-designator) pathname)
(defun link-file (source target)
  "Atomically give SOURCE's content the additional name TARGET.

Signals LINK-TARGET-EXISTS when TARGET is already occupied, leaving it
untouched, and LINK-FAILED for any other failure. Windows requires an NTFS
volume for hard links. The POSIX system currently supports SBCL."
  (declare (ignorable source target))
  #+(and sbcl (not win32))
  (handler-case
      (sb-posix:link (posix--native-namestring source)
                     (posix--native-namestring target))
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:eexist)
          (error 'link-target-exists :pathname (pathname target))
          (error 'link-failed
                 :pathname (pathname target)
                 :message (princ-to-string condition)))))
  #+(and sbcl win32)
  (when (zerop (win32--create-hard-link (posix--native-namestring target)
                                        (posix--native-namestring source)
                                        nil))
    (let ((code (win32--get-last-error)))
      (if (or (= code *win32-error-file-exists*)
              (= code *win32-error-already-exists*))
          (error 'link-target-exists :pathname (pathname target))
          (error 'link-failed
                 :pathname (pathname target)
                 :message (format nil "Windows error ~D" code)))))
  #-sbcl
  (posix--unsupported 'link-file)
  (pathname target))


;;;; -- File information --

(deftype file-kind ()
  "The kinds of filesystem object FILE-INFORMATION distinguishes."
  '(member :file :directory :symbolic-link :socket :other))

(defstruct (file-information
            (:constructor make-file-information
                (kind identity size modification-time change-time
                 &optional owned-p private-p read-only-p)))
  "One observation of a filesystem object.

IDENTITY names the object on its volume and compares with EQUAL: a device and
inode pair on POSIX, a volume serial number and file index pair on Windows.
The times are in host units and compare only for equality. OWNED-P reports
that the current user owns the object, PRIVATE-P that it is owned and no other
user may access it, and READ-ONLY-P that the owner may not write its content.
Windows judges ownership and privacy from the access control list and reports
all three as false when the security descriptor cannot be read."
  (kind ':other :type file-kind :read-only t)
  (identity nil :read-only t)
  (size 0 :type (integer 0) :read-only t)
  (modification-time 0 :type integer :read-only t)
  (change-time 0 :type integer :read-only t)
  (owned-p nil :type boolean :read-only t)
  (private-p nil :type boolean :read-only t)
  (read-only-p nil :type boolean :read-only t))

(ls-compat::-> file-information-same-object-p (file-information file-information) boolean)
(defun file-information-same-object-p (left right)
  "Return whether observations LEFT and RIGHT describe the same filesystem object."
  (and (equal (file-information-identity left) (file-information-identity right))
       t))

(ls-compat::-> file-information-unchanged-p (file-information file-information) boolean)
(defun file-information-unchanged-p (before after)
  "Return whether one object kept its size and times between BEFORE and AFTER."
  (and (file-information-same-object-p before after)
       (= (file-information-size before) (file-information-size after))
       (= (file-information-modification-time before)
          (file-information-modification-time after))
       (= (file-information-change-time before)
          (file-information-change-time after))))

#+(and sbcl netbsd)
(defparameter *posix--netbsd-eftype* 79
  "NetBSD EFTYPE from sys/errno.h, absent from SB-POSIX's exported errno set.")

#+(and sbcl (not win32))
(defun posix--failure-reason (errno)
  "Return the portable FAILURE-REASON for ERRNO."
  (cond
    ((= errno sb-posix:enoent) ':missing)
    ((= errno sb-posix:eexist) ':exists)
    ((= errno sb-posix:enotdir) ':not-directory)
    ((= errno sb-posix:eloop) ':symbolic-link)
    (t ':failed)))

(ls-compat::-> posix--operation-failure
  (keyword pathname-designator t &key (:reason (or null failure-reason))) nil)
(defun posix--operation-failure (operation pathname cause &key reason)
  "Signal FILE-OPERATION-FAILED for OPERATION on PATHNAME explained by CAUSE.

A POSIX system call failure contributes its errno. REASON, when supplied,
classifies an operation-specific error instead of the generic errno mapping."
  (let ((errno #+(and sbcl (not win32))
               (and (typep cause 'sb-posix:syscall-error)
                    (sb-posix:syscall-errno cause))
               #-(and sbcl (not win32))
               nil))
    (error 'file-operation-failed
           :operation operation
           :pathname (pathname pathname)
           :message (princ-to-string cause)
           :reason (or reason
                       (if errno
                           #+(and sbcl (not win32)) (posix--failure-reason errno)
                           #-(and sbcl (not win32)) ':failed
                           ':failed))
           :code errno)))

#+(and sbcl (not win32))
(defun posix--stat-information (stat)
  "Return the FILE-INFORMATION described by SB-POSIX STAT."
  (let* ((mode (sb-posix:stat-mode stat))
         (owned-p (= (sb-posix:stat-uid stat) (sb-posix:getuid))))
    (make-file-information
     (cond
       ((sb-posix:s-isreg mode) ':file)
       ((sb-posix:s-isdir mode) ':directory)
       ((sb-posix:s-islnk mode) ':symbolic-link)
       ((sb-posix:s-issock mode) ':socket)
       (t ':other))
     (cons (sb-posix:stat-dev stat) (sb-posix:stat-ino stat))
     (sb-posix:stat-size stat)
     (sb-posix:stat-mtime stat)
     (sb-posix:stat-ctime stat)
     owned-p
     (and owned-p (zerop (logand mode #o077)))
     (zerop (logand mode #o200)))))

(ls-compat::-> file-information (pathname-designator &key (:follow-links-p boolean))
  file-information)
(defun file-information (pathname &key follow-links-p)
  "Return the FILE-INFORMATION of PATHNAME.

A symbolic link is observed itself unless FOLLOW-LINKS-P. Signals
FILE-OPERATION-FAILED with operation :INSPECT when PATHNAME cannot be
inspected. The POSIX system currently supports SBCL."
  (declare (ignorable pathname follow-links-p))
  #+(and sbcl win32)
  (win32--file-information pathname follow-links-p)
  #+(and sbcl (not win32))
  (handler-case
      (posix--stat-information
       (if follow-links-p
           (sb-posix:stat (posix--native-namestring pathname))
           (sb-posix:lstat (posix--native-namestring pathname))))
    (sb-posix:syscall-error (condition)
      (posix--operation-failure ':inspect pathname condition)))
  #-sbcl
  (posix--unsupported 'file-information))

(ls-compat::-> stream-file-information (stream) file-information)
(defun stream-file-information (stream)
  "Return the FILE-INFORMATION of the object behind open file STREAM.

STREAM must come from OPEN-REGULAR-FILE or another native file stream. The
POSIX system currently supports SBCL."
  (declare (ignorable stream))
  #+(and sbcl win32)
  (win32--stream-file-information stream)
  #+(and sbcl (not win32))
  (handler-case
      (posix--stat-information (sb-posix:fstat (sb-sys:fd-stream-fd stream)))
    (sb-posix:syscall-error (condition)
      (posix--operation-failure ':inspect (or (pathname stream) "") condition)))
  #-sbcl
  (posix--unsupported 'stream-file-information))

(ls-compat::-> open-regular-file
  (pathname-designator &key (:follow-links-p boolean) (:element-type t)
                            (:external-format t))
  (values stream file-information))
(defun open-regular-file (pathname &key follow-links-p (element-type '(unsigned-byte 8))
                                        (external-format ':default))
  "Open regular file PATHNAME for reading and return the stream with its observation.

The observation comes from the opened object itself, so the stream and the
returned FILE-INFORMATION describe the same file. FILE-LENGTH and FILE-POSITION
operate on that opened object; with the default octet element type their units
are bytes. Opening never blocks on a FIFO or device and refuses a symbolic link
unless FOLLOW-LINKS-P. Signals NOT-REGULAR-FILE for anything but a regular file
and FILE-OPERATION-FAILED with operation :OPEN otherwise. The POSIX system
currently supports SBCL."
  (declare (ignorable pathname follow-links-p element-type external-format))
  #+(and sbcl win32)
  (win32--open-regular-file pathname follow-links-p element-type external-format)
  #+(and sbcl (not win32))
  (let ((descriptor
          (handler-case
              (sb-posix:open (posix--native-namestring pathname)
                             (logior sb-posix:o-rdonly
                                     sb-posix:o-nonblock
                                     (if follow-links-p 0 sb-posix:o-nofollow)))
            (sb-posix:syscall-error (condition)
              ;; NetBSD open(2) uses EFTYPE, rather than ELOOP, for a final
              ;; symlink with O_NOFOLLOW. Other EFTYPE contexts are not links.
              (posix--operation-failure
               ':open pathname condition
               :reason #+netbsd (when (and (not follow-links-p)
                                          (= (sb-posix:syscall-errno condition)
                                             *posix--netbsd-eftype*))
                                 ':symbolic-link)
                       #-netbsd nil)))))
    (unwind-protect
         (let ((information
                 (handler-case
                     (posix--stat-information (sb-posix:fstat descriptor))
                   (sb-posix:syscall-error (condition)
                     (posix--operation-failure ':open pathname condition)))))
           (unless (eq (file-information-kind information) ':file)
             (error 'not-regular-file
                    :pathname (pathname pathname)
                    :kind (file-information-kind information)))
           ;; SBCL's :FILE associates the descriptor with a file for FILE-LENGTH;
           ;; :PATHNAME alone supplies pathname metadata. Neither reopens the file.
           (let ((stream (sb-sys:make-fd-stream descriptor
                                                :input t
                                                :element-type element-type
                                                :external-format external-format
                                                :file (posix--native-namestring pathname)
                                                :pathname (pathname pathname)
                                                :auto-close t)))
             (setf descriptor nil)
             (values stream information)))
      (when descriptor
        (ignore-errors (sb-posix:close descriptor)))))
  #-sbcl
  (posix--unsupported 'open-regular-file))

(ls-compat::-> directory-entries
  (pathname-designator &key (:limit (integer 0)))
  (values list boolean))
(defun directory-entries (pathname &key (limit most-positive-fixnum))
  "Return the entries directly below directory PATHNAME and whether more exist.

Each entry is (NAME . KIND) with KIND a FILE-KIND observed without following
links. Enumeration stops at the first entry beyond LIMIT, so no more than
LIMIT entries are ever retained; the second value reports that excess. Signals
FILE-OPERATION-FAILED with operation :LIST when PATHNAME cannot be listed.
The POSIX system currently supports SBCL."
  (declare (ignorable pathname limit))
  #+(and sbcl win32)
  (win32--directory-entries pathname limit)
  #+(and sbcl (not win32))
  (let ((directory (uiop:ensure-directory-pathname (pathname pathname)))
        (handle nil)
        (entries nil)
        (count 0)
        (exceeded-p nil))
    (handler-case
        (unwind-protect
             (progn
               (setf handle (sb-posix:opendir (posix--native-namestring directory)))
               (loop for entry = (sb-posix:readdir handle)
                     until (sb-alien:null-alien entry)
                     for name = (sb-posix:dirent-name entry)
                     unless (member name '("." "..") :test #'string=)
                       do (when (>= count limit)
                            (setf exceeded-p t)
                            (return))
                          (push (cons name
                                      (file-information-kind
                                       (file-information
                                        (sb-ext:parse-native-namestring
                                         (concatenate 'string
                                                      (posix--native-namestring directory)
                                                      name)))))
                                entries)
                          (incf count)))
          (when handle
            (sb-posix:closedir handle)))
      (sb-posix:syscall-error (condition)
        (posix--operation-failure ':list pathname condition)))
    (values (nreverse entries) exceeded-p))
  #-sbcl
  (posix--unsupported 'directory-entries))

(ls-compat::-> directory-names
  (pathname-designator &key (:limit (integer 0)))
  (values list boolean))
(defun directory-names (pathname &key (limit most-positive-fixnum))
  "Return the entry names directly below directory PATHNAME and whether more exist.

Unlike DIRECTORY-ENTRIES, no entry is inspected, so listing a large directory
costs one enumeration. Names keep the order the host enumerates them in and
exclude the current and parent entries. Signals FILE-OPERATION-FAILED with
operation :LIST when PATHNAME cannot be listed. The POSIX system currently
supports SBCL."
  (declare (ignorable pathname limit))
  #+(and sbcl win32)
  (multiple-value-bind (entries exceeded-p)
      (win32--directory-entries pathname limit)
    (values (mapcar #'first entries) exceeded-p))
  #+(and sbcl (not win32))
  (let ((handle nil)
        (names nil)
        (count 0)
        (exceeded-p nil))
    (handler-case
        (unwind-protect
             (progn
               (setf handle (sb-posix:opendir (posix--native-namestring pathname)))
               (loop for entry = (sb-posix:readdir handle)
                     until (sb-alien:null-alien entry)
                     for name = (sb-posix:dirent-name entry)
                     unless (member name '("." "..") :test #'string=)
                       do (when (>= count limit)
                            (setf exceeded-p t)
                            (return))
                          (push name names)
                          (incf count)))
          (when handle
            (sb-posix:closedir handle)))
      (sb-posix:syscall-error (condition)
        (posix--operation-failure ':list pathname condition)))
    (values (nreverse names) exceeded-p))
  #-sbcl
  (posix--unsupported 'directory-names))


;;;; -- Path resolution --

(ls-compat::-> resolve-pathname (pathname-designator) pathname)
(defun resolve-pathname (pathname)
  "Return the absolute pathname PATHNAME resolves to through every symbolic link.

POSIX uses TRUENAME. Windows asks the kernel for the final path, because SBCL's
TRUENAME leaves links and junctions unresolved there. Signals
FILE-OPERATION-FAILED with operation :RESOLVE, whose reason is :MISSING only
when nothing at all exists at PATHNAME; a dangling link reports :FAILED. The
POSIX system currently supports SBCL."
  (declare (ignorable pathname))
  #+(and sbcl win32)
  (win32--resolve-pathname pathname)
  #+(and sbcl (not win32))
  (handler-case
      (truename pathname)
    (file-error (condition)
      (let ((errno (handler-case
                       (progn
                         (sb-posix:lstat (posix--native-namestring pathname))
                         nil)
                     (sb-posix:syscall-error (inspection)
                       (sb-posix:syscall-errno inspection)))))
        (error 'file-operation-failed
               :operation ':resolve
               :pathname (pathname pathname)
               :message (princ-to-string condition)
               :reason (if errno (posix--failure-reason errno) ':failed)
               :code errno))))
  #-sbcl
  (posix--unsupported 'resolve-pathname))

(ls-compat::-> posix--resolve-existing (pathname) (values (or null pathname) boolean))
(defun posix--resolve-existing (candidate)
  "Return CANDIDATE resolved and NIL, or NIL and true when nothing exists there."
  (handler-case
      (values (resolve-pathname candidate) nil)
    (file-operation-failed (condition)
      (if (eq (file-operation-failed-reason condition) ':missing)
          (values nil t)
          (error condition)))))

(ls-compat::-> posix--canonical-directory (pathname pathname) pathname)
(defun posix--canonical-directory (directory original)
  "Return directory pathname DIRECTORY with every existing ancestor resolved."
  (multiple-value-bind (canonical missing-p)
      (posix--resolve-existing directory)
    (if (not missing-p)
        canonical
        (let* ((components (pathname-directory directory))
               (leaf (first (last components))))
          (unless (stringp leaf)
            (error 'file-operation-failed
                   :operation ':resolve
                   :pathname original
                   :message (format nil "~A has no resolvable existing ancestor."
                                    (posix--native-namestring original))))
          (merge-pathnames
           (make-pathname :directory (list ':relative leaf) :name nil :type nil)
           (posix--canonical-directory
            (make-pathname :directory (butlast components)
                           :name nil :type nil :version nil
                           :defaults directory)
            original))))))

(ls-compat::-> canonical-pathname (pathname-designator) pathname)
(defun canonical-pathname (pathname)
  "Return PATHNAME with every symbolic link in its existing part resolved.

An existing PATHNAME resolves as RESOLVE-PATHNAME does. A missing one keeps its
missing tail literally under its nearest existing ancestor, resolved, so a path
that a link redirects elsewhere cannot hide behind a component that does not
exist yet. Failures other than absence propagate as FILE-OPERATION-FAILED
rather than being mistaken for a missing path."
  (let ((pathname (pathname pathname)))
    (multiple-value-bind (canonical missing-p)
        (posix--resolve-existing pathname)
      (if (not missing-p)
          canonical
          (merge-pathnames
           (make-pathname :name (pathname-name pathname)
                          :type (pathname-type pathname)
                          :version (pathname-version pathname))
           (posix--canonical-directory (uiop:pathname-directory-pathname pathname)
                                       pathname))))))

(ls-compat::-> pathname-within-p (pathname-designator pathname-designator) boolean)
(defun pathname-within-p (pathname root)
  "Return whether PATHNAME is directory ROOT or lies beneath it after canonicalization.

Both sides go through CANONICAL-PATHNAME, so a symbolic link inside ROOT that
points outside it does not count as within."
  (let ((candidate (canonical-pathname pathname))
        (directory (canonical-pathname (uiop:ensure-directory-pathname root))))
    (and (or (uiop:pathname-equal candidate directory)
             (uiop:subpathp candidate directory))
         t)))
