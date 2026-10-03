(in-package #:ls-compat.posix)

;;;; -- Win32 bindings --

;;; The Windows backend of the POSIX operations needs a handful of kernel32
;;; and advapi32 entry points. They are bound here with SB-ALIEN and
;;; wide-character entry points only, so no code below depends on SBCL's
;;; internal SB-WIN32 package.

(sb-alien:define-alien-type win32--wide-string
    (sb-alien:c-string :external-format :ucs-2le))

(sb-alien:define-alien-routine ("GetLastError" win32--get-last-error)
    (sb-alien:unsigned 32))

(sb-alien:define-alien-routine ("CloseHandle" win32--close-handle)
    sb-alien:int
  (handle (sb-alien:signed 64)))

(sb-alien:define-alien-routine ("OpenProcess" win32--open-process)
    (sb-alien:signed 64)
  (access (sb-alien:unsigned 32))
  (inherit sb-alien:int)
  (process-id (sb-alien:unsigned 32)))

(sb-alien:define-alien-routine ("GetExitCodeProcess" win32--get-exit-code-process)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (code (* (sb-alien:unsigned 32))))

(sb-alien:define-alien-routine ("CreateHardLinkW" win32--create-hard-link)
    sb-alien:int
  (new-name win32--wide-string)
  (existing-name win32--wide-string)
  (security (* t)))

(sb-alien:define-alien-routine ("GetFileAttributesW" win32--get-file-attributes)
    (sb-alien:unsigned 32)
  (path win32--wide-string))

(sb-alien:define-alien-routine ("SetFileAttributesW" win32--set-file-attributes)
    sb-alien:int
  (path win32--wide-string)
  (attributes (sb-alien:unsigned 32)))

(sb-alien:define-alien-routine ("GetCurrentProcess" win32--get-current-process)
    (sb-alien:signed 64))

(sb-alien:define-alien-routine ("GetCurrentProcessId" win32--get-current-process-id)
    (sb-alien:unsigned 32))

(sb-alien:define-alien-routine ("OpenProcessToken" win32--open-process-token)
    sb-alien:int
  (process (sb-alien:signed 64))
  (access (sb-alien:unsigned 32))
  (token (* (sb-alien:signed 64))))

(sb-alien:define-alien-routine ("GetTokenInformation" win32--get-token-information)
    sb-alien:int
  (token (sb-alien:signed 64))
  (class sb-alien:int)
  (buffer (* t))
  (size (sb-alien:unsigned 32))
  (returned (* (sb-alien:unsigned 32))))

(sb-alien:define-alien-routine ("GetLengthSid" win32--get-length-sid)
    (sb-alien:unsigned 32)
  (sid (* t)))

(sb-alien:define-alien-routine ("GetNamedSecurityInfoW" win32--get-named-security-info)
    (sb-alien:unsigned 32)
  (name win32--wide-string)
  (type sb-alien:int)
  (information (sb-alien:unsigned 32))
  (owner (* (* t)))
  (group (* (* t)))
  (dacl (* (* t)))
  (sacl (* (* t)))
  (descriptor (* (* t))))

(sb-alien:define-alien-routine ("SetNamedSecurityInfoW" win32--set-named-security-info)
    (sb-alien:unsigned 32)
  (name win32--wide-string)
  (type sb-alien:int)
  (information (sb-alien:unsigned 32))
  (owner (* t))
  (group (* t))
  (dacl (* t))
  (sacl (* t)))

(sb-alien:define-alien-routine ("SetEntriesInAclW" win32--set-entries-in-acl)
    (sb-alien:unsigned 32)
  (count (sb-alien:unsigned 32))
  (entries (* t))
  (old-acl (* t))
  (new-acl (* (* t))))

(sb-alien:define-alien-routine ("GetAclInformation" win32--get-acl-information)
    sb-alien:int
  (acl (* t))
  (information (* t))
  (size (sb-alien:unsigned 32))
  (class sb-alien:int))

(sb-alien:define-alien-routine ("GetAce" win32--get-ace)
    sb-alien:int
  (acl (* t))
  (index (sb-alien:unsigned 32))
  (ace (* (* t))))

(sb-alien:define-alien-routine ("LocalFree" win32--local-free)
    (* t)
  (memory (* t)))

(sb-alien:define-alien-routine ("CreateFileW" win32--create-file)
    (sb-alien:signed 64)
  (name win32--wide-string)
  (access (sb-alien:unsigned 32))
  (share (sb-alien:unsigned 32))
  (security (* t))
  (disposition (sb-alien:unsigned 32))
  (flags (sb-alien:unsigned 32))
  (template (sb-alien:signed 64)))

(sb-alien:define-alien-routine ("GetFileInformationByHandle"
                                win32--get-file-information-by-handle)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (information (* t)))

(sb-alien:define-alien-routine ("GetFileInformationByHandleEx"
                                win32--get-file-information-by-handle-ex)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (class sb-alien:int)
  (information (* t))
  (size (sb-alien:unsigned 32)))

(sb-alien:define-alien-routine ("GetFileType" win32--get-file-type)
    (sb-alien:unsigned 32)
  (handle (sb-alien:signed 64)))

(sb-alien:define-alien-routine ("FindFirstFileW" win32--find-first-file)
    (sb-alien:signed 64)
  (pattern win32--wide-string)
  (data (* t)))

(sb-alien:define-alien-routine ("FindNextFileW" win32--find-next-file)
    sb-alien:int
  (handle (sb-alien:signed 64))
  (data (* t)))

(sb-alien:define-alien-routine ("FindClose" win32--find-close)
    sb-alien:int
  (handle (sb-alien:signed 64)))

(sb-alien:define-alien-routine ("GetSecurityInfo" win32--get-security-info)
    (sb-alien:unsigned 32)
  (handle (sb-alien:signed 64))
  (type sb-alien:int)
  (information (sb-alien:unsigned 32))
  (owner (* (* t)))
  (group (* (* t)))
  (dacl (* (* t)))
  (sacl (* (* t)))
  (descriptor (* (* t))))

(defparameter *win32-process-query-limited-information* #x1000
  "The OpenProcess access right that reads a process's exit state.")

(defparameter *win32-still-active* 259
  "The exit code GetExitCodeProcess reports for a running process.")

(defparameter *win32-error-file-not-found* 2
  "ERROR_FILE_NOT_FOUND.")

(defparameter *win32-error-path-not-found* 3
  "ERROR_PATH_NOT_FOUND, reported when a directory on the way is missing.")

(defparameter *win32-error-directory* 267
  "ERROR_DIRECTORY, reported when a path component is not a directory.")

(defparameter *win32-read-control* #x20000
  "READ_CONTROL, the access right that reads an object's security descriptor.")

(defparameter *win32-error-access-denied* 5
  "The Windows error for a process that exists but may not be opened.")

(defparameter *win32-error-invalid-parameter* 87
  "The Windows error for a process identifier that names nothing.")

(defparameter *win32-error-file-exists* 80
  "The Windows error CreateHardLinkW reports when the new name is taken.")

(defparameter *win32-error-already-exists* 183
  "The other Windows error CreateHardLinkW reports for a taken new name.")

(defparameter *win32-invalid-file-attributes* #xFFFFFFFF
  "INVALID_FILE_ATTRIBUTES, GetFileAttributesW's failure value.")

(defparameter *win32-file-attribute-readonly* #x1
  "FILE_ATTRIBUTE_READONLY.")

(defparameter *win32-file-attribute-directory* #x10
  "FILE_ATTRIBUTE_DIRECTORY.")

(defparameter *win32-token-query* #x8
  "TOKEN_QUERY, the access needed to read a process token's user.")

(defparameter *win32-token-user* 1
  "The TokenUser information class.")

(defparameter *win32-se-file-object* 1
  "SE_FILE_OBJECT, the security object type of files and directories.")

(defparameter *win32-owner-and-dacl-information* 5
  "OWNER_SECURITY_INFORMATION combined with DACL_SECURITY_INFORMATION.")

(defparameter *win32-protected-dacl-information* #x80000004
  "DACL_SECURITY_INFORMATION with PROTECTED_DACL_SECURITY_INFORMATION.")

(defparameter *win32-sub-containers-and-objects-inherit* #x3
  "SUB_CONTAINERS_AND_OBJECTS_INHERIT, the inheritance of a directory's entries.")

(defparameter *win32-unprotected-dacl-information* #x20000004
  "DACL_SECURITY_INFORMATION with UNPROTECTED_DACL_SECURITY_INFORMATION.")

(defparameter *win32-file-all-access* #x1F01FF
  "FILE_ALL_ACCESS.")

(defparameter *win32-file-write-data* #x2
  "FILE_WRITE_DATA, which is also FILE_ADD_FILE on a directory.")

(defparameter *win32-file-append-data* #x4
  "FILE_APPEND_DATA, which is also FILE_ADD_SUBDIRECTORY on a directory.")

(defparameter *win32-explicit-access-size* 48
  "The size of one EXPLICIT_ACCESS_W structure on x86-64.")

(defparameter *win32-system-sid-octets*
  (coerce '(1 1 0 0 0 0 0 5 18 0 0 0) '(simple-array (unsigned-byte 8) (12)))
  "S-1-5-18, the SYSTEM account, which keeps its access to private objects.")

(defparameter *win32-administrators-sid-octets*
  (coerce '(1 2 0 0 0 0 0 5 32 0 0 0 32 2 0 0) '(simple-array (unsigned-byte 8) (16)))
  "S-1-5-32-544, the Administrators group that owns what an elevated process creates.")

(defparameter *win32-token-elevation* 20
  "The TokenElevation information class.")

(defparameter *win32-invalid-handle* -1
  "INVALID_HANDLE_VALUE.")

(defparameter *win32-generic-read* #x80000000
  "GENERIC_READ.")

(defparameter *win32-file-read-attributes* #x80
  "FILE_READ_ATTRIBUTES, enough access to inspect an object.")

(defparameter *win32-share-all* 7
  "FILE_SHARE_READ, FILE_SHARE_WRITE, and FILE_SHARE_DELETE.")

(defparameter *win32-open-existing* 3
  "OPEN_EXISTING.")

(defparameter *win32-file-attribute-normal* #x80
  "FILE_ATTRIBUTE_NORMAL.")

(defparameter *win32-file-attribute-reparse-point* #x400
  "FILE_ATTRIBUTE_REPARSE_POINT, carried by symbolic links and junctions.")

(defparameter *win32-file-flag-backup-semantics* #x02000000
  "FILE_FLAG_BACKUP_SEMANTICS, required to open a directory handle.")

(defparameter *win32-file-flag-open-reparse-point* #x00200000
  "FILE_FLAG_OPEN_REPARSE_POINT, which opens a link itself instead of its target.")

(defparameter *win32-file-type-disk* 1
  "FILE_TYPE_DISK, the GetFileType class of ordinary files.")

(defparameter *win32-file-basic-info* 0
  "The FileBasicInfo class of GetFileInformationByHandleEx.")


;;;; -- Processes --

(ls-compat::-> win32--process-state ((integer 1 *)) (member :alive :dead :unknown))
(defun win32--process-state (process-id)
  "Classify PROCESS-ID through OpenProcess and GetExitCodeProcess."
  (let ((handle (win32--open-process *win32-process-query-limited-information*
                                     0 process-id)))
    (if (zerop handle)
        (let ((code (win32--get-last-error)))
          (cond
            ((= code *win32-error-invalid-parameter*) ':dead)
            ((= code *win32-error-access-denied*) ':alive)
            (t ':unknown)))
        (unwind-protect
             (sb-alien:with-alien ((exit-code (sb-alien:unsigned 32)))
               (if (zerop (win32--get-exit-code-process handle
                                                        (sb-alien:addr exit-code)))
                   ':unknown
                   (if (= exit-code *win32-still-active*)
                       ':alive
                       ':dead)))
          (win32--close-handle handle)))))


;;;; -- Access control lists --

;;; Windows files carry no POSIX mode, so FILE-MODE maps between the two. A
;;; mode without group or other bits, or without the owner write bit, becomes
;;; a protected access control list granting the owner and SYSTEM alone,
;;; withholding FILE_WRITE_DATA and FILE_APPEND_DATA when the owner write bit
;;; is clear. Any other mode restores the inherited access control list. The
;;; read-only attribute is never set and is cleared when found, so a file can
;;; always be deleted and replaced, as an unwritable POSIX file in a writable
;;; directory can. Reading reverses the mapping, reporting a read-only
;;; attribute someone else set as clear write bits.

(defvar *win32-user-sid* nil
  "The current user's SID octets paired with the process identifier that read them.")

(defvar *win32-elevated-p* ':unknown
  "Whether this process runs elevated, read from its token once.")

(ls-compat::-> win32--elevated-p () boolean)
(defun win32--elevated-p ()
  "Return whether this process runs with an elevated token.

Windows makes the Administrators group the owner of what an elevated process
creates, so ownership checks treat that group as the user then."
  (when (eq *win32-elevated-p* ':unknown)
    (sb-alien:with-alien ((token (sb-alien:signed 64)))
      (when (zerop (win32--open-process-token (win32--get-current-process)
                                              *win32-token-query*
                                              (sb-alien:addr token)))
        (win32--security-failure "" "OpenProcessToken" (win32--get-last-error)))
      (unwind-protect
           (sb-alien:with-alien ((elevation (sb-alien:unsigned 32))
                                 (returned (sb-alien:unsigned 32)))
             (setf *win32-elevated-p*
                   (and (not (zerop (win32--get-token-information
                                     token *win32-token-elevation*
                                     (sb-alien:addr elevation) 4
                                     (sb-alien:addr returned))))
                        (not (zerop elevation)))))
        (win32--close-handle token))))
  *win32-elevated-p*)

(ls-compat::-> win32--owner-p ((simple-array (unsigned-byte 8) (*))) boolean)
(defun win32--owner-p (owner)
  "Return whether OWNER, a SID, counts as the current user."
  (or (equalp owner (win32--current-user-sid))
      (and (equalp owner *win32-administrators-sid-octets*)
           (win32--elevated-p))))

(ls-compat::-> win32--native (pathname-designator) string)
(defun win32--native (pathname)
  "Return PATHNAME as the namestring the security entry points expect."
  (let ((native (posix--native-namestring pathname)))
    (if (and (> (length native) 3)
             (char= (char native (1- (length native))) #\\))
        (subseq native 0 (1- (length native)))
        native)))

(ls-compat::-> win32--security-failure (pathname-designator string integer) nil)
(defun win32--security-failure (pathname operation code)
  "Signal MODE-FAILED for Windows error CODE raised by OPERATION on PATHNAME."
  (error 'mode-failed
         :pathname (pathname pathname)
         :message (format nil "~A failed with Windows error ~D" operation code)))

(ls-compat::-> win32--sid-octets (sb-sys:system-area-pointer)
  (simple-array (unsigned-byte 8) (*)))
(defun win32--sid-octets (sid)
  "Copy the security identifier at SID into a fresh octet vector."
  (let* ((length (win32--get-length-sid (sb-alien:sap-alien sid (* t))))
         (octets (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length octets)
      (setf (aref octets index) (sb-sys:sap-ref-8 sid index)))))

(ls-compat::-> win32--current-user-sid () (simple-array (unsigned-byte 8) (*)))
(defun win32--current-user-sid ()
  "Return the current process token's user SID octets, cached per process."
  (let ((process-id (win32--get-current-process-id)))
    (unless (and *win32-user-sid* (= (first *win32-user-sid*) process-id))
      (sb-alien:with-alien ((token (sb-alien:signed 64)))
        (when (zerop (win32--open-process-token (win32--get-current-process)
                                                *win32-token-query*
                                                (sb-alien:addr token)))
          (win32--security-failure "" "OpenProcessToken" (win32--get-last-error)))
        (unwind-protect
             (sb-alien:with-alien ((buffer (sb-alien:array (sb-alien:unsigned 8) 256))
                                   (returned (sb-alien:unsigned 32)))
               (when (zerop (win32--get-token-information
                             token *win32-token-user* (sb-alien:alien-sap buffer)
                             256 (sb-alien:addr returned)))
                 (win32--security-failure "" "GetTokenInformation"
                                          (win32--get-last-error)))
               (setf *win32-user-sid*
                     (list process-id
                           (win32--sid-octets
                            (sb-sys:int-sap
                             (sb-sys:sap-ref-64 (sb-alien:alien-sap buffer) 0))))))
          (win32--close-handle token))))
    (second *win32-user-sid*)))

(ls-compat::-> win32--acl-entries (sb-sys:system-area-pointer pathname-designator) list)
(defun win32--acl-entries (acl pathname)
  "Return ACL's entries as (TYPE MASK . SID-OCTETS), TYPE being :ALLOW or :OTHER."
  (sb-alien:with-alien ((information (sb-alien:array (sb-alien:unsigned 8) 12)))
    (when (zerop (win32--get-acl-information (sb-alien:sap-alien acl (* t))
                                             (sb-alien:alien-sap information) 12 2))
      (win32--security-failure pathname "GetAclInformation" (win32--get-last-error)))
    (loop for index below (sb-sys:sap-ref-32 (sb-alien:alien-sap information) 0)
          collect (sb-alien:with-alien ((ace (* t)))
                    (when (zerop (win32--get-ace (sb-alien:sap-alien acl (* t))
                                                 index (sb-alien:addr ace)))
                      (win32--security-failure pathname "GetAce" (win32--get-last-error)))
                    (let ((sap (sb-alien:alien-sap ace)))
                      (list* (if (zerop (sb-sys:sap-ref-8 sap 0)) ':allow ':other)
                             (sb-sys:sap-ref-32 sap 4)
                             (win32--sid-octets (sb-sys:sap+ sap 8))))))))

(ls-compat::-> win32--owner-access (pathname-designator string) (values boolean boolean))
(defun win32--owner-access (pathname native)
  "Return whether NATIVE is private to the current user and whether its owner may write.

A private object is owned by the user and allows only the user and SYSTEM.
The second value is true for any object that is not private, since the
read-only attribute then carries the owner write bit."
  (sb-alien:with-alien ((owner (* t)) (dacl (* t)) (descriptor (* t)))
    (let ((status (win32--get-named-security-info native *win32-se-file-object*
                                                  *win32-owner-and-dacl-information*
                                                  (sb-alien:addr owner) nil
                                                  (sb-alien:addr dacl) nil
                                                  (sb-alien:addr descriptor))))
      (unless (zerop status)
        (win32--security-failure pathname "GetNamedSecurityInfoW" status))
      (unwind-protect
           (let* ((user (win32--current-user-sid))
                  (owned-p (win32--owner-p (win32--sid-octets (sb-alien:alien-sap owner))))
                  (entries (if (zerop (sb-sys:sap-int (sb-alien:alien-sap dacl)))
                               ':unrestricted
                               (win32--acl-entries (sb-alien:alien-sap dacl) pathname)))
                  (private-p
                    (and owned-p
                         (listp entries)
                         (every (lambda (entry)
                                  (and (eq (first entry) ':allow)
                                       (or (equalp (cddr entry) user)
                                           (equalp (cddr entry) *win32-system-sid-octets*))))
                                entries)
                         t)))
             (values private-p
                     (or (not private-p)
                         (and (some (lambda (entry)
                                      (and (equalp (cddr entry) user)
                                           (logtest (second entry) *win32-file-write-data*)))
                                    entries)
                              t))))
        (win32--local-free descriptor)))))

(ls-compat::-> win32--apply-owner-acl (pathname-designator string integer) null)
(defun win32--apply-owner-acl (pathname native mask)
  "Replace NATIVE's access control list with the owner at MASK and SYSTEM in full.

A directory's entries are inheritable: a file created below it with no
security descriptor of its own takes them over, as a file created in a POSIX
0700 directory stays readable by its owner. Without inheritance such a file
would carry an empty list and deny everyone, its creator included."
  (let ((user (win32--current-user-sid))
        (inheritance (if (logtest (win32--attributes pathname native)
                                  *win32-file-attribute-directory*)
                         *win32-sub-containers-and-objects-inherit*
                         0)))
    (sb-alien:with-alien ((user-sid (sb-alien:array (sb-alien:unsigned 8) 68))
                          (system-sid (sb-alien:array (sb-alien:unsigned 8) 12))
                          (entries (sb-alien:array (sb-alien:unsigned 8) 96))
                          (new-acl (* t)))
      (dotimes (index (length user))
        (setf (sb-alien:deref user-sid index) (aref user index)))
      (dotimes (index 12)
        (setf (sb-alien:deref system-sid index) (aref *win32-system-sid-octets* index)))
      (let ((sap (sb-alien:alien-sap entries)))
        (dotimes (index 96)
          (setf (sb-sys:sap-ref-8 sap index) 0))
        (loop for sid in (list (sb-alien:alien-sap user-sid)
                               (sb-alien:alien-sap system-sid))
              for access in (list mask *win32-file-all-access*)
              for base from 0 by *win32-explicit-access-size*
              do (setf (sb-sys:sap-ref-32 sap base) access
                       (sb-sys:sap-ref-32 sap (+ base 4)) 2
                       (sb-sys:sap-ref-32 sap (+ base 8)) inheritance
                       (sb-sys:sap-ref-32 sap (+ base 28)) 0
                       (sb-sys:sap-ref-32 sap (+ base 32)) 0
                       (sb-sys:sap-ref-64 sap (+ base 40)) (sb-sys:sap-int sid))))
      (let ((status (win32--set-entries-in-acl 2 (sb-alien:alien-sap entries) nil
                                               (sb-alien:addr new-acl))))
        (unless (zerop status)
          (win32--security-failure pathname "SetEntriesInAclW" status)))
      (unwind-protect
           (let ((status (win32--set-named-security-info
                          native *win32-se-file-object*
                          *win32-protected-dacl-information* nil nil new-acl nil)))
             (unless (zerop status)
               (win32--security-failure pathname "SetNamedSecurityInfoW" status)))
        (win32--local-free new-acl))))
  nil)

(ls-compat::-> win32--restore-inherited-acl (pathname-designator string) null)
(defun win32--restore-inherited-acl (pathname native)
  "Give NATIVE an empty, unprotected access control list, so it inherits its parent's."
  (sb-alien:with-alien ((new-acl (* t)))
    (let ((status (win32--set-entries-in-acl 0 nil nil (sb-alien:addr new-acl))))
      (unless (zerop status)
        (win32--security-failure pathname "SetEntriesInAclW" status)))
    (unwind-protect
         (let ((status (win32--set-named-security-info
                        native *win32-se-file-object*
                        *win32-unprotected-dacl-information* nil nil new-acl nil)))
           (unless (zerop status)
             (win32--security-failure pathname "SetNamedSecurityInfoW" status)))
      (win32--local-free new-acl)))
  nil)

(ls-compat::-> win32--attributes (pathname-designator string) integer)
(defun win32--attributes (pathname native)
  "Return NATIVE's file attributes."
  (let ((attributes (win32--get-file-attributes native)))
    (when (= attributes *win32-invalid-file-attributes*)
      (win32--security-failure pathname "GetFileAttributesW" (win32--get-last-error)))
    attributes))

(ls-compat::-> win32--set-read-only-attribute (pathname-designator string integer boolean) null)
(defun win32--set-read-only-attribute (pathname native attributes read-only-p)
  "Set or clear the read-only bit in NATIVE's ATTRIBUTES according to READ-ONLY-P."
  (let ((updated (if read-only-p
                     (logior attributes *win32-file-attribute-readonly*)
                     (logandc2 attributes *win32-file-attribute-readonly*))))
    (unless (= updated attributes)
      (when (zerop (win32--set-file-attributes native updated))
        (win32--security-failure pathname "SetFileAttributesW" (win32--get-last-error)))))
  nil)

(ls-compat::-> win32--file-mode (pathname-designator) (integer 0 #o777))
(defun win32--file-mode (pathname)
  "Return the POSIX permission bits PATHNAME's access control list expresses."
  (let* ((native (win32--native pathname))
         (attributes (win32--attributes pathname native))
         (directory-p (logtest attributes *win32-file-attribute-directory*)))
    (multiple-value-bind (private-p owner-write-p)
        (win32--owner-access pathname native)
      (let ((mode (if private-p
                      (if directory-p #o700 #o600)
                      (if directory-p #o755 #o644)))
            (writable-p (if private-p
                            owner-write-p
                            (not (logtest attributes *win32-file-attribute-readonly*)))))
        (if writable-p
            mode
            (logandc2 mode #o222))))))

(ls-compat::-> win32--set-file-mode (pathname-designator (integer 0 #o777)) (integer 0 #o777))
(defun win32--set-file-mode (pathname mode)
  "Express permission MODE on PATHNAME through its access control list."
  (let* ((native (win32--native pathname))
         (attributes (win32--attributes pathname native))
         (owner-write-p (logtest mode #o200)))
    (cond
      ((not owner-write-p)
       (win32--apply-owner-acl pathname native
                               (logandc2 *win32-file-all-access*
                                         (logior *win32-file-write-data*
                                                 *win32-file-append-data*))))
      ((zerop (logand mode #o077))
       (win32--apply-owner-acl pathname native *win32-file-all-access*))
      (t
       (win32--restore-inherited-acl pathname native)))
    (win32--set-read-only-attribute pathname native attributes nil)
    mode))


;;;; -- File information --

(ls-compat::-> win32--failure-reason (integer) failure-reason)
(defun win32--failure-reason (code)
  "Return the portable FAILURE-REASON for Windows error CODE."
  (cond
    ((or (= code *win32-error-file-not-found*)
         (= code *win32-error-path-not-found*))
     ':missing)
    ((or (= code *win32-error-file-exists*)
         (= code *win32-error-already-exists*))
     ':exists)
    ((= code *win32-error-directory*)
     ':not-directory)
    (t
     ':failed)))

(ls-compat::-> win32--operation-failure (keyword pathname-designator integer) nil)
(defun win32--operation-failure (operation pathname code)
  "Signal FILE-OPERATION-FAILED for Windows error CODE raised by OPERATION."
  (error 'file-operation-failed
         :operation operation
         :pathname (pathname pathname)
         :message (format nil "Windows error ~D" code)
         :reason (win32--failure-reason code)
         :code code))

(ls-compat::-> win32--handle-privacy (integer) (values boolean boolean boolean))
(defun win32--handle-privacy (handle)
  "Return whether the object behind HANDLE is owned by the user, private to the
user, and withholds write access from the user.

Every value is false when the security descriptor cannot be read, which
happens for objects the user may see but not inspect. Write access is judged
only for private objects, whose access control list FILE-MODE wrote."
  (sb-alien:with-alien ((owner (* t)) (dacl (* t)) (descriptor (* t)))
    (let ((status (win32--get-security-info handle *win32-se-file-object*
                                            *win32-owner-and-dacl-information*
                                            (sb-alien:addr owner) nil
                                            (sb-alien:addr dacl) nil
                                            (sb-alien:addr descriptor))))
      (if (not (zerop status))
          (values nil nil nil)
          (unwind-protect
               (let* ((user (win32--current-user-sid))
                      (owned-p (win32--owner-p
                                (win32--sid-octets (sb-alien:alien-sap owner))))
                      (entries (if (zerop (sb-sys:sap-int (sb-alien:alien-sap dacl)))
                                   ':unrestricted
                                   (win32--acl-entries (sb-alien:alien-sap dacl) "")))
                      (private-p
                        (and owned-p
                             (listp entries)
                             (every (lambda (entry)
                                      (and (eq (first entry) ':allow)
                                           (or (equalp (cddr entry) user)
                                               (equalp (cddr entry)
                                                       *win32-system-sid-octets*))))
                                    entries)
                             t)))
                 (values owned-p
                         private-p
                         (and private-p
                              (notany (lambda (entry)
                                        (and (equalp (cddr entry) user)
                                             (logtest (second entry)
                                                      *win32-file-write-data*)))
                                      entries))))
            (win32--local-free descriptor))))))

(ls-compat::-> win32--handle-information (integer boolean) file-information)
(defun win32--handle-information (handle link-p)
  "Return the FILE-INFORMATION of the object behind HANDLE, a link when LINK-P."
  (sb-alien:with-alien ((information (sb-alien:array (sb-alien:unsigned 8) 64))
                        (basic (sb-alien:array (sb-alien:unsigned 8) 40)))
    (let ((sap (sb-alien:alien-sap information))
          (basic-sap (sb-alien:alien-sap basic)))
      (when (zerop (win32--get-file-information-by-handle handle sap))
        (win32--operation-failure ':inspect "" (win32--get-last-error)))
      (when (zerop (win32--get-file-information-by-handle-ex
                    handle *win32-file-basic-info* basic-sap 40))
        (win32--operation-failure ':inspect "" (win32--get-last-error)))
      (multiple-value-bind (owned-p private-p unwritable-p)
          (win32--handle-privacy handle)
        (let ((attributes (sb-sys:sap-ref-32 sap 0)))
          (make-file-information
           (cond
             ((or link-p (logtest attributes *win32-file-attribute-reparse-point*))
              ':symbolic-link)
             ((logtest attributes *win32-file-attribute-directory*)
              ':directory)
             ((= (win32--get-file-type handle) *win32-file-type-disk*)
              ':file)
             (t
              ':other))
           (cons (sb-sys:sap-ref-32 sap 28)
                 (logior (ash (sb-sys:sap-ref-32 sap 44) 32)
                         (sb-sys:sap-ref-32 sap 48)))
           (logior (ash (sb-sys:sap-ref-32 sap 32) 32)
                   (sb-sys:sap-ref-32 sap 36))
           (sb-sys:sap-ref-64 basic-sap 16)
           (sb-sys:sap-ref-64 basic-sap 24)
           owned-p
           private-p
           (or unwritable-p
               (logtest attributes *win32-file-attribute-readonly*))))))))

(ls-compat::-> win32--open-for-information (pathname-designator boolean integer integer)
  integer)
(defun win32--open-for-information (pathname follow-links-p access flags)
  "Open PATHNAME with ACCESS and FLAGS, following reparse points only when asked.

READ_CONTROL is requested as well, so the security descriptor is readable; an
object that refuses it is opened again with ACCESS alone."
  (flet ((open-with (rights)
           (win32--create-file (win32--native pathname)
                               rights
                               *win32-share-all*
                               nil
                               *win32-open-existing*
                               (logior flags
                                       (if follow-links-p
                                           0
                                           *win32-file-flag-open-reparse-point*))
                               0)))
    (let ((handle (open-with (logior access *win32-read-control*))))
      (when (and (= handle *win32-invalid-handle*)
                 (= (win32--get-last-error) *win32-error-access-denied*))
        (setf handle (open-with access)))
      (when (= handle *win32-invalid-handle*)
        (win32--operation-failure ':inspect pathname (win32--get-last-error)))
      handle)))

(ls-compat::-> win32--file-information (pathname-designator boolean) file-information)
(defun win32--file-information (pathname follow-links-p)
  "Observe PATHNAME through a handle opened for attribute reading."
  (let ((handle (win32--open-for-information pathname follow-links-p
                                             *win32-file-read-attributes*
                                             *win32-file-flag-backup-semantics*)))
    (unwind-protect
         (win32--handle-information handle nil)
      (win32--close-handle handle))))

(ls-compat::-> win32--stream-file-information (stream) file-information)
(defun win32--stream-file-information (stream)
  "Observe the object behind STREAM's handle."
  (win32--handle-information (sb-sys:fd-stream-fd stream) nil))

(ls-compat::-> win32--open-regular-file (pathname-designator boolean t t)
  (values stream file-information))
(defun win32--open-regular-file (pathname follow-links-p element-type external-format)
  "Open PATHNAME for reading, refusing links unless FOLLOW-LINKS-P and non-files always."
  (let ((handle (win32--create-file (win32--native pathname)
                                    *win32-generic-read*
                                    *win32-share-all*
                                    nil
                                    *win32-open-existing*
                                    (logior *win32-file-attribute-normal*
                                            (if follow-links-p
                                                0
                                                *win32-file-flag-open-reparse-point*))
                                    0)))
    (when (= handle *win32-invalid-handle*)
      (win32--operation-failure ':open pathname (win32--get-last-error)))
    (unwind-protect
         (let ((information (win32--handle-information handle nil)))
           (unless (eq (file-information-kind information) ':file)
             (error 'not-regular-file
                    :pathname (pathname pathname)
                    :kind (file-information-kind information)))
           (let ((stream (sb-sys:make-fd-stream handle
                                                :input t
                                                :element-type element-type
                                                :external-format external-format
                                                :pathname (pathname pathname)
                                                :auto-close t)))
             (setf handle nil)
             (values stream information)))
      (when handle
        (win32--close-handle handle)))))

(ls-compat::-> win32--find-data-name (sb-sys:system-area-pointer) string)
(defun win32--find-data-name (data)
  "Return the file name stored in the WIN32_FIND_DATAW at DATA."
  (coerce (loop for index from 0 below 260
                for code = (sb-sys:sap-ref-16 data (+ 44 (* 2 index)))
                until (zerop code)
                collect (code-char code))
          'string))

(ls-compat::-> win32--directory-entries (pathname-designator (integer 0)) (values list boolean))
(defun win32--directory-entries (pathname limit)
  "Enumerate PATHNAME with FindFirstFileW, classifying each entry by its attributes."
  (sb-alien:with-alien ((data (sb-alien:array (sb-alien:unsigned 8) 640)))
    (let* ((sap (sb-alien:alien-sap data))
           (handle (win32--find-first-file
                    (concatenate 'string (win32--native pathname) "\\*") sap))
           (entries nil)
           (count 0)
           (exceeded-p nil))
      (when (= handle *win32-invalid-handle*)
        (win32--operation-failure ':list pathname (win32--get-last-error)))
      (unwind-protect
           (loop
             (let ((name (win32--find-data-name sap))
                   (attributes (sb-sys:sap-ref-32 sap 0)))
               (unless (member name '("." "..") :test #'string=)
                 (when (>= count limit)
                   (setf exceeded-p t)
                   (return))
                 (push (cons name
                             (cond
                               ((logtest attributes *win32-file-attribute-reparse-point*)
                                ':symbolic-link)
                               ((logtest attributes *win32-file-attribute-directory*)
                                ':directory)
                               (t
                                ':file)))
                       entries)
                 (incf count)))
             (when (zerop (win32--find-next-file handle sap))
               (return)))
        (win32--find-close handle))
      (values (nreverse entries) exceeded-p))))
