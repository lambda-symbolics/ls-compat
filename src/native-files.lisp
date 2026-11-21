(in-package #:ls-compat.posix)

;;;; -- Native file mutation --

#+(and sbcl win32)
(progn
  (sb-alien:define-alien-routine ("MoveFileExW" win32--move-file-ex) sb-alien:int
    (source win32--wide-string) (target win32--wide-string)
    (flags (sb-alien:unsigned 32)))
  (sb-alien:define-alien-routine ("DeleteFileW" win32--delete-file) sb-alien:int
    (pathname win32--wide-string))
  (sb-alien:define-alien-routine ("RemoveDirectoryW" win32--remove-directory) sb-alien:int
    (pathname win32--wide-string))
  (sb-alien:define-alien-routine ("CreateDirectoryW" win32--create-directory) sb-alien:int
    (pathname win32--wide-string) (security (* t))))

(ls-compat::-> remove-file (pathname-designator) null)
(defun remove-file (pathname)
  "Unlink the exact native PATHNAME, signaling FILE-OPERATION-FAILED on failure.

Windows clears the read-only attribute first and restores the original attributes
if deletion fails. It does not change the access control list."
  (declare (ignorable pathname))
  #+(and sbcl win32)
  (let* ((native (win32--native pathname))
         (attributes (win32--get-file-attributes native))
         (complete-p nil))
    (unwind-protect
         (progn
           (unless (= attributes *win32-invalid-file-attributes*)
             (win32--clear-read-only-attribute pathname native attributes))
           (when (zerop (win32--delete-file native))
             (win32--operation-failure :delete pathname (win32--get-last-error)))
           (setf complete-p t))
      (when (and (not complete-p)
                 (/= attributes *win32-invalid-file-attributes*)
                 (/= (win32--get-file-attributes native) *win32-invalid-file-attributes*))
        (when (zerop (win32--set-file-attributes native attributes))
          (win32--operation-failure :protect pathname (win32--get-last-error))))))
  #+(and sbcl (not win32))
  (handler-case (sb-posix:unlink (posix--native-namestring pathname))
    (sb-posix:syscall-error (condition)
      (posix--operation-failure :delete pathname condition)))
  #-sbcl
  (posix--unsupported 'remove-file)
  nil)

(ls-compat::-> replace-file (pathname-designator pathname-designator) null)
(defun replace-file (source target)
  "Rename existing SOURCE over TARGET without a writer callback.

Windows uses MoveFileExW with replacement enabled. Any existing target read-only
attribute is cleared before the move and is not restored if the move fails."
  (declare (ignorable source target))
  #+(and sbcl win32)
  (let* ((native (win32--native target))
         (attributes (win32--get-file-attributes native)))
    (unless (= attributes *win32-invalid-file-attributes*)
      (win32--clear-read-only-attribute target native attributes))
    (when (zerop (win32--move-file-ex (win32--native source) native 1))
      (win32--operation-failure :replace target (win32--get-last-error))))
  #+(and sbcl (not win32))
  (handler-case
      (sb-posix:rename (posix--native-namestring source) (posix--native-namestring target))
    (sb-posix:syscall-error (condition)
      (posix--operation-failure :replace target condition)))
  #-sbcl
  (posix--unsupported 'replace-file)
  nil)

(ls-compat::-> make-temporary-directory (pathname-designator string) pathname)
(defun make-temporary-directory (parent prefix)
  "Create a private directory named PREFIX plus a unique suffix below PARENT.

POSIX uses MKDTEMP and mode 0700. Windows exclusively creates a directory then
applies an owner-only ACL, removing the new empty directory if ACL setup fails.
Collision retries are bounded at 100 on Windows. The caller owns its lifecycle."
  (declare (ignorable parent prefix))
  #+(and sbcl (not win32))
  (handler-case
      (uiop:ensure-directory-pathname
       (sb-posix:mkdtemp
        (posix--native-namestring
         (merge-pathnames (concatenate 'string prefix "XXXXXX") parent))))
    (sb-posix:syscall-error (condition)
      (posix--operation-failure :create parent condition)))
  #+(and sbcl win32)
  (let ((state (sb-ext:seed-random-state t)))
    (loop for suffix = (coerce (loop repeat 8
                                     collect (char "abcdefghijklmnopqrstuvwxyz0123456789"
                                                   (random 36 state))) 'string)
          for directory = (uiop:ensure-directory-pathname
                           (merge-pathnames (concatenate 'string prefix suffix) parent))
          repeat 100
          do (if (zerop (win32--create-directory (win32--native directory) nil))
                 (let ((code (win32--get-last-error)))
                   (unless (= code *win32-error-already-exists*)
                     (win32--operation-failure :create directory code)))
                 (let ((complete-p nil))
                   (unwind-protect
                        (progn
                          (win32--set-file-mode directory #o700)
                          (setf complete-p t))
                     (unless complete-p
                       (when (zerop (win32--remove-directory (win32--native directory)))
                         (win32--operation-failure :delete directory (win32--get-last-error)))))
                   (return directory)))
          finally (error 'file-operation-failed :operation :create :pathname (pathname parent)
                         :reason :exists :message "Temporary directory collision limit reached.")))
  #-sbcl
  (posix--unsupported 'make-temporary-directory))


;;;; -- Tree cleanup --

#+(and sbcl win32)
(ls-compat::-> win32--remove-tree (pathname-designator) null)
#+(and sbcl win32)
(defun win32--remove-tree (pathname)
  "Remove a native tree, including dot entries, without descending into reparse points."
  (let* ((native     (win32--native pathname))
         (attributes (win32--get-file-attributes native)))
    (when (= attributes *win32-invalid-file-attributes*)
      (win32--operation-failure :delete pathname (win32--get-last-error)))
    (when (and (logtest attributes *win32-file-attribute-directory*)
               (not (logtest attributes *win32-file-attribute-reparse-point*)))
      (dolist (name (directory-names pathname))
        (win32--remove-tree
         (uiop:parse-native-namestring
          (concatenate 'string (string-right-trim "\\/" native) "\\" name)))))
    (win32--clear-read-only-attribute pathname native attributes)
    (when (zerop (if (logtest attributes *win32-file-attribute-directory*)
                    (win32--remove-directory native)
                    (win32--delete-file native)))
      (win32--operation-failure :delete pathname (win32--get-last-error))))
  nil)

(ls-compat::-> delete-directory-tree
  (pathname &key (:validate t) (:if-does-not-exist (member :error :ignore))) t)
(defun delete-directory-tree (pathname &rest arguments
                              &key (validate nil validate-p) (if-does-not-exist :error))
  "Delete PATHNAME's tree after UIOP-style explicit validation.

PATHNAME must be a physical, non-wild directory pathname. VALIDATE is required
and must accept it before mutation. IF-DOES-NOT-EXIST is :ERROR or :IGNORE.
POSIX delegates to UIOP. Windows includes dot entries, removes reparse points
without traversing them, clears read-only attributes, and retries file failures
at most 100 times with 0.1 seconds between attempts. This is not a guarantee
against concurrent pathname replacement."
  (declare (ignorable pathname validate validate-p if-does-not-exist arguments))
  #+(and sbcl win32)
  (progn
    (check-type if-does-not-exist (member :error :ignore))
    (setf pathname (uiop:ensure-pathname pathname :want-pathname t :want-non-wild t
                                                :want-physical t :want-directory t))
    (unless (and validate-p (uiop:call-function validate pathname))
      (error 'file-operation-failed :operation :delete :pathname pathname
             :reason :failed :message "Tree deletion requires successful explicit validation."))
    (let ((attributes (win32--get-file-attributes (win32--native pathname))))
      (unless (or (= attributes *win32-invalid-file-attributes*)
                  (logtest attributes *win32-file-attribute-directory*))
        (error 'file-operation-failed :operation :delete :pathname pathname
               :reason :not-directory :message "Tree root is not a directory.")))
    (when (= (win32--get-file-attributes (win32--native pathname))
             *win32-invalid-file-attributes*)
      (let ((code (win32--get-last-error)))
        (when (eq (win32--failure-reason code) :missing)
          (if (eq if-does-not-exist :ignore)
              (return-from delete-directory-tree nil)
              (win32--operation-failure :delete pathname code)))))
    (loop for attempt from 1 to 100
          do (handler-case
                 (return (win32--remove-tree pathname))
               (file-error (condition)
                 (when (and (typep condition 'file-operation-failed)
                            (eq (file-operation-failed-reason condition) :missing)
                            (= (win32--get-file-attributes (win32--native pathname))
                               *win32-invalid-file-attributes*)
                            (eq if-does-not-exist :ignore))
                   (return nil))
                 (when (= attempt 100)
                   (error condition))
                 (sleep 0.1)))))
  #+(and sbcl (not win32))
  (apply #'uiop:delete-directory-tree pathname arguments)
  #-sbcl
  (posix--unsupported 'delete-directory-tree))
