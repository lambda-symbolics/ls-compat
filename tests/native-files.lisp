(in-package #:ls-compat/tests)

;;;; -- Native file lifecycle --

(ls-compat::-> tests--native-files () t)
(defun tests--native-files ()
  "Check private creation, exact replacement/deletion, validation and dot trees."
  (let* ((root     (ls-compat.posix:make-temporary-directory (uiop:temporary-directory) "native-"))
         (source   (merge-pathnames "source" root))
         (target   (merge-pathnames "target" root))
         (dot-file (merge-pathnames ".git/objects/.keep" root)))
    (unwind-protect
         (progn
           (tests--check (= #o700 (logand #o777 (file-mode root)))
                         "Temporary directory was not private.")
           (with-open-file (stream source :direction :output) (write-string "new" stream))
           (with-open-file (stream target :direction :output) (write-string "old" stream))
           (ls-compat.posix:replace-file source target)
           (tests--check (and (not (probe-file source))
                              (string= "new" (uiop:read-file-string target)))
                         "Replacement did not move the source over its target.")
           (ls-compat.posix:remove-file target)
           (tests--check
            (handler-case (progn (ls-compat.posix:remove-file target) nil)
              (file-operation-failed (condition)
                (and (eq :missing (file-operation-failed-reason condition))
                     (eq :delete (ls-compat.posix:file-operation-failed-operation condition)))))
            "Missing-file deletion lost its typed reason.")
           (ensure-directories-exist dot-file)
           (with-open-file (stream dot-file :direction :output) (write-string "keep" stream))
           (dolist (arguments (list nil (list :validate (lambda (path)
                                                         (declare (ignore path)) nil))))
             (tests--check
              (handler-case
                  (progn (apply #'ls-compat.posix:delete-directory-tree root arguments) nil)
                (error () t))
              "Tree deletion accepted absent or failed validation.")
             (tests--check (probe-file dot-file) "Failed validation mutated a dot tree."))
           (ls-compat.posix:delete-directory-tree root :validate t)
           (tests--check (not (probe-file root)) "Tree deletion omitted dot entries.")
           (ls-compat.posix:delete-directory-tree root :validate t :if-does-not-exist :ignore)
           (tests--check
            (handler-case
                (progn (ls-compat.posix:delete-directory-tree root :validate t) nil)
              (error () t))
            "Tree deletion ignored the default missing-path policy."))
      (ls-compat.posix:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

#+(and sbcl win32)
(ls-compat::-> tests--native-read-only-failures () t)
#+(and sbcl win32)
(defun tests--native-read-only-failures ()
  "Check failed deletion restores attributes while failed replacement does not."
  (let* ((root    (ls-compat.posix:make-temporary-directory (uiop:temporary-directory) "readonly-"))
         (target  (merge-pathnames "target" root))
         (missing (merge-pathnames "missing" root))
         (handle  nil))
    (unwind-protect
         (progn
           (with-open-file (stream target :direction :output) (write-string "keep" stream))
           (let* ((native     (ls-compat.posix::win32--native target))
                  (attributes (ls-compat.posix::win32--get-file-attributes native))
                  (read-only  (logior attributes ls-compat.posix::*win32-file-attribute-readonly*)))
             (tests--check (not (zerop (ls-compat.posix::win32--set-file-attributes native read-only)))
                           "Could not set the test read-only attribute.")
             ;; A handle with zero desired access never imposes a sharing restriction.
             (setf handle (ls-compat.posix::win32--create-file
                           native ls-compat.posix::*win32-generic-read* 0 nil
                           ls-compat.posix::*win32-open-existing* 0 0))
             (when (= handle ls-compat.posix::*win32-invalid-handle*)
               (error "Could not exclusively open the read-only test file."))
             (tests--check
              (handler-case (progn (ls-compat.posix:remove-file target) nil)
                (file-error () t))
              "Deletion succeeded despite an exclusive handle.")
             (tests--check (= read-only (ls-compat.posix::win32--get-file-attributes native))
                           "Failed deletion did not restore the original attributes.")
             (ls-compat.posix::win32--close-handle handle)
             (setf handle nil)
             (tests--check
              (handler-case (progn (ls-compat.posix:replace-file missing target) nil)
                (file-operation-failed () t))
              "Replacement accepted a missing source.")
             (tests--check (not (logtest (ls-compat.posix::win32--get-file-attributes native)
                                        ls-compat.posix::*win32-file-attribute-readonly*))
                           "Failed replacement restored the target read-only attribute.")
             (ls-compat.posix::win32--set-file-attributes native read-only)
             (ls-compat.posix:remove-file target)
             (tests--check (not (probe-file target)) "Read-only file deletion failed.")))
      (when handle (ls-compat.posix::win32--close-handle handle))
      (ls-compat.posix:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
