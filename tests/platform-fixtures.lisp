(in-package #:ls-compat/tests)

(declaim (special *test-failures*))

(defvar *test-skips* nil
  "Explicitly unsupported fixture contracts in this run.")

(ls-compat::-> tests--skip (symbol string) null)
(defun tests--skip (contract reason)
  "Record CONTRACT and its platform-specific REASON without hiding a failed check."
  (push (list contract reason) *test-skips*)
  (format t "SKIP ~A: ~A~%" contract reason)
  nil)

(ls-compat::-> tests--run-case (symbol) t)
(defun tests--run-case (test)
  "Run TEST, recording unexpected errors and reporting its actual outcome."
  (let ((before (length *test-failures*))
        (result nil))
    (handler-case (setf result (funcall test))
      (error (condition)
        (push (format nil "~A: ~A" test condition) *test-failures*)))
    (format t "~A ~A~%"
            (cond ((/= before (length *test-failures*))
                   "FAIL")
                  ((eq result ':skipped)
                   "SKIP")
                  (t
                   "PASS"))
            test))
  t)

(ls-compat::-> tests--sleep-command () list)
(defun tests--sleep-command ()
  "Return a real native sleeping child command for this host."
  #+win32
  '("powershell.exe" "-NoLogo" "-NoProfile" "-NonInteractive" "-Command"
    "Start-Sleep -Seconds 30")
  #-win32
  '("sleep" "30"))

(ls-compat::-> tests--powershell-literal (string) string)
(defun tests--powershell-literal (text)
  "Quote TEXT as one literal PowerShell string, including embedded apostrophes."
  (with-output-to-string (stream)
    (write-char #\' stream)
    (loop for character across text
          do (write-char character stream)
             (when (char= character #\') (write-char character stream)))
    (write-char #\' stream)))

(ls-compat::-> tests--make-symbolic-link (pathname pathname) t)
(defun tests--make-symbolic-link (target link)
  "Create a real native symbolic LINK to TARGET; creation failures are test failures."
  #+win32
  (uiop:run-program
   (list "powershell.exe" "-NoLogo" "-NoProfile" "-NonInteractive" "-Command"
         (format nil "$ErrorActionPreference='Stop'; New-Item -ItemType SymbolicLink -Path ~A -Target ~A | Out-Null"
                 (tests--powershell-literal (uiop:native-namestring link))
                 (tests--powershell-literal (uiop:native-namestring target)))))
  #-win32
  (uiop:run-program (list "ln" "-s" (uiop:native-namestring target)
                                  (uiop:native-namestring link)))
  t)
