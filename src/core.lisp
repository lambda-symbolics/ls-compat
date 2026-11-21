(in-package #:ls-compat)

;;;; -- Types --

(deftype octet-vector ()
  "A vector whose elements are octets."
  '(vector (unsigned-byte 8)))

(deftype timeout-seconds ()
  "A timeout duration in seconds, or NIL when no deadline is requested."
  '(or null (real 0 *)))


;;;; -- Function types --

(defmacro -> (name argument-types result-type)
  "Declare NAME's function type using the compact signature syntax used here.

ECL retains the checked definitions without a declaration because its function
type syntax cannot represent these named keyword parameters. Other
implementations use Serapeum's portable declaration macro."
  #+ecl
  (declare (ignore name argument-types result-type))
  #+ecl
  nil
  #-ecl
  `(serapeum:-> ,name ,argument-types ,result-type))


;;;; -- Conditions --

(define-condition timeout-expired (error)
  ((seconds :initarg       :seconds
            :reader        timeout-expired-seconds
            :type          (real 0 *)
            :documentation "The requested timeout duration in seconds."))
  (:documentation "Signaled when a ls-compat timeout reaches its deadline.")
  (:report (lambda (condition stream)
             (format stream "Operation timed out after ~,3F seconds."
                     (timeout-expired-seconds condition)))))

(define-condition utf8-conversion-failed (error)
  ((direction :initarg       :direction
              :reader        utf8-conversion-failed-direction
              :type          (member :encode :decode)
              :documentation ":ENCODE for a string with an unencodable character, :DECODE for invalid octets.")
   (cause :initarg       :cause
          :initform      nil
          :reader        utf8-conversion-failed-cause
          :documentation "The implementation's own condition, when one was signaled."))
  (:documentation "Signaled when text cannot be converted to or from UTF-8.")
  (:report (lambda (condition stream)
             (format stream "~:[Octets are not valid UTF-8~;A string contains a character UTF-8 cannot encode~]~@[: ~A~]"
                     (eq (utf8-conversion-failed-direction condition) :encode)
                     (utf8-conversion-failed-cause condition)))))

(define-condition unsupported-operation (error)
  ((name :initarg       :name
         :reader        unsupported-operation-name
         :type          symbol
         :documentation "The unavailable ls-compat operation."))
  (:documentation "Signaled when an implementation cannot safely provide an operation.")
  (:report (lambda (condition stream)
             (format stream "~S is not supported by this Common Lisp implementation."
                     (unsupported-operation-name condition)))))


;;;; -- UTF-8 --

(-> utf8-string-to-octets
  (string &key (:start (or null (integer 0 *))) (:end (or null (integer 0 *))))
  octet-vector)
(defun utf8-string-to-octets (string &key start end)
  "Encode STRING as UTF-8 octets.

START and END delimit the portion of STRING to encode. A surrogate code point
has no UTF-8 encoding, so it signals UTF8-CONVERSION-FAILED instead of
producing invalid octets. SBCL uses its native encoder."
  (let ((start (or start 0))
        (end   (or end (length string))))
    #+sbcl
    (handler-case
        (sb-ext:string-to-octets string :external-format ':utf-8 :start start :end end)
      (error (condition)
        (error 'utf8-conversion-failed :direction :encode :cause condition)))
    #-sbcl
    (progn
      (when (find-if (lambda (character)
                       (<= #xD800 (char-code character) #xDFFF))
                     string :start start :end end)
        (error 'utf8-conversion-failed :direction :encode))
      (babel:string-to-octets string :encoding ':utf-8 :start start :end end))))

(-> utf8-octets-to-string
  (octet-vector &key (:start (or null (integer 0 *))) (:end (or null (integer 0 *))))
  string)
(defun utf8-octets-to-string (octets &key start end)
  "Decode UTF-8 OCTETS into a string.

START and END delimit the portion of OCTETS to decode. Invalid, overlong, or
surrogate-encoding sequences signal UTF8-CONVERSION-FAILED. SBCL uses its
native decoder."
  (let ((start (or start 0))
        (end   (or end (length octets))))
    (handler-case
        #+sbcl
        (sb-ext:octets-to-string octets :external-format ':utf-8 :start start :end end)
        #-sbcl
        (babel:octets-to-string octets :encoding ':utf-8 :start start :end end)
      (error (condition)
        (error 'utf8-conversion-failed :direction :decode :cause condition)))))

(-> utf8--measure
  (string &key (:maximum-bytes (or null (integer 0 *)))
               (:start (or null (integer 0 *))) (:end (or null (integer 0 *))))
  (values (integer 0 *) (integer 0 *)))
(defun utf8--measure (string &key maximum-bytes start end)
  "Return STRING's encoded length and the last index fitting MAXIMUM-BYTES.

Scan the entire selected range, even after the prefix stops fitting. Delegate
invalid input to the encoder to retain its condition and native cause."
  (let* ((start      (or start 0))
         (end        (or end (length string)))
         (prefix-end start)
         (octets     0))
    (unless (<= 0 start end (length string))
      (return-from utf8--measure
        (values (length (utf8-string-to-octets string :start start :end end)) start)))
    (loop for index from start below end
          for code = (char-code (char string index))
          for width = (cond
                        ((< code #x80)
                         1)
                        ((< code #x800)
                         2)
                        ((or (<= #xD800 code #xDFFF) (> code #x10FFFF))
                         (utf8-string-to-octets string :start start :end end)
                         (error 'utf8-conversion-failed :direction :encode))
                        ((< code #x10000)
                         3)
                        (t
                         4))
          do (incf octets width)
             (when (or (null maximum-bytes) (<= octets maximum-bytes))
               (setf prefix-end (1+ index))))
    (values octets prefix-end)))

(-> utf8-string-length
  (string &key (:start (or null (integer 0 *))) (:end (or null (integer 0 *))))
  (integer 0 *))
(defun utf8-string-length (string &key start end)
  "Return the UTF-8 octet length of STRING without allocating encoded octets.

START and END delimit the measured range, as in UTF8-STRING-TO-OCTETS.
Unencodable characters signal UTF8-CONVERSION-FAILED."
  (nth-value 0 (utf8--measure string :start start :end end)))

(-> utf8-string-prefix-end
  (string (integer 0 *) &key (:start (or null (integer 0 *)))
                            (:end (or null (integer 0 *))))
  (integer 0 *))
(defun utf8-string-prefix-end (string maximum-bytes &key start end)
  "Return the longest character prefix end fitting MAXIMUM-BYTES UTF-8 octets.

The returned index is relative to STRING, starting at START. END bounds the
selected range. Validate that entire range, including characters beyond the
returned index, and signal UTF8-CONVERSION-FAILED for unencodable input.
No encoded octets or prefix strings are allocated."
  (check-type maximum-bytes (integer 0 *))
  (nth-value 1 (utf8--measure string :maximum-bytes maximum-bytes
                                   :start start :end end)))


;;;; -- Floating point --

(-> finite-float-p (float) boolean)
(defun finite-float-p (number)
  "Return whether NUMBER is neither a NaN nor an infinity.

The comparison uses the standardized finite LONG-FLOAT bounds, so it does not
expose implementation-specific floating-point predicates. Arithmetic failures
while inspecting a non-finite value count as false."
  (handler-case
      (and (= number number)
           (<= (- most-positive-long-float) number)
           (<= number most-positive-long-float))
    (arithmetic-error ()
      nil)))


;;;; -- Internal real time --

(-> internal-real-time-seconds () double-float)
(defun internal-real-time-seconds ()
  "Return GET-INTERNAL-REAL-TIME in double-float seconds.

Use this process-local reading for elapsed time, not calendar timestamps."
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))


;;;; -- Timeouts --

(-> call-with-timeout (timeout-seconds function) t)
(defun call-with-timeout (seconds thunk)
  "Call THUNK with a deadline of SECONDS.

NIL disables the deadline. SBCL interrupts the current thread. CCL runs THUNK
in a worker process and terminates that process when the deadline expires.
Other implementations signal UNSUPPORTED-OPERATION rather than ignoring the
deadline."
  (check-type seconds timeout-seconds)
  (if (null seconds)
      (funcall thunk)
      #+sbcl
      (handler-case
          (sb-ext:with-timeout seconds
            (funcall thunk))
        (sb-ext:timeout ()
          (error 'timeout-expired :seconds seconds)))
      #+ccl
      (let ((semaphore (ccl:make-semaphore))
            (values nil)
            (failure nil))
        (let ((process
                (ccl:process-run-function
                 "ls-compat timeout worker"
                 (lambda ()
                   (handler-case
                       (setf values (multiple-value-list (funcall thunk)))
                     (error (condition)
                       (setf failure condition)))
                   (ccl:signal-semaphore semaphore)))))
          (if (ccl:timed-wait-on-semaphore semaphore seconds)
              (if failure
                  (error failure)
                  (values-list values))
              (progn
                (ccl:process-kill process)
                (error 'timeout-expired :seconds seconds)))))
      #-(or sbcl ccl)
      (error 'unsupported-operation :name 'call-with-timeout)))

(defmacro with-timeout (seconds &body body)
  "Evaluate BODY with a deadline of SECONDS.

Signals TIMEOUT-EXPIRED when the deadline is reached."
  `(call-with-timeout ,seconds (lambda () ,@body)))
