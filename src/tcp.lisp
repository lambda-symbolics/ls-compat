(in-package #:ls-compat.tcp)

;;;; -- Types --

(deftype tcp-port ()
  "A valid TCP port number."
  '(integer 0 65535))


;;;; -- TCP lifecycle --

(ls-compat::-> tcp-connect
  (string tcp-port &key (:element-type t))
  t)
(defun tcp-connect (host port &key (element-type '(unsigned-byte 8)))
  "Open a TCP connection to HOST and PORT.

ELEMENT-TYPE controls the stream returned by TCP-STREAM."
  (usocket:socket-connect host port :element-type element-type))

(ls-compat::-> tcp-listen
  (string tcp-port &key (:backlog (integer 1 *)) (:reuse-address boolean)
         (:element-type t))
  t)
(defun tcp-listen (host port &key (backlog 16) (reuse-address t)
                            (element-type '(unsigned-byte 8)))
  "Listen for TCP connections on HOST and PORT.

ELEMENT-TYPE controls streams created for accepted sockets. A zero PORT requests
an operating-system-selected ephemeral port."
  (check-type backlog (integer 1 *))
  (usocket:socket-listen host port
                         :backlog       backlog
                         :reuse-address reuse-address
                         :element-type  element-type))

(ls-compat::-> tcp-accept (t &key (:element-type t)) t)
(defun tcp-accept (listener &key (element-type '(unsigned-byte 8)))
  "Accept one connection from LISTENER."
  (usocket:socket-accept listener :element-type element-type))

(ls-compat::-> tcp-stream (t) stream)
(defun tcp-stream (socket)
  "Return SOCKET's stream."
  (usocket:socket-stream socket))

(ls-compat::-> tcp-local-port (t) tcp-port)
(defun tcp-local-port (socket)
  "Return SOCKET's locally bound TCP port."
  (nth-value 1 (usocket:get-local-name socket)))

(ls-compat::-> close-tcp (t) null)
(defun close-tcp (socket)
  "Close SOCKET and return NIL."
  (usocket:socket-close socket)
  nil)
