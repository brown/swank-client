# Swank Client

Swank Client is a Common Lisp implementation of the client side of the Swank
debugging protocol used by [Slime](https://en.wikipedia.org/wiki/SLIME), a [GNU
Emacs](https://www.gnu.org/software/emacs) mode that implements an IDE for Lisp
programming.  Emacs uses the Swank protocol to communicate with a Lisp system
when a user runs the IDE, but the protocol is useful independently of Emacs
because it allows a client to evaluate expressions on a remote Lisp that's
running a Swank server.

Swank Client is used by [Swank Crew](https://github.com/brown/swank-crew) to
implement a Slime IDE for developing distributed algorithms in Lisp.

## The Swank Client API

#### slime-connect host-name port &optional connection-closed-hook

```
Connects to the Swank server running on HOST-NAME that is listening on PORT.
Returns a SWANK-CONNECTION if the connection attempt is successful.  Otherwise,
returns NIL.  May signal SLIME-NETWORK-ERROR if the user has a Slime secret
file and there are network problems sending its contents to the remote Swank
server.  If provided, function CONNECTION-CLOSED-HOOK is called when the
connection is closed.
```

#### slime-close connection

```
Closes CONNECTION to a Swank server.
```

#### slime-eval sexp connection

```
Sends SEXP over CONNECTION to a Swank server for evaluation and waits for the
result.  When the result is received, it is returned.  Signals
SLIME-NETWORK-ERROR when there are network problems sending SEXP.
```

#### slime-eval-async sexp connection &optional continuation

```
Sends SEXP over CONNECTION to a Swank server for evaluation, then immediately
returns.  Some time later, after the evaluation is finished, CONTINUATION is
called with the result as argument.  Signals SLIME-NETWORK-ERROR when there are
network problems sending SEXP.
```

#### slime-migrate-evals old-connection new-connection

```
Evaluates on NEW-CONNECTION all the work pending on a closed OLD-CONNECTION.
Signals SLIME-NETWORK-ERROR when there are network problems.
```

#### slime-pending-evals-p connection

```
Returns T if there are outstanding evaluations pending on CONNECTION;
otherwise, returns NIL.
```

#### with-slime-connection (variable host-name port &optional connection-closed-hook) &body body

```
Wraps BODY in a LET form where VARIABLE is bound to the value returned by
(SLIME-CONNECT HOST-NAME PORT CONNECTION-CLOSED-HOOK).  Arranges for the Swank
connection to be closed when control exits BODY.
```

For more information, see the documentation strings in
[swank-client.lisp](https://github.com/brown/swank-client/blob/master/swank-client.lisp)
and the example code in
[swank-client-test.lisp](https://github.com/brown/swank-client/blob/master/swank-client-test.lisp).

## Swank Client example

### Starting a Swank server

The code below starts two Swank servers, one listening on port 4005 and the
other listening on port 10000.

```
(load-quicklisp)
(asdf:load-system 'com.google.base)
(asdf:load-system 'swank)

(defvar *emacs-port* 4005)
(defvar *swank-client-port* 10000)

(defun swank-thread ()
  "Returns a thread that's acting as a Swank server."
  (dolist (thread (sb-thread:list-all-threads))
    (when (com.google.base:prefixp "Swank" (sb-thread:thread-name thread))
      (return thread))))

(defun wait-for-swank-thread ()
  "Wait for the Swank server thread to exit."
  (let ((swank-thread (swank-thread)))
    (when swank-thread
      (sb-thread:join-thread swank-thread))))

(defun main ()
  (setf swank:*configure-emacs-indentation* nil
        swank::*enable-event-history* nil
        swank:*log-events* t)
  (swank:create-server :port *emacs-port* :dont-close t)
  (swank:create-server :port *swank-client-port* :dont-close t)
  (wait-for-swank-thread))

(main)
```

### Using Swank Client to evaluate an expression on the server

Once the Swank servers are running, you can connect to the server on port 4005
from Emacs using the command ```M-x slime-connect```.  This connection is a
normal Slime IDE session.  From the Slime IDE you can evaluate the following
code, which creates a Swank Client connection to the server running on port
10000 and remotely evaluates the expression ```(cons 1 2)```.

```
(load-quicklisp)
(asdf:load-system 'swank-client)

(swank-client:with-slime-connection (connection "localhost" 10000)
  (swank-client:slime-eval '(cons 1 2) connection))
```
