;;; test-cap.el --- Tests for clatter-cap.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)

(ert-deftest clatter-test-get-password-prefers-explicit-password ()
  "Explicit network :password wins over auth-source."
  (let ((clatter-networks
         '(("znc"
            :server "192.0.2.1"
            :port 7777
            :nick "testnick"
            :password "explicit"))))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _)
                 (list (list :secret "auth-source")))))
      (should (equal (clatter-get-password "znc") "explicit")))))

(ert-deftest clatter-test-get-password-searches-network-id-and-port ()
  "Auth-source lookup can match the Clatter network id and port."
  (let ((clatter-networks
         '(("znc"
            :server "192.0.2.1"
            :port 7777
            :nick "testnick")))
        calls)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest plist)
                 (push plist calls)
                 (when (and (equal (plist-get plist :host) "znc")
                            (equal (plist-get plist :port) "7777")
                            (equal (plist-get plist :user) "testnick"))
                   (list (list :secret "from-network-id"))))))
      (should (equal (clatter-get-password "znc") "from-network-id"))
      (should calls))))

(ert-deftest clatter-test-get-password-uses-default-port ()
  "Auth-source lookup includes `clatter-default-port' when no port is set."
  (let ((clatter-networks
         '(("znc"
            :server "znc.example"
            :nick "testnick")))
        (clatter-default-port 6697))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest plist)
                 (when (and (equal (plist-get plist :host) "znc.example")
                            (equal (plist-get plist :port) "6697")
                            (equal (plist-get plist :user) "testnick"))
                   (list (list :secret "from-default-port"))))))
      (should (equal (clatter-get-password "znc") "from-default-port")))))

(ert-deftest clatter-test-get-password-prefers-live-config-password ()
  "Explicit :password from a live config wins over saved network config."
  (let ((clatter-networks
         '(("znc"
            :server "znc.example"
            :nick "testnick"
            :password "saved"))))
    (should (equal (clatter-get-password
                    "znc"
                    '(:server "temporary.example"
                      :nick "testnick"
                      :password "live"))
                   "live"))))

(ert-deftest clatter-test-registration-sends-auth-source-pass ()
  "Registration sends PASS from auth-source before NICK and USER."
  (let* ((clatter-networks
          '(("znc"
             :server "znc.example"
             :nick "testnick")))
         (conn (clatter-test-make-connection "znc" "testnick"))
         (proc (make-pipe-process :name "clatter-test-cap"
                                  :buffer nil)))
    (unwind-protect
        (progn
          (setf (clatter-connection-process conn) proc)
          (process-put proc :clatter-config
                       '(:username "testnick" :realname "Test User"))
          (cl-letf (((symbol-function 'auth-source-search)
                     (lambda (&rest _)
                       (list (list :secret "znc-password")))))
            (clatter-test-with-mock-send
             (clatter-cap--send-registration conn)
             (should (equal (nreverse clatter-test--sent-lines)
                            '("PASS znc-password"
                              "NICK testnick"
                              "USER testnick 0 * :Test User"))))))
      (delete-process proc)
      (clatter-test-cleanup))))

(ert-deftest clatter-test-registration-prefers-live-config-password ()
  "Registration sends PASS from the live process config when present."
  (let* ((clatter-networks
          '(("znc"
             :server "znc.example"
             :nick "testnick"
             :password "saved")))
         (conn (clatter-test-make-connection "znc" "testnick"))
         (proc (make-pipe-process :name "clatter-test-cap"
                                  :buffer nil)))
    (unwind-protect
        (progn
          (setf (clatter-connection-process conn) proc)
          (process-put proc :clatter-config
                       '(:server "temporary.example"
                         :nick "testnick"
                         :username "testnick"
                         :realname "Test User"
                         :password "live"))
          (clatter-test-with-mock-send
           (clatter-cap--send-registration conn)
           (should (equal (car (nreverse clatter-test--sent-lines))
                          "PASS live"))))
      (delete-process proc)
      (clatter-test-cleanup))))

(ert-deftest clatter-test-connect-refuses-without-sasl-password ()
  "A :sasl plain connection with no available password refuses, not loops.
auth-source returning nothing must not let clatter silently register
anonymous and reconnect forever.  No process is spawned and no connection
struct is registered."
  (let ((clatter-networks
         '(("libera"
            :server "irc.libera.chat"
            :port 6697
            :nick "testnick"
            :sasl plain)))
        (clatter-default-port 6697)
        (spawned 0))
    (unwind-protect
        (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil))
                  ((symbol-function 'clatter--watchdog) (lambda (&rest _)))
                  ((symbol-function 'make-network-process)
                   (lambda (&rest _)
                     (cl-incf spawned)
                     (error "make-network-process should not be called")))
                  ((symbol-function 'clatter--connect-external)
                   (lambda (&rest _)
                     (cl-incf spawned)
                     (error "clatter--connect-external should not be called"))))
          (should-not (clatter-connect "libera"))
          (should (= spawned 0))
          (should-not (clatter-get-connection "libera")))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-connect-refuse-disables-existing-reconnect ()
  "Refusing on a reconnect (existing conn) disables its auto-reconnect.
This is the loop that the user reported: the password later becomes
unavailable, the connection drops, and reconnect would otherwise keep
retrying a failure only the user can fix."
  (let ((clatter-networks
         '(("libera"
            :server "irc.libera.chat"
            :port 6697
            :nick "testnick"
            :sasl plain)))
        (clatter-default-port 6697))
    (unwind-protect
        (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil))
                  ((symbol-function 'clatter--watchdog) (lambda (&rest _))))
          (let ((conn (clatter-test-make-connection "libera" "testnick")))
            (setf (clatter-connection-reconnect-enabled conn) t)
            (should-not (clatter-connect "libera"))
            (should (clatter-get-connection "libera")) ; struct still exists
            (should-not (clatter-connection-reconnect-enabled conn))
            (should (eq (clatter-connection-state conn) :disconnected))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-connect-with-explicit-sasl-password-spawns ()
  "An explicit :password lets a :sasl plain connection proceed normally."
  (let ((clatter-networks
         '(("libera"
            :server "irc.libera.chat"
            :port 6697
            :nick "testnick"
            :password "secret"
            :sasl plain)))
        (clatter-default-port 6697)
        (spawned 0))
    (unwind-protect
        (cl-letf (((symbol-function 'clatter--watchdog) (lambda (&rest _)))
                  ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
                  ((symbol-function 'make-network-process)
                   (lambda (&rest _)
                     (cl-incf spawned)
                     (make-pipe-process :name "clatter-fake" :buffer nil))))
          (should (clatter-connect "libera"))
          (should (= spawned 1)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-sasl-plain-skipped-on-plaintext ()
  "SASL PLAIN is not requested on a plaintext connection."
  (let ((clatter-networks
         '(("plain"
            :server "irc.example"
            :nick "testnick"
            :password "secret"
            :sasl plain)))
        (conn (clatter-test-make-connection "plain" "testnick"))
        (proc (make-pipe-process :name "clatter-test-cap" :buffer nil)))
    (unwind-protect
        (progn
          (setf (clatter-connection-process conn) proc)
          (process-put proc :clatter-config
                       '(:server "irc.example" :tls nil :sasl plain))
          (cl-letf (((symbol-function 'clatter--debug) (lambda (&rest _))))
            (clatter-test-with-mock-send
             (clatter-cap--handle-ls conn "sasl")
             (should-not (clatter-test-sent-matching "CAP REQ.*sasl")))))
      (delete-process proc)
      (clatter-test-cleanup))))

(ert-deftest clatter-test-sasl-plain-requested-on-tls ()
  "SASL PLAIN is requested when the connection uses TLS."
  (let ((clatter-networks
         '(("tls"
            :server "irc.example"
            :nick "testnick"
            :password "secret"
            :sasl plain)))
        (conn (clatter-test-make-connection "tls" "testnick"))
        (proc (make-pipe-process :name "clatter-test-cap" :buffer nil)))
    (unwind-protect
        (progn
          (setf (clatter-connection-process conn) proc)
          (process-put proc :clatter-config
                       '(:server "irc.example" :tls t :sasl plain))
          (cl-letf (((symbol-function 'clatter--debug) (lambda (&rest _))))
            (clatter-test-with-mock-send
             (clatter-cap--handle-ls conn "sasl")
             (should (clatter-test-sent-matching "CAP REQ.*sasl")))))
      (delete-process proc)
      (clatter-test-cleanup))))

(ert-deftest clatter-test-scram-continue-aborts-on-bad-server-proof ()
  "An invalid SCRAM server signature aborts the connection (RFC 5802 s5)."
  (let ((conn (clatter-connection--create :network-id "t" :process 'fake))
        (state (clatter-scram-state--create :auth-message "m"
                                            :salted-password "sp"))
        aborted)
    (cl-letf (((symbol-function 'process-get) (lambda (_p _k) state))
              ((symbol-function 'clatter-scram-verify-server) (lambda (&rest _) nil))
              ((symbol-function 'clatter--connect-abort)
               (lambda (_proc reason) (setq aborted reason))))
      (clatter-cap--scram-continue conn "dj13ZA==")
      (should aborted)
      (should (string-match-p "MITM" aborted)))))

(ert-deftest clatter-test-scram-continue-accepts-valid-server-proof ()
  "A valid SCRAM server signature leaves the connection alone."
  (let ((conn (clatter-connection--create :network-id "t" :process 'fake))
        (state (clatter-scram-state--create :auth-message "m"
                                            :salted-password "sp"))
        abort-called)
    (cl-letf (((symbol-function 'process-get) (lambda (_p _k) state))
              ((symbol-function 'clatter-scram-verify-server) (lambda (&rest _) t))
              ((symbol-function 'clatter--connect-abort)
               (lambda (&rest _) (setq abort-called t))))
      (clatter-cap--scram-continue conn "dj13ZA==")
      (should-not abort-called))))

(provide 'test-cap)
;;; test-cap.el ends here
