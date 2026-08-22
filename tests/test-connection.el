;;; test-connection.el --- Tests for clatter-connection -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'clatter-connection)

(ert-deftest clatter-test-tls-external-args-openssl-verifies ()
  "openssl s_client must abort on verify failure and check the hostname."
  (let ((clatter-tls-external-command "openssl"))
    (let ((args (clatter--tls-external-args "irc.example.com" 6697 nil)))
      (should (member "-verify_return_error" args))
      (should (member "-verify_hostname" args))
      (should (member "-servername" args)))))

(ert-deftest clatter-test-tls-external-args-gnutls-unchanged ()
  "gnutls-cli aborts on verification failure by default; keep args as-is."
  (let ((clatter-tls-external-command "gnutls-cli"))
    (let ((args (clatter--tls-external-args "irc.example.com" 6697 nil)))
      (should (member "--port" args))
      (should (equal (car (last args)) "irc.example.com")))))

;;; test-connection.el ends here
