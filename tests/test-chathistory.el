;;; test-chathistory.el --- Tests for clatter chathistory -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-chathistory)

(ert-deftest clatter-chathistory-batch-tracks-latest-timestamp ()
  "Completed playback advances the history cursor to its newest message."
  (let ((conn (clatter-test-make-connection "testnet" "testnick"))
        (clatter-read-state-enabled nil)
        buf)
    (unwind-protect
        (let ((old (encode-time 0 0 12 1 1 2026 t))
              (latest (encode-time 0 1 12 1 1 2026 t)))
          (setq buf (clatter-get-or-create-buffer "testnet" "#emacs" 'channel))
          (clatter-chathistory--track-batch-timestamp
           conn "chathistory" "#emacs"
           (list (list :time old)
                 (list :time latest)))
          (with-current-buffer buf
            (should (equal clatter-chathistory--last-timestamp latest))))
      (clatter-test-cleanup)
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest clatter-chathistory-batch-ignores-nil-target ()
  "A target-less batch (e.g. soju.im/bouncer-networks) is a no-op.
It must not error on (downcase nil) and must not touch any buffer."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (progn
          ;; No error, no crash, no buffer created.
          (clatter-chathistory--track-batch-timestamp
           conn "soju.im/bouncer-networks" nil
           (list (list :time (encode-time 0 0 12 1 1 2026 t))))
          (should-not (clatter-get-buffer "testnet" nil)))
      (clatter-test-cleanup))))


;; --- CHATHISTORY TARGETS tests ---

(ert-deftest clatter-chathistory-fetch-targets-sends-command ()
  "fetch-targets sends CHATHISTORY TARGETS with the limit."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory-fetch-targets conn 50)
          (should (clatter-test-sent-matching
                   "^CHATHISTORY TARGETS timestamp=0001-01-01T00:00:00.000Z timestamp=9999-12-31T23:59:59.999Z 50$")))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-fetch-targets-no-cap-no-op ()
  "fetch-targets is a no-op when chathistory cap is not enabled."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory-fetch-targets conn)
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-on-welcome-sends-targets ()
  "Welcome hook fires TARGETS request when chathistory is available."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-welcome conn "testnick")
          (should (clatter-test-sent-matching
                   "^CHATHISTORY TARGETS ")))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-on-welcome-disabled-no-op ()
  "Welcome hook does nothing when chathistory is disabled."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory")))
        (clatter-chathistory-enabled nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-welcome conn "testnick")
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-fetches-latest-for-new-dm ()
  "TARGETS batch with a new DM target triggers CHATHISTORY LATEST."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory-targets" nil
           (list (list :target "alcor" :time (encode-time 0 0 12 1 1 2026 t))))
          (should (clatter-test-sent-matching
                   "^CHATHISTORY LATEST alcor \\* 50$")))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-fetches-since-for-existing-dm ()
  "TARGETS batch with an existing DM buffer triggers CHATHISTORY AFTER."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory")))
        (clatter-read-state-enabled nil)
        buf)
    (unwind-protect
        (progn
          (setq buf (clatter-get-or-create-buffer "testnet" "alcor" 'query))
          (with-current-buffer buf
            (setq clatter-chathistory--last-timestamp
                  (encode-time 0 0 10 1 1 2026 t)))
          (clatter-test-with-mock-send
            (clatter-chathistory--on-targets-batch
             conn "chathistory-targets" nil
             (list (list :target "alcor" :time (encode-time 0 0 12 1 1 2026 t))))
            (should (clatter-test-sent-matching
                     "^CHATHISTORY AFTER alcor timestamp="))))
      (clatter-test-cleanup)
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest clatter-chathistory-targets-batch-skips-channels ()
  "TARGETS batch skips channel targets (channels are handled on JOIN)."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory-targets" nil
           (list (list :target "#emacs" :time (encode-time 0 0 12 1 1 2026 t))))
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-skips-own-nick ()
  "TARGETS batch skips entries where the target is our own nick."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory")
               "testnet" "testnick")))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory-targets" nil
           (list (list :target "testnick" :time (encode-time 0 0 12 1 1 2026 t))))
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-multiple-dm-targets ()
  "TARGETS batch with multiple DM targets fetches history for each."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory-targets" nil
           (list (list :target "alcor" :time (encode-time 0 0 12 1 1 2026 t))
                 (list :target "jazzah" :time (encode-time 0 0 11 1 1 2026 t))
                 (list :target "#emacs" :time (encode-time 0 0 10 1 1 2026 t))))
          (should (clatter-test-sent-matching "^CHATHISTORY LATEST alcor "))
          (should (clatter-test-sent-matching "^CHATHISTORY LATEST jazzah "))
          ;; Channel target should NOT trigger a fetch
          (should-not (clatter-test-sent-matching "^CHATHISTORY.*#emacs")))
      (clatter-test-cleanup))))

(ert-deftest clatter-chathistory-targets-batch-empty-messages ()
  "TARGETS batch with no messages is a harmless no-op."
  (let ((conn (clatter-test-make-connection-with-caps
               '("server-time" "batch" "message-tags" "chathistory"))))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-chathistory--on-targets-batch
           conn "chathistory-targets" nil nil)
          (should-not clatter-test--sent-lines))
      (clatter-test-cleanup))))

(provide 'test-chathistory)

;;; test-chathistory.el ends here
