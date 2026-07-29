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

(provide 'test-chathistory)

;;; test-chathistory.el ends here
