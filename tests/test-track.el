;;; test-track.el --- Tests for clatter activity tracking -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-track)
(require 'clatter-ui)

(defmacro clatter-track-test--with-buffer (target &rest body)
  "Run BODY in a temporary clatter buffer for TARGET."
  (declare (indent 1))
  `(clatter-test-with-buffer
     (setq-local clatter--target ,target)
     (setq-local clatter--buffer-type
                 (if (string= ,target "*server*") 'server 'channel))
     ,@body))

(ert-deftest clatter-track-exclude-targets-omits-buffer-info ()
  "Excluded targets are absent from the tracker collection."
  (let ((clatter-track-exclude-targets '("#quiet")))
    (clatter-track-test--with-buffer "#quiet"
      (setq-local clatter--unread-count 2)
      (should-not (clatter-track--buffer-info (current-buffer)))
      (should-not (clatter-track--collect)))))

(ert-deftest clatter-track-muted-channels-remain-visible ()
  "Muted targets remain visible and use the muted tracker face."
  (let ((clatter-track-muted-channels '("#bots")))
    (clatter-track-test--with-buffer "#bots"
      (setq-local clatter--unread-count 1)
      (let* ((info (clatter-track--buffer-info (current-buffer)))
             (entry (clatter-track--format-entry info)))
        (should info)
        (should (plist-get info :muted))
        (should (eq (get-text-property 0 'face entry)
                    'clatter-track-muted))))))

(ert-deftest clatter-track-clears-only-the-selected-split-window ()
  "Selecting a visible chat clears it without clearing another split."
  (let ((first (generate-new-buffer " *clatter-track-first*"))
        (second (generate-new-buffer " *clatter-track-second*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (with-current-buffer first
            (clatter-mode)
            (setq-local clatter--target "#first")
            (setq-local clatter--unread-count 2))
          (with-current-buffer second
            (clatter-mode)
            (setq-local clatter--target "#second")
            (setq-local clatter--unread-count 3))
          (let* ((first-window (selected-window))
                 (second-window (split-window-right)))
            (set-window-buffer first-window first)
            (set-window-buffer second-window second)
            (select-window first-window)
            (clatter-track--window-change (selected-frame))
            (with-current-buffer first
              (should (zerop clatter--unread-count)))
            (with-current-buffer second
              (should (= clatter--unread-count 3)))
            ;; Merely changing an unselected split does not mark it read.
            (clatter-track--window-change second-window)
            (with-current-buffer second
              (should (= clatter--unread-count 3)))
            ;; Selecting that already-visible split clears it.
            (select-window second-window)
            (clatter-track--selection-change (selected-frame))
            (with-current-buffer second
              (should (zerop clatter--unread-count)))))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second)))))

(provide 'test-track)

;;; test-track.el ends here
