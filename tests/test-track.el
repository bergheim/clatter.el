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

(ert-deftest clatter-track-clear-all-includes-filtered-targets ()
  "Clear-all resets and records every target, including excluded ones."
  (let ((clatter-track-exclude-targets '("#hidden"))
        (clatter-track-muted-channels '("#muted"))
        recorded
        (updates 0))
    (unwind-protect
        (let ((hidden (clatter-get-or-create-buffer
                       "track-clear" "#hidden" 'channel))
              (muted (clatter-get-or-create-buffer
                      "track-clear" "#muted" 'channel)))
          (dolist (buffer (list hidden muted))
            (with-current-buffer buffer
              (setq-local clatter--unread-count 3)
              (setq-local clatter--has-mention t)))
          (cl-letf (((symbol-function 'clatter-read-state-record-buffer)
                     (lambda (buffer) (push buffer recorded)))
                    ((symbol-function 'clatter-track--update)
                     (lambda () (cl-incf updates))))
            (should (= (clatter-track-clear-all) 2)))
          (dolist (buffer (list hidden muted))
            (with-current-buffer buffer
              (should (zerop clatter--unread-count))
              (should-not clatter--has-mention)))
          (should (= updates 1))
          (should (equal (sort recorded
                               (lambda (a b)
                                 (string< (buffer-name a) (buffer-name b))))
                         (sort (list hidden muted)
                               (lambda (a b)
                                 (string< (buffer-name a) (buffer-name b)))))))
      (clatter-test-cleanup))))

(defmacro clatter-activity-test--cleanup (&rest body)
  "Run BODY then clean up clatter buffers and the activity list buffer."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (when-let* ((b (get-buffer "*clatter-activity*"))) (kill-buffer b))
     (clatter-test-cleanup)))

(ert-deftest clatter-activity-list-renders-tabulated-entries ()
  "`*clatter-activity*' is a tabulated list whose entry id is the clatter buffer."
  (let ((clatter-track-shorten 100)
        (clatter-track-exclude-targets nil)
        (clatter-track-muted-channels '("#muted")))
    (clatter-activity-test--cleanup
      (let ((alpha (clatter-get-or-create-buffer "act" "#alpha" 'channel))
            (beta (clatter-get-or-create-buffer "act" "#beta" 'channel))
            (bob (clatter-get-or-create-buffer "act" "bob" 'query))
            (muted (clatter-get-or-create-buffer "act" "#muted" 'channel)))
        (with-current-buffer alpha (setq clatter--unread-count 3))
        (with-current-buffer beta
          (setq clatter--unread-count 5 clatter--has-mention t))
        (with-current-buffer bob (setq clatter--unread-count 2))
        (with-current-buffer muted (setq clatter--unread-count 1))
        (clatter-track-list)
        (with-current-buffer (get-buffer "*clatter-activity*")
          (should (eq major-mode 'clatter-activity-mode))
          (should (eq revert-buffer-function #'tabulated-list-revert))
          (should (eq tabulated-list-entries #'clatter--activity-entries))
          ;; Mentions sort first, so the top entry resolves to #beta.
          (goto-char (point-min))
          (should (eq (tabulated-list-get-id) beta))
          ;; Status column surfaces mention, DM, and muted flags.
          (let ((contents (buffer-string)))
            (should (string-match-p "mention" contents))
            (should (string-match-p "DM" contents))
            (should (string-match-p "muted" contents)))
          ;; Revert drops buffers whose activity has been cleared.
          (with-current-buffer alpha (setq clatter--unread-count 0))
          (funcall revert-buffer-function nil t)
          (should-not (string-match-p "#alpha" (buffer-string)))
          (should (string-match-p "#beta" (buffer-string))))))))

(ert-deftest clatter-activity-jump-switches-to-buffer-and-clears ()
  "`clatter-activity-jump' selects the entry's clatter buffer and clears it."
  (let ((clatter-track-shorten 100)
        (clatter-track-exclude-targets nil)
        recorded)
    (clatter-activity-test--cleanup
      (cl-letf (((symbol-function 'clatter-read-state-record-buffer)
                 (lambda (buffer) (push buffer recorded))))
        (let ((chan (clatter-get-or-create-buffer "jump" "#chan" 'channel)))
          (with-current-buffer chan (setq clatter--unread-count 4))
          (clatter-track-list)
          (with-current-buffer (get-buffer "*clatter-activity*")
            (goto-char (point-min))
            (should (eq (tabulated-list-get-id) chan))
            (clatter-activity-jump))
          ;; `clatter-activity-jump' switches the selected window to chan and
          ;; clears its activity (`current-buffer' is restored by the
          ;; `with-current-buffer' above, so check the selected window).
          (should (eq (window-buffer (selected-window)) chan))
          (with-current-buffer chan (should (zerop clatter--unread-count)))
          (should (memq chan recorded)))))))

(ert-deftest clatter-activity-clear-clears-entry-at-point ()
  "`clatter-activity-clear' clears the entry at point and refreshes the list."
  (let ((clatter-track-shorten 100)
        (clatter-track-exclude-targets nil)
        recorded)
    (clatter-activity-test--cleanup
      (cl-letf (((symbol-function 'clatter-read-state-record-buffer)
                 (lambda (buffer) (push buffer recorded))))
        (let ((first (clatter-get-or-create-buffer "clear" "#first" 'channel))
              (second (clatter-get-or-create-buffer "clear" "#second" 'channel)))
          ;; Give #first a higher unread count so it sorts above #second,
          ;; making it the entry at point after `goto-char (point-min)'.
          (with-current-buffer first (setq clatter--unread-count 5))
          (with-current-buffer second (setq clatter--unread-count 1))
          (clatter-track-list)
          (with-current-buffer (get-buffer "*clatter-activity*")
            (goto-char (point-min))
            (should (eq (tabulated-list-get-id) first))
            (clatter-activity-clear)
            (should (with-current-buffer first (zerop clatter--unread-count)))
            (should (memq first recorded))
            (should-not (string-match-p "#first" (buffer-string)))
            (should (string-match-p "#second" (buffer-string)))))))))

(ert-deftest clatter-activity-mute-and-unmute-toggle-target-at-point ()
  "`clatter-activity-mute' and `clatter-activity-unmute' toggle the point target."
  (let ((clatter-track-shorten 100)
        (clatter-track-exclude-targets nil)
        (clatter-track-muted-channels nil))
    (clatter-activity-test--cleanup
      (let ((chan (clatter-get-or-create-buffer "mute" "#chan" 'channel)))
        (with-current-buffer chan (setq clatter--unread-count 2))
        (clatter-track-list)
        (with-current-buffer (get-buffer "*clatter-activity*")
          (goto-char (point-min))
          (should (eq (tabulated-list-get-id) chan))
          (clatter-activity-mute)
          (should (member "#chan" clatter-track-muted-channels))
          (should (string-match-p "muted" (buffer-string)))
          (clatter-activity-unmute)
          (should-not (member "#chan" clatter-track-muted-channels))
          (should-not (string-match-p "muted" (buffer-string))))))))

;;; clatter-track-switch return-to-origin

(ert-deftest clatter-track-switch-returns-to-origin-on-exhaustion ()
  "After visiting the last active buffer, the next switch returns to origin."
  (let ((clatter-track-switch-return-to-origin t)
        (clatter-track-shorten 100))
    (clatter-activity-test--cleanup
      (cl-letf (((symbol-function 'clatter-read-state-record-buffer)
                 (lambda (_buffer))))
        (let ((origin (generate-new-buffer " *origin*"))
              (chan (clatter-get-or-create-buffer "sw" "#chan" 'channel)))
          (with-current-buffer chan (setq clatter--unread-count 2))
          (unwind-protect
              (progn
                (switch-to-buffer origin)
                (setq clatter-track--switch-origin nil)
                ;; First switch jumps to the active buffer and captures origin.
                (clatter-track-switch)
                (should (eq (window-buffer (selected-window)) chan))
                (should (eq clatter-track--switch-origin origin))
                ;; Activity is now exhausted: return to origin.
                (clatter-track-switch)
                (should (eq (window-buffer (selected-window)) origin))
                (should (null clatter-track--switch-origin)))
            (kill-buffer origin)))))))

(ert-deftest clatter-track-switch-no-activity-when-nothing-active ()
  "With no active buffers and no prior switch, it messages and stays put."
  (let ((clatter-track-switch-return-to-origin t))
    (clatter-activity-test--cleanup
      (let ((origin (generate-new-buffer " *origin*")))
        (unwind-protect
            (progn
              (switch-to-buffer origin)
              (setq clatter-track--switch-origin nil)
              (clatter-track-switch)
              (should (eq (window-buffer (selected-window)) origin))
              (should (null clatter-track--switch-origin)))
          (kill-buffer origin))))))

(ert-deftest clatter-track-switch-cycles-then-returns-to-origin ()
  "Multiple active buffers are visited by priority, then origin is restored."
  (let ((clatter-track-switch-return-to-origin t)
        (clatter-track-shorten 100))
    (clatter-activity-test--cleanup
      (cl-letf (((symbol-function 'clatter-read-state-record-buffer)
                 (lambda (_buffer))))
        (let ((origin (generate-new-buffer " *origin*"))
              (alpha (clatter-get-or-create-buffer "sw" "#alpha" 'channel))
              (beta (clatter-get-or-create-buffer "sw" "#beta" 'channel)))
          (with-current-buffer alpha (setq clatter--unread-count 5))
          (with-current-buffer beta (setq clatter--unread-count 1))
          (unwind-protect
              (progn
                (switch-to-buffer origin)
                (setq clatter-track--switch-origin nil)
                ;; Higher unread first.
                (clatter-track-switch)
                (should (eq (window-buffer (selected-window)) alpha))
                (clatter-track-switch)
                (should (eq (window-buffer (selected-window)) beta))
                ;; Exhausted: return to origin.
                (clatter-track-switch)
                (should (eq (window-buffer (selected-window)) origin))
                (should (null clatter-track--switch-origin)))
            (kill-buffer origin)))))))

(provide 'test-track)

;;; test-track.el ends here
