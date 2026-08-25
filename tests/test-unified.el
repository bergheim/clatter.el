;;; test-unified.el --- Tests for clatter-unified.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-ui)
(require 'clatter-pals)
(require 'clatter-unified)

;;; Helpers

(defun clatter-test-unified--buffer ()
  "Return the live unified buffer, or nil."
  (let ((buf (get-buffer clatter-unified--buffer-name)))
    (and (buffer-live-p buf) buf)))

(defun clatter-test-unified--kill-buffer ()
  "Kill the unified buffer if it exists."
  (let ((buf (get-buffer clatter-unified--buffer-name)))
    (when (buffer-live-p buf)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(defmacro clatter-test-unified--with-capture (conn &rest body)
  "Run BODY with CONN bound to a mock connection, then clean up.

Removes the unified buffer and any clatter buffers BODY created, and
unregisters the unified hooks whether or not BODY enabled them."
  (declare (indent 1))
  `(let ((initial-buffers clatter--buffer-alist)
         (,conn (clatter-test-make-connection)))
     (clatter-test-unified--kill-buffer)
     (unwind-protect
         (progn ,@body)
       (clatter-unified-disable)
       (clatter-test-unified--kill-buffer)
       (dolist (entry clatter--buffer-alist)
         (when (and (not (memq entry initial-buffers))
                    (buffer-live-p (cdr entry)))
           (kill-buffer (cdr entry))))
       (setq clatter--buffer-alist initial-buffers)
       (clatter-test-cleanup))))

(defun clatter-test-unified--line-bol (buffer text)
  "Return the bol of the line in BUFFER whose text contains TEXT, or nil."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward text nil t)
        (beginning-of-line)
        (point)))))

(defun clatter-test-unified--nick-column-at (buffer bol)
  "Return the visible nick-column text at BOL in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char bol)
      (buffer-substring-no-properties
       bol (min (+ bol clatter-nick-column-width) (line-end-position))))))

(defun clatter-test-unified--string (buffer)
  "Return the unpropertized contents of BUFFER."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun clatter-test-unified--count (buffer regexp)
  "Return the number of lines in BUFFER matching REGEXP."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward regexp nil t)
          (setq count (1+ count)))
        count))))

(defun clatter-test-unified--separator-pos (buffer label)
  "Return the position of the LABEL text inside a separator line in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat "──[^\n]*" (regexp-quote label)) nil t)
        (match-beginning 0)))))

;;; Hook registration

(ert-deftest clatter-test-unified-enable-registers-hooks ()
  "Enabling adds both live message hooks; disabling removes them."
  (unwind-protect
      (progn
        (clatter-unified-enable)
        (should (memq #'clatter-unified--on-privmsg
                      (default-value 'clatter-privmsg-hook)))
        (should (memq #'clatter-unified--on-action
                      (default-value 'clatter-action-hook)))
        (clatter-unified-disable)
        (should-not (memq #'clatter-unified--on-privmsg
                          (default-value 'clatter-privmsg-hook)))
        (should-not (memq #'clatter-unified--on-action
                          (default-value 'clatter-action-hook))))
    (clatter-unified-disable)))

(ert-deftest clatter-test-unified-disabled-by-default ()
  "The unified buffer is opt-in."
  (should-not (default-value 'clatter-unified-enabled)))

;;; Rendering

(ert-deftest clatter-test-unified-channel-privmsg-renders ()
  "A channel PRIVMSG creates the buffer lazily and renders with a separator."
  (clatter-test-unified--with-capture conn
    (should-not (clatter-test-unified--buffer))
    (clatter-unified--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "hello there" nil)
    (let ((buf (clatter-test-unified--buffer)))
      (should buf)
      (let ((text (clatter-test-unified--string buf)))
        (should (string-match-p "──[^\n]*testnet/#emacs" text))
        (should (string-match-p "<alice>" text))
        (should (string-match-p "hello there" text)))
      ;; The separator is doc-faced and carries no sender, so grouping
      ;; and navigation both skip it.
      (let ((sep (clatter-test-unified--separator-pos buf "testnet/#emacs")))
        (should sep)
        (with-current-buffer buf
          (should (memq 'font-lock-doc-face
                        (ensure-list (get-text-property sep 'face))))
          (should-not (get-text-property sep 'clatter-sender))))
      ;; The message line carries its source for jump-back.
      (let ((bol (clatter-test-unified--line-bol buf "hello there")))
        (should bol)
        (with-current-buffer buf
          (should (equal (get-text-property bol 'clatter-unified-network)
                         "testnet"))
          (should (equal (get-text-property bol 'clatter-unified-target)
                         "#emacs"))
          (should (equal (get-text-property bol 'clatter-sender) "alice"))
          (should (eq (get-text-property bol 'clatter-msg-type) 'privmsg)))))))

(ert-deftest clatter-test-unified-action-renders ()
  "A CTCP ACTION renders with action formatting and its source props."
  (clatter-test-unified--with-capture conn
    (clatter-unified--on-action conn '("alice" "user" "host") "#emacs"
                                "waves" nil)
    (let* ((buf (clatter-test-unified--buffer))
           (bol (and buf (clatter-test-unified--line-bol buf "waves"))))
      (should buf)
      (should bol)
      (should (string-match-p "\\*" (clatter-test-unified--nick-column-at
                                     buf bol)))
      (with-current-buffer buf
        (should (eq (get-text-property bol 'clatter-msg-type) 'action))
        (should (equal (get-text-property bol 'clatter-unified-target)
                       "#emacs"))))))

;;; Grouping and separators

(ert-deftest clatter-test-unified-groups-consecutive-same-sender ()
  "Two messages from one nick in one channel show the nick once."
  (let ((clatter-group-messages-by-nick t))
    (clatter-test-unified--with-capture conn
      (clatter-unified--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "first" nil)
      (clatter-unified--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "second" nil)
      (let* ((buf (clatter-test-unified--buffer))
             (first-bol (clatter-test-unified--line-bol buf "first"))
             (second-bol (clatter-test-unified--line-bol buf "second")))
        (should first-bol)
        (should second-bol)
        (should (string-match-p
                 "<alice>" (clatter-test-unified--nick-column-at
                            buf first-bol)))
        (should (string-match-p
                 "\\` *\\'" (clatter-test-unified--nick-column-at
                             buf second-bol)))
        ;; The blanked line keeps its sender metadata.
        (with-current-buffer buf
          (should (equal (get-text-property second-bol 'clatter-sender)
                         "alice")))
        ;; One channel run, one separator.
        (should (= 1 (clatter-test-unified--count
                      buf "──[^\n]*testnet/#emacs")))))))

(ert-deftest clatter-test-unified-separator-breaks-grouping-across-channels ()
  "The same nick alternating between channels gets a separator each time."
  (let ((clatter-group-messages-by-nick t))
    (clatter-test-unified--with-capture conn
      (clatter-unified--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "first" nil)
      (clatter-unified--on-privmsg conn '("alice" "user" "host") "#lisp"
                                   "second" nil)
      (clatter-unified--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   "third" nil)
      (let ((buf (clatter-test-unified--buffer)))
        ;; Each source change emits its own separator.
        (should (= 2 (clatter-test-unified--count
                      buf "──[^\n]*testnet/#emacs")))
        (should (= 1 (clatter-test-unified--count
                      buf "──[^\n]*testnet/#lisp")))
        ;; The separator broke the burst: every line shows its nick.
        (dolist (text '("first" "second" "third"))
          (let ((bol (clatter-test-unified--line-bol buf text)))
            (should bol)
            (should (string-match-p
                     "<alice>" (clatter-test-unified--nick-column-at
                                buf bol)))))
        ;; Each line points back at its own channel.
        (with-current-buffer buf
          (should (equal (get-text-property
                          (clatter-test-unified--line-bol buf "second")
                          'clatter-unified-target)
                         "#lisp"))
          (should (equal (get-text-property
                          (clatter-test-unified--line-bol buf "third")
                          'clatter-unified-target)
                         "#emacs")))))))

(ert-deftest clatter-test-unified-query-keyed-by-sender ()
  "A message addressed to my nick is keyed by the sender's nick."
  (clatter-test-unified--with-capture conn
    ;; TARGET is my nick, so the source is the query with alice.
    (clatter-unified--on-privmsg conn '("alice" "user" "host") "testnick"
                                 "psst" nil)
    (let* ((buf (clatter-test-unified--buffer))
           (bol (and buf (clatter-test-unified--line-bol buf "psst"))))
      (should buf)
      (should bol)
      (should (clatter-test-unified--separator-pos buf "testnet/alice"))
      (should-not (string-match-p "testnet/testnick"
                                  (clatter-test-unified--string buf)))
      (with-current-buffer buf
        (should (equal (get-text-property bol 'clatter-unified-target)
                       "alice"))
        (should (equal (get-text-property bol 'clatter-unified-network)
                       "testnet"))))))

;;; Highlighting and filtering

(ert-deftest clatter-test-unified-mention-gets-mention-face ()
  "Text containing my nick for that connection is mention-faced."
  (clatter-test-unified--with-capture conn
    (clatter-unified--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "testnick: ping" nil)
    (let ((buf (clatter-test-unified--buffer)))
      (should buf)
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "ping" nil t))
        (should (memq 'clatter-mention
                      (ensure-list (get-text-property (match-beginning 0)
                                                      'face))))))))

(ert-deftest clatter-test-unified-muted-sender-line-is-invisible ()
  "A muted sender's captured line carries the invisibility category."
  (let ((clatter-fools '("troll")))
    (clatter-test-unified--with-capture conn
      (clatter-unified--on-privmsg conn '("troll" "user" "host") "#emacs"
                                   "bait" nil)
      (let* ((buf (clatter-test-unified--buffer))
             (bol (and buf (clatter-test-unified--line-bol buf "bait"))))
        (should buf)
        (should bol)
        (with-current-buffer buf
          (let ((invisible (get-text-property bol 'invisible)))
            (should invisible)
            (should (memq 'clatter-fool (ensure-list invisible)))))))))

;;; Buffer properties

(ert-deftest clatter-test-unified-buffer-is-read-only ()
  "The unified buffer has no input area, so typing into it is blocked."
  (clatter-test-unified--with-capture conn
    (clatter-unified--on-privmsg conn '("alice" "user" "host") "#emacs"
                                 "hello" nil)
    (let ((buf (clatter-test-unified--buffer)))
      (should buf)
      (should (buffer-local-value 'buffer-read-only buf))
      (should-error (with-current-buffer buf
                      (goto-char (point-max))
                      (insert "stray"))
                    :type 'buffer-read-only))))

(ert-deftest clatter-test-unified-truncation-keeps-newest ()
  "Truncation deletes the oldest lines and keeps the newest capture.

Regression: the buffer is oldest-first regardless of the user's global
`clatter-message-order', so truncation must cut from the top."
  (let ((clatter-buffer-max-lines 4))
    (clatter-test-unified--with-capture conn
      (dotimes (i 12)
        (clatter-unified--on-privmsg conn '("alice" "user" "host") "#emacs"
                                     (format "line-%02d" i) nil))
      (let* ((buf (clatter-test-unified--buffer))
             (text (clatter-test-unified--string buf)))
        (should (string-match-p "line-11" text))
        (should-not (string-match-p "line-00" text))))))

;;; Batch safety

(ert-deftest clatter-test-unified-batched-messages-do-not-appear ()
  "Batched (chathistory) traffic never reaches the unified buffer."
  (clatter-test-unified--with-capture conn
    (clatter-unified-enable)
    (clatter-dispatch-message
     conn (clatter-test-parse ":server BATCH +history chathistory #emacs"))
    (clatter-dispatch-message
     conn (clatter-test-parse
           "@batch=history :alice!~a@host PRIVMSG #emacs :old message"))
    (clatter-dispatch-message
     conn (clatter-test-parse ":server BATCH -history"))
    ;; Nothing was captured, so the buffer was never even created.
    (should-not (clatter-test-unified--buffer))))

(ert-deftest clatter-test-unified-live-message-reaches-buffer-through-hook ()
  "An unbatched PRIVMSG dispatched normally is captured via the hook."
  (clatter-test-unified--with-capture conn
    (clatter-unified-enable)
    (run-hook-with-args 'clatter-privmsg-hook
                        conn '("alice" "user" "host") "#emacs" "live" nil)
    (let ((buf (clatter-test-unified--buffer)))
      (should buf)
      (should (string-match-p "live" (clatter-test-unified--string buf))))))

;;; Jump back to the source message

(ert-deftest clatter-test-unified-visit-jumps-to-source-message ()
  "RET on a captured line lands on that message in the source buffer."
  (clatter-test-unified--with-capture conn
    (let ((text (propertize "jumpable" 'clatter-msgid "msg-42")))
      (clatter-ui--on-privmsg conn '("alice" "user" "host") "#emacs" text nil)
      (clatter-unified--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   text nil))
    (let ((chan (clatter-get-buffer "testnet" "#emacs"))
          (unified (clatter-test-unified--buffer)))
      (should chan)
      (should unified)
      (should (clatter--find-message-position-by-msgid chan "msg-42"))
      (save-window-excursion
        (with-current-buffer unified
          (goto-char (clatter-test-unified--line-bol unified "jumpable"))
          (clatter-unified-visit)
          ;; Visiting pops to the source buffer...
          (should (eq (current-buffer) chan))))
      ;; ...and leaves point on the referenced message.
      (with-current-buffer chan
        (should (equal (get-text-property (point) 'clatter-msgid) "msg-42"))))))

(ert-deftest clatter-test-unified-visit-dead-source-signals-user-error ()
  "Visiting a line whose source buffer is gone reports an error, not a jump."
  (clatter-test-unified--with-capture conn
    (let ((text (propertize "orphan" 'clatter-msgid "msg-99")))
      (clatter-ui--on-privmsg conn '("alice" "user" "host") "#emacs" text nil)
      (clatter-unified--on-privmsg conn '("alice" "user" "host") "#emacs"
                                   text nil))
    (let ((chan (clatter-get-buffer "testnet" "#emacs"))
          (unified (clatter-test-unified--buffer)))
      (should chan)
      (should unified)
      (kill-buffer chan)
      (clatter-remove-buffer "testnet" "#emacs")
      (save-window-excursion
        (with-current-buffer unified
          (goto-char (clatter-test-unified--line-bol unified "orphan"))
          (should-error (clatter-unified-visit) :type 'user-error))))))

(provide 'test-unified)

;;; test-unified.el ends here
