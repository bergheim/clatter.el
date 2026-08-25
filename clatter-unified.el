;;; clatter-unified.el --- Unified inbox buffer for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Collects live messages from every channel and query, across all
;; networks, into a single `*clatter-unified*' buffer.  Lines are rendered
;; by `clatter-insert-generic', the same entry point the per-buffer UI
;; uses, so sender colors, mentions, grouping and filtering behave as they
;; do in a channel.  (Nicks inside message text are not highlighted: that
;; keys off a channel's member list, which this buffer has none of.)
;; Enable with `clatter-unified-enabled'; RET on a message jumps to it in
;; its source buffer.

;;; Code:

(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-model)
(require 'clatter-handlers)
(require 'clatter-smart)
(require 'clatter-pals)
(require 'clatter-ui)

(defcustom clatter-unified-enabled nil
  "Collect messages from all buffers into `*clatter-unified*'."
  :type 'boolean
  :group 'clatter)

(defconst clatter-unified--buffer-name "*clatter-unified*"
  "Name of the unified inbox buffer.")

(defvar-local clatter-unified--last-source nil
  "Source (NETWORK . TARGET) of the previously captured message.
Used to decide when to insert a channel separator line.")

(defvar clatter-unified-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'clatter-unified-visit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `clatter-unified-mode'.")

(define-derived-mode clatter-unified-mode clatter-mode "CLatter-Unified"
  "Major mode for the unified inbox buffer."
  ;; The insert path reads `clatter-message-order' in the destination
  ;; buffer.  This buffer has no prompt and no markers, so insertion
  ;; always appends at point-max; with the global `newest-first' default,
  ;; truncation would delete the newest lines and grouping would inspect
  ;; the wrong neighbor.
  (setq-local clatter-message-order 'oldest-first)
  ;; `clatter-mode' leaves buffers writable for their input area; there is
  ;; none here, so stray typing must be blocked.
  (setq buffer-read-only t)
  ;; Seed the invisibility spec the way `clatter-ui-setup-buffer' does,
  ;; otherwise mute and fool properties have no effect here.
  (setq buffer-invisibility-spec (copy-sequence clatter-suppress-messages))
  (when (and clatter-smart-enabled clatter-smart-noise)
    (add-to-invisibility-spec 'noise))
  (unless clatter-fools-visible
    (add-to-invisibility-spec 'clatter-fool)))

(defun clatter-unified--buffer ()
  "Return the unified buffer, creating it on first use."
  (or (get-buffer clatter-unified--buffer-name)
      (with-current-buffer (get-buffer-create clatter-unified--buffer-name)
        (clatter-unified-mode)
        (current-buffer))))

;; --- Capture ---

(defun clatter-unified--capture (msg-type conn sender target text server-time)
  "Insert SENDER's MSG-TYPE TEXT to TARGET on CONN into the unified buffer.
SERVER-TIME is the IRCv3 server-time of the message, if any."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (isupport (clatter-connection-isupport conn))
         (case-mapping (and isupport (gethash "CASEMAPPING" isupport)))
         (sender-nick (clatter-prefix-nick sender))
         (buf-target (if (clatter-channel-name-p target)
                         target
                       (if (clatter-nick-equal-p target my-nick case-mapping)
                           sender-nick target)))
         (invisible (clatter-sender-invisibility sender network))
         (buf (clatter-unified--buffer))
         (last (buffer-local-value 'clatter-unified--last-source buf)))
    (unless (and last
                 (equal (car last) network)
                 (string-equal-ignore-case (cdr last) buf-target))
      ;; Spell the label out rather than using `clatter-buffer-name': its
      ;; `channel' style drops the network, which is the whole point here.
      ;; The separator shares the message's invisibility, else a hidden
      ;; muted/fool message would leave a bare visible separator behind.
      (clatter--insert-message
       buf
       (propertize (format "── %s/%s ──" network buf-target)
                   'face 'font-lock-doc-face)
       t nil nil invisible))
    ;; The source buffer already scanned this message for inline images;
    ;; scanning again would fetch every URL twice.
    (let ((clatter--suppress-image-scan t))
      (clatter-insert-generic msg-type buf sender-nick text conn
                              server-time invisible
                              (list 'clatter-unified-network network
                                    'clatter-unified-target buf-target)))
    ;; Hidden messages must not claim the source context: the next visible
    ;; message still needs its own separator, or it would sit under a
    ;; separator the reader cannot see.
    (unless invisible
      (with-current-buffer buf
        (setq clatter-unified--last-source (cons network buf-target))))))

(defun clatter-unified--on-privmsg (conn sender target text server-time)
  "Capture SENDER's PRIVMSG TEXT to TARGET on CONN at SERVER-TIME."
  (clatter-unified--capture 'privmsg conn sender target text server-time))

(defun clatter-unified--on-action (conn sender target text server-time)
  "Capture SENDER's ACTION TEXT to TARGET on CONN at SERVER-TIME."
  (clatter-unified--capture 'action conn sender target text server-time))

;; --- Jump back to the source ---

(defun clatter-unified--find-message (buffer sender text server-time)
  "Return the position of SENDER's TEXT in BUFFER, or nil.
Fallback for servers without message IDs.  With several matches, prefer
the one nearest SERVER-TIME, else the newest."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((matches nil))
        (while (not (eobp))
          (when (and (equal sender (get-text-property (point) 'clatter-sender))
                     (equal text (get-text-property (point) 'clatter-text)))
            (push (point) matches))
          (forward-line 1))
        (cond
         ((null matches) nil)
         ((null server-time)
          ;; MATCHES is in reverse document order.  Which end is newest
          ;; depends on this buffer's message order: `newest-first' puts
          ;; new messages at the top.
          (if (eq clatter-message-order 'newest-first)
              (car (last matches))
            (car matches)))
         (t
          (let ((best nil)
                (best-distance nil))
            (dolist (pos matches best)
              (let* ((time (get-text-property pos 'clatter-server-time))
                     (distance (if time
                                   (abs (float-time
                                         (time-subtract server-time time)))
                                 most-positive-fixnum)))
                (when (or (null best-distance) (< distance best-distance))
                  (setq best pos
                        best-distance distance)))))))))))

(defun clatter-unified-visit ()
  "Jump to the source buffer of the message at point."
  (interactive)
  (let ((network (get-text-property (point) 'clatter-unified-network))
        (target (get-text-property (point) 'clatter-unified-target))
        (msgid (get-text-property (point) 'clatter-msgid))
        (sender (get-text-property (point) 'clatter-sender))
        (text (get-text-property (point) 'clatter-text))
        (server-time (get-text-property (point) 'clatter-server-time)))
    (unless (and network target)
      (user-error "No message at point"))
    (let ((buf (clatter-get-buffer network target)))
      (unless (buffer-live-p buf)
        (user-error "No buffer for %s/%s" network target))
      ;; Resolve the position before showing the buffer, so a message the
      ;; source has truncated away errors out instead of leaving the user
      ;; at an arbitrary point.  A stale msgid falls through to the
      ;; sender+text match.
      (let ((pos (or (and msgid
                          (clatter--find-message-position-by-msgid buf msgid))
                     (clatter-unified--find-message buf sender text
                                                    server-time))))
        (unless pos
          (user-error "Message not found in %s" (buffer-name buf)))
        ;; Keep the inbox window: the jump target opens elsewhere, like a
        ;; compilation or occur match.
        (pop-to-buffer buf '(nil (inhibit-same-window . t)))
        (goto-char pos)))))

;; --- Setup ---

(defun clatter-unified-enable ()
  "Start collecting messages into the unified buffer."
  (interactive)
  (add-hook 'clatter-privmsg-hook #'clatter-unified--on-privmsg)
  (add-hook 'clatter-action-hook #'clatter-unified--on-action))

(defun clatter-unified-disable ()
  "Stop collecting messages into the unified buffer."
  (interactive)
  (remove-hook 'clatter-privmsg-hook #'clatter-unified--on-privmsg)
  (remove-hook 'clatter-action-hook #'clatter-unified--on-action))

(defun clatter-unified ()
  "Display the unified inbox buffer."
  (interactive)
  (pop-to-buffer (clatter-unified--buffer)))

(provide 'clatter-unified)

;;; clatter-unified.el ends here
