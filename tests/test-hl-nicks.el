;;; test-hl-nicks.el --- Tests for nick highlighting faces -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the named-face nick coloring in clatter-hl-nicks.el.
;; These guard the theme-driven design where each `clatter-nick-color-N'
;; face inherits from a standard face in `clatter-hl-nick-base-faces',
;; so nick colors follow the active Emacs theme.

;;; Code:

(require 'ert)
(require 'clatter-hl-nicks)

(ert-deftest clatter-hl-urls-are-standard-buttons ()
  "URL highlighting produces a standard Emacs button."
  (with-temp-buffer
    (insert (clatter-hl-urls-in-string "see https://example.com/a"))
    (goto-char (point-min))
    (search-forward "https")
    (let ((button (button-at (match-beginning 0))))
      (should button)
      (should (equal (button-get button 'clatter-url)
                     "https://example.com/a")))))

(ert-deftest clatter-hl-elisp-symbols-are-help-buttons ()
  "Doc-quoted Elisp symbols produce standard help buttons."
  (dolist (case '(("`clatter-hl-nicks-enabled'" . "clatter-hl-nicks-enabled")
                  ("‘1+’" . "1+")
                  ("`string='" . "string=")))
    (with-temp-buffer
      (insert (clatter-hl-elisp-symbols-in-string (car case)))
      (goto-char (1+ (point-min)))
      (let ((button (button-at (point))))
        (should button)
        (should (equal (button-get button 'clatter-elisp-symbol)
                       (cdr case)))))))

(ert-deftest clatter-hl-elisp-symbol-buttons-describe-or-run-apropos ()
  "Known symbols open help; unknown names run apropos."
  (let (described searched)
    (cl-letf (((symbol-function 'describe-symbol)
               (lambda (symbol &rest _) (setq described symbol)))
              ((symbol-function 'apropos)
               (lambda (name &rest _) (setq searched name))))
      (dolist (name '("clatter-hl-nicks-enabled" "clatter-no-such-symbol-xyzzy"))
        (with-temp-buffer
          (insert (clatter-hl-elisp-symbols-in-string (format "`%s'" name)))
          (goto-char (+ (point-min) 1))
          (push-button))))
    (should (eq described 'clatter-hl-nicks-enabled))
    (should (equal searched "clatter-no-such-symbol-xyzzy"))))

(ert-deftest clatter-hl-elisp-symbols-never-buttonize-code ()
  "Quoted Elisp forms remain inert text."
  (with-temp-buffer
    (insert (clatter-hl-elisp-symbols-in-string "`(+ 1 2)'"))
    (should-not (next-button (point-min) t))))

(ert-deftest clatter-hl-nick-index-deterministic-and-in-range ()
  "The palette index is stable and within bounds."
  (let ((n (length clatter-hl-nick-base-faces)))
    (should (equal (clatter-hl-nick-index "alice")
                   (clatter-hl-nick-index "alice")))
    (dolist (nick '("alice" "bob" "Carol" "knighthk" "x" ""))
      (let ((idx (clatter-hl-nick-index nick)))
        (should (integerp idx))
        (should (>= idx 0))
        (should (< idx n))))))

(ert-deftest clatter-hl-nick-index-case-insensitive ()
  "Index ignores case (matches color cache behavior)."
  (should (equal (clatter-hl-nick-index "Alice")
                 (clatter-hl-nick-index "alice"))))

(ert-deftest clatter-hl-nick-face-symbol-is-real-face ()
  "The face returned for a nick is an actually defined face."
  (clatter-hl-rebuild-nick-faces)
  (dolist (nick '("alice" "bob" "knighthk"))
    (let ((face (clatter-hl-nick-face-symbol nick)))
      (should (symbolp face))
      (should (facep face)))))

(ert-deftest clatter-hl-nick-face-symbol-stable ()
  "Same nick always maps to the same face symbol."
  (should (eq (clatter-hl-nick-face-symbol "alice")
              (clatter-hl-nick-face-symbol "alice"))))

(ert-deftest clatter-hl-nick-face-inherits-base-face ()
  "Each named nick face inherits from its palette base face.
This is the theme-driven guarantee: the nick color follows whatever the
active theme gives the base face, instead of a hardcoded hex."
  (clatter-hl-rebuild-nick-faces)
  (dolist (nick '("alice" "bob" "knighthk" "Carol"))
    (let* ((face (clatter-hl-nick-face-symbol nick))
           (expected (nth (clatter-hl-nick-index nick)
                          clatter-hl-nick-base-faces)))
      (should (eq (face-attribute face :inherit nil t) expected))
      ;; `clatter-hl-nick-color' must report the base face's resolved
      ;; foreground (the color the user actually sees) for compatibility
      ;; callers.
      (should (equal (clatter-hl-nick-color nick)
                     (face-foreground expected nil t))))))

(ert-deftest clatter-hl-rebuild-nick-faces-covers-palette ()
  "Rebuilding defines one face per palette entry."
  (clatter-hl-rebuild-nick-faces)
  (dotimes (i (length clatter-hl-nick-base-faces))
    (should (facep (intern (format "clatter-nick-color-%d" i))))))

(ert-deftest clatter-hl-rebuild-nick-faces-preserves-without-force ()
  "Without FORCE, an existing customized face is not overwritten."
  (let ((face (intern "clatter-nick-color-0"))
        (base (nth 0 clatter-hl-nick-base-faces)))
    (clatter-hl-rebuild-nick-faces t)
    (set-face-attribute face nil :inherit 'shadow)
    (clatter-hl-rebuild-nick-faces)            ; no force: must not reset
    (should (eq (face-attribute face :inherit nil t) 'shadow))
    ;; force: refreshes back to the palette base face
    (clatter-hl-rebuild-nick-faces t)
    (should (eq (face-attribute face :inherit nil t) base))))

(provide 'test-hl-nicks)

;;; test-hl-nicks.el ends here