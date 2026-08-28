;;; test-evil.el --- Tests for clatter-evil.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-evil)

(ert-deftest clatter-test-evil-inert-without-evil ()
  "The evil module loads in an evil-free session without side effects.
Bindings are deferred behind `with-eval-after-load', so the vanilla
keymaps keep their own definitions."
  (should (featurep 'clatter-evil))
  (should-not (featurep 'evil))
  (should (eq (lookup-key clatter-feed-mode-map (kbd "RET"))
              #'clatter-feed-visit))
  (should (eq (lookup-key clatter-nicklist-mode-map (kbd "RET"))
              #'clatter-nicklist-query)))

(provide 'test-evil)

;;; test-evil.el ends here
