;;; clatter-format.el --- mIRC color/formatting parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Parses and renders mIRC formatting codes in IRC messages.
;; Handles: bold (\x02), italic (\x1D), underline (\x1F),
;; strikethrough (\x1E), reverse (\x16), monospace (\x11),
;; color (\x03 fg[,bg]), hex color (\x04), and reset (\x0F).

;;; Code:

(require 'cl-lib)

;; --- Configuration ---

(defcustom clatter-format-enable t
  "Enable mIRC color and formatting code rendering.
When nil, formatting codes are stripped but not rendered."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-format-strip-only nil
  "If non-nil, strip all formatting codes without rendering.
Overrides `clatter-format-enable'."
  :type 'boolean
  :group 'clatter)

;; --- mIRC Color Palette ---
;; Standard 16-color mIRC palette

(defconst clatter-format--mirc-colors
  ["#ffffff"   ; 0  white
   "#000000"   ; 1  black
   "#00007f"   ; 2  blue (navy)
   "#009300"   ; 3  green
   "#ff0000"   ; 4  red
   "#7f0000"   ; 5  brown (maroon)
   "#9c009c"   ; 6  purple
   "#fc7f00"   ; 7  orange (olive)
   "#ffff00"   ; 8  yellow
   "#00fc00"   ; 9  light green
   "#009393"   ; 10 teal (cyan)
   "#00ffff"   ; 11 light cyan (aqua)
   "#0000fc"   ; 12 light blue (royal)
   "#ff00ff"   ; 13 pink (light purple)
   "#7f7f7f"   ; 14 grey
   "#d2d2d2"]  ; 15 light grey (silver)
  "Standard 16-color mIRC palette.")

;; Extended 99-color palette (indices 16-98)
(defconst clatter-format--mirc-colors-extended
  ["#470000" "#472100" "#474700" "#324700" "#004700" "#00472c"
   "#004747" "#002747" "#000047" "#2e0047" "#470047" "#47002a"
   "#740000" "#743a00" "#747400" "#517400" "#007400" "#007449"
   "#007474" "#004074" "#000074" "#4b0074" "#740074" "#740045"
   "#b50000" "#b56300" "#b5b500" "#7db500" "#00b500" "#00b571"
   "#00b5b5" "#0063b5" "#0000b5" "#7500b5" "#b500b5" "#b5006b"
   "#ff0000" "#ff8c00" "#ffff00" "#b2ff00" "#00ff00" "#00ffa0"
   "#00ffff" "#008cff" "#0000ff" "#a500ff" "#ff00ff" "#ff0098"
   "#ff5959" "#ffb459" "#ffff71" "#cfff60" "#6fff6f" "#65ffc9"
   "#6dffff" "#59b4ff" "#5959ff" "#c459ff" "#ff66ff" "#ff59bc"
   "#ff9c9c" "#ffd39c" "#ffff9c" "#e2ff9c" "#9cff9c" "#9cffdb"
   "#9cffff" "#9cd3ff" "#9c9cff" "#dc9cff" "#ff9cff" "#ff94d3"
   "#000000" "#131313" "#282828" "#363636" "#4d4d4d" "#656565"
   "#818181" "#9f9f9f" "#bcbcbc" "#e2e2e2" "#ffffff"]
  "Extended mIRC color palette (indices 16-98).")

(defconst clatter-format--mirc-color-names
  ["white" "black" "blue" "green" "light red" "brown" "purple" "orange"
   "yellow" "light green" "cyan" "light cyan" "light blue" "pink" "grey"
   "light grey"]
  "Names of the standard mIRC colors.")

(defconst clatter-format--extended-color-hues
  ["red" "orange" "yellow" "lime" "green" "mint"
   "cyan" "azure" "blue" "violet" "magenta" "pink"])

(defconst clatter-format--extended-color-shades
  ["dark" "deep" "medium" "vivid" "light" "pale"])

(defconst clatter-format--extended-grey-names
  ["black" "near black" "charcoal" "dark grey" "deep grey" "medium grey"
   "grey" "light grey" "silver" "pale grey" "white"])

(defun clatter-format--color-name-for-index (idx)
  "Return a readable name for IRC color index IDX."
  (cond
   ((and (>= idx 0) (< idx 16))
    (aref clatter-format--mirc-color-names idx))
   ((and (>= idx 16) (< idx 88))
    ;; Extended colors have fixed values but no standardized names.
    (let ((offset (- idx 16)))
      (format "%s %s"
              (aref clatter-format--extended-color-shades (/ offset 12))
              (aref clatter-format--extended-color-hues (% offset 12)))))
   ((and (>= idx 88) (< idx 99))
    (aref clatter-format--extended-grey-names (- idx 88)))
   (t nil)))

(defun clatter-format--color-for-index (idx)
  "Return hex color string for mIRC color index IDX."
  (cond
   ((and (>= idx 0) (< idx 16))
    (aref clatter-format--mirc-colors idx))
   ((and (>= idx 16) (< idx 99))
    (aref clatter-format--mirc-colors-extended (- idx 16)))
   (t nil)))

(defun clatter-format--ascii-digit-p (ch)
  "Return non-nil if CH is an ASCII digit (?0..?9).
mIRC color codes only use ASCII digits; unlike `cl-digit-char-p',
this never errors on non-Latin-1 codepoints (e.g. U+2234)."
  (and (<= ?0 ch) (<= ch ?9)))

;; --- Formatting code constants ---

(defconst clatter-format--bold      ?\x02)
(defconst clatter-format--color     ?\x03)
(defconst clatter-format--hex-color ?\x04)
(defconst clatter-format--reset     ?\x0F)
(defconst clatter-format--reverse   ?\x16)
(defconst clatter-format--italic    ?\x1D)
(defconst clatter-format--strikethrough ?\x1E)
(defconst clatter-format--underline ?\x1F)
(defconst clatter-format--monospace ?\x11)

;; --- Parser ---

(defun clatter-format-parse (text)
  "Parse mIRC formatting codes in TEXT and return propertized string.
If `clatter-format-strip-only' is non-nil, strips codes without rendering.
If `clatter-format-enable' is nil, returns TEXT unchanged."
  (if (not (string-match-p "[\x02\x03\x04\x0F\x11\x16\x1D\x1E\x1F]" text))
      text  ; fast path: no formatting codes
    (if clatter-format-strip-only
        (clatter-format--strip text)
      (if clatter-format-enable
          (clatter-format--render text)
        text))))

(defun clatter-format--strip (text)
  "Strip all mIRC formatting codes from TEXT, returning plain string."
  (let ((result (replace-regexp-in-string
                 "\x03\\([0-9]\\{1,2\\}\\(,[0-9]\\{1,2\\}\\)?\\)?" "" text)))
    (setq result (replace-regexp-in-string
                  "\x04\\([0-9a-fA-F]\\{6\\}\\(,[0-9a-fA-F]\\{6\\}\\)?\\)?" "" result))
    (replace-regexp-in-string "[\x02\x0F\x11\x16\x1D\x1E\x1F]" "" result)))

(defun clatter-format--scan (text function)
  "Scan IRC formatting in TEXT and call FUNCTION for each source run.
FUNCTION receives START, END, FACE and CONTROL-P.  START and END are
positions in TEXT, FACE is the active face for visible text, and CONTROL-P
is non-nil for a recognized formatting control and its consumed arguments."
  (let ((pos 0)
        (len (length text))
        (controls "[\x02\x03\x04\x0F\x11\x16\x1D\x1E\x1F]")
        (bold nil)
        (italic nil)
        (underline nil)
        (strikethrough nil)
        (reverse-video nil)
        (monospace nil)
        (fg-color nil)
        (bg-color nil))
    (while (< pos len)
      (let ((start pos)
            (ch (aref text pos))
            (control-p t))
        (cond
         ((= ch clatter-format--bold)
          (setq bold (not bold))
          (cl-incf pos))
         ((= ch clatter-format--italic)
          (setq italic (not italic))
          (cl-incf pos))
         ((= ch clatter-format--underline)
          (setq underline (not underline))
          (cl-incf pos))
         ((= ch clatter-format--strikethrough)
          (setq strikethrough (not strikethrough))
          (cl-incf pos))
         ((= ch clatter-format--reverse)
          (setq reverse-video (not reverse-video))
          (cl-incf pos))
         ((= ch clatter-format--monospace)
          (setq monospace (not monospace))
          (cl-incf pos))
         ((= ch clatter-format--reset)
          (setq bold nil italic nil underline nil
                strikethrough nil reverse-video nil
                monospace nil fg-color nil bg-color nil)
          (cl-incf pos))
         ((= ch clatter-format--color)
          (cl-incf pos)
          (if (and (< pos len) (clatter-format--ascii-digit-p (aref text pos)))
              (let ((fg-start pos))
                (while (and (< pos len)
                            (clatter-format--ascii-digit-p (aref text pos))
                            (< (- pos fg-start) 2))
                  (cl-incf pos))
                (setq fg-color
                      (clatter-format--color-for-index
                       (string-to-number (substring text fg-start pos))))
                (when (and (< pos len) (= (aref text pos) ?,)
                           (< (1+ pos) len)
                           (clatter-format--ascii-digit-p (aref text (1+ pos))))
                  (cl-incf pos)
                  (let ((bg-start pos))
                    (while (and (< pos len)
                                (clatter-format--ascii-digit-p (aref text pos))
                                (< (- pos bg-start) 2))
                      (cl-incf pos))
                    (setq bg-color
                          (clatter-format--color-for-index
                           (string-to-number
                            (substring text bg-start pos)))))))
            (setq fg-color nil bg-color nil)))
         ((= ch clatter-format--hex-color)
          (cl-incf pos)
          (if (and (<= (+ pos 6) len)
                   (string-match-p "\\`[0-9a-fA-F]\\{6\\}"
                                   (substring text pos (min (+ pos 6) len))))
              (progn
                (setq fg-color (concat "#" (substring text pos (+ pos 6))))
                (cl-incf pos 6)
                (when (and (< pos len) (= (aref text pos) ?,)
                           (<= (+ pos 7) len)
                           (string-match-p "\\`[0-9a-fA-F]\\{6\\}"
                                           (substring text (1+ pos) (+ pos 7))))
                  (cl-incf pos)
                  (setq bg-color (concat "#" (substring text pos (+ pos 6))))
                  (cl-incf pos 6)))
            (setq fg-color nil bg-color nil)))
         (t
          (setq control-p nil
                pos (or (string-match controls text pos) len))))
        (if control-p
            (funcall function start pos nil t)
          (funcall function
                   start pos
                   (clatter-format--build-face
                    bold italic underline strikethrough
                    reverse-video monospace fg-color bg-color)
                   nil))))))

(defun clatter-format--render (text)
  "Render mIRC formatting codes in TEXT as Emacs face properties."
  (let (result)
    (clatter-format--scan
     text
     (lambda (start end face control-p)
       (unless control-p
         (let ((run (substring-no-properties text start end)))
           (push (if face (propertize run 'face face) run) result)))))
    (apply #'concat (nreverse result))))

(defun clatter-format-propertize-region (beg end)
  "Render IRC formatting between BEG and END using overlays.
The buffer text and its existing text properties are not changed.  Return
the generated overlays, or nil when the region contains no formatting."
  (let ((text (buffer-substring-no-properties beg end))
        overlays)
    (clatter-format--scan
     text
     (lambda (start finish face control-p)
       (when (or control-p face)
         (let ((overlay (make-overlay (+ beg start) (+ beg finish))))
           (overlay-put overlay 'evaporate t)
           (overlay-put overlay 'clatter-input-formatting t)
           (overlay-put overlay (if control-p 'display 'face)
                        (if control-p "" face))
           (push overlay overlays)))))
    (nreverse overlays)))

(defun clatter-format--build-face (bold italic underline strikethrough
                                        reverse-video monospace fg-color bg-color)
  "Build a face spec from the current formatting state.
BOLD, ITALIC, UNDERLINE, STRIKETHROUGH, REVERSE-VIDEO, MONOSPACE,
FG-COLOR and BG-COLOR are the active formatting attributes.
Returns nil if no formatting is active."
  (let ((face nil))
    (when bold (push :weight face) (push 'bold face))
    (when italic (push :slant face) (push 'italic face))
    (when underline (push :underline face) (push t face))
    (when strikethrough (push :strike-through face) (push t face))
    (when monospace (push :family face) (push "Monospace" face))
    (when (and fg-color (not reverse-video))
      (push :foreground face) (push fg-color face))
    (when (and bg-color (not reverse-video))
      (push :background face) (push bg-color face))
    (when (and reverse-video fg-color)
      (push :background face) (push fg-color face))
    (when (and reverse-video bg-color)
      (push :foreground face) (push bg-color face))
    (when (and reverse-video (not fg-color) (not bg-color))
      (push :inverse-video face) (push t face))
    (when face
      (nreverse face))))

(provide 'clatter-format)

;;; clatter-format.el ends here
