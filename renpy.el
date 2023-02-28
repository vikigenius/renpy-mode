;;; renpy.el --- Emacs does RenPy -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 vikigenius
;;
;; Author: vikigenius <master.bvik@gmail.com>
;; Maintainer: vikigenius <master.bvik@gmail.com>
;; Created: February 28, 2023
;; Modified: February 28, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/void/renpy
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'comint)

(defvar renpy-imenu-generic-expression
  `(( nil "\\b\\(label\\|menu\\)\\s-+\\(\\w+\\):" 2)
    ( nil "\\b\\(screen\\)\\s-+\\(\\w+\\):" 2)
    ( nil "\\b\\(transform\\)\\s-+\\(\\w+\\):" 2)
    ( nil "\\b\\(def\\|class\\)\\s-+\\(\\w+\\)" 2)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rpy\\'" . renpy-mode))
(add-to-list 'auto-mode-alist '("\\.rpym\\'" . renpy-mode))
(add-to-list 'same-window-buffer-names "*Renpy*")

(defgroup renpy nil
  "Python Language's flying circus support for Emacs."
  :group 'languages
  :version "24.3"
  :link '(emacs-commentary-link "renpy"))

;; Bindings
(defvar renpy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Indent specific
    (define-key map "\177" 'python-indent-dedent-line-backspace)
    (define-key map (kbd "<backtab>") 'python-indent-dedent-line)
    (define-key map "\C-c<" 'python-indent-shift-left)
    (define-key map "\C-c>" 'python-indent-shift-right))
  "Keymap for `python-mode'.")

;;; Python specialized rx

(defmacro renpy-rx (&rest regexps)
  "RenPy mode specialized rx macro.
This variant of `rx' supports common Python named REGEXPS."
  `(rx-let ((block-start       (seq symbol-start
                                    (or "def" "class" "if" "elif" "else" "try"
                                        "except" "finally" "for" "while" "with"
                                        ;; Python 3.5+ PEP492
                                        (and "async" (+ space)
                                             (or "def" "for" "with")))
                                    symbol-end))
            (dedenter          (seq symbol-start
                                    (or "elif" "else" "except" "finally")
                                    symbol-end))
            (block-ender       (seq symbol-start
                                    (or
                                     "break" "continue" "pass" "raise" "return")
                                    symbol-end))
            (decorator         (seq line-start (* space) ?@ (any letter ?_)
                                    (* (any word ?_))))
            (defun             (seq symbol-start
                                    (or "def" "class"
                                        ;; Python 3.5+ PEP492
                                        (and "async" (+ space) "def"))
                                    symbol-end))
            (symbol-name       (seq (any letter ?_) (* (any word ?_))))
            (assignment-target (seq (? ?*)
                                    (* symbol-name ?.) symbol-name
                                    (? ?\[ (+ (not ?\])) ?\])))
            (grouped-assignment-target (seq (? ?*)
                                            (* symbol-name ?.) (group symbol-name)
                                            (? ?\[ (+ (not ?\])) ?\])))
            (open-paren        (or "{" "[" "("))
            (close-paren       (or "}" "]" ")"))
            (simple-operator   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))
            (not-simple-operator (not simple-operator))
            (operator          (or "==" ">=" "is" "not"
                                   "**" "//" "<<" ">>" "<=" "!="
                                   "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                   "=" "%"))
            (assignment-operator (or "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                     ">>=" "<<=" "&=" "^=" "|="
                                     "="))
            (string-delimiter  (seq
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by an
                                    ;; escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"\"\"" "\"" "'''" "'"))))
            (coding-cookie (seq line-start ?# (* space)
                                (or
                                 ;; # coding=<encoding name>
                                 (: "coding" (or ?: ?=) (* space)
                                    (group-n 1 (+ (or word ?-))))
                                 ;; # -*- coding: <encoding name> -*-
                                 (: "-*-" (* space) "coding:" (* space)
                                    (group-n 1 (+ (or word ?-)))
                                    (* space) "-*-")
                                 ;; # vim: set fileencoding=<encoding name> :
                                 (: "vim:" (* space) "set" (+ space)
                                    "fileencoding" (* space) ?= (* space)
                                    (group-n 1 (+ (or word ?-)))
                                    (* space) ":")))))
     (rx ,@regexps)))


(defvar renpy-font-lock-keywords-level-1
  `((,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-function-name-face))
    (,(rx symbol-start "class" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-type-face))
    (,(rx symbol-start "label" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-type-face))
    (,(rx symbol-start "screen" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-type-face))
    (,(rx symbol-start "transform" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-type-face)))
  "Font lock keywords to use in `renpy-mode' for level 1 decoration.

This is the minimum decoration level, including function and
class declarations.")


(defvar renpy-font-lock-keywords-level-2
  `(,@renpy-font-lock-keywords-level-1
    ,(rx symbol-start
         ;; Reference
         (or "and" "as" "assert" "break" "continue" "def" "del" "elif" "else"
             "except" "exec" "finally" "for" "from" "global" "if"
             "import" "in" "is" "lambda" "not" "or" "pass" "print"
             "raise" "return" "try" "while" "with" "yield"
             "nonlocal"
             ;; Python 3.5+ PEP492
             (and "async" (+ space) (or "def" "for" "with"))
             "await"

             ;; Not real keywords, but close enough to be fontified as such
             "self"

             ;; Additional
             "$" "add" "animation" "at" "bar" "behind" "block" "break" "button"
             "call" "choice" "circles" "class" "clockwise" "contains" "continue"
             "counterclockwise" "define" "event" "expression" "fixed" "frame"
             "function" "grid" "has" "hbox" "hide" "hotbar" "hotspot"
             "image" "imagebutton" "imagemap" "init" "input" "jump" "key" "knot"
             "label" "menu" "null" "nvl" "on" "onlayer" "parallel" "pause" "play"
             "python" "queue" "repeat" "scene" "screen" "set" "show" "side" "stop"
             "text" "textbutton" "time" "timer" "transform" "transform" "use"
             "vbar" "vbox" "viewport" "window" "zorder") symbol-end)
    ;; Builtins
    (,(rx (or line-start (not (any ". \t"))) (* (any " \t")) symbol-start
          (group (or
                  ;; callable built-ins, fontified when not appearing as
                  ;; object attributes. image.dir won't highlight dir
                  ;; The default python font-lock does not do this.
                  "abs" "all" "any" "bin" "bool" "callable" "chr" "classmethod"
                  "compile" "complex" "delattr" "dict" "dir" "divmod" "enumerate"
                  "eval" "filter" "float" "format" "frozenset" "getattr" "globals"
                  "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
                  "issubclass" "iter" "len" "list" "locals" "map" "max" "memoryview"
                  "min" "next" "object" "oct" "open" "ord" "pow" "print" "property"
                  "range" "repr" "reversed" "round" "set" "setattr" "slice" "sorted"
                  "staticmethod" "str" "sum" "super" "tuple" "type" "vars" "zip"
                  ;; Python 2:
                  "basestring" "cmp" "execfile" "file" "long" "raw_input" "reduce"
                  "reload" "unichr" "unicode" "xrange" "apply" "buffer" "coerce"
                  "intern"
                  ;; Python 3:
                  "ascii" "breakpoint" "bytearray" "bytes" "exec"
                  ;; RenPy
                  "action"
                  "activate_align"
                  "activate_alignaround"
                  "activate_alpha"
                  "activate_anchor"
                  "activate_angle"
                  "activate_antialias"
                  "activate_area"
                  "activate_around"
                  "activate_background"
                  "activate_bar_invert"
                  "activate_bar_resizing"
                  "activate_bar_vertical"
                  "activate_black_color"
                  "activate_bold"
                  "activate_bottom_bar"
                  "activate_bottom_gutter"
                  "activate_bottom_margin"
                  "activate_bottom_padding"
                  "activate_box_layout"
                  "activate_clipping"
                  "activate_color"
                  "activate_corner1"
                  "activate_corner2"
                  "activate_crop"
                  "activate_delay"
                  "activate_drop_shadow"
                  "activate_drop_shadow_color"
                  "activate_first_indent"
                  "activate_first_spacing"
                  "activate_fit_first"
                  "activate_font"
                  "activate_foreground"
                  "activate_italic"
                  "activate_justify"
                  "activate_language"
                  "activate_layout"
                  "activate_left_bar"
                  "activate_left_gutter"
                  "activate_left_margin"
                  "activate_left_padding"
                  "activate_line_spacing"
                  "activate_min_width"
                  "activate_minwidth"
                  "activate_mouse"
                  "activate_offset"
                  "activate_outlines"
                  "activate_pos"
                  "activate_radius"
                  "activate_rest_indent"
                  "activate_right_bar"
                  "activate_right_gutter"
                  "activate_right_margin"
                  "activate_right_padding"
                  "activate_rotate"
                  "activate_rotate_pad"
                  "activate_size"
                  "activate_size_group"
                  "activate_slow_abortable"
                  "activate_slow_cps"
                  "activate_slow_cps_multiplier"
                  "activate_sound"
                  "activate_spacing"
                  "activate_subpixel"
                  "activate_text_align"
                  "activate_text_y_fudge"
                  "activate_thumb"
                  "activate_thumb_offset"
                  "activate_thumb_shadow"
                  "activate_top_bar"
                  "activate_top_gutter"
                  "activate_top_margin"
                  "activate_top_padding"
                  "activate_underline"
                  "activate_unscrollable"
                  "activate_xalign"
                  "activate_xanchor"
                  "activate_xanchoraround"
                  "activate_xaround"
                  "activate_xfill"
                  "activate_xmargin"
                  "activate_xmaximum"
                  "activate_xminimum"
                  "activate_xoffset"
                  "activate_xpadding"
                  "activate_xpos"
                  "activate_xzoom"
                  "activate_yalign"
                  "activate_yanchor"
                  "activate_yanchoraround"
                  "activate_yaround"
                  "activate_yfill"
                  "activate_ymargin"
                  "activate_ymaximum"
                  "activate_yminimum"
                  "activate_yoffset"
                  "activate_ypadding"
                  "activate_ypos"
                  "activate_yzoom"
                  "activate_zoom"
                  "adjustment"
                  "align"
                  "alignaround"
                  "allow"
                  "alpha"
                  "anchor"
                  "angle"
                  "antialias"
                  "area"
                  "around"
                  "auto"
                  "background"
                  "bar_invert"
                  "bar_resizing"
                  "bar_vertical"
                  "black_color"
                  "bold"
                  "bottom_bar"
                  "bottom_gutter"
                  "bottom_margin"
                  "bottom_padding"
                  "box_layout"
                  "changed"
                  "child_size"
                  "clicked"
                  "clipping"
                  "color"
                  "corner1"
                  "corner2"
                  "crop"
                  "default"
                  "delay"
                  "draggable"
                  "drop_shadow"
                  "drop_shadow_color"
                  "exclude"
                  "first_indent"
                  "first_spacing"
                  "fit_first"
                  "focus"
                  "font"
                  "foreground"
                  "ground"
                  "height"
                  "hover"
                  "hover_align"
                  "hover_alignaround"
                  "hover_alpha"
                  "hover_anchor"
                  "hover_angle"
                  "hover_antialias"
                  "hover_area"
                  "hover_around"
                  "hover_background"
                  "hover_bar_invert"
                  "hover_bar_resizing"
                  "hover_bar_vertical"
                  "hover_black_color"
                  "hover_bold"
                  "hover_bottom_bar"
                  "hover_bottom_gutter"
                  "hover_bottom_margin"
                  "hover_bottom_padding"
                  "hover_box_layout"
                  "hover_clipping"
                  "hover_color"
                  "hover_corner1"
                  "hover_corner2"
                  "hover_crop"
                  "hover_delay"
                  "hover_drop_shadow"
                  "hover_drop_shadow_color"
                  "hover_first_indent"
                  "hover_first_spacing"
                  "hover_fit_first"
                  "hover_font"
                  "hover_foreground"
                  "hover_italic"
                  "hover_justify"
                  "hover_language"
                  "hover_layout"
                  "hover_left_bar"
                  "hover_left_gutter"
                  "hover_left_margin"
                  "hover_left_padding"
                  "hover_line_spacing"
                  "hover_min_width"
                  "hover_minwidth"
                  "hover_mouse"
                  "hover_offset"
                  "hover_outlines"
                  "hover_pos"
                  "hover_radius"
                  "hover_rest_indent"
                  "hover_right_bar"
                  "hover_right_gutter"
                  "hover_right_margin"
                  "hover_right_padding"
                  "hover_rotate"
                  "hover_rotate_pad"
                  "hover_size"
                  "hover_size_group"
                  "hover_slow_abortable"
                  "hover_slow_cps"
                  "hover_slow_cps_multiplier"
                  "hover_sound"
                  "hover_spacing"
                  "hover_subpixel"
                  "hover_text_align"
                  "hover_text_y_fudge"
                  "hover_thumb"
                  "hover_thumb_offset"
                  "hover_thumb_shadow"
                  "hover_top_bar"
                  "hover_top_gutter"
                  "hover_top_margin"
                  "hover_top_padding"
                  "hover_underline"
                  "hover_unscrollable"
                  "hover_xalign"
                  "hover_xanchor"
                  "hover_xanchoraround"
                  "hover_xaround"
                  "hover_xfill"
                  "hover_xmargin"
                  "hover_xmaximum"
                  "hover_xminimum"
                  "hover_xoffset"
                  "hover_xpadding"
                  "hover_xpos"
                  "hover_xzoom"
                  "hover_yalign"
                  "hover_yanchor"
                  "hover_yanchoraround"
                  "hover_yaround"
                  "hover_yfill"
                  "hover_ymargin"
                  "hover_ymaximum"
                  "hover_yminimum"
                  "hover_yoffset"
                  "hover_ypadding"
                  "hover_ypos"
                  "hover_yzoom"
                  "hover_zoom"
                  "hovered"
                  "id"
                  "idle"
                  "idle_align"
                  "idle_alignaround"
                  "idle_alpha"
                  "idle_anchor"
                  "idle_angle"
                  "idle_antialias"
                  "idle_area"
                  "idle_around"
                  "idle_background"
                  "idle_bar_invert"
                  "idle_bar_resizing"
                  "idle_bar_vertical"
                  "idle_black_color"
                  "idle_bold"
                  "idle_bottom_bar"
                  "idle_bottom_gutter"
                  "idle_bottom_margin"
                  "idle_bottom_padding"
                  "idle_box_layout"
                  "idle_clipping"
                  "idle_color"
                  "idle_corner1"
                  "idle_corner2"
                  "idle_crop"
                  "idle_delay"
                  "idle_drop_shadow"
                  "idle_drop_shadow_color"
                  "idle_first_indent"
                  "idle_first_spacing"
                  "idle_fit_first"
                  "idle_font"
                  "idle_foreground"
                  "idle_italic"
                  "idle_justify"
                  "idle_language"
                  "idle_layout"
                  "idle_left_bar"
                  "idle_left_gutter"
                  "idle_left_margin"
                  "idle_left_padding"
                  "idle_line_spacing"
                  "idle_min_width"
                  "idle_minwidth"
                  "idle_mouse"
                  "idle_offset"
                  "idle_outlines"
                  "idle_pos"
                  "idle_radius"
                  "idle_rest_indent"
                  "idle_right_bar"
                  "idle_right_gutter"
                  "idle_right_margin"
                  "idle_right_padding"
                  "idle_rotate"
                  "idle_rotate_pad"
                  "idle_size"
                  "idle_size_group"
                  "idle_slow_abortable"
                  "idle_slow_cps"
                  "idle_slow_cps_multiplier"
                  "idle_sound"
                  "idle_spacing"
                  "idle_subpixel"
                  "idle_text_align"
                  "idle_text_y_fudge"
                  "idle_thumb"
                  "idle_thumb_offset"
                  "idle_thumb_shadow"
                  "idle_top_bar"
                  "idle_top_gutter"
                  "idle_top_margin"
                  "idle_top_padding"
                  "idle_underline"
                  "idle_unscrollable"
                  "idle_xalign"
                  "idle_xanchor"
                  "idle_xanchoraround"
                  "idle_xaround"
                  "idle_xfill"
                  "idle_xmargin"
                  "idle_xmaximum"
                  "idle_xminimum"
                  "idle_xoffset"
                  "idle_xpadding"
                  "idle_xpos"
                  "idle_xzoom"
                  "idle_yalign"
                  "idle_yanchor"
                  "idle_yanchoraround"
                  "idle_yaround"
                  "idle_yfill"
                  "idle_ymargin"
                  "idle_ymaximum"
                  "idle_yminimum"
                  "idle_yoffset"
                  "idle_ypadding"
                  "idle_ypos"
                  "idle_yzoom"
                  "idle_zoom"
                  "image_style"
                  "insensitive"
                  "insensitive_align"
                  "insensitive_alignaround"
                  "insensitive_alpha"
                  "insensitive_anchor"
                  "insensitive_angle"
                  "insensitive_antialias"
                  "insensitive_area"
                  "insensitive_around"
                  "insensitive_background"
                  "insensitive_bar_invert"
                  "insensitive_bar_resizing"
                  "insensitive_bar_vertical"
                  "insensitive_black_color"
                  "insensitive_bold"
                  "insensitive_bottom_bar"
                  "insensitive_bottom_gutter"
                  "insensitive_bottom_margin"
                  "insensitive_bottom_padding"
                  "insensitive_box_layout"
                  "insensitive_clipping"
                  "insensitive_color"
                  "insensitive_corner1"
                  "insensitive_corner2"
                  "insensitive_crop"
                  "insensitive_delay"
                  "insensitive_drop_shadow"
                  "insensitive_drop_shadow_color"
                  "insensitive_first_indent"
                  "insensitive_first_spacing"
                  "insensitive_fit_first"
                  "insensitive_font"
                  "insensitive_foreground"
                  "insensitive_italic"
                  "insensitive_justify"
                  "insensitive_language"
                  "insensitive_layout"
                  "insensitive_left_bar"
                  "insensitive_left_gutter"
                  "insensitive_left_margin"
                  "insensitive_left_padding"
                  "insensitive_line_spacing"
                  "insensitive_min_width"
                  "insensitive_minwidth"
                  "insensitive_mouse"
                  "insensitive_offset"
                  "insensitive_outlines"
                  "insensitive_pos"
                  "insensitive_radius"
                  "insensitive_rest_indent"
                  "insensitive_right_bar"
                  "insensitive_right_gutter"
                  "insensitive_right_margin"
                  "insensitive_right_padding"
                  "insensitive_rotate"
                  "insensitive_rotate_pad"
                  "insensitive_size"
                  "insensitive_size_group"
                  "insensitive_slow_abortable"
                  "insensitive_slow_cps"
                  "insensitive_slow_cps_multiplier"
                  "insensitive_sound"
                  "insensitive_spacing"
                  "insensitive_subpixel"
                  "insensitive_text_align"
                  "insensitive_text_y_fudge"
                  "insensitive_thumb"
                  "insensitive_thumb_offset"
                  "insensitive_thumb_shadow"
                  "insensitive_top_bar"
                  "insensitive_top_gutter"
                  "insensitive_top_margin"
                  "insensitive_top_padding"
                  "insensitive_underline"
                  "insensitive_unscrollable"
                  "insensitive_xalign"
                  "insensitive_xanchor"
                  "insensitive_xanchoraround"
                  "insensitive_xaround"
                  "insensitive_xfill"
                  "insensitive_xmargin"
                  "insensitive_xmaximum"
                  "insensitive_xminimum"
                  "insensitive_xoffset"
                  "insensitive_xpadding"
                  "insensitive_xpos"
                  "insensitive_xzoom"
                  "insensitive_yalign"
                  "insensitive_yanchor"
                  "insensitive_yanchoraround"
                  "insensitive_yaround"
                  "insensitive_yfill"
                  "insensitive_ymargin"
                  "insensitive_ymaximum"
                  "insensitive_yminimum"
                  "insensitive_yoffset"
                  "insensitive_ypadding"
                  "insensitive_ypos"
                  "insensitive_yzoom"
                  "insensitive_zoom"
                  "italic"
                  "justify"
                  "language"
                  "layout"
                  "left_bar"
                  "left_gutter"
                  "left_margin"
                  "left_padding"
                  "length"
                  "line_spacing"
                  "min_width"
                  "minwidth"
                  "mouse"
                  "mousewheel"
                  "offset"
                  "outlines"
                  "pos"
                  "prefix"
                  "radius"
                  "range"
                  "rest_indent"
                  "right_bar"
                  "right_gutter"
                  "right_margin"
                  "right_padding"
                  "rotate"
                  "rotate_pad"
                  "selected_activate_align"
                  "selected_activate_alignaround"
                  "selected_activate_alpha"
                  "selected_activate_anchor"
                  "selected_activate_angle"
                  "selected_activate_antialias"
                  "selected_activate_area"
                  "selected_activate_around"
                  "selected_activate_background"
                  "selected_activate_bar_invert"
                  "selected_activate_bar_resizing"
                  "selected_activate_bar_vertical"
                  "selected_activate_black_color"
                  "selected_activate_bold"
                  "selected_activate_bottom_bar"
                  "selected_activate_bottom_gutter"
                  "selected_activate_bottom_margin"
                  "selected_activate_bottom_padding"
                  "selected_activate_box_layout"
                  "selected_activate_clipping"
                  "selected_activate_color"
                  "selected_activate_corner1"
                  "selected_activate_corner2"
                  "selected_activate_crop"
                  "selected_activate_delay"
                  "selected_activate_drop_shadow"
                  "selected_activate_drop_shadow_color"
                  "selected_activate_first_indent"
                  "selected_activate_first_spacing"
                  "selected_activate_fit_first"
                  "selected_activate_font"
                  "selected_activate_foreground"
                  "selected_activate_italic"
                  "selected_activate_justify"
                  "selected_activate_language"
                  "selected_activate_layout"
                  "selected_activate_left_bar"
                  "selected_activate_left_gutter"
                  "selected_activate_left_margin"
                  "selected_activate_left_padding"
                  "selected_activate_line_spacing"
                  "selected_activate_min_width"
                  "selected_activate_minwidth"
                  "selected_activate_mouse"
                  "selected_activate_offset"
                  "selected_activate_outlines"
                  "selected_activate_pos"
                  "selected_activate_radius"
                  "selected_activate_rest_indent"
                  "selected_activate_right_bar"
                  "selected_activate_right_gutter"
                  "selected_activate_right_margin"
                  "selected_activate_right_padding"
                  "selected_activate_rotate"
                  "selected_activate_rotate_pad"
                  "selected_activate_size"
                  "selected_activate_size_group"
                  "selected_activate_slow_abortable"
                  "selected_activate_slow_cps"
                  "selected_activate_slow_cps_multiplier"
                  "selected_activate_sound"
                  "selected_activate_spacing"
                  "selected_activate_subpixel"
                  "selected_activate_text_align"
                  "selected_activate_text_y_fudge"
                  "selected_activate_thumb"
                  "selected_activate_thumb_offset"
                  "selected_activate_thumb_shadow"
                  "selected_activate_top_bar"
                  "selected_activate_top_gutter"
                  "selected_activate_top_margin"
                  "selected_activate_top_padding"
                  "selected_activate_underline"
                  "selected_activate_unscrollable"
                  "selected_activate_xalign"
                  "selected_activate_xanchor"
                  "selected_activate_xanchoraround"
                  "selected_activate_xaround"
                  "selected_activate_xfill"
                  "selected_activate_xmargin"
                  "selected_activate_xmaximum"
                  "selected_activate_xminimum"
                  "selected_activate_xoffset"
                  "selected_activate_xpadding"
                  "selected_activate_xpos"
                  "selected_activate_xzoom"
                  "selected_activate_yalign"
                  "selected_activate_yanchor"
                  "selected_activate_yanchoraround"
                  "selected_activate_yaround"
                  "selected_activate_yfill"
                  "selected_activate_ymargin"
                  "selected_activate_ymaximum"
                  "selected_activate_yminimum"
                  "selected_activate_yoffset"
                  "selected_activate_ypadding"
                  "selected_activate_ypos"
                  "selected_activate_yzoom"
                  "selected_activate_zoom"
                  "selected_align"
                  "selected_alignaround"
                  "selected_alpha"
                  "selected_anchor"
                  "selected_angle"
                  "selected_antialias"
                  "selected_area"
                  "selected_around"
                  "selected_background"
                  "selected_bar_invert"
                  "selected_bar_resizing"
                  "selected_bar_vertical"
                  "selected_black_color"
                  "selected_bold"
                  "selected_bottom_bar"
                  "selected_bottom_gutter"
                  "selected_bottom_margin"
                  "selected_bottom_padding"
                  "selected_box_layout"
                  "selected_clipping"
                  "selected_color"
                  "selected_corner1"
                  "selected_corner2"
                  "selected_crop"
                  "selected_delay"
                  "selected_drop_shadow"
                  "selected_drop_shadow_color"
                  "selected_first_indent"
                  "selected_first_spacing"
                  "selected_fit_first"
                  "selected_font"
                  "selected_foreground"
                  "selected_hover"
                  "selected_hover_align"
                  "selected_hover_alignaround"
                  "selected_hover_alpha"
                  "selected_hover_anchor"
                  "selected_hover_angle"
                  "selected_hover_antialias"
                  "selected_hover_area"
                  "selected_hover_around"
                  "selected_hover_background"
                  "selected_hover_bar_invert"
                  "selected_hover_bar_resizing"
                  "selected_hover_bar_vertical"
                  "selected_hover_black_color"
                  "selected_hover_bold"
                  "selected_hover_bottom_bar"
                  "selected_hover_bottom_gutter"
                  "selected_hover_bottom_margin"
                  "selected_hover_bottom_padding"
                  "selected_hover_box_layout"
                  "selected_hover_clipping"
                  "selected_hover_color"
                  "selected_hover_corner1"
                  "selected_hover_corner2"
                  "selected_hover_crop"
                  "selected_hover_delay"
                  "selected_hover_drop_shadow"
                  "selected_hover_drop_shadow_color"
                  "selected_hover_first_indent"
                  "selected_hover_first_spacing"
                  "selected_hover_fit_first"
                  "selected_hover_font"
                  "selected_hover_foreground"
                  "selected_hover_italic"
                  "selected_hover_justify"
                  "selected_hover_language"
                  "selected_hover_layout"
                  "selected_hover_left_bar"
                  "selected_hover_left_gutter"
                  "selected_hover_left_margin"
                  "selected_hover_left_padding"
                  "selected_hover_line_spacing"
                  "selected_hover_min_width"
                  "selected_hover_minwidth"
                  "selected_hover_mouse"
                  "selected_hover_offset"
                  "selected_hover_outlines"
                  "selected_hover_pos"
                  "selected_hover_radius"
                  "selected_hover_rest_indent"
                  "selected_hover_right_bar"
                  "selected_hover_right_gutter"
                  "selected_hover_right_margin"
                  "selected_hover_right_padding"
                  "selected_hover_rotate"
                  "selected_hover_rotate_pad"
                  "selected_hover_size"
                  "selected_hover_size_group"
                  "selected_hover_slow_abortable"
                  "selected_hover_slow_cps"
                  "selected_hover_slow_cps_multiplier"
                  "selected_hover_sound"
                  "selected_hover_spacing"
                  "selected_hover_subpixel"
                  "selected_hover_text_align"
                  "selected_hover_text_y_fudge"
                  "selected_hover_thumb"
                  "selected_hover_thumb_offset"
                  "selected_hover_thumb_shadow"
                  "selected_hover_top_bar"
                  "selected_hover_top_gutter"
                  "selected_hover_top_margin"
                  "selected_hover_top_padding"
                  "selected_hover_underline"
                  "selected_hover_unscrollable"
                  "selected_hover_xalign"
                  "selected_hover_xanchor"
                  "selected_hover_xanchoraround"
                  "selected_hover_xaround"
                  "selected_hover_xfill"
                  "selected_hover_xmargin"
                  "selected_hover_xmaximum"
                  "selected_hover_xminimum"
                  "selected_hover_xoffset"
                  "selected_hover_xpadding"
                  "selected_hover_xpos"
                  "selected_hover_xzoom"
                  "selected_hover_yalign"
                  "selected_hover_yanchor"
                  "selected_hover_yanchoraround"
                  "selected_hover_yaround"
                  "selected_hover_yfill"
                  "selected_hover_ymargin"
                  "selected_hover_ymaximum"
                  "selected_hover_yminimum"
                  "selected_hover_yoffset"
                  "selected_hover_ypadding"
                  "selected_hover_ypos"
                  "selected_hover_yzoom"
                  "selected_hover_zoom"
                  "selected_idle"
                  "selected_idle_align"
                  "selected_idle_alignaround"
                  "selected_idle_alpha"
                  "selected_idle_anchor"
                  "selected_idle_angle"
                  "selected_idle_antialias"
                  "selected_idle_area"
                  "selected_idle_around"
                  "selected_idle_background"
                  "selected_idle_bar_invert"
                  "selected_idle_bar_resizing"
                  "selected_idle_bar_vertical"
                  "selected_idle_black_color"
                  "selected_idle_bold"
                  "selected_idle_bottom_bar"
                  "selected_idle_bottom_gutter"
                  "selected_idle_bottom_margin"
                  "selected_idle_bottom_padding"
                  "selected_idle_box_layout"
                  "selected_idle_clipping"
                  "selected_idle_color"
                  "selected_idle_corner1"
                  "selected_idle_corner2"
                  "selected_idle_crop"
                  "selected_idle_delay"
                  "selected_idle_drop_shadow"
                  "selected_idle_drop_shadow_color"
                  "selected_idle_first_indent"
                  "selected_idle_first_spacing"
                  "selected_idle_fit_first"
                  "selected_idle_font"
                  "selected_idle_foreground"
                  "selected_idle_italic"
                  "selected_idle_justify"
                  "selected_idle_language"
                  "selected_idle_layout"
                  "selected_idle_left_bar"
                  "selected_idle_left_gutter"
                  "selected_idle_left_margin"
                  "selected_idle_left_padding"
                  "selected_idle_line_spacing"
                  "selected_idle_min_width"
                  "selected_idle_minwidth"
                  "selected_idle_mouse"
                  "selected_idle_offset"
                  "selected_idle_outlines"
                  "selected_idle_pos"
                  "selected_idle_radius"
                  "selected_idle_rest_indent"
                  "selected_idle_right_bar"
                  "selected_idle_right_gutter"
                  "selected_idle_right_margin"
                  "selected_idle_right_padding"
                  "selected_idle_rotate"
                  "selected_idle_rotate_pad"
                  "selected_idle_size"
                  "selected_idle_size_group"
                  "selected_idle_slow_abortable"
                  "selected_idle_slow_cps"
                  "selected_idle_slow_cps_multiplier"
                  "selected_idle_sound"
                  "selected_idle_spacing"
                  "selected_idle_subpixel"
                  "selected_idle_text_align"
                  "selected_idle_text_y_fudge"
                  "selected_idle_thumb"
                  "selected_idle_thumb_offset"
                  "selected_idle_thumb_shadow"
                  "selected_idle_top_bar"
                  "selected_idle_top_gutter"
                  "selected_idle_top_margin"
                  "selected_idle_top_padding"
                  "selected_idle_underline"
                  "selected_idle_unscrollable"
                  "selected_idle_xalign"
                  "selected_idle_xanchor"
                  "selected_idle_xanchoraround"
                  "selected_idle_xaround"
                  "selected_idle_xfill"
                  "selected_idle_xmargin"
                  "selected_idle_xmaximum"
                  "selected_idle_xminimum"
                  "selected_idle_xoffset"
                  "selected_idle_xpadding"
                  "selected_idle_xpos"
                  "selected_idle_xzoom"
                  "selected_idle_yalign"
                  "selected_idle_yanchor"
                  "selected_idle_yanchoraround"
                  "selected_idle_yaround"
                  "selected_idle_yfill"
                  "selected_idle_ymargin"
                  "selected_idle_ymaximum"
                  "selected_idle_yminimum"
                  "selected_idle_yoffset"
                  "selected_idle_ypadding"
                  "selected_idle_ypos"
                  "selected_idle_yzoom"
                  "selected_idle_zoom"
                  "selected_insensitive_align"
                  "selected_insensitive_alignaround"
                  "selected_insensitive_alpha"
                  "selected_insensitive_anchor"
                  "selected_insensitive_angle"
                  "selected_insensitive_antialias"
                  "selected_insensitive_area"
                  "selected_insensitive_around"
                  "selected_insensitive_background"
                  "selected_insensitive_bar_invert"
                  "selected_insensitive_bar_resizing"
                  "selected_insensitive_bar_vertical"
                  "selected_insensitive_black_color"
                  "selected_insensitive_bold"
                  "selected_insensitive_bottom_bar"
                  "selected_insensitive_bottom_gutter"
                  "selected_insensitive_bottom_margin"
                  "selected_insensitive_bottom_padding"
                  "selected_insensitive_box_layout"
                  "selected_insensitive_clipping"
                  "selected_insensitive_color"
                  "selected_insensitive_corner1"
                  "selected_insensitive_corner2"
                  "selected_insensitive_crop"
                  "selected_insensitive_delay"
                  "selected_insensitive_drop_shadow"
                  "selected_insensitive_drop_shadow_color"
                  "selected_insensitive_first_indent"
                  "selected_insensitive_first_spacing"
                  "selected_insensitive_fit_first"
                  "selected_insensitive_font"
                  "selected_insensitive_foreground"
                  "selected_insensitive_italic"
                  "selected_insensitive_justify"
                  "selected_insensitive_language"
                  "selected_insensitive_layout"
                  "selected_insensitive_left_bar"
                  "selected_insensitive_left_gutter"
                  "selected_insensitive_left_margin"
                  "selected_insensitive_left_padding"
                  "selected_insensitive_line_spacing"
                  "selected_insensitive_min_width"
                  "selected_insensitive_minwidth"
                  "selected_insensitive_mouse"
                  "selected_insensitive_offset"
                  "selected_insensitive_outlines"
                  "selected_insensitive_pos"
                  "selected_insensitive_radius"
                  "selected_insensitive_rest_indent"
                  "selected_insensitive_right_bar"
                  "selected_insensitive_right_gutter"
                  "selected_insensitive_right_margin"
                  "selected_insensitive_right_padding"
                  "selected_insensitive_rotate"
                  "selected_insensitive_rotate_pad"
                  "selected_insensitive_size"
                  "selected_insensitive_size_group"
                  "selected_insensitive_slow_abortable"
                  "selected_insensitive_slow_cps"
                  "selected_insensitive_slow_cps_multiplier"
                  "selected_insensitive_sound"
                  "selected_insensitive_spacing"
                  "selected_insensitive_subpixel"
                  "selected_insensitive_text_align"
                  "selected_insensitive_text_y_fudge"
                  "selected_insensitive_thumb"
                  "selected_insensitive_thumb_offset"
                  "selected_insensitive_thumb_shadow"
                  "selected_insensitive_top_bar"
                  "selected_insensitive_top_gutter"
                  "selected_insensitive_top_margin"
                  "selected_insensitive_top_padding"
                  "selected_insensitive_underline"
                  "selected_insensitive_unscrollable"
                  "selected_insensitive_xalign"
                  "selected_insensitive_xanchor"
                  "selected_insensitive_xanchoraround"
                  "selected_insensitive_xaround"
                  "selected_insensitive_xfill"
                  "selected_insensitive_xmargin"
                  "selected_insensitive_xmaximum"
                  "selected_insensitive_xminimum"
                  "selected_insensitive_xoffset"
                  "selected_insensitive_xpadding"
                  "selected_insensitive_xpos"
                  "selected_insensitive_xzoom"
                  "selected_insensitive_yalign"
                  "selected_insensitive_yanchor"
                  "selected_insensitive_yanchoraround"
                  "selected_insensitive_yaround"
                  "selected_insensitive_yfill"
                  "selected_insensitive_ymargin"
                  "selected_insensitive_ymaximum"
                  "selected_insensitive_yminimum"
                  "selected_insensitive_yoffset"
                  "selected_insensitive_ypadding"
                  "selected_insensitive_ypos"
                  "selected_insensitive_yzoom"
                  "selected_insensitive_zoom"
                  "selected_italic"
                  "selected_justify"
                  "selected_language"
                  "selected_layout"
                  "selected_left_bar"
                  "selected_left_gutter"
                  "selected_left_margin"
                  "selected_left_padding"
                  "selected_line_spacing"
                  "selected_min_width"
                  "selected_minwidth"
                  "selected_mouse"
                  "selected_offset"
                  "selected_outlines"
                  "selected_pos"
                  "selected_radius"
                  "selected_rest_indent"
                  "selected_right_bar"
                  "selected_right_gutter"
                  "selected_right_margin"
                  "selected_right_padding"
                  "selected_rotate"
                  "selected_rotate_pad"
                  "selected_size"
                  "selected_size_group"
                  "selected_slow_abortable"
                  "selected_slow_cps"
                  "selected_slow_cps_multiplier"
                  "selected_sound"
                  "selected_spacing"
                  "selected_subpixel"
                  "selected_text_align"
                  "selected_text_y_fudge"
                  "selected_thumb"
                  "selected_thumb_offset"
                  "selected_thumb_shadow"
                  "selected_top_bar"
                  "selected_top_gutter"
                  "selected_top_margin"
                  "selected_top_padding"
                  "selected_underline"
                  "selected_unscrollable"
                  "selected_xalign"
                  "selected_xanchor"
                  "selected_xanchoraround"
                  "selected_xaround"
                  "selected_xfill"
                  "selected_xmargin"
                  "selected_xmaximum"
                  "selected_xminimum"
                  "selected_xoffset"
                  "selected_xpadding"
                  "selected_xpos"
                  "selected_xzoom"
                  "selected_yalign"
                  "selected_yanchor"
                  "selected_yanchoraround"
                  "selected_yaround"
                  "selected_yfill"
                  "selected_ymargin"
                  "selected_ymaximum"
                  "selected_yminimum"
                  "selected_yoffset"
                  "selected_ypadding"
                  "selected_ypos"
                  "selected_yzoom"
                  "selected_zoom"
                  "size"
                  "size_group"
                  "slow"
                  "slow_abortable"
                  "slow_cps"
                  "slow_cps_multiplier"
                  "sound"
                  "spacing"
                  "style"
                  "style_group"
                  "subpixel"
                  "suffix"
                  "text_align"
                  "text_style"
                  "text_y_fudge"
                  "thumb"
                  "thumb_offset"
                  "thumb_shadow"
                  "top_bar"
                  "top_gutter"
                  "top_margin"
                  "top_padding"
                  "transpose"
                  "underline"
                  "unhovered"
                  "unscrollable"
                  "value"
                  "width"
                  "xadjustment"
                  "xalign"
                  "xanchor"
                  "xanchoraround"
                  "xaround"
                  "xfill"
                  "xmargin"
                  "xmaximum"
                  "xminimum"
                  "xoffset"
                  "xpadding"
                  "xpos"
                  "xzoom"
                  "yadjustment"
                  "yalign"
                  "yanchor"
                  "yanchoraround"
                  "yaround"
                  "yfill"
                  "ymargin"
                  "ymaximum"
                  "yminimum"
                  "yoffset"
                  "ypadding"
                  "ypos"
                  "yzoom"
                  "zoom"))
          symbol-end) (1 font-lock-builtin-face)))
  "Font lock keywords to use in `python-mode' for level 2 decoration.")


(eval-and-compile
  (defun renpy-syntax--context-compiler-macro (form type &optional syntax-ppss)
    (pcase type
      (''comment
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 4 ppss) (nth 8 ppss))))
      (''string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 3 ppss) (nth 8 ppss))))
      (''paren
       `(nth 1 (or ,syntax-ppss (syntax-ppss))))
      (_ form))))


(defun renpy-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro renpy-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      ('comment (and (nth 4 ppss) (nth 8 ppss)))
      ('string (and (nth 3 ppss) (nth 8 ppss)))
      ('paren (nth 1 ppss))
      (_ nil))))

(defun renpy-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defsubst renpy-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside comment or string."
  (nth 8 (or ppss (syntax-ppss))))

(defsubst renpy-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (eql (syntax-class (syntax-after (point)))
       (syntax-class (string-to-syntax ")"))))



(defun renpy-font-lock-assignment-matcher (regexp)
  "Font lock matcher for assignments based on REGEXP.
Return nil if REGEXP matched within a `paren' context (to avoid,
e.g., default values for arguments or passing arguments by name
being treated as assignments) or is followed by an '=' sign (to
avoid '==' being treated as an assignment."
  (lambda (limit)
    (let ((res (re-search-forward regexp limit t)))
      (unless (or (renpy-syntax-context 'paren)
                  (equal (char-after (point)) ?=))
        res))))



(defvar renpy-font-lock-keywords-maximum-decoration
  `((),@renpy-font-lock-keywords-level-2
    ;; Constants
    (,(rx symbol-start (or "False" "None" "NotImplemented" "True")
          symbol-end)
     . font-lock-constant-face)
     ;; Decorators.
    (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                            (0+ "." (1+ (or word ?_)))))
     (1 font-lock-type-face))
    ;; Builtin Exceptions
   (,(rx symbol-start
          (or
           ;; Python 2 and 3:
           "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
           "BufferError" "BytesWarning" "DeprecationWarning" "EOFError"
           "EnvironmentError" "Exception" "FloatingPointError" "FutureWarning"
           "GeneratorExit" "IOError" "ImportError" "ImportWarning"
           "IndentationError" "IndexError" "KeyError" "KeyboardInterrupt"
           "LookupError" "MemoryError" "NameError" "NotImplementedError"
           "OSError" "OverflowError" "PendingDeprecationWarning"
           "ReferenceError" "RuntimeError" "RuntimeWarning" "StopIteration"
           "SyntaxError" "SyntaxWarning" "SystemError" "SystemExit" "TabError"
           "TypeError" "UnboundLocalError" "UnicodeDecodeError"
           "UnicodeEncodeError" "UnicodeError" "UnicodeTranslateError"
           "UnicodeWarning" "UserWarning" "ValueError" "Warning"
           "ZeroDivisionError"
           ;; Python 2:
           "StandardError"
           ;; Python 3:
           "BlockingIOError" "BrokenPipeError" "ChildProcessError"
           "ConnectionAbortedError" "ConnectionError" "ConnectionRefusedError"
           "ConnectionResetError" "FileExistsError" "FileNotFoundError"
           "InterruptedError" "IsADirectoryError" "NotADirectoryError"
           "PermissionError" "ProcessLookupError" "RecursionError"
           "ResourceWarning" "StopAsyncIteration" "TimeoutError"
           ;; OS specific
           "VMSError" "WindowsError")
          symbol-end)
     . font-lock-type-face))
    ;; multiple assignment
    ;; (note that type hints are not allowed for multiple assignments)
    ;;   a, b, c = 1, 2, 3
    ;;   a, *b, c = 1, 2, 3, 4, 5
    ;;   [a, b] = (1, 2)
    ;;   (l[1], l[2]) = (10, 11)
    ;;   (a, b, c, *d) = *x, y = 5, 6, 7, 8, 9
    ;;   (a,) = 'foo'
    ;;   (*a,) = ['foo', 'bar', 'baz']
    ;;   d.x, d.y[0], *d.z = 'a', 'b', 'c', 'd', 'e'
    ;; and variants thereof
    ;; the cases
    ;;   (a) = 5
    ;;   [a] = 5
    ;;   [*a] = 5, 6
    ;; are handled separately below
    (,(python-font-lock-assignment-matcher
        (python-rx (? (or "[" "(") (* space))
                   grouped-assignment-target (* space) ?, (* space)
                   (* assignment-target (* space) ?, (* space))
                   (? assignment-target (* space))
                   (? ?, (* space))
                   (? (or ")" "]") (* space))
                   (group assignment-operator)))
     (1 font-lock-variable-name-face)
     (,(python-rx grouped-assignment-target)
      (progn
        (goto-char (match-end 1))       ; go back after the first symbol
        (match-beginning 2))            ; limit the search until the assignment
      nil
      (1 font-lock-variable-name-face)))
    ;; single assignment with type hints, e.g.
    ;;   a: int = 5
    ;;   b: Tuple[Optional[int], Union[Sequence[str], str]] = (None, 'foo')
    ;;   c: Collection = {1, 2, 3}
    ;;   d: Mapping[int, str] = {1: 'bar', 2: 'baz'}
    (,(python-font-lock-assignment-matcher
        (python-rx grouped-assignment-target (* space)
                   (? ?: (* space) (+ not-simple-operator) (* space))
                   assignment-operator))
     (1 font-lock-variable-name-face))
    ;; special cases
    ;;   (a) = 5
    ;;   [a] = 5
    ;;   [*a] = 5, 6
    (,(python-font-lock-assignment-matcher
       (python-rx (or "[" "(") (* space)
                  grouped-assignment-target (* space)
                  (or ")" "]") (* space)
                  assignment-operator))
     (1 font-lock-variable-name-face))
  "Font lock keywords to use in `python-mode' for maximum decoration.

This decoration level includes everything in
`python-font-lock-keywords-level-2', as well as constants,
decorators, exceptions, and assignments.")


(defvar renpy-font-lock-keywords
  '(renpy-font-lock-keywords-level-1   ; When `font-lock-maximum-decoration' is nil.
    renpy-font-lock-keywords-level-1   ; When `font-lock-maximum-decoration' is 1.
    renpy-font-lock-keywords-level-2   ; When `font-lock-maximum-decoration' is 2.
    renpy-font-lock-keywords-maximum-decoration ; When `font-lock-maximum-decoration'
                                                 ; is more than 1, or t (which it is,
                                                 ; by default).
    )
  "List of font lock keyword specifications to use in `renpy-mode'.

Which one will be chosen depends on the value of
`font-lock-maximum-decoration'.")

(defconst renpy-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (or "\"\"\"" "'''"))
    (0 (ignore (renpy-syntax-stringify))))))


(defvar renpy-prettify-symbols-alist
  '(("lambda"  . ?λ)
    ("and" . ?∧)
    ("or" . ?∨))
  "Value for `prettify-symbols-alist' in `renpy-mode'.")


(defsubst renpy-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun renpy-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((ppss (save-excursion (backward-char 3) (syntax-ppss)))
         (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) 3))
         (quote-ending-pos (point)))
    (cond ((or (nth 4 ppss)             ;Inside a comment
               (and string-start
                    ;; Inside of a string quoted with different triple quotes.
                    (not (eql (char-after string-start)
                              (char-after quote-starting-pos)))))
           ;; Do nothing.
           nil)
          ((nth 5 ppss)
           ;; The first quote is escaped, so it's not part of a triple quote!
           (goto-char (1+ quote-starting-pos)))
          ((null string-start)
           ;; This set of quotes delimit the start of a string.  Put
           ;; string fence syntax on last quote. (bug#49518)
           ;; FIXME: This makes sexp-movement a bit suboptimal since """a"""
           ;; is now treated as 3 strings.
           ;; We could probably have our cake and eat it too by
           ;; putting the string fence on the first quote and then
           ;; convincing `syntax-ppss-flush-cache' to flush to before
           ;; that fence when any char of the 3-char delimiter
           ;; is modified.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|")))
          (t
           ;; This set of quotes delimit the end of a string.  Put
           ;; string fence syntax on first quote. (bug#49518)
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|"))))))

(defvar renpy-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for Renpy files.")

(defvar renpy-dotty-syntax-table
  (let ((table (make-syntax-table renpy-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Renpy files.
It makes underscores and dots word constituent chars.")


;;; Fill paragraph

(defcustom renpy-fill-comment-function 'renpy-fill-comment
  "Function to fill comments.
This is the function used by `renpy-fill-paragraph' to
fill comments."
  :type 'symbol
  :group 'renpy)

(defcustom renpy-fill-string-function 'renpy-fill-string
  "Function to fill strings.
This is the function used by `renpy-fill-paragraph' to
fill strings."
  :type 'symbol
  :group 'renpy)

(defcustom renpy-fill-decorator-function 'renpy-fill-decorator
  "Function to fill decorators.
This is the function used by `renpy-fill-paragraph' to
fill decorators."
  :type 'symbol
  :group 'renpy)

(defcustom renpy-fill-paren-function 'renpy-fill-paren
  "Function to fill parens.
This is the function used by `renpy-fill-paragraph' to
fill parens."
  :type 'symbol
  :group 'renpy)

(defcustom renpy-fill-docstring-style 'pep-257
  "Style used to fill docstrings.
This affects `renpy-fill-string' behavior with regards to
triple quotes positioning.

Possible values are `django', `onetwo', `pep-257', `pep-257-nn',
`symmetric', and nil.  A value of nil won't care about quotes
position and will treat docstrings a normal string, any other
value may result in one of the following docstring styles:

`django':

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

`onetwo':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

`pep-257':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

`pep-257-nn':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

`symmetric':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\""
  :type '(choice
          (const :tag "Don't format docstrings" nil)
          (const :tag "Django's coding standards style." django)
          (const :tag "One newline and start and Two at end style." onetwo)
          (const :tag "PEP-257 with 2 newlines at end of string." pep-257)
          (const :tag "PEP-257 with 1 newline at end of string." pep-257-nn)
          (const :tag "Symmetric style." symmetric))
  :group 'renpy
  :safe (lambda (val)
          (memq val '(django onetwo pep-257 pep-257-nn symmetric nil))))

(defun renpy-fill-paragraph (&optional justify)
  "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation.
Optional argument JUSTIFY defines if the paragraph should be justified."
  (interactive "P")
  (save-excursion
    (cond
     ;; Comments
     ((renpy-syntax-context 'comment)
      (funcall renpy-fill-comment-function justify))
     ;; Strings/Docstrings
     ((save-excursion (or (renpy-syntax-context 'string)
                          (equal (string-to-syntax "|")
                                 (syntax-after (point)))))
      (funcall renpy-fill-string-function justify))
     ;; Decorators
     ((equal (char-after (save-excursion
                           (renpy-nav-beginning-of-statement))) ?@)
      (funcall renpy-fill-decorator-function justify))
     ;; Parens
     ((or (renpy-syntax-context 'paren)
          (looking-at (renpy-rx open-paren))
          (save-excursion
            (skip-syntax-forward "^(" (line-end-position))
            (looking-at (renpy-rx open-paren))))
      (funcall renpy-fill-paren-function justify))
     (t t))))

(defun renpy-fill-comment (&optional justify)
  "Comment fill function for `renpy-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (fill-comment-paragraph justify))

(defun renpy-fill-string (&optional justify)
  "String fill function for `renpy-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (let* ((str-start-pos
          (set-marker
           (make-marker)
           (or (renpy-syntax-context 'string)
               (and (equal (string-to-syntax "|")
                           (syntax-after (point)))
                    (point)))))
         ;; JT@2021-09-21: Since bug#49518's fix this will always be 1
         (num-quotes (renpy-syntax-count-quotes
                      (char-after str-start-pos) str-start-pos))
         (str-line-start-pos
          (save-excursion
            (goto-char str-start-pos)
            (beginning-of-line)
            (point-marker)))
         (str-end-pos
          (save-excursion
            (goto-char (+ str-start-pos num-quotes))
            (or (re-search-forward (rx (syntax string-delimiter)) nil t)
                (goto-char (point-max)))
            (point-marker)))
         (multi-line-p
          ;; Docstring styles may vary for one-liners and multi-liners.
          (> (count-matches "\n" str-start-pos str-end-pos) 0))
         (delimiters-style
          (pcase renpy-fill-docstring-style
            ;; delimiters-style is a cons cell with the form
            ;; (START-NEWLINES .  END-NEWLINES). When any of the sexps
            ;; is NIL means to not add any newlines for start or end
            ;; of docstring.  See `renpy-fill-docstring-style' for a
            ;; graphic idea of each style.
            ('django (cons 1 1))
            ('onetwo (and multi-line-p (cons 1 2)))
            ('pep-257 (and multi-line-p (cons nil 2)))
            ('pep-257-nn (and multi-line-p (cons nil 1)))
            ('symmetric (and multi-line-p (cons 1 1)))))
         (fill-paragraph-function))
    (save-restriction
      (narrow-to-region str-line-start-pos str-end-pos)
      (fill-paragraph justify))
    (save-excursion
      (when (and (renpy-info-docstring-p) renpy-fill-docstring-style)
        ;; Add the number of newlines indicated by the selected style
        ;; at the start of the docstring.
        (goto-char (+ str-start-pos num-quotes))
        (delete-region (point) (progn
                                 (skip-syntax-forward "> ")
                                 (point)))
        (and (car delimiters-style)
             (or (newline (car delimiters-style)) t)
             ;; Indent only if a newline is added.
             (indent-according-to-mode))
        ;; Add the number of newlines indicated by the selected style
        ;; at the end of the docstring.
        (goto-char (if (not (= str-end-pos (point-max)))
                       (- str-end-pos num-quotes)
                     str-end-pos))
        (delete-region (point) (progn
                                 (skip-syntax-backward "> ")
                                 (point)))
        (and (cdr delimiters-style)
             ;; Add newlines only if string ends.
             (not (= str-end-pos (point-max)))
             (or (newline (cdr delimiters-style)) t)
             ;; Again indent only if a newline is added.
             (indent-according-to-mode))))) t)

(defun renpy-fill-decorator (&optional _justify)
  "Decorator fill function for `renpy-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  t)

(defun renpy-fill-paren (&optional justify)
  "Paren fill function for `renpy-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (save-restriction
    (narrow-to-region (progn
                        (while (renpy-syntax-context 'paren)
                          (goto-char (1- (point))))
                        (line-beginning-position))
                      (progn
                        (when (not (renpy-syntax-context 'paren))
                          (end-of-line)
                          (when (not (renpy-syntax-context 'paren))
                            (skip-syntax-backward "^)")))
                        (while (and (renpy-syntax-context 'paren)
                                    (not (eobp)))
                          (goto-char (1+ (point))))
                        (point)))
    (let ((paragraph-start "\f\\|[ \t]*$")
          (paragraph-separate ",")
          (fill-paragraph-function))
      (goto-char (point-min))
      (fill-paragraph justify))
    (while (not (eobp))
      (forward-line 1)
      (renpy-indent-line)
      (goto-char (line-end-position))))
  t)

(defun renpy-do-auto-fill ()
  "Like `do-auto-fill', but bind `fill-indent-according-to-mode'."
  ;; See Bug#36056.
  (let ((fill-indent-according-to-mode t))
    (do-auto-fill)))


;;; Indentation

(defcustom renpy-indent-offset 4
  "Default indentation offset for Renpy."
  :group 'renpy
  :type 'integer
  :safe 'integerp)

(defcustom renpy-indent-guess-indent-offset t
  "Non-nil tells Renpy mode to guess `renpy-indent-offset' value."
  :type 'boolean
  :group 'renpy
  :safe 'booleanp)

(defcustom renpy-indent-guess-indent-offset-verbose t
  "Non-nil means to emit a warning when indentation guessing fails."
  :version "25.1"
  :type 'boolean
  :group 'renpy
  :safe' booleanp)

(defcustom renpy-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `renpy-indent-line' call."
  :type '(repeat symbol)
  :group 'renpy)

(defcustom renpy-indent-def-block-scale 2
  "Multiplier applied to indentation inside multi-line def blocks."
  :version "26.1"
  :type 'integer
  :safe 'natnump)


(defun renpy-indent-guess-indent-offset ()
  "Guess and set `renpy-indent-offset' for the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((block-end))
        (while (and (not block-end)
                    (re-search-forward
                     (renpy-rx line-start block-start) nil t))
          (when (and
                 (not (renpy-syntax-context-type))
                 (progn
                   (goto-char (line-end-position))
                   (renpy-util-forward-comment -1)
                   (if (equal (char-before) ?:)
                       t
                     (forward-line 1)
                     (when (renpy-info-block-continuation-line-p)
                       (while (and (renpy-info-continuation-line-p)
                                   (not (eobp)))
                         (forward-line 1))
                       (renpy-util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         t)))))
            (setq block-end (point-marker))))
        (let ((indentation
               (when block-end
                 (goto-char block-end)
                 (renpy-util-forward-comment)
                 (current-indentation))))
          (if (and indentation (not (zerop indentation)))
              (setq-local renpy-indent-offset indentation)
            (when renpy-indent-guess-indent-offset-verbose
              (message "Can't guess renpy-indent-offset, using defaults: %s"
                       renpy-indent-offset))))))))

(defun renpy-indent-context ()
  "Get information about the current indentation context.
Context is returned in a cons with the form (STATUS . START).

STATUS can be one of the following:

keyword
-------

:after-comment
 - Point is after a comment line.
 - START is the position of the \"#\" character.
:inside-string
 - Point is inside string.
 - START is the position of the first quote that starts it.
:no-indent
 - No possible indentation case matches.
 - START is always zero.

:inside-paren
 - Fallback case when point is inside paren.
 - START is the first non space char position *after* the open paren.
:inside-paren-at-closing-nested-paren
 - Point is on a line that contains a nested paren closer.
 - START is the position of the open paren it closes.
:inside-paren-at-closing-paren
 - Point is on a line that contains a paren closer.
 - START is the position of the open paren.
:inside-paren-newline-start
 - Point is inside a paren with items starting in their own line.
 - START is the position of the open paren.
:inside-paren-newline-start-from-block
 - Point is inside a paren with items starting in their own line
   from a block start.
 - START is the position of the open paren.

:after-backslash
 - Fallback case when point is after backslash.
 - START is the char after the position of the backslash.
:after-backslash-assignment-continuation
 - Point is after a backslashed assignment.
 - START is the char after the position of the backslash.
:after-backslash-block-continuation
 - Point is after a backslashed block continuation.
 - START is the char after the position of the backslash.
:after-backslash-dotted-continuation
 - Point is after a backslashed dotted continuation.  Previous
   line must contain a dot to align with.
 - START is the char after the position of the backslash.
:after-backslash-first-line
 - First line following a backslashed continuation.
 - START is the char after the position of the backslash.

:after-block-end
 - Point is after a line containing a block ender.
 - START is the position where the ender starts.
:after-block-start
 - Point is after a line starting a block.
 - START is the position where the block starts.
:after-line
 - Point is after a simple line.
 - START is the position where the previous line starts.
:at-dedenter-block-start
 - Point is on a line starting a dedenter block.
 - START is the position where the dedenter block starts."
    (let ((ppss (save-excursion
                  (beginning-of-line)
                  (syntax-ppss))))
      (cond
       ;; Beginning of buffer.
       ((= (line-number-at-pos) 1)
        (cons :no-indent 0))
       ;; Inside a string.
       ((let ((start (renpy-syntax-context 'string ppss)))
          (when start
            (cons (if (renpy-info-docstring-p)
                      :inside-docstring
                    :inside-string) start))))
       ;; Inside a paren.
       ((let* ((start (renpy-syntax-context 'paren ppss))
               (starts-in-newline
                (when start
                  (save-excursion
                    (goto-char start)
                    (forward-char)
                    (not
                     (= (line-number-at-pos)
                        (progn
                          (renpy-util-forward-comment)
                          (line-number-at-pos))))))))
          (when start
            (cond
             ;; Current line only holds the closing paren.
             ((save-excursion
                (skip-syntax-forward " ")
                (when (and (renpy-syntax-closing-paren-p)
                           (progn
                             (forward-char 1)
                             (not (renpy-syntax-context 'paren))))
                  (cons :inside-paren-at-closing-paren start))))
             ;; Current line only holds a closing paren for nested.
             ((save-excursion
                (back-to-indentation)
                (renpy-syntax-closing-paren-p))
              (cons :inside-paren-at-closing-nested-paren start))
             ;; This line starts from an opening block in its own line.
             ((save-excursion
                (goto-char start)
                (when (and
                       starts-in-newline
                       (save-excursion
                         (back-to-indentation)
                         (looking-at (renpy-rx block-start))))
                  (cons
                   :inside-paren-newline-start-from-block start))))
             (starts-in-newline
              (cons :inside-paren-newline-start start))
             ;; General case.
             (t (cons :inside-paren
                      (save-excursion
                        (goto-char (1+ start))
                        (skip-syntax-forward "(" 1)
                        (skip-syntax-forward " ")
                        (point))))))))
       ;; After backslash.
       ((let ((start (when (not (renpy-syntax-comment-or-string-p ppss))
                       (renpy-info-line-ends-backslash-p
                        (1- (line-number-at-pos))))))
          (when start
            (cond
             ;; Continuation of dotted expression.
             ((save-excursion
                (back-to-indentation)
                (when (eq (char-after) ?\.)
                  ;; Move point back until it's not inside a paren.
                  (while (prog2
                             (forward-line -1)
                             (and (not (bobp))
                                  (renpy-syntax-context 'paren))))
                  (goto-char (line-end-position))
                  (while (and (search-backward
                               "." (line-beginning-position) t)
                              (renpy-syntax-context-type)))
                  ;; Ensure previous statement has dot to align with.
                  (when (and (eq (char-after) ?\.)
                             (not (renpy-syntax-context-type)))
                    (cons :after-backslash-dotted-continuation (point))))))
             ;; Continuation of block definition.
             ((let ((block-continuation-start
                     (renpy-info-block-continuation-line-p)))
                (when block-continuation-start
                  (save-excursion
                    (goto-char block-continuation-start)
                    (re-search-forward
                     (renpy-rx block-start (* space))
                     (line-end-position) t)
                    (cons :after-backslash-block-continuation (point))))))
             ;; Continuation of assignment.
             ((let ((assignment-continuation-start
                     (renpy-info-assignment-continuation-line-p)))
                (when assignment-continuation-start
                  (save-excursion
                    (goto-char assignment-continuation-start)
                    (cons :after-backslash-assignment-continuation (point))))))
             ;; First line after backslash continuation start.
             ((save-excursion
                (goto-char start)
                (when (or (= (line-number-at-pos) 1)
                          (not (renpy-info-beginning-of-backslash
                                (1- (line-number-at-pos)))))
                  (cons :after-backslash-first-line start))))
             ;; General case.
             (t (cons :after-backslash start))))))
       ;; After beginning of block.
       ((let ((start (save-excursion
                       (back-to-indentation)
                       (renpy-util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         (renpy-nav-beginning-of-block)))))
          (when start
            (cons :after-block-start start))))
       ;; At dedenter statement.
       ((let ((start (renpy-info-dedenter-statement-p)))
          (when start
            (cons :at-dedenter-block-start start))))
       ;; After normal line, comment or ender (default case).
       ((save-excursion
          (back-to-indentation)
          (skip-chars-backward " \t\n")
          (if (bobp)
              (cons :no-indent 0)
            (renpy-nav-beginning-of-statement)
            (cons
             (cond ((renpy-info-current-line-comment-p)
                    :after-comment)
                   ((save-excursion
                      (goto-char (line-end-position))
                      (renpy-util-forward-comment -1)
                      (renpy-nav-beginning-of-statement)
                      (looking-at (renpy-rx block-ender)))
                    :after-block-end)
                   (t :after-line))
             (point))))))))

(defun renpy-indent--calculate-indentation ()
  "Internal implementation of `renpy-indent-calculate-indentation'.
May return an integer for the maximum possible indentation at
current context or a list of integers.  The latter case is only
happening for :at-dedenter-block-start context since the
possibilities can be narrowed to specific indentation points."
    (save-excursion
      (pcase (renpy-indent-context)
        (`(:no-indent . ,_) (prog-first-column)) ; usually 0
        (`(,(or :after-line
                :after-comment
                :inside-string
                :after-backslash) . ,start)
         ;; Copy previous indentation.
         (goto-char start)
         (current-indentation))
        (`(,(or :inside-paren-at-closing-paren
                :inside-paren-at-closing-nested-paren) . ,start)
         (goto-char (+ 1 start))
         (if (looking-at "[ \t]*\\(?:#\\|$\\)")
             ;; Copy previous indentation.
             (current-indentation)
           ;; Align with opening paren.
           (current-column)))
        (`(:inside-docstring . ,start)
         (let* ((line-indentation (current-indentation))
                (base-indent (progn
                               (goto-char start)
                               (current-indentation))))
           (max line-indentation base-indent)))
        (`(,(or :after-block-start
                :after-backslash-first-line
                :after-backslash-assignment-continuation
                :inside-paren-newline-start) . ,start)
         ;; Add one indentation level.
         (goto-char start)
         (+ (current-indentation) renpy-indent-offset))
        (`(,(or :inside-paren
                :after-backslash-block-continuation
                :after-backslash-dotted-continuation) . ,start)
         ;; Use the column given by the context.
         (goto-char start)
         (current-column))
        (`(:after-block-end . ,start)
         ;; Subtract one indentation level.
         (goto-char start)
         (- (current-indentation) renpy-indent-offset))
        (`(:at-dedenter-block-start . ,_)
         ;; List all possible indentation levels from opening blocks.
         (let ((opening-block-start-points
                (renpy-info-dedenter-opening-block-positions)))
           (if (not opening-block-start-points)
               (prog-first-column) ; if not found default to first column
             (mapcar (lambda (pos)
                       (save-excursion
                         (goto-char pos)
                         (current-indentation)))
                     opening-block-start-points))))
        (`(,(or :inside-paren-newline-start-from-block) . ,start)
         (goto-char start)
         (+ (current-indentation)
            (* renpy-indent-offset renpy-indent-def-block-scale))))))

(defun renpy-indent--calculate-levels (indentation)
  "Calculate levels list given INDENTATION.
Argument INDENTATION can either be an integer or a list of
integers.  Levels are returned in ascending order, and in the
case INDENTATION is a list, this order is enforced."
  (if (listp indentation)
      (sort (copy-sequence indentation) #'<)
    (nconc (number-sequence (prog-first-column) (1- indentation)
                            renpy-indent-offset)
           (list indentation))))

(defun renpy-indent--previous-level (levels indentation)
  "Return previous level from LEVELS relative to INDENTATION."
  (let* ((levels (sort (copy-sequence levels) #'>))
         (default (car levels)))
    (catch 'return
      (dolist (level levels)
        (when (funcall #'< level indentation)
          (throw 'return level)))
      default)))

(defun renpy-indent-calculate-indentation (&optional previous)
  "Calculate indentation.
Get indentation of PREVIOUS level when argument is non-nil.
Return the max level of the cycle when indentation reaches the
minimum."
  (let* ((indentation (renpy-indent--calculate-indentation))
         (levels (renpy-indent--calculate-levels indentation)))
    (if previous
        (renpy-indent--previous-level levels (current-indentation))
      (if levels
          (apply #'max levels)
        (prog-first-column)))))

(defun renpy-indent-line (&optional previous)
  "Internal implementation of `renpy-indent-line-function'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level."
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to
       (renpy-indent-calculate-indentation previous))
      (renpy-info-dedenter-opening-block-message))
    (when follow-indentation-p
      (back-to-indentation))))

(defun renpy-indent-calculate-levels ()
  "Return possible indentation levels."
  (renpy-indent--calculate-levels
   (renpy-indent--calculate-indentation)))

(defun renpy-indent-line-function ()
  "`indent-line-function' for Renpy mode.
When the variable `last-command' is equal to one of the symbols
inside `renpy-indent-trigger-commands' it cycles possible
indentation levels from right to left."
  (renpy-indent-line
   (and (memq this-command renpy-indent-trigger-commands)
        (eq last-command this-command))))

(defun renpy-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp))
           (not (renpy-syntax-comment-or-string-p))
           (= (current-indentation) (current-column)))
      (renpy-indent-line t)
      t))

(defun renpy-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is not in between the indentation."
  (interactive "*p")
  (unless (renpy-indent-dedent-line)
    (backward-delete-char-untabify arg)))

(put 'renpy-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun renpy-indent-region (start end)
  "Indent a Renpy region automagically.

Called from a program, START and END specify the region to indent."
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (when (and
                   ;; Skip if previous line is empty or a comment.
                   (save-excursion
                     (let ((line-is-comment-p
                            (renpy-info-current-line-comment-p)))
                       (forward-line -1)
                       (not
                        (or (and (renpy-info-current-line-comment-p)
                                 ;; Unless this line is a comment too.
                                 (not line-is-comment-p))
                            (renpy-info-current-line-empty-p)))))
                   ;; Don't mess with strings, unless it's the
                   ;; enclosing set of quotes or a docstring.
                   (or (not (renpy-syntax-context 'string))
                       (eq
                        (syntax-after
                         (+ (1- (point))
                            (current-indentation)
                            (renpy-syntax-count-quotes (char-after) (point))))
                        (string-to-syntax "|"))
                       (renpy-info-docstring-p))
                   ;; Skip if current line is a block start, a
                   ;; dedenter or block ender.
                   (save-excursion
                     (back-to-indentation)
                     (not (looking-at
                           (renpy-rx
                            (or block-start dedenter block-ender))))))
              (renpy-indent-line)))
        (forward-line 1))
      (move-marker end nil))))

(defun renpy-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `renpy-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count renpy-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (user-error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(defun renpy-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
COUNT defaults to `renpy-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count)
                  renpy-indent-offset))
    (indent-rigidly start end count)))

(defun renpy-indent-post-self-insert-function ()
  "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event)
             (not (renpy-syntax-context 'string))
             (save-excursion
               (beginning-of-line)
               (not (renpy-syntax-context 'string (syntax-ppss)))))
    (cond
     ;; Electric indent inside parens
     ((and
       (not (bolp))
       (let ((paren-start (renpy-syntax-context 'paren)))
         ;; Check that point is inside parens.
         (when paren-start
           (not
            ;; Filter the case where input is happening in the same
            ;; line where the open paren is.
            (= (line-number-at-pos)
               (line-number-at-pos paren-start)))))
       ;; When content has been added before the closing paren or a
       ;; comma has been inserted, it's ok to do the trick.
       (or
        (memq (char-after) '(?\) ?\] ?\}))
        (eq (char-before) ?,)))
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((indentation (renpy-indent-calculate-indentation)))
          (when (and (numberp indentation) (< (current-indentation) indentation))
            (indent-line-to indentation)))))
     ;; Electric colon
     ((and (eq ?: last-command-event)
           (memq ?: electric-indent-chars)
           (not current-prefix-arg)
           ;; Trigger electric colon only at end of line
           (eolp)
           ;; Avoid re-indenting on extra colon
           (not (equal ?: (char-before (1- (point)))))
           (not (renpy-syntax-comment-or-string-p)))
      ;; Just re-indent dedenters
      (let ((dedenter-pos (renpy-info-dedenter-statement-p)))
        (when dedenter-pos
          (let ((start (copy-marker dedenter-pos))
                (end (point-marker)))
            (save-excursion
              (goto-char start)
              (renpy-indent-line)
              (unless (= (line-number-at-pos start)
                         (line-number-at-pos end))
                ;; Reindent region if this is a multiline statement
                (renpy-indent-region start end))))))))))


;;; Navigation

(defvar renpy-nav-beginning-of-defun-regexp
  (renpy-rx line-start (* space) defun (+ space) (group symbol-name))
  "Regexp matching class or function definition.
The name of the defun should be grouped so it can be retrieved
via `match-string'.")

(defun renpy-nav--beginning-of-defun (&optional arg)
  "Internal implementation of `renpy-nav-beginning-of-defun'.
With positive ARG search backwards, else search forwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let* ((re-search-fn (if (> arg 0)
                           #'re-search-backward
                         #'re-search-forward))
         (line-beg-pos (line-beginning-position))
         (line-content-start (+ line-beg-pos (current-indentation)))
         (pos (point-marker))
         (body-indentation
          (and (> arg 0)
               (save-excursion
                 (while (and
                         (not (renpy-info-looking-at-beginning-of-defun))
                         (renpy-nav-backward-block)))
                 (or (and (renpy-info-looking-at-beginning-of-defun)
                          (+ (current-indentation) renpy-indent-offset))
                     0))))
         (found
          (progn
            (when (and (renpy-info-looking-at-beginning-of-defun)
                       (or (< arg 0)
                           ;; If looking at beginning of defun, and if
                           ;; pos is > line-content-start, ensure a
                           ;; backward re search match this defun by
                           ;; going to end of line before calling
                           ;; re-search-fn bug#40563
                           (and (> arg 0) (> pos line-content-start))))
              (end-of-line 1))

            (while (and (funcall re-search-fn
                                 renpy-nav-beginning-of-defun-regexp nil t)
                        (or (renpy-syntax-context-type)
                            ;; Handle nested defuns when moving
                            ;; backwards by checking indentation.
                            (and (> arg 0)
                                 (not (= (current-indentation) 0))
                                 (>= (current-indentation) body-indentation)))))
            (and (renpy-info-looking-at-beginning-of-defun)
                 (or (not (= (line-number-at-pos pos)
                             (line-number-at-pos)))
                     (and (>= (point) line-beg-pos)
                          (<= (point) line-content-start)
                          (> pos line-content-start)))))))
    (if found
        (or (beginning-of-line 1) t)
      (and (goto-char pos) nil))))

(defun renpy-nav-beginning-of-defun (&optional arg)
  "Move point to `beginning-of-defun'.
With positive ARG search backwards else search forward.
ARG nil or 0 defaults to 1.  When searching backwards,
nested defuns are handled with care depending on current
point position.  Return non-nil if point is moved to
`beginning-of-defun'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((found))
    (while (and (not (= arg 0))
                (let ((keep-searching-p
                       (renpy-nav--beginning-of-defun arg)))
                  (when (and keep-searching-p (null found))
                    (setq found t))
                  keep-searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun renpy-nav-end-of-defun ()
  "Move point to the end of def or class.
Returns nil if point is not in a def or class."
  (interactive)
  (let ((beg-defun-indent)
        (beg-pos (point)))
    (when (or (renpy-info-looking-at-beginning-of-defun)
              (renpy-nav-beginning-of-defun 1)
              (renpy-nav-beginning-of-defun -1))
      (setq beg-defun-indent (current-indentation))
      (while (progn
               (renpy-nav-end-of-statement)
               (renpy-util-forward-comment 1)
               (and (> (current-indentation) beg-defun-indent)
                    (not (eobp)))))
      (renpy-util-forward-comment -1)
      (forward-line 1)
      ;; Ensure point moves forward.
      (and (> beg-pos (point)) (goto-char beg-pos)))))

(defun renpy-nav--syntactically (fn poscompfn &optional contextfn)
  "Move point using FN avoiding places with specific context.
FN must take no arguments.  POSCOMPFN is a two arguments function
used to compare current and previous point after it is moved
using FN, this is normally a less-than or greater-than
comparison.  Optional argument CONTEXTFN defaults to
`renpy-syntax-context-type' and is used for checking current
point context, it must return a non-nil value if this point must
be skipped."
  (let ((contextfn (or contextfn 'renpy-syntax-context-type))
        (start-pos (point-marker))
        (prev-pos))
    (catch 'found
      (while t
        (let* ((newpos
                (and (funcall fn) (point-marker)))
               (context (funcall contextfn)))
          (cond ((and (not context) newpos
                      (or (and (not prev-pos) newpos)
                          (and prev-pos newpos
                               (funcall poscompfn newpos prev-pos))))
                 (throw 'found (point-marker)))
                ((and newpos context)
                 (setq prev-pos (point)))
                (t (when (not newpos) (goto-char start-pos))
                   (throw 'found nil))))))))

(defun renpy-nav--forward-defun (arg)
  "Internal implementation of renpy-nav-{backward,forward}-defun.
Uses ARG to define which function to call, and how many times
repeat it."
  (let ((found))
    (while (and (> arg 0)
                (setq found
                      (renpy-nav--syntactically
                       (lambda ()
                         (re-search-forward
                          renpy-nav-beginning-of-defun-regexp nil t))
                       '>)))
      (setq arg (1- arg)))
    (while (and (< arg 0)
                (setq found
                      (renpy-nav--syntactically
                       (lambda ()
                         (re-search-backward
                          renpy-nav-beginning-of-defun-regexp nil t))
                       '<)))
      (setq arg (1+ arg)))
    found))

(defun renpy-nav-backward-defun (&optional arg)
  "Navigate to closer defun backward ARG times.
Unlikely `renpy-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (renpy-nav--forward-defun (- (or arg 1))))

(defun renpy-nav-forward-defun (&optional arg)
  "Navigate to closer defun forward ARG times.
Unlikely `renpy-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (renpy-nav--forward-defun (or arg 1)))

(defun renpy-nav-beginning-of-statement ()
  "Move to start of current statement."
  (interactive "^")
  (forward-line 0)
  (let* ((ppss (syntax-ppss))
         (context-point
          (or
           (renpy-syntax-context 'paren ppss)
           (renpy-syntax-context 'string ppss))))
    (cond ((bobp))
          (context-point
           (goto-char context-point)
           (renpy-nav-beginning-of-statement))
          ((save-excursion
             (forward-line -1)
             (renpy-info-line-ends-backslash-p))
           (forward-line -1)
           (renpy-nav-beginning-of-statement))))
  (back-to-indentation)
  (point-marker))

(defun renpy-nav-end-of-statement (&optional noend)
  "Move to end of current statement.
Optional argument NOEND is internal and makes the logic to not
jump to the end of line when moving forward searching for the end
of the statement."
  (interactive "^")
  (let (string-start bs-pos (last-string-end 0))
    (while (and (or noend (goto-char (line-end-position)))
                (not (eobp))
                (cond ((setq string-start (renpy-syntax-context 'string))
                       ;; The assertion can only fail if syntax table
                       ;; text properties and the `syntax-ppss' cache
                       ;; are somehow out of whack.  This has been
                       ;; observed when using `syntax-ppss' during
                       ;; narrowing.
                       (when (>= string-start last-string-end)
                         (goto-char string-start)
                         (if (renpy-syntax-context 'paren)
                             ;; Ended up inside a paren, roll again.
                             (renpy-nav-end-of-statement t)
                           ;; This is not inside a paren, move to the
                           ;; end of this string.
                           (goto-char (+ (point)
                                         (renpy-syntax-count-quotes
                                          (char-after (point)) (point))))
                           (setq last-string-end
                                 (or (re-search-forward
                                      (rx (syntax string-delimiter)) nil t)
                                     (goto-char (point-max)))))))
                      ((renpy-syntax-context 'paren)
                       ;; The statement won't end before we've escaped
                       ;; at least one level of parenthesis.
                       (condition-case err
                           (goto-char (scan-lists (point) 1 -1))
                         (scan-error (goto-char (nth 3 err)))))
                      ((setq bs-pos (renpy-info-line-ends-backslash-p))
                       (goto-char bs-pos)
                       (forward-line 1))))))
  (point-marker))

(defun renpy-nav-backward-statement (&optional arg)
  "Move backward to previous statement.
With ARG, repeat.  See `renpy-nav-forward-statement'."
  (interactive "^p")
  (or arg (setq arg 1))
  (renpy-nav-forward-statement (- arg)))

(defun renpy-nav-forward-statement (&optional arg)
  "Move forward to next statement.
With ARG, repeat.  With negative argument, move ARG times
backward to previous statement."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (renpy-nav-end-of-statement)
    (renpy-util-forward-comment)
    (renpy-nav-beginning-of-statement)
    (setq arg (1- arg)))
  (while (< arg 0)
    (renpy-nav-beginning-of-statement)
    (renpy-util-forward-comment -1)
    (renpy-nav-beginning-of-statement)
    (setq arg (1+ arg))))

(defun renpy-nav-beginning-of-block ()
  "Move to start of current block."
  (interactive "^")
  (let ((starting-pos (point)))
    (if (progn
          (renpy-nav-beginning-of-statement)
          (looking-at (renpy-rx block-start)))
        (point-marker)
      ;; Go to first line beginning a statement
      (while (and (not (bobp))
                  (or (and (renpy-nav-beginning-of-statement) nil)
                      (renpy-info-current-line-comment-p)
                      (renpy-info-current-line-empty-p)))
        (forward-line -1))
      (let ((block-matching-indent
             (- (current-indentation) renpy-indent-offset)))
        (while
            (and (renpy-nav-backward-block)
                 (> (current-indentation) block-matching-indent)))
        (if (and (looking-at (renpy-rx block-start))
                 (= (current-indentation) block-matching-indent))
            (point-marker)
          (and (goto-char starting-pos) nil))))))

(defun renpy-nav-end-of-block ()
  "Move to end of current block."
  (interactive "^")
  (when (renpy-nav-beginning-of-block)
    (let ((block-indentation (current-indentation)))
      (renpy-nav-end-of-statement)
      (while (and (forward-line 1)
                  (not (eobp))
                  (or (and (> (current-indentation) block-indentation)
                           (or (renpy-nav-end-of-statement) t))
                      (renpy-info-current-line-comment-p)
                      (renpy-info-current-line-empty-p))))
      (renpy-util-forward-comment -1)
      (point-marker))))

(defun renpy-nav-backward-block (&optional arg)
  "Move backward to previous block of code.
With ARG, repeat.  See `renpy-nav-forward-block'."
  (interactive "^p")
  (or arg (setq arg 1))
  (renpy-nav-forward-block (- arg)))

(defun renpy-nav-forward-block (&optional arg)
  "Move forward to next block of code.
With ARG, repeat.  With negative argument, move ARG times
backward to previous block."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((block-start-regexp
         (renpy-rx line-start (* whitespace) block-start))
        (starting-pos (point)))
    (while (> arg 0)
      (renpy-nav-end-of-statement)
      (while (and
              (re-search-forward block-start-regexp nil t)
              (renpy-syntax-context-type)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (renpy-nav-beginning-of-statement)
      (while (and
              (re-search-backward block-start-regexp nil t)
              (renpy-syntax-context-type)))
      (setq arg (1+ arg)))
    (renpy-nav-beginning-of-statement)
    (if (not (looking-at (renpy-rx block-start)))
        (and (goto-char starting-pos) nil)
      (and (not (= (point) starting-pos)) (point-marker)))))

(defun renpy-nav--lisp-forward-sexp (&optional arg)
  "Standard version `forward-sexp'.
It ignores completely the value of `forward-sexp-function' by
setting it to nil before calling `forward-sexp'.  With positive
ARG move forward only one sexp, else move backwards."
  (let ((forward-sexp-function)
        (arg (if (or (not arg) (> arg 0)) 1 -1)))
    (forward-sexp arg)))

(defun renpy-nav--lisp-forward-sexp-safe (&optional arg)
  "Safe version of standard `forward-sexp'.
When at end of sexp (i.e. looking at an opening/closing paren)
skips it instead of throwing an error.  With positive ARG move
forward only one sexp, else move backwards."
  (let* ((arg (if (or (not arg) (> arg 0)) 1 -1))
         (paren-regexp
          (if (> arg 0) (renpy-rx close-paren) (renpy-rx open-paren)))
         (search-fn
          (if (> arg 0) #'re-search-forward #'re-search-backward)))
    (condition-case nil
        (renpy-nav--lisp-forward-sexp arg)
      (error
       (while (and (funcall search-fn paren-regexp nil t)
                   (renpy-syntax-context 'paren)))))))

(defun renpy-nav--forward-sexp (&optional dir safe skip-parens-p)
  "Move to forward sexp.
With positive optional argument DIR direction move forward, else
backwards.  When optional argument SAFE is non-nil do not throw
errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction."
  (setq dir (or dir 1))
  (unless (= dir 0)
    (let* ((forward-p (if (> dir 0)
                          (and (setq dir 1) t)
                        (and (setq dir -1) nil)))
           (context-type (renpy-syntax-context-type)))
      (cond
       ((memq context-type '(string comment))
        ;; Inside of a string, get out of it.
        (let ((forward-sexp-function))
          (forward-sexp dir)))
       ((and (not skip-parens-p)
             (or (eq context-type 'paren)
                 (if forward-p
                     (eq (syntax-class (syntax-after (point)))
                         (car (string-to-syntax "(")))
                   (eq (syntax-class (syntax-after (1- (point))))
                       (car (string-to-syntax ")"))))))
        ;; Inside a paren or looking at it, lisp knows what to do.
        (if safe
            (renpy-nav--lisp-forward-sexp-safe dir)
          (renpy-nav--lisp-forward-sexp dir)))
       (t
        ;; This part handles the lispy feel of
        ;; `renpy-nav-forward-sexp'.  Knowing everything about the
        ;; current context and the context of the next sexp tries to
        ;; follow the lisp sexp motion commands in a symmetric manner.
        (let* ((context
                (cond
                 ((renpy-info-beginning-of-block-p) 'block-start)
                 ((renpy-info-end-of-block-p) 'block-end)
                 ((renpy-info-beginning-of-statement-p) 'statement-start)
                 ((renpy-info-end-of-statement-p) 'statement-end)))
               (next-sexp-pos
                (save-excursion
                  (if safe
                      (renpy-nav--lisp-forward-sexp-safe dir)
                    (renpy-nav--lisp-forward-sexp dir))
                  (point)))
               (next-sexp-context
                (save-excursion
                  (goto-char next-sexp-pos)
                  (cond
                   ((renpy-info-beginning-of-block-p) 'block-start)
                   ((renpy-info-end-of-block-p) 'block-end)
                   ((renpy-info-beginning-of-statement-p) 'statement-start)
                   ((renpy-info-end-of-statement-p) 'statement-end)
                   ((renpy-info-statement-starts-block-p) 'starts-block)
                   ((renpy-info-statement-ends-block-p) 'ends-block)))))
          (if forward-p
              (cond ((and (not (eobp))
                          (renpy-info-current-line-empty-p))
                     (renpy-util-forward-comment dir)
                     (renpy-nav--forward-sexp dir safe skip-parens-p))
                    ((eq context 'block-start)
                     (renpy-nav-end-of-block))
                    ((eq context 'statement-start)
                     (renpy-nav-end-of-statement))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'ends-block))
                     (goto-char next-sexp-pos)
                     (renpy-nav-end-of-block))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'starts-block))
                     (goto-char next-sexp-pos)
                     (renpy-nav-end-of-block))
                    ((memq context '(statement-end block-end))
                     (goto-char next-sexp-pos)
                     (renpy-nav-end-of-statement))
                    (t (goto-char next-sexp-pos)))
            (cond ((and (not (bobp))
                        (renpy-info-current-line-empty-p))
                   (renpy-util-forward-comment dir)
                   (renpy-nav--forward-sexp dir safe skip-parens-p))
                  ((eq context 'block-end)
                   (renpy-nav-beginning-of-block))
                  ((eq context 'statement-end)
                   (renpy-nav-beginning-of-statement))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'starts-block))
                   (goto-char next-sexp-pos)
                   (renpy-nav-beginning-of-block))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'ends-block))
                   (goto-char next-sexp-pos)
                   (renpy-nav-beginning-of-block))
                  ((memq context '(statement-start block-start))
                   (goto-char next-sexp-pos)
                   (renpy-nav-beginning-of-statement))
                  (t (goto-char next-sexp-pos))))))))))

(defun renpy-nav-forward-sexp (&optional arg safe skip-parens-p)
  "Move forward across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times.  When optional argument SAFE is non-nil do not
throw errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction (forced to t
in interactive calls)."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; Do not follow parens on interactive calls.  This hack to detect
  ;; if the function was called interactively copes with the way
  ;; `forward-sexp' works by calling `forward-sexp-function', losing
  ;; interactive detection by checking `current-prefix-arg'.  The
  ;; reason to make this distinction is that lisp functions like
  ;; `blink-matching-open' get confused causing issues like the one in
  ;; Bug#16191.  With this approach the user gets a symmetric behavior
  ;; when working interactively while called functions expecting
  ;; paren-based sexp motion work just fine.
  (or
   skip-parens-p
   (setq skip-parens-p
         (memq real-this-command
               (list
                #'forward-sexp #'backward-sexp
                #'renpy-nav-forward-sexp #'renpy-nav-backward-sexp
                #'renpy-nav-forward-sexp-safe #'renpy-nav-backward-sexp))))
  (while (> arg 0)
    (renpy-nav--forward-sexp 1 safe skip-parens-p)
    (setq arg (1- arg)))
  (while (< arg 0)
    (renpy-nav--forward-sexp -1 safe skip-parens-p)
    (setq arg (1+ arg))))

(defun renpy-nav-backward-sexp (&optional arg safe skip-parens-p)
  "Move backward across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times.  When optional argument SAFE is non-nil do not
throw errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction (forced to t
in interactive calls)."
  (interactive "^p")
  (or arg (setq arg 1))
  (renpy-nav-forward-sexp (- arg) safe skip-parens-p))

(defun renpy-nav-forward-sexp-safe (&optional arg skip-parens-p)
  "Move forward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times.  With optional argument SKIP-PARENS-P force
sexp motion to ignore parenthesized expressions when looking at
them in either direction (forced to t in interactive calls)."
  (interactive "^p")
  (renpy-nav-forward-sexp arg t skip-parens-p))

(defun renpy-nav-backward-sexp-safe (&optional arg skip-parens-p)
  "Move backward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times.  With optional argument SKIP-PARENS-P force sexp
motion to ignore parenthesized expressions when looking at them in
either direction (forced to t in interactive calls)."
  (interactive "^p")
  (renpy-nav-backward-sexp arg t skip-parens-p))

(defun renpy-nav--up-list (&optional dir)
  "Internal implementation of `renpy-nav-up-list'.
DIR is always 1 or -1 and comes sanitized from
`renpy-nav-up-list' calls."
  (let ((context (renpy-syntax-context-type))
        (forward-p (> dir 0)))
    (cond
     ((memq context '(string comment)))
     ((eq context 'paren)
      (let ((forward-sexp-function))
        (up-list dir)))
     ((and forward-p (renpy-info-end-of-block-p))
      (let ((parent-end-pos
             (save-excursion
               (let ((indentation (and
                                   (renpy-nav-beginning-of-block)
                                   (current-indentation))))
                 (while (and indentation
                             (> indentation 0)
                             (>= (current-indentation) indentation)
                             (renpy-nav-backward-block)))
                 (renpy-nav-end-of-block)))))
        (and (> (or parent-end-pos (point)) (point))
             (goto-char parent-end-pos))))
     (forward-p (renpy-nav-end-of-block))
     ((and (not forward-p)
           (> (current-indentation) 0)
           (renpy-info-beginning-of-block-p))
      (let ((prev-block-pos
             (save-excursion
               (let ((indentation (current-indentation)))
                 (while (and (renpy-nav-backward-block)
                             (>= (current-indentation) indentation))))
               (point))))
        (and (> (point) prev-block-pos)
             (goto-char prev-block-pos))))
     ((not forward-p) (renpy-nav-beginning-of-block)))))

(defun renpy-nav-up-list (&optional arg)
  "Move forward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move backward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (renpy-nav--up-list 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (renpy-nav--up-list -1)
    (setq arg (1+ arg))))

(defun renpy-nav-backward-up-list (&optional arg)
  "Move backward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move forward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (renpy-nav-up-list (- arg)))


;;; Skeletons

(defcustom renpy-skeleton-autoinsert nil
  "Non-nil means template skeletons will be automagically inserted.
This happens when pressing \"if<SPACE>\", for example, to prompt for
the if condition."
  :type 'boolean
  :group 'renpy
  :safe 'booleanp)

(defvar renpy-skeleton-available '()
  "Internal list of available skeletons.")

(define-abbrev-table 'renpy-mode-skeleton-abbrev-table ()
  "Abbrev table for Renpy mode skeletons."
  :case-fixed t
  ;; Allow / inside abbrevs.
  :regexp "\\(?:^\\|[^/]\\)\\<\\([[:word:]/]+\\)\\W*"
  ;; Only expand in code.
  :enable-function (lambda ()
                     (and
                      (not (renpy-syntax-comment-or-string-p))
                      renpy-skeleton-autoinsert)))

(defmacro renpy-skeleton-define (name doc &rest skel)
  "Define a `renpy-mode' skeleton using NAME DOC and SKEL.
The skeleton will be bound to renpy-skeleton-NAME and will
be added to `renpy-mode-skeleton-abbrev-table'."
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (function-name (intern (concat "renpy-skeleton-" name))))
    `(progn
       (define-abbrev renpy-mode-skeleton-abbrev-table
         ,name "" ',function-name :system t)
       (setq renpy-skeleton-available
             (cons ',function-name renpy-skeleton-available))
       (define-skeleton ,function-name
         ,(or doc
              (format "Insert %s statement." name))
         ,@skel))))

(define-abbrev-table 'renpy-mode-abbrev-table ()
  "Abbrev table for Renpy mode."
  :parents (list renpy-mode-skeleton-abbrev-table))

(defmacro renpy-define-auxiliary-skeleton (name &optional doc &rest skel)
  "Define a `renpy-mode' auxiliary skeleton using NAME DOC and SKEL.
The skeleton will be bound to renpy-skeleton-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (function-name (intern (concat "renpy-skeleton--" name)))
         (msg (funcall (if (fboundp 'format-message) #'format-message #'format)
                       "Add `%s' clause? " name)))
    (when (not skel)
      (setq skel
            `(< ,(format "%s:" name) \n \n
                > _ \n)))
    `(define-skeleton ,function-name
       ,(or doc
            (format "Auxiliary skeleton for %s statement." name))
       nil
       (unless (y-or-n-p ,msg)
         (signal 'quit t))
       ,@skel)))

(renpy-define-auxiliary-skeleton else)

(renpy-define-auxiliary-skeleton except)

(renpy-define-auxiliary-skeleton finally)

(renpy-skeleton-define if nil
  "Condition: "
  "if " str ":" \n
  _ \n
  ("other condition, %s: "
   <
   "elif " str ":" \n
   > _ \n nil)
  '(renpy-skeleton--else) | ^)

(renpy-skeleton-define while nil
  "Condition: "
  "while " str ":" \n
  > _ \n
  '(renpy-skeleton--else) | ^)

(renpy-skeleton-define for nil
  "Iteration spec: "
  "for " str ":" \n
  > _ \n
  '(renpy-skeleton--else) | ^)

(renpy-skeleton-define import nil
  "Import from module: "
  "from " str & " " | -5
  "import "
  ("Identifier: " str ", ") -2 \n _)

(renpy-skeleton-define try nil
  nil
  "try:" \n
  > _ \n
  ("Exception, %s: "
   <
   "except " str ":" \n
   > _ \n nil)
  resume:
  '(renpy-skeleton--except)
  '(renpy-skeleton--else)
  '(renpy-skeleton--finally) | ^)

(renpy-skeleton-define def nil
  "Function name: "
  "def " str "(" ("Parameter, %s: "
                  (unless (equal ?\( (char-before)) ", ")
                  str) "):" \n
                  "\"\"\"" - "\"\"\"" \n
                  > _ \n)

(renpy-skeleton-define class nil
  "Class name: "
  "class " str "(" ("Inheritance, %s: "
                    (unless (equal ?\( (char-before)) ", ")
                    str)
  & ")" | -1
  ":" \n
  "\"\"\"" - "\"\"\"" \n
  > _ \n)

(defun renpy-skeleton-add-menu-items ()
  "Add menu items to Renpy->Skeletons menu."
  (let ((skeletons (sort renpy-skeleton-available 'string<)))
    (dolist (skeleton skeletons)
      (easy-menu-add-item
       nil '("Renpy" "Skeletons")
       `[,(format
           "Insert %s" (nth 2 (split-string (symbol-name skeleton) "-")))
         ,skeleton t]))))


;;; Imenu

(defvar renpy-imenu-format-item-label-function
  'renpy-imenu-format-item-label
  "Imenu function used to format an item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar renpy-imenu-format-parent-item-label-function
  'renpy-imenu-format-parent-item-label
  "Imenu function used to format a parent item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar renpy-imenu-format-parent-item-jump-label-function
  'renpy-imenu-format-parent-item-jump-label
  "Imenu function used to format a parent jump item label.
It must be a function with two arguments: TYPE and NAME.")

(defun renpy-imenu-format-item-label (type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s (%s)" name type))

(defun renpy-imenu-format-parent-item-label (type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s..." (renpy-imenu-format-item-label type name)))

(defun renpy-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (if (string= type "class")
      "*class definition*"
    "*function definition*"))

(defun renpy-imenu--get-defun-type-name ()
  "Return defun type and name at current position."
  (when (looking-at renpy-nav-beginning-of-defun-regexp)
    (let ((split (split-string (match-string-no-properties 0))))
      (if (= (length split) 2)
          split
        (list (concat (car split) " " (cadr split))
              (car (last split)))))))

(defun renpy-imenu--put-parent (type name pos tree)
  "Add the parent with TYPE, NAME and POS to TREE."
  (let ((label
         (funcall renpy-imenu-format-item-label-function type name))
        (jump-label
         (funcall renpy-imenu-format-parent-item-jump-label-function type name)))
    (if (not tree)
        (cons label pos)
      (cons label (cons (cons jump-label pos) tree)))))

(defun renpy-imenu--build-tree (&optional min-indent prev-indent tree)
  "Recursively build the tree of nested definitions of a node.
Arguments MIN-INDENT, PREV-INDENT and TREE are internal and should
not be passed explicitly unless you know what you are doing."
  (setq min-indent (or min-indent 0)
        prev-indent (or prev-indent renpy-indent-offset))
  (let* ((pos (renpy-nav-backward-defun))
         (defun-type-name (and pos (renpy-imenu--get-defun-type-name)))
         (type (car defun-type-name))
         (name (cadr defun-type-name))
         (label (when name
                  (funcall renpy-imenu-format-item-label-function type name)))
         (indent (current-indentation))
         (children-indent-limit (+ renpy-indent-offset min-indent)))
    (cond ((not pos)
           ;; Nothing found, probably near to bobp.
           nil)
          ((<= indent min-indent)
           ;; The current indentation points that this is a parent
           ;; node, add it to the tree and stop recursing.
           (renpy-imenu--put-parent type name pos tree))
          (t
           (renpy-imenu--build-tree
            min-indent
            indent
            (if (<= indent children-indent-limit)
                ;; This lies within the children indent offset range,
                ;; so it's a normal child of its parent (i.e., not
                ;; a child of a child).
                (cons (cons label pos) tree)
              ;; Oh no, a child of a child?!  Fear not, we
              ;; know how to roll.  We recursively parse these by
              ;; swapping prev-indent and min-indent plus adding this
              ;; newly found item to a fresh subtree.  This works, I
              ;; promise.
              (cons
               (renpy-imenu--build-tree
                prev-indent indent (list (cons label pos)))
               tree)))))))

(defun renpy-imenu-create-index ()
  "Return tree Imenu alist for the current Renpy buffer.
Change `renpy-imenu-format-item-label-function',
`renpy-imenu-format-parent-item-label-function',
`renpy-imenu-format-parent-item-jump-label-function' to
customize how labels are formatted."
  (goto-char (point-max))
  (let ((index)
        (tree))
    (while (setq tree (renpy-imenu--build-tree))
      (setq index (cons tree index)))
    index))

(defun renpy-imenu-create-flat-index (&optional alist prefix)
  "Return flat outline of the current Renpy buffer for Imenu.
Optional argument ALIST is the tree to be flattened; when nil
`renpy-imenu-create-index' is used with
`renpy-imenu-format-parent-item-jump-label-function'
`renpy-imenu-format-parent-item-label-function'
`renpy-imenu-format-item-label-function' set to
  (lambda (type name) name)
Optional argument PREFIX is used in recursive calls and should
not be passed explicitly.

Converts this:

    ((\"Foo\" . 103)
     (\"Bar\" . 138)
     (\"decorator\"
      (\"decorator\" . 173)
      (\"wrap\"
       (\"wrap\" . 353)
       (\"wrapped_f\" . 393))))

To this:

    ((\"Foo\" . 103)
     (\"Bar\" . 138)
     (\"decorator\" . 173)
     (\"decorator.wrap\" . 353)
     (\"decorator.wrapped_f\" . 393))"
  ;; Inspired by imenu--flatten-index-alist removed in revno 21853.
  (apply
   'nconc
   (mapcar
    (lambda (item)
      (let ((name (if prefix
                      (concat prefix "." (car item))
                    (car item)))
            (pos (cdr item)))
        (cond ((or (numberp pos) (markerp pos))
               (list (cons name pos)))
              ((listp pos)
               (cons
                (cons name (cdar pos))
                (renpy-imenu-create-flat-index (cddr item) name))))))
    (or alist
        (let* ((fn (lambda (_type name) name))
               (renpy-imenu-format-item-label-function fn)
              (renpy-imenu-format-parent-item-label-function fn)
              (renpy-imenu-format-parent-item-jump-label-function fn))
          (renpy-imenu-create-index))))))


;;; Misc helpers

(defun renpy-info-current-defun (&optional include-type)
  "Return name of surrounding function with Renpy compatible dotty syntax.
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function can be used as the value of `add-log-current-defun-function'
since it returns nil if point is not inside a defun."
  (save-restriction
    (widen)
    (save-excursion
      (end-of-line 1)
      (let ((names)
            (starting-indentation (current-indentation))
            (starting-pos (point))
            (first-run t)
            (last-indent)
            (type))
        (catch 'exit
          (while (renpy-nav-beginning-of-defun 1)
            (when (save-match-data
                    (and
                     (or (not last-indent)
                         (< (current-indentation) last-indent))
                     (or
                      (and first-run
                           (save-excursion
                             ;; If this is the first run, we may add
                             ;; the current defun at point.
                             (setq first-run nil)
                             (goto-char starting-pos)
                             (renpy-nav-beginning-of-statement)
                             (beginning-of-line 1)
                             (looking-at-p
                              renpy-nav-beginning-of-defun-regexp)))
                      (< starting-pos
                         (save-excursion
                           (let ((min-indent
                                  (+ (current-indentation)
                                     renpy-indent-offset)))
                             (if (< starting-indentation  min-indent)
                                 ;; If the starting indentation is not
                                 ;; within the min defun indent make the
                                 ;; check fail.
                                 starting-pos
                               ;; Else go to the end of defun and add
                               ;; up the current indentation to the
                               ;; ending position.
                               (renpy-nav-end-of-defun)
                               (+ (point)
                                  (if (>= (current-indentation) min-indent)
                                      (1+ (current-indentation))
                                    0)))))))))
              (save-match-data (setq last-indent (current-indentation)))
              (if (or (not include-type) type)
                  (setq names (cons (match-string-no-properties 1) names))
                (let ((match (split-string (match-string-no-properties 0))))
                  (setq type (car match))
                  (setq names (cons (cadr match) names)))))
            ;; Stop searching ASAP.
            (and (= (current-indentation) 0) (throw 'exit t))))
        (and names
             (concat (and type (format "%s " type))
                     (mapconcat 'identity names ".")))))))

(defun renpy-info-current-symbol (&optional replace-self)
  "Return current symbol using dotty syntax.
With optional argument REPLACE-SELF convert \"self\" to current
parent defun name."
  (let ((name
         (and (not (renpy-syntax-comment-or-string-p))
              (with-syntax-table renpy-dotty-syntax-table
                (let ((sym (symbol-at-point)))
                  (and sym
                       (substring-no-properties (symbol-name sym))))))))
    (when name
      (if (not replace-self)
          name
        (let ((current-defun (renpy-info-current-defun)))
          (if (not current-defun)
              name
            (replace-regexp-in-string
             (renpy-rx line-start word-start "self" word-end ?.)
             (concat
              (mapconcat 'identity
                         (butlast (split-string current-defun "\\."))
                         ".") ".")
             name)))))))

(defun renpy-info-statement-starts-block-p ()
  "Return non-nil if current statement opens a block."
  (save-excursion
    (renpy-nav-beginning-of-statement)
    (looking-at (renpy-rx block-start))))

(defun renpy-info-statement-ends-block-p ()
  "Return non-nil if point is at end of block."
  (let ((end-of-block-pos (save-excursion
                            (renpy-nav-end-of-block)))
        (end-of-statement-pos (save-excursion
                                (renpy-nav-end-of-statement))))
    (and end-of-block-pos end-of-statement-pos
         (= end-of-block-pos end-of-statement-pos))))

(defun renpy-info-beginning-of-statement-p ()
  "Return non-nil if point is at beginning of statement."
  (= (point) (save-excursion
               (renpy-nav-beginning-of-statement)
               (point))))

(defun renpy-info-end-of-statement-p ()
  "Return non-nil if point is at end of statement."
  (= (point) (save-excursion
               (renpy-nav-end-of-statement)
               (point))))

(defun renpy-info-beginning-of-block-p ()
  "Return non-nil if point is at beginning of block."
  (and (renpy-info-beginning-of-statement-p)
       (renpy-info-statement-starts-block-p)))

(defun renpy-info-end-of-block-p ()
  "Return non-nil if point is at end of block."
  (and (renpy-info-end-of-statement-p)
       (renpy-info-statement-ends-block-p)))

(define-obsolete-function-alias
  'renpy-info-closing-block
  'renpy-info-dedenter-opening-block-position "24.4")

(defun renpy-info-dedenter-opening-block-position ()
  "Return the point of the closest block the current line closes.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid renpy file."
  (let ((positions (renpy-info-dedenter-opening-block-positions))
        (indentation (current-indentation))
        (position))
    (while (and (not position)
                positions)
      (save-excursion
        (goto-char (car positions))
        (if (<= (current-indentation) indentation)
            (setq position (car positions))
          (setq positions (cdr positions)))))
    position))

(defun renpy-info-dedenter-opening-block-positions ()
  "Return points of blocks the current line may close sorted by closer.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid renpy file."
  (save-excursion
    (let ((dedenter-pos (renpy-info-dedenter-statement-p)))
      (when dedenter-pos
        (goto-char dedenter-pos)
        (let* ((cur-line (line-beginning-position))
               (pairs '(("elif" "elif" "if")
                        ("else" "if" "elif" "except" "for" "while")
                        ("except" "except" "try")
                        ("finally" "else" "except" "try")))
               (dedenter (match-string-no-properties 0))
               (possible-opening-blocks (cdr (assoc-string dedenter pairs)))
               (collected-indentations)
               (opening-blocks))
          (catch 'exit
            (while (renpy-nav--syntactically
                    (lambda ()
                      (re-search-backward (renpy-rx block-start) nil t))
                    #'<)
              (let ((indentation (current-indentation)))
                (when (and (not (memq indentation collected-indentations))
                           (or (not collected-indentations)
                               (< indentation (apply #'min collected-indentations)))
                           ;; There must be no line with indentation
                           ;; smaller than `indentation' (except for
                           ;; blank lines) between the found opening
                           ;; block and the current line, otherwise it
                           ;; is not an opening block.
                           (save-excursion
                             (forward-line)
                             (let ((no-back-indent t))
                               (save-match-data
                                 (while (and (< (point) cur-line)
                                             (setq no-back-indent
                                                   (or (> (current-indentation) indentation)
                                                       (renpy-info-current-line-empty-p))))
                                   (forward-line)))
                               no-back-indent)))
                  (setq collected-indentations
                        (cons indentation collected-indentations))
                  (when (member (match-string-no-properties 0)
                                possible-opening-blocks)
                    (setq opening-blocks (cons (point) opening-blocks))))
                (when (zerop indentation)
                  (throw 'exit nil)))))
          ;; sort by closer
          (nreverse opening-blocks))))))

(define-obsolete-function-alias
  'renpy-info-closing-block-message
  'renpy-info-dedenter-opening-block-message "24.4")

(defun renpy-info-dedenter-opening-block-message  ()
  "Message the first line of the block the current statement closes."
  (let ((point (renpy-info-dedenter-opening-block-position)))
    (when point
        (message "Closes %s" (save-excursion
                               (goto-char point)
                               (buffer-substring
                                (point) (line-end-position)))))))

(defun renpy-info-dedenter-statement-p ()
  "Return point if current statement is a dedenter.
Sets `match-data' to the keyword that starts the dedenter
statement."
  (save-excursion
    (renpy-nav-beginning-of-statement)
    (when (and (not (renpy-syntax-context-type))
               (looking-at (renpy-rx dedenter)))
      (point))))

(defun renpy-info-line-ends-backslash-p (&optional line-number)
  "Return non-nil if current line ends with backslash.
With optional argument LINE-NUMBER, check that line instead."
  (save-excursion
      (when line-number
        (renpy-util-goto-line line-number))
      (while (and (not (eobp))
                  (goto-char (line-end-position))
                  (renpy-syntax-context 'paren)
                  (not (equal (char-before (point)) ?\\)))
        (forward-line 1))
      (when (equal (char-before) ?\\)
        (point-marker))))

(defun renpy-info-beginning-of-backslash (&optional line-number)
  "Return the point where the backslashed line begins.
Optional argument LINE-NUMBER forces the line number to check against."
  (save-excursion
      (when line-number
        (renpy-util-goto-line line-number))
      (when (renpy-info-line-ends-backslash-p)
        (while (save-excursion
                 (goto-char (line-beginning-position))
                 (renpy-syntax-context 'paren))
          (forward-line -1))
        (back-to-indentation)
        (point-marker))))

(defun renpy-info-continuation-line-p ()
  "Check if current line is continuation of another.
When current line is continuation of another return the point
where the continued line ends."
  (save-excursion
      (let* ((context-type (progn
                             (back-to-indentation)
                             (renpy-syntax-context-type)))
             (line-start (line-number-at-pos))
             (context-start (when context-type
                              (renpy-syntax-context context-type))))
        (cond ((equal context-type 'paren)
               ;; Lines inside a paren are always a continuation line
               ;; (except the first one).
               (renpy-util-forward-comment -1)
               (point-marker))
              ((member context-type '(string comment))
               ;; move forward an roll again
               (goto-char context-start)
               (renpy-util-forward-comment)
               (renpy-info-continuation-line-p))
              (t
               ;; Not within a paren, string or comment, the only way
               ;; we are dealing with a continuation line is that
               ;; previous line contains a backslash, and this can
               ;; only be the previous line from current
               (back-to-indentation)
               (renpy-util-forward-comment -1)
               (when (and (equal (1- line-start) (line-number-at-pos))
                          (renpy-info-line-ends-backslash-p))
                 (point-marker)))))))

(defun renpy-info-block-continuation-line-p ()
  "Return non-nil if current line is a continuation of a block."
  (save-excursion
    (when (renpy-info-continuation-line-p)
      (forward-line -1)
      (back-to-indentation)
      (when (looking-at (renpy-rx block-start))
        (point-marker)))))

(defun renpy-info-assignment-statement-p (&optional current-line-only)
  "Check if current line is an assignment.
With argument CURRENT-LINE-ONLY is non-nil, don't follow any
continuations, just check the if current line is an assignment."
  (save-excursion
    (let ((found nil))
      (if current-line-only
          (back-to-indentation)
        (renpy-nav-beginning-of-statement))
      (while (and
              (re-search-forward (renpy-rx not-simple-operator
                                            assignment-operator
                                            (group not-simple-operator))
                                 (line-end-position) t)
              (not found))
        (save-excursion
          ;; The assignment operator should not be inside a string.
          (backward-char (length (match-string-no-properties 1)))
          (setq found (not (renpy-syntax-context-type)))))
      (when found
        (skip-syntax-forward " ")
        (point-marker)))))

;; TODO: rename to clarify this is only for the first continuation
;; line or remove it and move its body to `renpy-indent-context'.
(defun renpy-info-assignment-continuation-line-p ()
  "Check if current line is the first continuation of an assignment.
When current line is continuation of another with an assignment
return the point of the first non-blank character after the
operator."
  (save-excursion
    (when (renpy-info-continuation-line-p)
      (forward-line -1)
      (renpy-info-assignment-statement-p t))))

(defun renpy-info-looking-at-beginning-of-defun (&optional syntax-ppss)
  "Check if point is at `beginning-of-defun' using SYNTAX-PPSS."
  (and (not (renpy-syntax-context-type (or syntax-ppss (syntax-ppss))))
       (save-excursion
         (beginning-of-line 1)
         (looking-at renpy-nav-beginning-of-defun-regexp))))

(defun renpy-info-current-line-comment-p ()
  "Return non-nil if current line is a comment line."
  (char-equal
   (or (char-after (+ (line-beginning-position) (current-indentation))) ?_)
   ?#))

(defun renpy-info-current-line-empty-p ()
  "Return non-nil if current line is empty, ignoring whitespace."
  (save-excursion
    (beginning-of-line 1)
    (looking-at
     (renpy-rx line-start (* whitespace)
                (group (* not-newline))
                (* whitespace) line-end))
    (string-equal "" (match-string-no-properties 1))))

(defun renpy-info-docstring-p (&optional syntax-ppss)
  "Return non-nil if point is in a docstring.
When optional argument SYNTAX-PPSS is given, use that instead of
point's current `syntax-ppss'."
  ;;; https://www.renpy.org/dev/peps/pep-0257/#what-is-a-docstring
  (save-excursion
    (when (and syntax-ppss (renpy-syntax-context 'string syntax-ppss))
      (goto-char (nth 8 syntax-ppss)))
    (renpy-nav-beginning-of-statement)
    (let ((counter 1)
          (indentation (current-indentation))
          (backward-sexp-point)
          (re (concat "[uU]?[rR]?"
                      (renpy-rx string-delimiter))))
      (when (and
             (not (renpy-info-assignment-statement-p))
             (looking-at-p re)
             ;; Allow up to two consecutive docstrings only.
             (>=
              2
              (let (last-backward-sexp-point)
                (while (and (<= counter 2)
                            (save-excursion
                              (renpy-nav-backward-sexp)
                              (setq backward-sexp-point (point))
                              (and (= indentation (current-indentation))
                                   ;; Make sure we're always moving point.
                                   ;; If we get stuck in the same position
                                   ;; on consecutive loop iterations,
                                   ;; bail out.
                                   (prog1 (not (eql last-backward-sexp-point
                                                    backward-sexp-point))
                                     (setq last-backward-sexp-point
                                           backward-sexp-point))
                                   (looking-at-p
                                    (concat "[uU]?[rR]?"
                                            (renpy-rx string-delimiter))))))
                  ;; Previous sexp was a string, restore point.
                  (goto-char backward-sexp-point)
                  (cl-incf counter))
                counter)))
        (renpy-util-forward-comment -1)
        (renpy-nav-beginning-of-statement)
        (cond ((bobp))
              ((renpy-info-assignment-statement-p) t)
              ((renpy-info-looking-at-beginning-of-defun))
              (t nil))))))

(defun renpy-info-encoding-from-cookie ()
  "Detect current buffer's encoding from its coding cookie.
Returns the encoding as a symbol."
  (let ((first-two-lines
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (forward-line 2)
             (buffer-substring-no-properties
              (point)
              (point-min))))))
    (when (string-match (renpy-rx coding-cookie) first-two-lines)
      (intern (match-string-no-properties 1 first-two-lines)))))

(defun renpy-info-encoding ()
  "Return encoding for file.
Try `renpy-info-encoding-from-cookie', if none is found then
default to utf-8."
  ;; If no encoding is defined, then it's safe to use UTF-8: Renpy 2
  ;; uses ASCII as default while Renpy 3 uses UTF-8.  This means that
  ;; in the worst case scenario renpy.el will make things work for
  ;; Renpy 2 files with unicode data and no encoding defined.
  (or (renpy-info-encoding-from-cookie)
      'utf-8))

;;; Utility functions

(defun renpy-util-forward-comment (&optional direction)
  "Renpy mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (renpy-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

(defun renpy-util-goto-line (line-number)
  "Move point to LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defcustom renpy-forward-sexp-function #'renpy-nav-forward-sexp
  "Function to use when navigating between expressions."
  :version "28.1"
  :group 'renpy
  :group 'renpy-flymake
  :type '(choice (const :tag "Python blocks" renpy-nav-forward-sexp)
                 (const :tag "CC-mode like" nil)
                 function))

(defun renpy-electric-pair-string-delimiter ()
  "Electric pair string delimter."
  (when (and electric-pair-mode
             (memq last-command-event '(?\" ?\'))
             (let ((count 0))
               (while (eq (char-before (- (point) count)) last-command-event)
                 (cl-incf count))
               (= count 3))
             (eq (char-after) last-command-event))
    (save-excursion (insert (make-string 2 last-command-event)))))

(defvar electric-indent-inhibit)
(defvar prettify-symbols-alist)

;;;###autoload
(define-derived-mode renpy-mode prog-mode "Ren'Py"
"Major mode for editing Renpy files.

\\{renpy-mode-map}}"
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")

  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)

  (setq-local forward-sexp-function renpy-forward-sexp-function)

  (setq-local font-lock-defaults
              `(,renpy-font-lock-keywords
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . renpy-font-lock-syntactic-face-function)))
  (setq-local syntax-propertize-function
              renpy-syntax-propertize-function)
  (setq-local indent-line-function #'renpy-indent-line-function)
  (setq-local indent-region-function #'renpy-indent-region)
  ;; Because indentation is not redundant, we cannot safely reindent code.
  (setq-local electric-indent-inhibit t)
  (setq-local electric-indent-chars
              (cons ?: electric-indent-chars))
  ;; Add """ ... """ pairing to electric-pair-mode.
  (add-hook 'post-self-insert-hook
            #'renpy-electric-pair-string-delimiter 'append t)

  (setq-local paragraph-start "\\s-*$")
  (setq-local fill-paragraph-function #'renpy-fill-paragraph)
  (setq-local normal-auto-fill-function #'renpy-do-auto-fill)

  (setq-local beginning-of-defun-function #'renpy-nav-beginning-of-defun)
  (setq-local end-of-defun-function #'renpy-nav-end-of-defun)

  ;; Removed completion at point from python base

  (add-hook 'post-self-insert-hook
            #'renpy-indent-post-self-insert-function 'append 'local)

  (setq-local imenu-create-index-function
              #'renpy-imenu-create-index)

  (setq-local add-log-current-defun-function
              #'renpy-info-current-defun)

  (add-hook 'which-func-functions #'renpy-info-current-defun nil t)

  (setq-local skeleton-further-elements
              '((abbrev-mode nil)
                (< '(backward-delete-char-untabify (min renpy-indent-offset
                                                        (current-column))))
                (^ '(- (1+ (current-indentation))))))

  ;; Removed eldoc-documentation-function-reference

  (add-to-list
   'hs-special-modes-alist
   '(renpy-mode
     "\\s-*\\_<\\(?:def\\|class\\)\\_>"
     ;; Use the empty string as end regexp so it doesn't default to
     ;; "\\s)".  This way parens at end of defun are properly hidden.
     ""
     "#"
     renpy-hideshow-forward-sexp-function
     nil))

  (setq-local outline-regexp (renpy-rx (* space) block-start))
  (setq-local outline-heading-end-regexp ":[^\n]*\n")
  (setq-local outline-level
              (lambda ()
                "`outline-level' function for Renpy mode."
                (1+ (/ (current-indentation) renpy-indent-offset))))

  (setq-local prettify-symbols-alist renpy-prettify-symbols-alist)

  (renpy-skeleton-add-menu-items)

  (make-local-variable 'renpy-shell-internal-buffer)

  ;; Remove flymake reference
  (when renpy-indent-guess-indent-offset
    (renpy-indent-guess-indent-offset)))


(provide 'renpy)
;;; renpy.el ends here
