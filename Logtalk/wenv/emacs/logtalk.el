;; logtalk.el -- font lock support for Logtalk (http://www.logtalk.org/)

;; Copyright (C) 2003 Paulo Moura

;; Author: Paulo Moura
;; Creation date: November 15, 2003
;; Last modification date: December 16, 2003
;; Version: 0.4

;; Installation:
;;
;; Add the following lines in your Emacs init file, for example
;; your ~/.emacs file.
;;
;; (add-hook 'logtalk-mode-hook 'my-logtalk-mode-hook)
;;
;; (defun my-logtalk-mode-hook ()
;;      (cond (window-system
;;              (require 'logtalk-font-lock)
;;              (turn-on-font-lock))))
;;
;; (setq font-lock-maximum-decoration t)
;;
;; (autoload 'logtalk-mode "logtalk" "Major mode for editing Logtalk programs." t)
;; (setq auto-mode-alist (cons '("\\.lgt\\'" . logtalk-mode) auto-mode-alist))



;; setup 

(defvar logtalk-mode-version "0.2"
	"Logtalk mode version number")

(defvar logtalk-mode-hook nil)

(defvar logtalk-mode-map nil) 

(if logtalk-mode-map nil
	(setq logtalk-mode-map (make-keymap)))



;; syntax highlighting 

(defvar logtalk-font-lock-keywords nil)



;; syntax table 

(defvar logtalk-mode-syntax-table
	(let ((logtalk-mode-syntax-table (make-syntax-table)))
		(modify-syntax-entry ?_ "w" logtalk-mode-syntax-table)
		(modify-syntax-entry ?_ "_" logtalk-mode-syntax-table)
		(modify-syntax-entry ?/ ". 14b" logtalk-mode-syntax-table)
		(modify-syntax-entry ?* ". 23b" logtalk-mode-syntax-table)
		(modify-syntax-entry ?% "<" logtalk-mode-syntax-table)
		(modify-syntax-entry ?\n ">" logtalk-mode-syntax-table)
		logtalk-mode-syntax-table)
	"Syntax table for logtalk-mode")



;; create logtalk font-lock-faces

(make-face 'logtalk-directive-face)
(set-face-foreground 'logtalk-directive-face "brown")

(make-face 'logtalk-built-in-predicate-face)
(set-face-foreground 'logtalk-built-in-predicate-face "darkmagenta")

(make-face 'logtalk-built-in-method-face)
(set-face-foreground 'logtalk-built-in-method-face "darkmagenta")

(make-face 'logtalk-message-operator-face)
(set-face-foreground 'logtalk-message-operator-face "blue")

(make-face 'logtalk-string-face)
(set-face-foreground 'logtalk-string-face "firebrick")

(make-face 'logtalk-number-face)
(set-face-foreground 'logtalk-number-face "blue")

(make-face 'logtalk-comment-face)
(set-face-foreground 'logtalk-comment-face "forest green")


;; set the font-lock-comment-face to the logtalk-comment-face

(setq font-lock-comment-face 'logtalk-comment-face)


;; set the font-lock-string-face to the logtalk-string-face

(setq font-lock-string-face 'logtalk-string-face)



(setq logtalk-font-lock-strings
	'(
		("\\(\"\\([^\n\"]\\|\\\\\"\\)*\"\\)" 1 'logtalk-string-face)
		("\\(^\\|[^0-9]\\)\\('\\([^\n']\\|\\\\'\\)*'\\)" 2 'logtalk-string-face)
	))


(setq logtalk-font-lock-directives
	'(
		("\\(\\(end_\\(?:category\\|object\\|protocol\\)\\)\\)\\([\.]\\)" 1 'logtalk-directive-face)
		("\\(category\\|object\\|protocol\\)\\([(]\\)" 1 'logtalk-directive-face)
		("\\(p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\)\\([(]\\)" 1 'logtalk-directive-face)
		("\\(calls\\|d\\(?:iscontiguous\\|ynamic\\)\\|in\\(?:fo\\|itialization\\)\\|m\\(?:\\(?:etapredicat\\|od\\)e\\)\\|op\\|uses\\)\\([(]\\)" 1 'logtalk-directive-face)
		("\\(dynamic\\)\\([\.]\\)" 1 'logtalk-directive-face)
		("\\(\\(?:extend\\|i\\(?:mp\\(?:\\(?:lemen\\|or\\)t\\)\\|nstantiate\\)\\|specialize\\)s\\)\\([(]\\)" 1 'logtalk-directive-face)
	))


(setq logtalk-font-lock-built-in-methods
	'(
		("\\(parameter\\|se\\(?:lf\\|nder\\)\\|this\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
		("\\(current_predicate\\|predicate_property\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
		("\\(a\\(?:bolish\\|ssert[az]\\)\\|clause\\|retract\\(?:all\\)?\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
		("\\(bagof\\|f\\(?:\\(?:ind\\|or\\)all\\)\\|setof\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
		("\\(after\\|before\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
		("\\(phrase\\)\\([(]\\)" 1 'logtalk-built-in-method-face)
	))


(setq logtalk-font-lock-built-in-predicates
	'(
		("\\(current_\\(?:category\\|object\\|protocol\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(create\\(?:_object\\|e_\\(?:category\\|protocol\\)\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(abolish_\\(?:category\\|object\\|protocol\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(\\(?:category\\|object\\|protocol\\)_property\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(extends_\\(?:object\\|protocol\\)\\|i\\(?:mp\\(?:lements_protocol\\|orts_category\\)\\|nstantiates_object\\)\\|specializes_object\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(abolish_events\\|current_event\\|define_events\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(\\(?:curren\\|se\\)t_logtalk_flag\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(logtalk_\\(?:compile\\|load\\|version\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(forall\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(retractall\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; control constructs:
		;;
		("\\(ca\\(?:ll\\|tch\\)\\|throw\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(fail\\|true\\|!\\)" . 'logtalk-built-in-predicate-face)
		;;
		;; logic and control:
		;;
		("\\(once\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\\\\\+\\|repeat" . 'logtalk-built-in-predicate-face)
		;;
		;; term testing:
		;;
		("\\(atom\\(?:ic\\)?\\|compound\\|float\\|\\(?:intege\\|n\\(?:onva\\|umbe\\)\\|va\\)r\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; term comparison:
		;;
		("==\\|@\\(?:=<\\|>=\\|[<>]\\)\\|\\\\==" . 'logtalk-built-in-predicate-face)
		;;
		;; term creation and decomposition:
		;;
		("\\(arg\\|copy_term\\|functor\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("=\\.\\." . 'logtalk-built-in-predicate-face)
		;;
		;; arithemtic evaluation:
		;;
		("\\<\\is\\>" . 'logtalk-built-in-predicate-face)
		;;
		;; arithemtic comparison:
		("=:=\\|@\\(?:=<\\|>=\\|[<>]\\)\\|\\\\==" . 'logtalk-built-in-predicate-face)
		;;
		;; term unification:
		;;
		("\\\\?=" . 'logtalk-built-in-predicate-face)
		;;
		;; evaluable functors:
		;;
		("\\(abs\\|ceiling\\|flo\\(?:at\\(?:_\\(?:\\(?:fractional\\|integer\\)_part\\)\\)?\\|or\\)\\|mod\\|r\\(?:em\\|ound\\)\\|sign\\|truncate\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("//\\|[*+/-]" . 'logtalk-built-in-predicate-face)
		("\\([[:blank:]]\\)\\(rem\\|mod\\)\\([[:blank:]]\\)" 2 'logtalk-built-in-predicate-face)
		;;
		;; other arithemtic functors:
		;;
		("\\(atan\\|cos\\|exp\\|log\\|s\\(?:in\\|qrt\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\*\\*" 1 'logtalk-built-in-predicate-face)
		;;
		;; stream selection and control:
		;;
		("\\(at_end_of_stream\\|c\\(?:lose\\|urrent_\\(?:\\(?:in\\|out\\)put\\)\\)\\|flush_output\\|open\\|s\\(?:et_\\(?:input\\|output\\|stream_position\\)\\|tream_property\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("\\(at_end_of_stream\\)" . 'logtalk-built-in-predicate-face)
		;;
		;; character input/output:
		;;
		("\\(get_c\\(?:har\\|ode\\)\\|nl\\|p\\(?:eek_c\\(?:har\\|ode\\)\\|ut_c\\(?:har\\|ode\\)\\)\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; byte input/output:
		;;
		("\\(\\(?:get\\|p\\(?:eek\\|ut\\)\\)_byte\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; term input/output:
		;;
		("\\(c\\(?:har_conversion\\|urrent_\\(?:char_conversion\\|op\\)\\)\\|op\\|read\\(?:_term\\)?\\|write\\(?:_\\(?:canonical\\|term\\)\\|q\\)?\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; implementation defined hooks functions:
		;;
		("\\(\\(?:curren\\|se\\)t_prolog_flag\\|halt\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		("halt" . 'logtalk-built-in-predicate-face)
		;;
		;; atomic term processing:
		;;
		("\\(atom_\\(?:c\\(?:hars\\|o\\(?:des\\|ncat\\)\\)\\|length\\)\\|char_code\\|number_c\\(?:\\(?:har\\|ode\\)s\\)\\|sub_atom\\)\\([(]\\)" 1 'logtalk-built-in-predicate-face)
		;;
		;; bitwise functors:
		("/\\\\\\|<<\\|>>\\|\\\\/" . 'logtalk-built-in-predicate-face)
		("\\\\" . 'logtalk-built-in-predicate-face)
	))



(setq logtalk-font-lock-operators
	'(
		("::\\|\\^\\^\\|[{}]" . 'logtalk-message-operator-face)
	))



(setq logtalk-font-lock-keywords
	(append
		logtalk-font-lock-strings
		logtalk-font-lock-directives
		logtalk-font-lock-built-in-methods
		logtalk-font-lock-built-in-predicates
		logtalk-font-lock-operators
	))



;; entry function

(defun logtalk-mode ()
	"Major mode for editing Logtalk files"
	(interactive)
	(kill-all-local-variables)
	(setq tab-width 4)
	(set-syntax-table logtalk-mode-syntax-table)
	(set (make-local-variable 'font-lock-defaults) '(logtalk-font-lock-keywords))
	(turn-on-font-lock)
	(setq major-mode 'logtalk-mode)
	(setq mode-name "Logtalk")
	(run-hooks 'logtalk-mode-hook))

(provide 'logtalk-mode) 
