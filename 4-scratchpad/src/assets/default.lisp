; Every expression will be evaluated and displayed on its own line.
; A basic tutorial for and the list of primitives & functions: https://github.com/GollyTicker/LISP

'====_Quotes_and_Atoms_====
(quote a)
'&%|take#a-*-look-@-me! ; an atom can be any ANSI string except whitespace and curly braces

'======_Lists_=============
(list '(a b c d) 'and (list (+ '1 '2) '4 '5) )

'======_Conditionals_======= ; t is boolean truth and everything else is false
(cond ('abc '1) ('t '1))
((cond ('() '+) ('t 'car)) '(a b c)) ; the function itself can be computed

'===_Definitions_and_inspections_=====
(define! language 'MiniLISP)
(define! swap '(lambda (x y) (list y x)) )
(list language (swap '1 '2))
(define! swap_expression swap) ; <- note the missing quote!
(list 'accessed_definition_of_swap swap_expression)

'======_Standard_Library_==== ; github.com/GollyTicker/LISP/blob/main/1-interpreter/standard-library.lisp
(list 'append: (append '(1 2 3) '(4 5 6)))
(list 'ifelse: (ifelse 't '1 '2))

'====_play_around_with_me!_====
'(hello world)
