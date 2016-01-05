; Chapter 7
(load "graph-util")

(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                             a wizard is snoring loudly on the couch.))
                               (garden (you are in a beautiful garden. there is a well in front of you.))
                               (attic (you are in the attic. there is a giant welding torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)
                                (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))

(dot-name 'living-room)
(dot-name 'foo!)
(dot-name '24)

(substitute-if #\e #'digit-char-p "I'm a l33t hack3r!")
(substitute-if 0 #'oddp '(1 2 3 4 5 6 7 8))

(nodes->dot *wizard-nodes*)
(edges->dot *wizard-edges*)

(graph->dot *wizard-nodes* *wizard-edges*)

(graph->png "wizard.dot" *wizard-nodes* *wizard-edges*)

;; `mapcar' receives each argument in turn
(mapcar #'print '(a b c))
; Prints: A then B then C

;; `maplist'  receives the entire remained of the list
(maplist #'print '(a b c))
; Prints: (A B C) then (B C) then (C)

(ugraph->png "wizard.dot" *wizard-nodes* *wizard-edges*)
