(load "../chapter-7/graph-util")

(defmacro comment (&rest forms)
  nil)

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *player-pos* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

(comment
  (loop repeat 10 collect 1) ; Creates a list of 10 ones
  (loop for n from 1 to 10 collect n) ; Starts n at 1, loops to 10 and returns list of n
  (loop for n from 1 to 10 collect (+ 100 n)) ; Same as above except we add 100 to it

  ; Note that you can yous keyword argmunts in place of some of the options to loop
  ; IMO this is more readable:
  (loop :for n :from 1 :to 10 :collect (+ 100 n))
  )

(defun direct-edges (node edge-list)
  ; Find all edges in given list that start at node
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  ; Get all nodes connected to this node
  (let ((visited nil))
    (labels ((traverse (node)
               (unless (member node visited)
                 (push node visited)
                 (mapc (lambda (edge)
                         (traverse (cdr edge)))
                       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
               ; Checks which nodes are connected to first node, then
               ; subtracts these nodes from the full list of nodes
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 ; `let*' allows us to refernce previously bound variables, like
                 ; `connected' here
                 (push connected islands)
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))
(comment

  ; Let's see `let*' in action:
  (let ((a 5)
        (b (+ a 2)))
    b) ; => The variable A is unbound

  (let* ((a 5)
         (b (+ a 2)))
    b) ; => 7
  )

(defun connect-with-bridges (islands)
  ;; Returns a list of edges that will connect all islands together
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  (let* ((nodes (loop :for i :from 1 :to *node-num* :collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal)))) ; We want to use `equal' here as we're testing the equaly of cons cell pairs
          (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                  (list node2 'cops)
                                  edge)))
                            node1-edges))))
          edge-alist))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(comment
  (neighbors 1 '((1 (2) (3) (4 COPS)))) ; => (2 3 4)
  )

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(comment
  (within-one 1 2 '((1 (2) (3) (4 COPS)) (2 (5)))) ; => (2 3 4)
  ; Don't forget member returens the cdr (rest) of the cons list when it finds
  ; a matching item
  )

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbors a edge-alist))))

(comment
  (within-one 1 3 '((1 (2)) (2 (1) (3)) (3 (2)))) ; => nil
  (within-two 1 3 '((1 (2)) (2 (1) (3)) (3 (2)))) ; => (3)
  )

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop :for i :below *worm-num*
                          :collect (random-node))))
    (loop :for n :from 1 :to *node-num*
          :collect (append (list n)
                           (cond ((eql n wumpus) '(wumpus))
                                 ((within-two n wumpus edge-alist) '(blood!)))
                           (cond ((member n glow-worms)
                                  '(glow-worm))
                                 ((some (lambda (worm) (within-one n worm edge-alist))
                                        glow-worms)
                                  '(lights!)))
                           (when (some #'cdr (cdr (assoc n edge-alist)))
                             '(sirens!))))))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
      (find-empty-node)
      x)))

(defun draw-city ()
  (ugraph->png "/Users/michaelcramm/tmp/city" *congestion-city-nodes* *congestion-city-edges*))

(comment
  (new-game)
  )

(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
              (let ((n (assoc node *congestion-city-nodes*)))
                (if (eql node *player-pos*)
                  (append n '(*))
                  n))
              (list node '?)))
          (remove-duplicates
            (append *visited-nodes*
                    (mapcan (lambda (node)
                              (mapcar #'car
                                      (cdr (assoc node *congestion-city-edges*))))
                            *visited-nodes*))))))
(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                   x
                                   (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

(comment
  ; mapcan assums the values generated by the mapping function are lists
  ; that need to be appended together

  (defun ingredients (order)
    (mapcan (lambda (burger)
              (case burger
                (single '(patty))
                (double '(patty patty))
                (double-cheese '(patty patty cheese))))
            order))
  (ingredients '(single double-cheese double))
  )

(defun draw-known-city ()
  (ugraph->png "/Users/michaelcramm/tmp/known-city" (known-city-nodes) (known-city-edges)))

; Redefining new game here, as the book does

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(comment
  (new-game)
  )

(defun walk (pos)
  (handle-direction pos nil))

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
      (handle-new-place edge pos charging)
      (princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into cops. Game Over you lose."))
          ((member 'wumpus node) (if charging
                                   (princ "You found the Wumpus! You win.")
                                   (princ "You ran into the Wumpus. Game Over you lose.")))
          (charging (princ "You wasted your last bullet. Game Over you lose."))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at ")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))

(comment
  (new-game)
  )
