;rta search
;charlotte and bryce

;expands
;calculates f(n)=g(n)+h(n) where g(n) is the steps from the current position and h(n)
;is the distance to the goal
;moves to the best spot
;runs branch and bound to find the distance to the frontiers to calculate g(n)
;visited is 1000 so block-status of 1002 means visited and two steps away
;reset all to 1000 once you move
;repeat

;want branch and bound to return the length of the path
;want it to only search through visited blocks 
;(change the adjacent method to prefer visited rather than unvisited)
;searching to a frontier so need to pass in another parameter

(define distpt
  (lambda (p1 p2)
  (+ (abs (- (car p2) (car p1))) (abs (- (cadr p2) (cadr p1))))))

;distpt gets the distance to the goal from the frontier and block-status gives you
;the distance to the frontier from youre current spot
(define distfn
	(lambda (B C)
    ;want this to be the third term (the distance)
	(+ (distpt B C) (block-status B))))

(define helper3
  (lambda (newblock pathlist)
    (enqueue (list newblock pathlist))))

(define minfind2
  (lambda (lst mn)
    (cond
     ((null? lst) mn)
     ((< (cadr (car lst)) mn) (minfind (cdr lst) (cadr (car lst))))
    (else (minfind (cdr lst) mn)))))
    
;finds the path that the smallest distance is related to
;doesn't matter if more than one is the same distance, pick either
(define findpath2
  (lambda (minNum lst)
    (cond
      ((equal? minNum (cadr (car lst))) (car lst))
      (else (findpath minNum (cdr lst))))))

(define search
  (lambda (grid stop-count)
    ;adds the start to the queue
    (block-set! start 1000)
    (enqueue (list (list start (+ 1000 (distpt start goal)))))
    (let ((stepcount 0))
    (BFS (dequeue) stepcount stop-count))))

;main function
(define BFS
  (lambda (path stepLength stop-count)
    (pause pause-num)
    ;mark visited
    (draw-visited (car (car path)) (cadr (car path)))
    ;mark frontier
    (draw-frontier (car (car path)) (cadr (car path)))
    ;check if its goal
    (cond
      ((equal? stop-count stepLength) (display "You are stuck. No solution."))
      ((equal? (car path) goal) (draw-moved-robot (car (car path)) (cadr (car path))) 
      	(display "Step Count: ")(display stepLength) (draw-path-node (car (car path)) (cadr (car path))))
      (else
        ;get adjacent list
        (let ((adjblocks (adjacentv (car path))))
          ;loop? map function
          ;check if goal
          ;if obstacle or visited don't do anything
          ;if new space

          ;set all frontiers as visited
          (map 
            (lambda (a) 
              (block-set! a 1000)) adjblocks)
          
          (map
            (lambda (a)
              (draw-frontier (car a) (cadr a))) adjblocks)
          
          
          ;want to mark up the grid with 1000 + the distance
          (map
          	(lambda (t)
          		(block-set! t (+ 1000 (findDistance grid 20000 (car path) t)))) adjblocks)

          ;make each point a list
          (map
            (lambda (e)
              (list e)) adjblocks)
          ;(display adjblocks)

          ;make each frontier block into a list that includes its distance
          (let((hlist
          (map
            (lambda (c)
              (list c (distfn c goal))) adjblocks)))
          ;add all the objects with the points of the block and the respective distance
          ;to the queue
         
          (map
            (lambda (b)
              
              (helper3 b path)) hlist))

          ;need to mark the adjacents distance back to 1000 before you run it again
          ;don't want to mess up the distances
          ;how?
         
          (map
          	(lambda (z)
          		(block-set! z 1000)) adjblocks)

          (let((temp (findpath2 (minfind2 queue 2000) queue)))
           (set! queue (remove-lst queue temp))
           (set! stepLength (+ stepLength 1))
          ;run the BFS function recursively on the path that has the shortest distance
          (BFS temp stepLength stop-count)))))))



