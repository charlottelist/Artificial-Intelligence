;Charlotte List and Bryce Kopp
;Depth-First Search
;AI
;Due February 8

;if the current node is not visited, visit the node and mark it as visited
;for all its neighbors that havent been visited, push them to the stack


        ;pop off stack
        ;path from origin to somewhere
        ;mark as visited
        ;take other three points - is it an obstacle? if its the goal? is it visited? go to new spot
        ;create new list - append free spot to current path and add it into the stack

        ;one stack of lists of paths
        ;appending to previous list

(define search
  (lambda (grid stop-count)
    ;add the first the to stack
    (push (list (list start)))
    (DFS (pop))))

;adds the new paths to the stack
(define helper
  (lambda (newblock pathlist)
    (push (list (cons newblock pathlist)))))

(define member
  (lambda (status lst)
    (cond
      ((null? lst) #f)
      ((equal? (car lst) status) #t)
      (else (member status (cdr lst))))))

;moves the robot along the path to the goal at the end
(define moverobot
  (lambda (route)
    (cond
      ((null? route))
      (else
        (draw-path-node (caar route) (cadar route))
        (moverobot (cdr route))))))

;main function
(define DFS
  (lambda (path)
    ;mark visited
    (draw-visited (caar path) (cadar path))
    ;mark frontier
    (draw-frontier (caar path) (cadar path))
    ;check if its goal
    (cond
      ((equal? (car path) goal) (moverobot path))
      (else
        ;get adjacent list
        (let ((adjblocks (adjacentv (car path))))
          ;loop? map function
          ;check if goal
          ;if obstacle or visited don't do anything
          ;if new space
          ;set all as visited
          (map 
            (lambda (a) 
              (block-set! a 1)) adjblocks) 
          ;add all possible moves to stack
          (map
            (lambda (b)
              (helper b path)) adjblocks)
          ;recursive
          (DFS (pop)))))))









