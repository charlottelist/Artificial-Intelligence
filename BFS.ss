;Charlotte List and Bryce Kopp
;Breadth-First Search
;AI
;Due February 8

        ;front off queue
        ;path from origin to somewhere
        ;mark as visited
        ;take other three points - is it an obstacle? if its the goal? is it visited? go to new spot
        ;create new list - append free spot to current path and add it into the queue

        ;one queue of lists of paths
        ;appending to previous list


(define search
  (lambda (grid stop-count)
    ;adds the start to the queue
    (enqueue (list (list start)))
    (BFS (dequeue))))

;adds the paths to the queue
(define helper
  (lambda (newblock pathlist)
    (enqueue (list (cons newblock pathlist)))))

(define member
  (lambda (status lst)
    (cond
      ((null? lst) #f)
      ((equal? (car lst) status) #t)
      (else (member status (cdr lst))))))

;moves the robot along the path to the goal when found
(define moverobot
  (lambda (route)
    (cond
      ((null? route))
      (else
        (draw-path-node (caar route) (cadar route))
        (moverobot (cdr route))))))

;main function
(define BFS
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
          (map 
            (lambda (a) 
              (block-set! a 1)) adjblocks) 
        
          (map
            (lambda (b)
              (helper b path)) adjblocks)
          ;recursive
          (BFS (dequeue)))))))