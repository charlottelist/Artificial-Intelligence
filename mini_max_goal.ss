;Charlotte List & Bryce Kopp
;Mini Max Search

;need to make a static evaluator
;how can we evaluate the different moves 
;distance from robot? obstacle density? moves gained/lost?

;need to get the possible moves over a series of levels (plies)

;1: get all the available moves
;2: score each available move with static evaluator
;3: move a clone of the goal and robot to see what the outcome would be
;4: run it on each of the levels comparing the moves
;5: 

(define dist_pt
  (lambda (p1 p2)
  (+ (abs (- (car p2) (car p1))) (abs (- (cadr p2) (cadr p1))))))

;find the obstacle density
(define dist_ptB
  (lambda (point)
    (length (adjacento point))))

;find the distance between the two possible points 
(define dist_ptR
  (lambda (ptG pt2)
  (+ (abs (- (car pt2) (car ptG))) (abs (- (cadr pt2) (cadr ptG))))))

;find the distance from the center to avoid the edges
(define dist_ptC
  (lambda (pt gridSize)
  (let ((g (* .5 gridSize)))
  (+ (abs (- (car pt) g)) (abs (- (cadr pt) g))))))

;function that scores the point based on
;distance to robot, distance to edge, and obstacle density
(define getScore-goal
  (lambda (point)
    (+ (- 4 (dist_ptB point)) (* (dist_ptC point num-col-row) .01) 
      (/ 18 (+ (dist_ptR point robot) 0.0001)))))

;function used to map all the scores of all the points takes two parameters
(define getScore-goal2
  (lambda (point point2)
    (+ (- 4 (dist_ptB point)) (* (dist_ptC point num-col-row) .1) 
      (/ 18 (+ (dist_ptR point point2) 0.0001)))))


(define expand-block-goal
  (lambda (lst finallst)
    (let*((lst1 lst)
          (lst2 (cons lst (adjacento lst)))
          (lst3 (randomize lst2))
          (flst (calculate-h-goal lst3))
          (lstfin (map list flst lst3))
          (final (append finallst lst2)))
    (enqueue lstfin) 
    final)))

(define expand-block-robotx
  (lambda (lst finallst)
    (let*((lst1 lst)
          (lst2 (cons lst (adjacento lst)))
          (lst3 (randomize lst2))
          (flst (calculate-r-goal lst3))
          (lstfin (map list flst lst3))
          (final (append finallst lst2)))
    (enqueue lstfin)
    final)))

(define expand-find-max-branch-goal
  (lambda (lst scoreList)
    (let((sList (calculate-h-goal lst)))
      (set! scoreList (find_max_goal sList 0)) scoreList)))

(define expand-find-max-branch-robot
  (lambda (lst scoreList)
    (let((sList (calculate-r-goal lst)))
      (set! scoreList (append scoreList (find_max_goal sList 0))) scoreList)))

;maximize the branch
(define find_max_goal
  (lambda (lst max)
    (cond
      ((null? lst) max)
      ((> (car (car lst)) max) (find_max_goal (cdr lst) (car (car lst))))
      (else (find_max_goal (cdr lst) max)))))

;minimize the maximized numbers
(define find_minimum_goal
  (lambda (lst num)
    (cond
      ((null? lst) num)
      ((< (car (car lst)) num) (find_minimum_goal (cdr lst) (car (car lst))))
      (else (find_minimum_goal (cdr lst) num)))))

;find the block that has the minimum score
(define search_queue_minimum_path
  (lambda (lst blk)
    (cond
      ((null? lst) null)
      ((equal? (car (car lst)) blk) (car lst))
      (else
        (search_queue_minimum_path (cdr lst) blk)))))


;map all the points one to many to get all the scores of our possible moves
;in comparison to their possible moves
(define get-score-lst-goal
  (lambda (lst1 lst2 scoreList)
    (let((tempList '()) 
      (maxNum 0)
      (maxPath '()))
    (cond
      ((null? lst1) scoreList)
      (else
        (map 
          (lambda (x)
            (set! tempList (append tempList (cons (cons (getScore-goal2 (car lst1) x) (list (car lst1))) '())))) lst2)
            
            (set! maxNum (find_max_goal tempList -1111111))
            (set! maxPath (search_queue_minimum_path tempList maxNum))
            (set! scoreList (append scoreList (list maxPath)))

        (get-score-lst-goal (cdr lst1) lst2 scoreList))))))

;find the move closest to the robot that will on the next turn get you to the block that was considered
;the best choice after max and min analysis
(define hillclimb_helper
  (lambda (adjblocks)
    (cond
      ((equal? (dist_pt (car adjblocks) goal) 1) (car adjblocks))
      (else
        (hillclimb_helper (cdr adjblocks))))))

;main function called from the chase file
(define get-next-goal 
  (lambda (point)
    (let* ((lst1 (adjacento point))
      ;lst1 is all the adjacent points to the goal
           (lst0 (randomize lst1))
           ;lst0 puts it in a randomized order
           (flst (calculate-h-goal lst0))
           (lst (map list flst lst0))
           ;get the adjacent points with score for the robot
           (lstr1 (adjacento robot))
           (lstr0 (randomize lstr1))
           (rflst (calculate-r-goal lstr0))
           (lstr (map list rflst lstr0))
           ;score list
           (scorlst '())
           (listScore '())
           ;can change the ply count to 2 or 4
           (plyCount 4)
           ;list of our possible moves
           (ourMoves lst1)
           ;list of their possible moves (robot)
           (theirMoves lstr1)
           ;next move and next path
           (nextMove '())
           (nextPath '())
           (adjblocksPT '()))

      (set! queue '())

      ;find the scores of the newly expanded block and find the max - put in a list
      (cond
        
        ;if ply count is 2
        ((equal? plyCount 2)
          ;get the adjacent points
          (enqueue lst)
          (enqueue lstr)
          ;map our moves onto theirs to determine the score of each possible move and put in list
          (set! scorlst (get-score-lst-goal ourMoves theirMoves scorlst))
          ;set next move to be the move with the best score
          (set! nextMove (find_minimum_goal scorlst 100000)) 
          ;find the block that goes with the minimum score
          (set! nextPath (search_queue_minimum_path scorlst nextMove))
          ;return the move of the block you want to go to
          (cadr nextPath))
        
          
        ((equal? plyCount 4)
          ;get the adjacent points
          (enqueue lst)
          (enqueue lstr)
          ;add the next round of moves to our move list from the adjacents of the first possible
          ;moves we found
          (map
            (lambda (g)
              (set! ourMoves (append ourMoves (adjacento g)))) lst1)
          ;add the next round of moves to their move list from the adjacents of their first possible
          ;moves that we calculated
          (map
            (lambda (h)
              (set! theirMoves (append theirMoves (adjacento h)))) lstr1)
          ;map ours to theirs to determine all the possible scores of each move in the tree structure
          (set! scorlst (get-score-lst-goal ourMoves theirMoves scorlst))
          ;find the lowest score -- aka the one you want to go to
          (set! nextMove (find_minimum_goal scorlst 100000)) 
          ;find the block that goes with the lowest score
          (set! nextPath (search_queue_minimum_path scorlst nextMove))
          ;the best move could be two steps away -- need to find the best move that can get you 
          ;there next time
          ;calculate the adjacents of the move you want to go to
          (set! adjblocksPT (adjacento (cadr nextPath)))
          ;if the block is two away run hillclimb helper to find best first move -- its parent so to speak
          ;else just go where you want to go!
          (cond
            ((equal? (dist_pt goal (cadr nextPath)) 2) (hillclimb_helper adjblocksPT))
            (else
          (cadr nextPath))))))))

      ;what if we in the queue made the object have another element in the list
      ;like a letter, then you could find all the ones in the queue with that letter
      ;and then see which is closest to the original position and that will be the one
      ;we want to move to
 
(define calculate-h-goal
  (lambda (lst)
    (map getScore-goal lst)))

(define h-goal
  (lambda (point)
    (+ (abs (- (car point) (car robot)))
       (abs (- (cadr point) (cadr robot))))))

(define calculate-r-goal
  (lambda (lst)
    (map getScore-robot lst)))




