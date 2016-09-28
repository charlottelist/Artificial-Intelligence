;Charlotte List and Bryce Kopp
;AI Project #5
;Reasoning

;defines the list of rules
(define rules
  ;rules if we visit the spot that is the goal - return finished
  '(((visited00 goal00) (finished))
    ((visited01 goal01) (finished))
    ((visited02 goal02) (finished))
    ((visited10 goal10) (finished))
    ((visited11 goal11) (finished))
    ((visited12 goal12) (finished))
    ((visited20 goal20) (finished))
    ((visited21 goal21) (finished))
    ((visited22 goal22) (finished))

    ;adjacent rules
    ;not obstacle and adjacent --> go there (visited)
    ((visited00 (temp visited01) (temp obstacle01)) visited01)
    ((visited00 (temp visited10) (temp obstacle10)) visited10)
    ((visited01 (temp visited00) (temp obstacle00)) visited00)
    ((visited01 (temp visited02) (temp obstacle02)) visited02)
    ((visited01 (temp visited11) (temp obstacle11)) visited11)
    ((visited02 (temp visited01) (temp obstacle01)) visited01)
    ((visited02 (temp visited12) (temp obstacle12)) visited12)
    ((visited10 (temp visited00) (temp obstacle00)) visited00)
    ((visited10 (temp visited11) (temp obstacle11)) visited11)
    ((visited10 (temp visited20) (temp obstacle20)) visited20)
    ((visited11 (temp visited01) (temp obstacle01)) visited01)
    ((visited11 (temp visited10) (temp obstacle10)) visited10)
    ((visited11 (temp visited21) (temp obstacle21)) visited21)
    ((visited11 (temp visited12) (temp obstacle12)) visited12)
    ((visited12 (temp visited11) (temp obstacle11)) visited11)
    ((visited12 (temp visited02) (temp obstacle02)) visited02)
    ((visited12 (temp visited22) (temp obstacle22)) visited22)
    ((visited20 (temp visited10) (temp obstacle10)) visited10)
    ((visited20 (temp visited21) (temp obstacle21)) visited21)
    ((visited21 (temp visited20) (temp obstacle20)) visited20)
    ((visited21 (temp visited11) (temp obstacle11)) visited11)
    ((visited21 (temp visited22) (temp obstacle22)) visited22)
    ((visited22 (temp visited12) (temp obstacle12)) visited12)
    ((visited22 (temp visited21) (temp obstacle21)) visited21)))

  ;next set of rules

  ;if not visited and its adjacent to a visited and not an obstacle
  ;new fact of new visited

  ;if not visited & adjacent to visited & a short stable obstacle
  ;new fact of new visited

;defines the given facts to start about the 3x3 grid
(define facts
  '(goal22 visited00 obstacle11 obstacle21))

;find out whether the conditions in the argument are true
(define ModusPonens
  (lambda (rule)
    (ModusPonens2 (car rule) (cadr rule) (- (length (car rule)) 1) 0)))

;call helper function
(define ModusPonens2 
  (lambda (b a stop count)
    (cond
      ((< count stop) 
        (cond
          ((and (list? (car b))
          (not (list? (member (cadr (car b)) facts))))
          (set! count (+ count 1))
          (ModusPonens2 (cdr b) a stop count))
          
          ((and (not (list? (car b)))
          (list? (member (car b) facts))) 
          (set! count (+ count 1))

          (ModusPonens2 (cdr b) a stop count))

          (else
            '())))

      ((equal? count stop)
        (cond
          ((and (list? (car b))
          (not (list? (member (cadr (car b)) facts))))
          a)

          ((and (not (list? (car b)))
          (list? (member (car b) facts))) a)

          (else
            '()))))))


;search function
(define search
  (lambda (count ruleList)
    (cond
      ;if finished is in the list of facts we know the goal was found
      ((member '(finished) facts)
          (display "goal found")
          (newline))
      ;if the count reaches said number
      ((>= count 500)
        (display "not found")
        (newline))
      ;else
      (else
        ;learn a new fact by going through rules
        (let* ((firstRule (car ruleList))
               (remainingRules (append (cdr ruleList) (list firstRule)))
               (newFact (ModusPonens firstRule)))

          (cond 
            ;what do we do here?
            ((equal? newFact '()) 
              (set! count (+ count 1))
              (search count remainingRules))
            (else
            (set! facts (append (list newFact) facts))
            (set! count (+ count 1))
            (search count remainingRules))))))))

(search 0 rules)


