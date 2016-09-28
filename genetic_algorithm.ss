;Charlotte List & Bryce Kopp
;Genetic Algorithm
;Due May 9

;goal: learn all ones for a 32 bit chromosome
;take in any size input string though

;list to hold population
(define pop '())

;create chromosome of given length
(define (make-list make-element length)
  (if (= 0 length)
      '()
      (cons (make-element) (make-list make-element (- length 1)))))

;random 0 or 1
(define (random-bit)
  (if (= 0 (random 2)) 0 1))

;make a list of given size number of chromosomes
(define (make-pop length size lst)
  (cond 
  	((= 0 size) lst)
    (else
		(make-pop length (- size 1) (append lst (list (make-list random-bit length)))))))

;initializes the population
(define (init_pop size length)
	(set! pop (make-pop length size '())))

;fitness function
;want to learn all ones, so the more ones the higher the fitness
(define (get_fitness lst)
		(set! lst (filter 
			(lambda (x) (= x 1)) lst)) (length lst))

;returns the best fitness of the population
(define (pop_fitness lst bestFit)
	(cond
		((null? lst) bestFit)
		 ((> (get_fitness (car lst)) bestFit) (pop_fitness (cdr lst) (get_fitness (car lst))))
		(else (pop_fitness (cdr lst) bestFit))))

;find best fit chromosome
(define (best_fitness lst)
	(cond
		((= (get_fitness (car lst)) (pop_fitness lst 0)) (car lst))
		(else (best_fitness (cdr lst)))))

;returns the average fitness of the population
(define (avg_pop lst size total)
		(cond
			((null? lst) (* 1.0 (/ total size)))
			((= 0 (get_fitness (car lst))) (avg_pop (cdr lst) size total))
			(else (avg_pop (cdr lst) size (+ total (get_fitness (car lst)))))))

(define (make_fit_array lst lst2)
	(cond
		((null? lst) lst2)
		(else (make_fit_array (cdr lst) (append lst2 (list (get_fitness (car lst))))))))

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(define (split lst at)
  ; splits the list
  ; n - new list
  ; l - an old
  ; i - a counter
  (define (iter n l i)
    (if (or (null? l) (= i at))
      (cons (reverse n) l)
      (iter (cons (car l) n) (cdr l) (+ i 1))
    )
  )
  (iter '() lst 0)
)

;crossover- switch one of the places in one chromosome for the respective
;spot in another
;randomly pick a point - crossover and make a kid
(define (crossover spot lst1 lst2 n1 n2)
	(set! n1 (append (car (split lst1 spot)) (cdr (split lst2 spot))))
	(set! n2 (append (car (split lst2 spot)) (cdr (split lst1 spot))))
	n1)

;mutation - send kid through mutation
(define (mutate lst lst2)
	(cond 
		((null? lst) lst2)
		((and (= 167 (random 200)) (= (car lst) 0)) (set! lst2 (append lst2 (list 1))) (mutate (cdr lst) lst2))
		((and (= 167 (random 200)) (= (car lst) 1)) (set! lst2 (append lst2 (list 0))) (mutate (cdr lst) lst2))
	(else (set! lst2 (append lst2 (list (car lst)))) (mutate (cdr lst) lst2))))

;add the car to the car of the cdr make a list of those numbers
;generate a random number between 1 and the sum of all of them
;search through if its greater than the car and less than the car of the cdr (that one)
;take the index to find the selected chromosome
;repeat

(define (prob_list lst lst2 sum)
	(cond
		((null? lst) lst2)
		((null? lst2) (set! lst2 (list (car lst))) (prob_list (cdr lst) lst2 (car lst)))
		(else (set! sum (+ sum (car lst))) (set! lst2 (append lst2 (list sum))) (prob_list (cdr lst) lst2 sum))))

;selection - assign fitness to all the chromosome
;roulette wheel - percentages based on fitness
;random but guy with highest fitness has better chance of getting picked
;those are the two parents
(define (select_parents lst lst2)
	(let* ((fit (prob_list (make_fit_array lst '()) '() 0))
		 	(num (random (sum (make_fit_array lst '()))))
			(index1 (select_parents_2 fit num 0)))
	(list-ref lst index1)))

(define (select_parents_2 prob num x)
	(cond
		((null? prob) x)
		((< num (car prob)) x)
		(else (select_parents_2 (cdr prob) num (+ x 1)))))

;then randomize the order of the two parents
(define (get_kids lst lst2)
	(set! lst2 (append lst2 (list (select_parents lst '()))))
	(set! lst2 (append lst2 (list (select_parents lst '()))))
	lst2)

(define (generation lst lst2 count)
	(let ((kids (get_kids lst '())))
		(set! kids (crossover (+ 1 (random 31)) (car kids) (car (cdr kids)) '() '()))
		;(set! kids (mutate kids '()))

	(cond
		((= 0 count) lst2)
		(else
			(set! lst2 (append lst2 (list kids)))
			(generation lst lst2 (- count 1))))))


;run the learning
(define do-learning
  (lambda (lst count)
  	(init_pop 100 32)
  	(set! lst pop)
    (do-learning2 lst count)))

;helper learning function
;display best individual, its fitness, and avg fitness
(define (do-learning2 lst count)
	(cond
		((= count 0) (display "done"))
		(else
			(display "Generation: ") (display (- 100 count)) (newline)
			(display "Best individual:  ")
			(display (best_fitness lst)) (newline)
			(display "Fitness:  ") (display (pop_fitness lst 0)) (newline)
			(display "Average fitness of population:  ")
			(display (avg_pop lst (length lst) 0)) (newline)
			;crossover and mutation happen here
			;then call it again on the new population
			(let((newPop (generation lst '() 100)))
			(do-learning2 newPop (- count 1))))))

(do-learning '() 100)
