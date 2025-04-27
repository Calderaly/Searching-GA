; Using Steel Bank Common Lisp Compiler.
; Other Common Lisp compiler may have it's own implementation for codes below

(defpackage :genetic-searching
  (:use :cl :cl-tuples))
(in-package :my-package)

; Custom error function (Need to tested it's validity in SBCL)
; In this example, the value-error condition includes a value slot to store the invalid value. 
; The :report method customizes how the error is printed.
; (define-condition value-error (simple-error)
;  ((value :initarg :value :reader value-error-value))
;  (:report (lambda (condition stream)
;             (format stream "Invalid value: ~A" (value-error-value condition)))))

; Constants
(defparameter *bits* 10)  ;; Number of bits for binary representation
(defparameter *domain-min* -10)
(defparameter *domain-max* 10)

; Function to convert binary representation to real value in the interval [lower, upper]
(defun binary-to-real (bin-str &optional (lower *domain-min*) (upper *domain-max*))
  (let* ((nilai-int (parse-integer bin-str :radix 2))  ;; Convert binary to integer
         (max-int (1- (expt 2 *bits*))))  ;; Maximum possible value
    (+ lower (* (/ (- upper lower) max-int) nilai-int))))

; Function to decode chromosome to get values x1 and x2
(defun decode (chromosome)
  (let* ((x1-bin (subseq chromosome 0 *bits*))
         (x2-bin (subseq chromosome *bits*))
         (x1 (binary-to-real x1-bin))
         (x2 (binary-to-real x2-bin)))
    (values x1 x2)))

; Function to encode numerical data types into binary
(defun encode-to-binary (value)
  (handler-case
    (cond
    ((integerp value) (format nil "~0,10b" value))
    ((floatp value) (format nil "~0,10b" (truncate (* (/ (- value *domain-min*) (- *domain-max* *domain-min*)) (1- (expt 2 *bits*)))
      )))
    (t (simple-error "Unsupported data type for encoding.")))
    (simple-error (se)
      format t "Penyebab error: ~A" (:error se)
    nil))) ; error in general is good, using this to specify the error

; Objective function to be minimized
(defun objective (x1 x2)
  (+ (expt x1 2) (expt x2 2)))

; Genetic algorithm parameters
(defparameter *population-size* 20) ; Number of individuals in the population
(defparameter *generations* 50)     ; Number of generations
(defparameter *tournament-size* 3)  ; Number of individuals in tournament selection
(defparameter *crossover-rate* 0.8) ; Probability of performing crossover
(defparameter *mutation-rate* 0.1)  ; Probability of mutation on each variable

; Function to generate a random individual
(defun create-individual ()
  (values (+ *domain-min* (random (- *domain-max* *domain-min*))
          (+ *domain-min* (random (- *domain-max* *domain-min*))))))

(defun create-individual ()
  (cl-tuples:make-tuple (values (+ *domain-min* (random (- *domain-max* *domain-min*))
          (+ *domain-min* (random (- *domain-max* *domain-min*)))))))

; Create initial population
(defun create-population ()
  (loop for i from 1 to *population-size*
        collect (create-individual)))

; Evaluate fitness of each individual (the smaller the objective value, the better)
(defun evaluate-population (population)
  (loop for individual in population
        ; collect (cl-tuples:make-tuple (list individual (apply 'objective individual)))
        collect (cl-tuples:make-tuple (values individual (apply 'objective individual)))))

; Selection: Tournament Selection
(defun tournament-selection (evaluated-pop)
  (let ((tournament (loop for individual in (loop repeat *tournament-size* collect (nth (random (length evaluated-pop)) 
      evaluated-pop))
                          collect individual)))
    (reduce (lambda (a b) (if (< (second a) (second b)) a b)) tournament)))

; Crossover: Arithmetic Crossover
(defun crossover (parent1 parent2)
  (if (< (random 1.0) *crossover-rate*)
      (let* ((parent1-bin (concatenate 'string (encode-to-binary (first parent1)) (encode-to-binary (second parent1))))
             (parent2-bin (concatenate 'string (encode-to-binary (first parent2)) (encode-to-binary (second parent2))))
             (crossover-point (1+ (random (length parent1-bin)))))
        (multiple-value-bind (child1 child2) (values (decode (concatenate 'string (subseq parent1-bin 0 crossover-point) 
            (subseq parent2-bin crossover-point)))
                                                      (decode (concatenate 'string (subseq parent2-bin 0 crossover-point) 
                                                        (subseq parent1-bin crossover-point))))
          (values child1 child2)))
      (values parent1 parent2)))

; Mutation: Adding random disturbance to each variable
(defun mutate (individual)
  (multiple-value-bind (x1 x2) individual
    (when (< (random 1.0) *mutation-rate*)
      (setf x1 (min (max (+ x1 (random 2.0 -1.0)) *domain-min*) *domain-max*)))
    (when (< (random 1.0) *mutation-rate*)
      (setf x2 (min (max (+ x2 (random 2.0 -1.0)) *domain-min*) *domain-max*)))
    (values x1 x2)))

; Main Genetic Algorithm
(defun genetic-algorithm ()
  (let ((population (create-population)))
    (loop for generation from 1 to *generations*
          do (let ((evaluated-pop (evaluate-population population))
                   (new-population '()))
               (loop while (< (length new-population) *population-size*)
                     do (let* ((parent1 (tournament-selection evaluated-pop))
                               (parent2 (tournament-selection evaluated-pop))
                               (values (crossover parent1 parent2)))
                          (multiple-value-bind (child1 child2) values
                            (setf child1 (mutate child1))
                            (setf child2 (mutate child2))
                            (push child1 new-population)
                            (when (< (length new-population) *population-size*)
                              (push child2 new-population)))))
               (setf population (nreverse new-population))
               (multiple-value-bind (best-individual best-value) (reduce (lambda (a b) (if (< (second a) (second b)) a b)) 
                  evaluated-pop)
                 (format t "Generation ~A: Best = ~A with value = ~A~%" generation best-individual best-value)
                 (let* ((binary-representation-1 (encode-to-binary (first best-individual)))
                        (binary-representation-2 (encode-to-binary (second best-individual)))
                        (binary-representation-total (concatenate 'string binary-representation-1 binary-representation-2))
                        (best-value-binary (encode-to-binary (truncate best-value))))
                   (format t "Binary representation of first best individual ~A, second best individual ~A, 
                      and total best individual: ~A~%" binary-representation-1 binary-representation-2 binary-representation-total)
                   (format t "Best value in binary: ~A~%" best-value-binary)))))
    (multiple-value-bind (best-individual best-value) (reduce (lambda (a b) (if (< (second a) (second b)) a b)) (evaluate-population 
      population))
      (values best-individual best-value))))

; Running the genetic algorithm
(defun main ()
  (multiple-value-bind (best-solution best-score) (genetic-algorithm)
    (format t "~%Best solution found:~%x1 = ~A, x2 = ~A, with value = ~A~%"
            (first best-solution) (second best-solution) best-score)
    (let* ((best-solution-bin-1 (encode-to-binary (first best-solution)))
           (best-solution-bin-2 (encode-to-binary (second best-solution)))
           (best-solution-bin-total (concatenate 'string best-solution-bin-1 best-solution-bin-2))
           (best-score-binary (encode-to-binary (truncate best-score))))
      (format t "Binary representation of the best first solution ~A, the best second solution ~A, and total best solution ~A~%" 
      best-solution-bin-1 best-solution-bin-2 best-solution-bin-total)
      (format t "Binary representation of the best score: ~A~%" best-score-binary)
      (format t "~%Comparison of binary numbers after crossover and mutation:~%")
      (loop for individual in (create-population)
            do (let* ((binary-representation-1 (encode-to-binary (first individual)))
                      (binary-representation-2 (encode-to-binary (second individual)))
                      (binary-representation-total (concatenate 'string binary-representation-1 binary-representation-2)))
                 (format t "Individual: ~A, Binary1: ~A, Binary2: ~A, sum: ~A~%" individual binary-representation-1 
                 binary-representation-2 binary-representation-total))))))
(main)