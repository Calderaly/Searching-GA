; This version of Common Lisp code is of SBCL compiler standard
; Other Common Lisp compiler/ interpreter might have different implementation

(defpackage :genetic-searching 
  (:use :cl :cl-tuples))
(in-package :my-package)

(defconstant +bits-per-variable+ 32) ; Jumlah bit per variabel
(defconstant +range-upper-bound+ 10.0) ; Batas atas x1 dan x2
(defconstant +range-lower-bound+ -10.0) ; Batas bawah x1 dan x2
(defconstant +range-domain+ (- +range-upper-bound+ +range-lower-bound+)) ; Rentang x1 dan x2
(defconstant +chromosome-length+ (* +bits-per-variable+ 2)) ; Panjang kromosom

; Parameter GA
(defconstant +population-size+ 2000) ; Ukuran populasi
(defconstant +generations-size+ 200) ; Jumlah generasi
(defconstant +best-individual+ 2) ; Jumlah individu terbaik yang dipertahankan
(defconstant +generations-without-improvement+ 40) ; Batas untuk hasil yang stagnan

; Operator GA
(defconstant +crossover-rate+ 0.8) ; Peluang crossover
(defconstant +mutation-rate+ 0.01) ; Peluang mutasi
(defconstant +roulette-size+ 5) ; Jumlah seleksi pada rolet untuk mendapatkan parent

(defun init-population (population-size)
  "Inisialisasi populasi awal secara acak.
  Args:
    population-size: Ukuran populasi.
  Returns:
    List: Populasi yang berisi kromosom-kromosom biner."
  (loop for _ below population-size
        collect (loop for _ below +chromosome-length+
                      collect (random 2) ; Menyederhanakan pilihan angka biner
                      into chromosome
                      finally (return (format nil "~{~a~^~}" chromosome))))) ; Konversi list dari bit ke string

(defun objective-function (x1 x2)
  "Fungsi objektif yang akan dioptimasi (minimisasi).
  Args:
    x1: Nilai variabel x1.
    x2: Nilai variabel x2.
  Returns:
    float: Hasil perhitungan fungsi objektif."
  (handler-case
      (let ((term1 (* (sin x1) (cos x2) (tan (+ x1 x2))))
            (term2 (* (/ 3 4) (exp (- 1 (sqrt (expt x1 2)))))))
        (let ((result (- (+ term1 term2))))  ; Minimalkan fungsi ini
          ; Mengecek hasil dari perhitungan dalam if-else untuk menyederhanakan error handling (menggunakan numberp dan finitep)
          (if (or (not (numberp result)) (not (finitep result)))
              (floating-point-overflow)
              result)))
    (floating-point-overflow () ; Menangkap OverflowError di Common Lisp
      (float '-inf))))


(defun binary-to-float (binary-string)
  "Mengonversi string biner menjadi nilai float dalam rentang yang ditentukan.
  Args:
    binary-string: String biner yang akan dikonversi.
  Returns:
    float: Nilai float yang dihasilkan."
  (let ((decimal (parse-integer binary-string :radix 2)))
    (+ +range-lower-bound+
       (* (/ decimal (1- (expt 2 +bits-per-variable+)))
          +range-domain+))))

(defun decode-chromosome (chromosome)
  "Mendekode kromosom menjadi nilai x1 dan x2.
  Args:
    chromosome: String biner yang merepresentasikan kromosom.
  Returns:
    tuple: Tuple yang berisi nilai x1 dan x2 sebagai float."
  (let ((bin-x1 (subseq chromosome 0 +bits-per-variable+))
        (bin-x2 (subseq chromosome +bits-per-variable+)))
    (values (binary-to-float bin-x1) (binary-to-float bin-x2))))

(defun calculate-fitness-values (population)
  "Menghitung nilai fitness untuk setiap kromosom dalam populasi.
  Args:
    population: List yang berisi kromosom-kromosom biner.
  Returns:
    List: List yang berisi dictionary dengan informasi kromosom, x1, x2, dan nilai fitness."
  (loop for chromosome in population
        for (x1 x2) = (multiple-value-list (decode-chromosome chromosome))  ;unpack berbagai nilai
        collect (cl-tuples:make-tuple (list :chromosome chromosome :x1 x1 :x2 x2 :objective fitness))))

(defun roulette-wheel-selection (population fitness-values size)
  "Melakukan seleksi menggunakan metode rolet.
  Args:
    population: List kromosom.
    fitness-values: List nilai fitness yang sesuai dengan populasi.
    size: Jumlah parent yang akan dipilih.
  Returns:
    List: List kromosom yang terpilih sebagai parent."
  (let* ((objective-values (mapcar #'(lambda (individual) (getf individual :objective)) fitness-values))
         (max-objective (apply #'max objective-values))
         (adjusted-fitness-values (mapcar #'- (make-list (length objective-values) :initial-element max-objective) objective-values)
         ))
    (if (every #'(lambda (f) (= f (first objective-values))) objective-values)
        (loop repeat size collect (nth (random (length population)) population)) ; Mengubah pilihan acak
        (let* ((total-adjusted-fitness (reduce #'+ adjusted-fitness-values))
               (probabilities (mapcar #'/ adjusted-fitness-values (list total-adjusted-fitness))))
          (loop repeat size
                collect (let ((cumulative-probability 0)
                              (random-value (random 1.0)))
                          (loop for i from 0 below (length population)
                                do (incf cumulative-probability (nth i probabilities)) ; Menggunakan incf untuk inkremen 
                                ; when (>= cumulative-probability random-value)
                                  return (nth i population)))))))) ; Mengembalikan orang tua

(defun uniform-crossover (parent1 parent2 crossover-rate)
  "Melakukan crossover uniform pada dua parent.
  Args:
    parent1: Kromosom parent pertama.
    parent2: Kromosom parent kedua.
    crossover-rate: Peluang terjadinya crossover.
  Returns:
    tuple: Tuple yang berisi kromosom child1 dan child2 hasil crossover."
  (let ((child1 (make-array (length parent1) :element-type 'character))
        (child2 (make-array (length parent2) :element-type 'character')))
    (loop for i from 0 below (length parent1)
          do (if (< (random 1.0) crossover-rate)
                 (progn
                   (setf (aref child1 i) (char parent1 i))
                   (setf (aref child2 i) (char parent2 i)))
                 (progn
                   (setf (aref child1 i) (char parent2 i))
                   (setf (aref child2 i) (char parent1 i)))))
    (cl-tuples:make-tuple (values (coerce child1 'string') (coerce child2 'string'))))) ; Konversi array

(defun scramble-mutation (chromosome)
  "Melakukan mutasi scramble pada kromosom.
  Args:
    chromosome: Kromosom yang akan dimutasi.
  Returns:
    str: Kromosom hasil mutasi."
  (if (< (random 1.0) +mutation-rate+)
      (let* ((length (length chromosome))
             (indices (sort (list (random length) (random length)) #'<))
             (i (first indices))
             (j (second indices)))
        (let ((segment (subseq chromosome i (1+ j))))
          (setf segment (loop for k from 0 below (length segment) collect (char segment k)))
          (setf segment (sort segment #'(lambda (a b) (< (random 1.0) 0.5)))) ; Shuffle dengan sort
          (concatenate 'string (subseq chromosome 0 i) (coerce segment 'string') (subseq chromosome (1+ j)))))
      return chromosome)
      ; Asumsi perhitungan di atas hanya mengembalikan nilai dalam if (bisa salah)
      ; dimana chromosome dikerjakan di dalam fungsi concatenate, baru di return
      (return chromosome))

(defun binary-string-to-integer (binary-string)
  (parse-integer binary-string :radix 2))

(defun main ()
  "Fungsi utama yang menjalankan algoritma genetika."
  ; Disini, get-internal-run time adalah ekuivalensi untuk time.time() dari Python (dapatkan waktu nyata dari sistem)
  (let* ((start-time (get-internal-run-time))
         (population (init-population +population-size+))
         (best-solution-overall nil)
         (last-best-fitness (float '-inf'))
         (no-improvement-count 0)
         (parent1-chromosome-best "")
         (parent2-chromosome-best ""))
    (loop for generasi from 0 below +generations-size+
          do (let* ((evaluated-population (calculate-fitness-values population))
                    ; Sort ascending untuk minimalisasi "#'<", sort descending untuk maksimalisasi "#'>"
                    (sorted-population (sort evaluated-population #'< :key #'(lambda (x) (getf x :objective)))))
               (let ((best-individual (first sorted-population)) ; Dapatkan individu terbaik
                     (parents (roulette-wheel-selection population sorted-population +roulette-size+)))
                 (let ((parent1-chromosome (nth 0 parents))
                       (parent2-chromosome (nth 1 parents)))
                   (if (or (not best-solution-overall) (> (getf best-individual :objective) (getf best-solution-overall :objective))
                   )
                       (progn
                         (setf best-solution-overall best-individual)
                         (setf last-best-fitness (getf best-individual :objective))
                         (setf no-improvement-count 0)
                         (setf parent1-chromosome-best parent1-chromosome)
                         (setf parent2-chromosome-best parent2-chromosome))
                       (incf no-improvement-count)))
                 (let* ((x1-bin (subseq (getf best-individual :chromosome) 0 +bits-per-variable+))
                        (x2-bin (subseq (getf best-individual :chromosome) +bits-per-variable+))
                        (fitness-bin (format nil "~32,'0b" (binary-string-to-integer (format nil "~,f" (getf best-individual 
                        :objective))))))
                   (format t "Generasi ~d/~d = x1: ~,4f (bin: ~a), x2: ~,4f (bin: ~a), fitness: ~,4f (bin: ~a)~%"
                           (1+ generasi) +generations-size+
                           (getf best-individual :x1) x1-bin
                           (getf best-individual :x2) x2-bin
                           (getf best-individual :objective) fitness-bin))
                 (when (> generasi 0)
                   (format t "  Orang Tua Terbaik: ~a...~a, ~a...~a~%"
                           (subseq parent1-chromosome-best 0 10) (subseq parent1-chromosome-best (- (length parent1-chromosome-best) 
                           10))
                           (subseq parent2-chromosome-best 0 10) (subseq parent2-chromosome-best (- (length parent2-chromosome-best) 
                           10))))
                 ; Elistism
                 (let ((new-population (loop for i below +best-individual+ collect (getf (nth i sorted-population) :chromosome))))
                   (loop while (< (length new-population) +population-size+)
                         do (multiple-value-bind (child1 child2) (uniform-crossover parent1-chromosome parent2-chromosome 
                         +crossover-rate+)
                              (setf child1 (scramble-mutation child1))
                              (setf child2 (scramble-mutation child2))
                              (push child1 new-population)
                              (push child2 new-population))
                         finally (if (> (length new-population) +population-size+)
                                     (setf new-population (subseq new-population 0 +population-size+))))
                   (setf population new-population)))))
    (let ((end-time (get-internal-run-time)))
      (format t "~%------------------------------~%")
      (format t "---Evolusi Selesai---~%")
      (format t "Waktu eksekusi: ~,2f detik~%" (/ (- end-time start-time) internal-time-units-per-second)))

    (if best-solution-overall
        (progn
          (format t "~%Solusi terbaik ditemukan (Minimalisasi): ~%")
          (format t "Kromosom: ~a...~a~%" (subseq (getf best-solution-overall :chromosome) 0 15) (subseq (getf best-solution-overall 
          :chromosome) (- (length (getf best-solution-overall :chromosome)) 15))))
          (format t "Nilai x1 dan x2: (~,4f, ~,4f)~%" (getf best-solution-overall :x1) (getf best-solution-overall :x2))
          (format t "Nilai Fungsi Objektif (Minimum): ~,6f~%" (getf best-solution-overall :objective)))
        (format t "~%Tidak ada solusi terbaik yang ditemukan.~%"))
    
    ; Ringkasan dari nilai loop fitness, rekombinasi, dan mutasi.
    (format t "~%Top 3 individu di populasi terakhir:~%")
    (let ((final-evaluated-population (calculate-fitness-values population)))
      (setf final-evaluated-population (sort final-evaluated-population #'< :key #'(lambda (x) (getf x :objective))))
      (loop for i from 0 below (min 3 (length final-evaluated-population))
            for ind in final-evaluated-population
            do (let* ((x1-bin-top3 (subseq (getf ind :chromosome) 0 +bits-per-variable+))
                      (x2-bin-top3 (subseq (getf ind :chromosome) +bits-per-variable+))
                      (fitness-bin-top3 (format nil "~32,'0b" (binary-string-to-integer (format nil "~,f" (getf ind :objective))))))
                 (format t "  ~d. Nilai fitness: ~,6f (bin: ~a)~%     (x1 = ~,4f (bin: ~a))~%     (x2 = ~,4f (bin: ~a))~%"
                         (1+ i)
                         (getf ind :objective) fitness-bin-top3
                         (getf ind :x1) x1-bin-top3
                         (getf ind :x2) x2-bin-top3)))))
(main)