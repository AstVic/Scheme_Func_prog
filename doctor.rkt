; заготовка "Доктора". Сентябрь 2026
; В учебных целях используется базовая версия Scheme
#lang scheme/base

; Подключаем Racket-библиотеки для векторов и списков, на всякий случай
(require racket/vector)
(require racket/list)

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name (vector))
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)

; цикл диалога Доктора с пациентом с запоминанием предыдущих реплик пациента
; параметр name -- имя пациента
; параметр history -- вектор предыдущих различных реплик пациента, каждый элемент -- пара (реплика . флаг),
; флаг равен #t, если реплика уже использовалась для построения ответа, и #f иначе
(define (doctor-driver-loop-v2 name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond
            ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply-v2 user-response history)) ; иначе Доктор генерирует ответ по истории предыдущих реплик и печатает его
                  (doctor-driver-loop-v2 name (add-to-history user-response history)) ; затем реплика запоминается, и цикл продолжается
             )
       )
      )
)

; добавление реплики user-response в начало вектора history с флагом #f, если такой реплики в нём ещё нет
(define (add-to-history user-response history)
  (if (vector-member user-response history (lambda (response record) (equal? response (car record)))) ; ищем реплику среди запомненных
      history ; если реплика уже есть, то вектор не меняется
      (vector-append (vector (cons user-response #f)) history) ; иначе новая реплика дописывается в начало вектора
  )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
      (case (random 0 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (hedge-answer))  ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ

      )
)

; генерация ответной реплики по user-response -- реплике от пользователя, и history -- вектору предыдущих реплик
(define (reply-v2 user-response history)
      (case (random 0 (if (history-applicable? history) 3 2)) ; с равной вероятностью выбирается один из применимых способов построения ответа
          ((0) (hedge-answer))  ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ
          ((2) (history-answer history)) ; 3й способ, применим только при наличии неиспользованной реплики в истории
      )
)

; проверка применимости 3го способа: не #f, если в history есть реплика с флагом #f
; vector-member возвращает индекс первой такой реплики, и проход по вектору на ней прекращается
(define (history-applicable? history)
  (vector-member #f history (lambda (flag record) (equal? flag (cdr record))))
)

; 3й способ генерации ответной реплики -- возврат к одной из ранее сказанных пациентом реплик с заменой лица
; выбранная реплика помечается в history флагом #t, чтобы повторно не использоваться
(define (history-answer history)
  (let* ((record (pick-random-vector (vector-filter (lambda (record) (not (cdr record))) history))) ; случайная реплика из неиспользованных
         (index (vector-member record history))) ; её положение в векторе history
    (vector-set! history index (cons (car record) #t)) ; реплика отмечается как использованная
    (append '(earlier you said that) (change-person (car record)))
  )
)

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random-vector #((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (tell me more about it)
                              (how does that make you feel)
                              (that is very interesting)
                              (what else comes to your mind))
         )
)

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
)

; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector #((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (what makes you think that)
                                       (it seems to you that)
                                       (so you are telling me that)
                                       (how long have you felt that))
                )
                (change-person user-response)
        )
 )

; замена лица во фразе
(define (change-person phrase)
        (many-replace-v3
		'((am are)
        (are am)
        (i you)
        (me you)
        (mine yours)
        (my your)
        (myself yourself)
        (you i)
        (your my)
        (yours mine)
        (yourself myself)
        (we you)
        (us you)
        (our your)
        (ours yours)
        (ourselves yourselves)
        (yourselves ourselves)
        (shall will))
                      phrase)
 )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
)

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs, итеративный процесс
(define (many-replace-v2 replacement-pairs lst)
        (let loop ((lst lst) (result '())) ; в result накапливаются результаты замен в обратном порядке
             (if (null? lst) (reverse result) ; список пройден, порядок слов восстанавливается
                 (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (loop (cdr lst)
                            (cons (if pat-rep (cadr pat-rep) (car lst)) result)) ; замена или исходное слово добавляется к результату
                 )
             )
        )
)

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs с помощью map
(define (many-replace-v3 replacement-pairs lst)
        (map (lambda (word) ; каждое слово заменяется по ассоциативному списку или остаётся без изменений
                     (let ((pat-rep (assoc word replacement-pairs)))
                          (if pat-rep (cadr pat-rep) word)))
             lst)
)

; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i 0) (result init))
   (if (= i length) result
    (loop (add1 i) (f i result (vector-ref vctr i)))))))
	
; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i (sub1 length)) (result init))
   (if (= i -1) result
    (loop (sub1 i) (f i result (vector-ref vctr i)))))))
