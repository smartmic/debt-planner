(setlocale LC_ALL "de_DE.UTF-8")
(use-modules (ice-9 format))
(define total-rates 0)
(define total-interest 0)
(define total-redemption 0)
(define open-dept 0)
(define start (list 2011 11))
(define months '((01 . Januar)
                (02 . Februar)
                (03 . März)
                (04 . April)
                (05 . Mai)
                (06 . Juni)
                (07 . Juli)
                (08 . August)
                (09 . September)
                (10 . Oktober)
                (11 . November)
                (12 . Dezember)))
(set! start (list (car start) (car (cdr start)) ))

(define print-header 
  (lambda (loan interest repay)
    (let ([z (/ (/ interest 100) 12)])
      (format #t "┌~77,,'─t┐~%")
      (format #t "│  Darlehen über ~v_~10,2h EUR   │~%" 43 loan)
      (format #t "│  Laufzeitbeginn zum 1. des Monats ~v_~10d-~2,'0d   │~%" 
              25 (car start) (car (cdr start)))
      (format #t "│  Sollzinssatz ~v_~3,2f %   │~%" 52 interest)
      (format #t "│  Effektivzins p.a. ~v_~3,2f %   │~%" 
              47 (* 100 (- (expt (+ 1 z) 12) 1)) )
      (if (> (car repay) 0)
        (format #t "│  Sondertilgung p.a. im ~s in Höhe von ~v_~6,2h EUR   │~%" 
                 (assq-ref months (cdr repay)) 22 (car repay))))))

(define print-column-header
  (lambda ()
      (format #t "├~77,,'─t┤~%")
      (format #t "│ ~v_Rate~v_Monatsende~v_Zinsen~v_Tilgung~v_Restschuld   │~%"
              2 3 11 10 9)
      (format #t "├~77,,'─t┤~%")))

(define print-monthly
  (lambda (item loan interest redemption)
          (format #t "~a ~5d ~v_~4d-~2,'0d ~v_~11,2,'·h ~v_~11,2,'·h ~v_~13,2,'·h ~a~%" 
                (if (= 1 (car (cdr start))) "├─" "│ ")  
                item 
                5 (car start) (car (cdr start))
                5 interest 
                5 redemption 
                5 (max 0 loan)
                (if (= 1 (car (cdr start))) " ─┤" "  │"))))

(define print-subtotals
  (lambda (items loan subtotal-interest subtotal-redemption)
    (format #t "├~77,,'─t┤~%")
    (format #t "│  Zwischenbilanz ~77t│ ~%")
    (print-column-header)
    (format #t "│ ∑~5d ~v_[~4d-~2,'0d] ~v_~11,2,h ~v_~11,2,h ~v_~12,2,h ~77t│ ~%" 
            items 
            3 (car start) (car (cdr start))
            4 subtotal-interest 
            5 subtotal-redemption 
            6 (- loan subtotal-redemption))
    (format #t "└~77,,'─t┘~%")))

(define print-totals
  (lambda ()
    (format #t "┌~77,,'─t┐~%")
    (format #t "│  Gesamtbilanz ~77t│ ~%")
    (print-column-header)
    (format #t "│ ∑~5d ~v_[~4d-~2,'0d] ~v_~11,2,h ~v_~11,2,h ~v_~12,2,h ~77t│ ~%" 
            total-rates 
            3 (car start) (car (cdr start))
            4 total-interest 
            5 total-redemption 
            6 open-dept)
    (format #t "└~77,,'─t┘~%")))

(define dept 
  (lambda (quasi-rent repay interest laufzeit loan)
    (let ([z (/ (/ interest 100) 12)] [n 0] [subtotal-interest 0] [subtotal-redemption 0])
      (if header-verbose (print-header loan interest repay))
      (if monthly-verbose (print-column-header))
          (let ret (
                [p 0] 
                [i 0] 
                [t 0]
                [l loan] )
            (set! start (list 
                          (if (= 12 (car (cdr start))) 
                            (+ 1 (car start)) 
                            (car start)) 
                          (if (not (= p 0)) 
                            (+ 1 (modulo (car (cdr start)) 12))
                            (- (car (cdr start)) 1))))
            (if monthly-verbose  (print-monthly p l i t))
            (if (or (> p laufzeit) (< l 0))
              0
              (ret (+ p 1)
                   (* l z)
                   (min 
                     (+ 
                   (if (= (cdr repay) (+ (car (cdr start)) 1)) (car repay) 0) 
                   (- quasi-rent (* l z))) l)
               (- l 
                 (+
                  (if (= (cdr repay) (+ (car (cdr start)) 1)) (car repay) 0) 
                  (- quasi-rent (* l z))))))
            (set! n (+ n 1))
            (set! subtotal-interest (+ subtotal-interest i))
            (set! subtotal-redemption (+ subtotal-redemption t))
            (set! total-rates (+ total-rates 1))
            (set! total-interest (+ total-interest i))
            (set! total-redemption (+ total-redemption t)) )
      (print-subtotals (- n 1) loan subtotal-interest subtotal-redemption)
      (set! start (list 
                    (if (= 12 (car (cdr start))) 
                      (+ 1 (car start)) 
                      (car start)) 
                    (+ 1 (modulo (car (cdr start)) 12))))
      (set! total-rates (- total-rates 1))
      (set! open-dept (- loan subtotal-redemption))
      (- loan subtotal-redemption)
    )))

            
(define header-verbose #t)
(define monthly-verbose #t)
(define first `((rate . 725)
                (repay . (4500 . 3))
                (interest . 3.7)
                (runtime . ,(+ (* 8 12) 3))
                (loan . 100000)))

(define second '((rate . 725)
                (repay . (0 . 3))
                (interest . 3.73)
                (runtime . 999)))
;(dept 100000 725 0.037 116)
(dept (assq-ref second 'rate)
      (assq-ref second 'repay)
      (assq-ref second 'interest)
      (assq-ref second 'runtime)
      (dept (assq-ref first 'rate)
            (assq-ref first 'repay)
            (assq-ref first 'interest)
            (assq-ref first 'runtime)
            (assq-ref first 'loan )) )
(print-totals)
