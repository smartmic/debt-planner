(setlocale LC_ALL "de_DE.UTF-8")
(use-modules (srfi srfi-9) (ice-9 format) (ice-9 i18n))
(define total-rates 0)
(define total-interest 0)
(define total-redemption 0)
(define open-dept 0)
(define start (list 2011 11))
(set! start (list (car start) (car (cdr start)) ))

(define _loan (gettext "Darlehen über"))
(define _runtime (gettext "Laufzeitbeginn zum 1. des Monats"))
(define _annual-interest (gettext "Sollzinssatz"))
(define _effrate (gettext "Effektivzins"))
(define _srepay (gettext "Sondertilgung p.a. im"))
(define _value (gettext "in Höhe von"))
(define _subtotal (gettext "Zwischenbilanz"))
(define _total (gettext "Gesamtbilanz"))
(define _rate (gettext "Rate"))
(define _eom (gettext "Monatsende"))
(define _interest (gettext "Zinsen"))
(define _repay (gettext "Tilgung"))
(define _rest (gettext "Restschuld"))

(define header-verbose #t)
(define monthly-verbose #t)
(define-record-type <loan>
                    (make-loan rate scheduled-repay interest runtime amount)
                    loan?
                    (rate loan-rate)
                    (scheduled-repay loan-scheduled-repay)
                    (interest loan-interest)
                    (runtime loan-runtime)
                    (amount loan-amount set-loan-amount!))


(define print-header 
  (lambda (loan interest repay)
    (let ([z (/ (/ interest 100) 12)])
      (format #t "┌~77,,'─t┐~%")

      (format #t "│  ~a ~v_~10,2h ~a   │~%" _loan 42 loan
              (locale-currency-symbol #t))

      (format #t "│  ~a ~v_~10d-~2,'0d  │~%" _runtime 26 (car start)
              (car (cdr start)))

      (format #t "│  ~a ~v_~3,2f %   │~%" _annual-interest 52 interest)
      (format #t "│  ~a p.a. ~v_~3,2f %   │~%" _effrate 
              47 (* 100 (- (expt (+ 1 z) 12) 1)) )

      (if (> (car repay) 0)
        (format #t "│  ~a ~a ~a ~v_~6,2h ~a  │~%" _srepay
                (locale-month (cdr repay)) _value 22 (car repay)
                (locale-currency-symbol #t) )))))

(define print-column-header
  (lambda ()
      (format #t "├~77,,'─t┤~%")
      (format #t "│ ~v_~a~v_~a~v_~a~v_~a~v_~a   │~%"
              2 _rate 3 _eom 11 _interest 10 _repay 9 _rest )
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
    (format #t "│  ~a ~77t│ ~%" _subtotal)
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
    (format #t "│  ~a ~77t│ ~%" _total)
    (print-column-header)
    (format #t "│ ∑~5d ~v_[~4d-~2,'0d] ~v_~11,2,h ~v_~11,2,h ~v_~12,2,h ~77t│ ~%" 
            total-rates 
            3 (car start) (car (cdr start))
            4 total-interest 
            5 total-redemption 
            6 open-dept)
    (format #t "└~77,,'─t┘~%")))

(define dept 
  (lambda (inquiry)
    (let ([z (/ (/ (loan-interest inquiry) 100) 12)] [n 0] [subtotal-interest 0] [subtotal-redemption 0])
      (if header-verbose (print-header 
                           (loan-amount inquiry) 
                           (loan-interest inquiry)
                           (loan-scheduled-repay inquiry)))
      (if monthly-verbose (print-column-header))
          (let ret (
                [p 0] 
                [i 0] 
                [t 0]
                [l (loan-amount inquiry)] )
            (set! start (list 
                          (if (= 12 (car (cdr start))) 
                            (+ 1 (car start)) 
                            (car start)) 
                          (if (not (= p 0)) 
                            (+ 1 (modulo (car (cdr start)) 12))
                            (- (car (cdr start)) 1))))
            (if monthly-verbose  (print-monthly p l i t))
            (if (or (> p (loan-runtime inquiry)) (< l 0))
              0
              (ret (+ p 1)
                   (* l z)
                   (min 
                     (+ 
                       (if (= (cdr (loan-scheduled-repay inquiry)) 
                          (+ (car (cdr start)) 1)) 
                       (car (loan-scheduled-repay inquiry)) 0) 
                     (- (loan-rate inquiry) (* l z))) 
                     l)
               (- l 
                 (+
                  (if (= (cdr (loan-scheduled-repay inquiry)) (+ (car (cdr start)) 1)) (car (loan-scheduled-repay inquiry)) 0) 
                  (- (loan-rate inquiry) (* l z))))))
            (set! n (+ n 1))
            (set! subtotal-interest (+ subtotal-interest i))
            (set! subtotal-redemption (+ subtotal-redemption t))
            (set! total-rates (+ total-rates 1))
            (set! total-interest (+ total-interest i))
            (set! total-redemption (+ total-redemption t)) )
      (print-subtotals (- n 1) (loan-amount inquiry) subtotal-interest subtotal-redemption)
      (set! start (list 
                    (if (= 12 (car (cdr start))) 
                      (+ 1 (car start)) 
                      (car start)) 
                    (+ 1 (modulo (car (cdr start)) 12))))
      (set! total-rates (- total-rates 1))
      (set! open-dept (- (loan-amount inquiry) subtotal-redemption))
      (- (loan-amount inquiry) subtotal-redemption)
    )))

            
(define initial (make-loan 725 '(4500 . 3) 3.7 (+ (* 8 12) 3) 100000))
(define suite (make-loan 725 '(4500 . 3) 3.7 (+ (* 8 12) 3) (dept initial)))
(dept suite)
;(dept 100000 725 0.037 116)
;(print-totals)
