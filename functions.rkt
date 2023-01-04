#lang racket

(require odysseus)

(provide (all-defined-out))

(define (get-distr-deltas couples)
  ; фактическое распределение пар по интервалам
  (define distr
    (for/fold
      ((res (hash)))
      ((couple couples))
      (hash-update res couple add1 0)))

  ; общее число пар
  (define N (apply + (hash-values distr)))

  ; распределение мужчин по интервалам
  (define Freq_m
    (for/fold
      ((res (hash)))
      (((k v) distr))
      (hash-update res (car k) (curry + v) 0)))

  ; распределение женщин по интервалам
  (define Freq_f
    (for/fold
      ((res (hash)))
      (((k v) distr))
      (hash-update res (cdr k) (curry + v) 0)))

  ; гипотетическое равномерное распределение пар
  ; e.g. N_leo_aries = P_leo_taurus * N = Pf_leo * Pm_taurus * N = (F_leo / F) * (M_taurus / M) * N = (F_leo * M_taurus / N)
  (define distr_norm
    (for/hash
      (((k v) distr))
      (let ((P_m (hash-ref Freq_m (car k)))
            (P_f (hash-ref Freq_f (cdr k))))
        (values k (/ (* P_m P_f 1.0) N)))))

  ; разница в значениях между фактическим и гипотетическим распределением
  (define distr_delta
    (hash-map
      (λ (k v)
        (values
          k
          (- (hash-ref distr k) (hash-ref distr_norm k))))
      distr))

  distr_delta)
