#lang racket

(require odysseus)
(require odysseus/json)
(require odysseus/time)
(require "functions.rkt")

(provide (all-defined-out))

; (define couples_json (read-file "couples.json"))

; (define couples_m_f (map
;                       (λ (h)
;                         (cons
;                           ($ he.birthdate h)
;                           ($ she.birthdate h)))
;                       (json->hash couples_json)))
;
; (write-data-to-file couples_m_f "couples.rktd")

(define zodiac_intervals (hash
                            'capricorn '(("01.01" . "19.01") ("22.12" . "31.12"))
                            'aquarius '(("20.01" . "18.02"))
                            'pisces '(("19.02" . "20.03"))
                            'aries '(("21.03" . "19.04"))
                            'taurus '(("20.04" . "20.05"))
                            'gemini '(("21.05" . "20.06"))
                            'cancer '(("21.06" . "22.07"))
                            'leo '(("23.07" . "22.08"))
                            'virgo '(("23.08" . "22.09"))
                            'libra '(("23.09" . "22.10"))
                            'scorpio '(("23.10" . "21.11"))
                            'sagittarius '(("22.11" . "21.12"))))

(define month_intervals (hash
                            'jan '(("01.01" . "31.01"))
                            'feb '(("01.02" . "29.02"))
                            'mar '(("01.03" . "31.03"))
                            'apr '(("01.04" . "31.04"))
                            'mai '(("01.05" . "31.05"))
                            'jun '(("01.06" . "31.06"))
                            'jul '(("01.07" . "31.07"))
                            'aug '(("01.08" . "31.08"))
                            'sep '(("01.09" . "31.09"))
                            'oct '(("01.10" . "31.10"))
                            'nov '(("01.11" . "31.11"))
                            'dec '(("01.12" . "31.12"))))

(define month_10_intervals (hash
                            'jan10 '(("10.01" . "09.02"))
                            'feb10 '(("10.02" . "09.03"))
                            'mar10 '(("10.03" . "09.04"))
                            'apr10 '(("10.04" . "09.05"))
                            'mai10 '(("10.05" . "09.06"))
                            'jun10 '(("10.06" . "09.07"))
                            'jul10 '(("10.07" . "09.08"))
                            'aug10 '(("10.08" . "09.09"))
                            'sep10 '(("10.09" . "09.10"))
                            'oct10 '(("10.10" . "09.11"))
                            'nov10 '(("10.11" . "09.12"))
                            'dec10 '(("10.12" . "31.12") ("01.01" . "09.01"))))

(define (get-zodiac adate intervals)
  (with-handlers
    ((exn:fail? (λ (v) (--- (format "error while processing date: ~a" adate)) 'n/a)))
      (let ((adm (d.m adate)))
        (->
          (hash-filter
            (λ (k v)
              (ormap
                (λ (interval)
                  (and
                    (d>= adm (car interval))
                    (d<= adm (cdr interval))))
                v))
            intervals)
          hash-keys
          first))))

(define couples_m_f (read-serialized-data-from-file "couples.rktd"))

; все пары по зодиаку
(define couples_zodiacs (map
                          (λ (mf)
                            (cons
                              (get-zodiac (car mf) zodiac_intervals)
                              (get-zodiac (cdr mf) zodiac_intervals)))
                          couples_m_f))

; все пары по месяцам
(define couples_months (map
                          (λ (mf)
                            (cons
                              (get-zodiac (car mf) month_intervals)
                              (get-zodiac (cdr mf) month_intervals)))
                          couples_m_f))

; все пары между десятыми числами месяцев
(define couples_months_10 (map
                          (λ (mf)
                            (cons
                              (get-zodiac (car mf) month_10_intervals)
                              (get-zodiac (cdr mf) month_10_intervals)))
                          couples_m_f))

(let ((zodiac_deltas (hash-values (get-distr-deltas couples_zodiacs)))
      (month_deltas (hash-values (get-distr-deltas couples_months)))
      (month_10_deltas (hash-values (get-distr-deltas couples_months_10)))
      )
  (--- (format
          "deltas zodiacs: [~a, ~a],\ndeltas by months: [~a, ~a],\ndeltas by months, starting from 10th day: [~a, ~a]"
          (apply min zodiac_deltas)
          (apply max zodiac_deltas)
          (apply min month_deltas)
          (apply max month_deltas)
          (apply min month_10_deltas)
          (apply max month_10_deltas)
          )))
