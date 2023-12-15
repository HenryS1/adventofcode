(defpackage day15-tests
  (:use :cl :day15 :parachute :pears))

(in-package :day15-tests)

(define-test day15-suite)

(define-test hash-step-calculates-the-hash-for-a-step 
  :parent day15-suite
  (is = 30 (hash-step "rn=1"))
  (is = 253 (hash-step "cm-"))
  (is = 97 (hash-step "qp=3"))
  (is = 47 (hash-step "cm=2"))
  (is = 14 (hash-step "qp-"))
  (is = 180 (hash-step "pc=4"))
  (is = 9 (hash-step "ot=9"))
  (is = 197 (hash-step "ab=5"))
  (is = 48 (hash-step "pc-"))
  (is = 214 (hash-step "pc=6"))
  (is = 231 (hash-step "ot=7")))

(define-test focusing-power-for-instructions-finds-the-focusing-power-after-applying-instructions
  :parent day15-suite
  (let ((instructions (parse-string (parse-instructions) 
                                    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")))
    (is = 145 (focusing-power-for-instructions instructions))))
