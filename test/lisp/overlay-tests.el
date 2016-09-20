


(require 'ert)

(ert-deftest overlay-create-test ()
  "  "
  (with-temp-buffer
    (insert  "blueberrypancakes")
    (let ((o1 (make-overlay 4 9)))
      (should-not (overlay-get o1 'face))
      (should (overlayp o1))
      (should (= (overlay-start o1) 4))
      (should (= (overlay-end o1) 9))
      (should (eq (overlay-buffer o1) (current-buffer)))
      (let ((b (current-buffer)))
        (with-temp-buffer
          (should (eq (overlay-buffer o1) b))))
      (should (= (length (overlays-in (point-min) (point-max))) 1))
      (should (eq (car (overlays-in (point-min) (point-max))) o1)))))


(ert-deftest overlay-move-test ()
  "  "
  (with-temp-buffer
    (insert "blueberrypancakes")
    (let ((o1 (make-overlay 4 9)))
      ;; Test a "normal" move
      (should (= (overlay-start o1) 4))
      (should (= (overlay-end o1) 9))
      (should (eq (overlay-buffer o1) (current-buffer)))
      (move-overlay o1 3 10)
      (should (= (overlay-start o1) 3))
      (should (= (overlay-end o1) 10))
      (let ((b (current-buffer)))
        (with-temp-buffer
          (insert "blueberry")
          (move-overlay o1 2 4)
          (should (eq (overlay-buffer o1) b))
          (move-overlay o1 2 4 (current-buffer))
          (should (eq (overlay-buffer o1) (current-buffer)))
          (should (= (overlay-start o1) 2))
          (should (= (overlay-end o1) 4))))
      (move-overlay o1 1 50 (current-buffer))
      (should (eq (overlay-buffer o1) (current-buffer)))
      (should (= (overlay-start o1) 1))
      (should (= (overlay-end o1) (point-max))))))

(ert-deftest overlay-front-advance-test ()
  (with-temp-buffer
    (insert "blueberrypancakes")
    (let ((o1 (make-overlay 1 5 nil t))
          (o2 (make-overlay 1 5))
          (str "creamy "))
      (goto-char (point-min))
      (insert str)
      (should (= (overlay-start o2) 1))
      (should (= (overlay-start o1) (1+ (length str)))))))

(ert-deftest overlay-rear-advance-test ()
  (with-temp-buffer
    (insert "blueberrypancakes")
    (let ((o1 (make-overlay 7 18 nil nil t))
          (o2 (make-overlay 7 18))
          (str " for dinner"))
      (should (= (point-max) 18))
      (goto-char (point-max))
      (insert str)
      (should (= (overlay-end o1) (point-max)))
      (should (= (overlay-end o2) 18)))))
