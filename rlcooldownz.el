
;; Time Points & Countdowns

(defun make-time-point (name time)
  (list name (apply 'encode-time time)))

(defun load-time-points (lst)
  (let (time-points)
    (dolist (elt lst time-points)
      (setq time-points (cons (make-time-point (nth 0 elt) (nth 1 elt)) time-points)))))

(defun time-point-less-p (l r)
  (time-less-p (nth 1 l) (nth 1 r)))

(defun format-countdown (time-point)
  (format "%s : %s : %s" (nth 0 time-point)
	  (my-format-seconds (seconds-until (nth 1 time-point)))
	  (my-format-time-string (nth 1 time-point))))

(defun print-countdowns (time-points)
  (dolist (x (sort time-points 'time-point-less-p)) (print (format-countdown x))))

;; TODO: save-countdowns-to-file
;; TODO: load-countdowns-from-file

;; Utilities

(defun fake-current-time ()
  (apply 'encode-time '(0 30 17 21 4 2018 6 nil -25200)))

(defun seconds-until (time &optional test)
  (if test
      (truncate (float-time (time-subtract (fake-current-time) time)))
    (truncate (float-time (time-subtract (current-time) time)))))

(defun my-format-seconds (seconds)
  (format-seconds "%d days %h hours %m minutes %z%s seconds" (- seconds)))

(defun my-format-time-string (time)
  (format-time-string "%x %T" time))

;; Test

(ert-deftest test-seconds-until ()
  (should (= -305 (seconds-until (encode-time 5 35 17 21 4 2018 6 nil -25200) t)))
  (should (= -305 (seconds-until (encode-time 5 35 17 21 4 2018) t)))
  (should (equal "5 minutes 5 seconds" (my-format-seconds -305))))

(ert-test-passed-p (ert-run-test (ert-get-test 'test-seconds-until)))

(defun test-countdowns ()
  (print-countdowns (list (make-time-point "Sell the Acura" '(0 0 0 1 6 2018))
			  (make-time-point "High adventure" '(0 0 5 8 6 2018))
			  (make-time-point "Do family history" '(0 30 20 22 4 2018)))))

(defun test-load-countdowns ()
  (print-countdowns (load-time-points '(("Sell the Acura" (0 0 0 1 6 2018))
					("High adventure" (0 0 5 8 6 2018))
					("Do family history" (0 30 20 22 4 2018))))))

(setq bird-dog (run-at-time "1 sec" 0.1 (lambda (x) (print "hello")) nil))
(cancel-timer bird-dog)

(test-countdowns)
(test-load-countdowns)
