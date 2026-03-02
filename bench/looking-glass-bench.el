;;; looking-glass-bench.el --- Microbenchmarks for looking-glass -*- lexical-binding: t; -*-

(require 'benchmark)
(require 'cl-lib)
(require 'looking-glass)

(defun lg-bench--run (label iterations fn)
  "Run ITERATIONS of FN and print LABEL with benchmark numbers."
  (let ((result (benchmark-run iterations (funcall fn))))
    (message "%-28s %8.4fs total  %8.6fs/iter  %d gc"
             label
             (nth 0 result)
             (/ (nth 0 result) iterations)
             (nth 1 result))
    result))

(defun lg-bench-run ()
  "Run looking-glass microbenchmarks in batch or interactive Emacs."
  (interactive)
  (let* ((iterations 3000)
         (small-list (number-sequence 1 64))
         (large-list (number-sequence 1 1024))
         (small-vector (vconcat small-list))
         (table (let ((h (make-hash-table :test 'equal)))
                  (dotimes (i 128 h)
                    (puthash (format "k%d" i) i h))))
         (plist (cl-loop for i from 0 below 128
                         append (list (intern (format ":k%d" i)) i)))
         (alist (cl-loop for i from 0 below 128
                         collect (cons (format "k%d" i) i))))
    (message "looking-glass microbenchmarks (iterations=%d)" iterations)
    (lg-bench--run "over list x64"
                   iterations
                   (lambda ()
                     (lg-over lg-list (lambda (x) (+ x 1)) small-list)))
    (lg-bench--run "over list x1024"
                   iterations
                   (lambda ()
                     (lg-over lg-list (lambda (x) (+ x 1)) large-list)))
    (lg-bench--run "iover list x64"
                   iterations
                   (lambda ()
                     (lg-iover lg-indexed-list
                               (lambda (i x) (+ i x))
                               small-list)))
    (lg-bench--run "over vector x64"
                   iterations
                   (lambda ()
                     (lg-over lg-vector (lambda (x) (+ x 1)) small-vector)))
    (lg-bench--run "ix plist update"
                   iterations
                   (lambda ()
                     (lg-set (lg-ix :k42) 999 plist)))
    (lg-bench--run "ix alist update"
                   iterations
                   (lambda ()
                     (lg-set (lg-ix "k42" #'string=) 999 alist)))
    (lg-bench--run "ix hash update"
                   iterations
                   (lambda ()
                     (lg-set (lg-ix "k42") 999 table)))
    (message "done")))

(provide 'looking-glass-bench)

;;; looking-glass-bench.el ends here
