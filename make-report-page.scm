(use posix html-tags html-utils srfi-1 srfi-13 regex)

(define reports-dir  (make-parameter "./testdrive/output"))

(define year-pattern  "[0-9][0-9][0-9][0-9]")
(define month-pattern "[0-9][0-9]")
(define day-pattern   "[0-9][0-9]")

(define report-regex
  (regexp (string-append
           ".*/" year-pattern "/" month-pattern "/" day-pattern "$")))



(define date-regex
  (regexp (string-append year-pattern "/" month-pattern "/" day-pattern)))

(define (report-date path)
  (let ((m (string-search date-regex path)))
    (and (pair? m) (car m))))

(define (abs-path->www-path path)
  (make-pathname "/model-ci" (report-date path)))

(define (safe-take lst n)
  (let ((len (length lst)))
    (if (>= n len) lst
        (take lst n))))

(begin
  (let ((args (command-line-arguments)))
    (if (pair? args)
        (reports-dir (car args))))
  
  (let* ((all-reports
          (sort
           (find-files (reports-dir) test: report-regex)
           (lambda (t1 t2)
             (string> (report-date t1)
                      (report-date t2)))))
         (reports (safe-take all-reports 30))
         )

    (pp reports (current-error-port))
  
    (print
     (html-page
      (string-append
       (<div> id: "content"
              (<h2> style: "margin-bottom: 30px;" "Model continuous integration reports")
              (tabularize
               (map (lambda (report)
                      (let ((date (report-date report)))
                        (list
                         (<a> href: (abs-path->www-path report) date))))
                    reports)
               header: (list "Date" ))))
      ))
    )
  )
