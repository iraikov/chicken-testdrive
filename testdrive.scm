
;;
;; A driver program for continuous integration scripts.
;;
;; Copyright 2013-2016 Ivan Raikov.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;
;;

(use data-structures posix files tcp srfi-1 srfi-13 regex setup-api
     matchable uri-generic ersatz-lib)


(define v:quiet 0)
(define v:info  1)
(define v:debug 2)

(define ($ s) (if (symbol? s) s (string->symbol (->string s))))

(define verbose (make-parameter v:info))

(define prefix (make-parameter 
                (make-pathname (or (get-environment-variable "HOME") ".")
                               "testdrive")))

(define modules (make-parameter '()))
(define env (make-parameter '()))


(define (template-env)
  (map (lambda (x)
         (let ((k (string->symbol (->string (car x))))
               (v (Tstr (->string (cdr x)))))
           (cons k v)))
       (env)))


(define (temp-path)
  (make-parameter (or (get-environment-variable "TMPDIR") 
                      (make-pathname (prefix) "/tmp"))))



(define (version-path)
  (make-pathname (prefix) "module.versions"))
  
(define (manifest-path)
  (make-pathname (prefix) "module.manifest"))
  
(define (output-location-prefix)
  (make-pathname (prefix) "output"))
  
(define (build-location-prefix)
  (make-pathname (prefix) "build"))

(define (scripts-location module-name) 
  (make-pathname (prefix) (sprintf "scripts/~A" module-name)))

(define (build-location module-name version) 
  (make-pathname (build-location-prefix) 
		 (sprintf "~A.~A" module-name version)))

(define (process-lock-path module-name version) 
  (make-pathname (build-location module-name version) 
		 (string-append (sprintf "~A-process-lock." module-name)
				(->string version)) ))

(define (build-log-path module-name version) 
  (make-pathname (build-location module-name version) 
		 (string-append (sprintf "~A-log." module-name) 
				(->string version)) ))

(define (build-lock-path module-name version) 
  (make-pathname (build-location module-name version) 
		 (string-append (sprintf "~A-build-lock." module-name)
				(->string version)) ))

(define (tests-lock-path module-name version) 
  (make-pathname (build-location module-name version) 
		 (string-append (sprintf "~A-tests-lock." module-name)
				(->string version)) ))

(define (tests-log-path module-name version) 
  (make-pathname (build-location module-name version) 
		 (string-append (sprintf "~A-tests-log." module-name)
				(->string version)) ))

(define (plots-lock-path module-name version) 
  (make-pathname (build-location module-name version) 
		 (string-append (sprintf "~A-plots-lock." module-name)
				(->string version)) ))

(define (plots-log-path module-name version) 
  (make-pathname (build-location module-name version) 
		 (string-append (sprintf "~A-plots-log." module-name)
				(->string version)) ))




(define (sed-quote str)
  (let ((lst (string->list str)))
    (let recur ((lst lst) (ax '()))
      (if (null? lst) (list->string (reverse ax))
	  (let ((c (car lst)))
	    (if (char=? c #\/) (recur (cdr lst) (cons c (cons #\\ ax)))
		(recur (cdr lst) (cons c ax))))
	  ))))


(define (quotewrap str)
  (cond ((quotewrapped? str) str)
	((string-any char-whitespace? str)
	 (string-append "\"" str "\""))
	(else str)))


(define (d fstr . args)
  (if (= (verbose)  v:debug)
      (let ([port (current-output-port)])
	(apply fprintf port fstr args)
	(flush-output port) ) ))


(define (info fstr . args)
  (if (>= (verbose) v:info)
      (let ([port (current-output-port)])
	(apply fprintf port fstr args)
	(flush-output port) ) ))


(define (run:execute explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  (for-each (lambda (cmd)
	      (info "  ~A~%~" cmd)
	      (system (->string cmd)))
	    (map smooth explist)))


(define (run:execute* explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  (for-each (lambda (cmd)
	      (info "  ~A~%~" cmd)
	      (system* "~a" cmd))
	    (map smooth explist)))



(define-syntax run
  (syntax-rules ()
    ((_ exp ...)
     (run:execute* (list `exp ...)))))


(define-syntax run-
  (syntax-rules ()
    ((_ exp ...)
     (run:execute (list `exp ...)))))


(define (ipipe:execute lam cmd)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  ((lambda (cmd) 
     (info "  ~A~%~" cmd)
     (call-with-input-pipe (sprintf "~a" cmd) lam))
   (smooth cmd)))


(define-syntax ipipe
  (syntax-rules ()
    ((_ lam exp)
     (ipipe:execute lam `exp ))))


;; From spiffy

(define (link url label)
  `(a (@ (href ,url)) ,label))


(define (call-with-input-file* file proc)
  (call-with-input-file
      file (lambda (p)
	     (handle-exceptions exn
		(begin (close-input-port p) (signal exn))
		(proc p)))))


(define (call-with-output-file* file proc)
  (call-with-output-file
      file (lambda (p)
	     (handle-exceptions exn
		(begin (close-output-port p) (signal exn))
		(proc p)))))


(define (template-script source-path target-path)
  (let (
        (lexer (make-lexer 
                begin-expand: "{{" end-expand: "}}"
                begin-logic: "{%" end-logic: "%}" 
                compile: #f))
        )
    (lexer-trace #t)

    (if (and (file-exists? source-path) (not (file-exists? target-path)))
        (begin
          (run- (mkdir -p ,(pathname-directory target-path))) 
          (call-with-output-file target-path
            (lambda (out)
              (let ((source-file (pathname-strip-directory source-path))
                    (source-dir (pathname-directory source-path)))
                (display (from-file source-file 
                                    models: (template-env) 
                                    env: (template-std-env 
                                          lexer: lexer
                                          search-path: `(,source-dir)))
                         out))))
          (run (chmod u+x ,target-path))
          ))
    ))
         



(define (revisions-command module-name config)
  (or (alist-ref 'revision-command config)
      (alist-ref 'revisions-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'revisions-command "unable to find configuration directory of module" 
                   module-name))
        (let ((source-path (make-pathname config-dir "revisions")))
          (if (not (file-exists? source-path))
              (error 'revisions-command "unable to find revisions command of module" 
                     module-name))
          source-path)
        ))
      )


(define (fetch-command module-name config)
  (or (alist-ref 'fetch-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'fetch-command "unable to find configuration directory of module" 
                   module-name))
	(let ((source-path (make-pathname config-dir "fetch"))
              (target-path (make-pathname (scripts-location module-name) "fetch")))
          (if (not (file-exists? source-path))
              (error 'fetch-command "unable to find fetch command of module" 
                     module-name))
          (template-script source-path target-path)
          target-path
        ))
  ))


(define (build-command module-name config)
  (or (alist-ref 'build-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'build-command "unable to find configuration directory of module" 
                   module-name))
	(let ((source-path (make-pathname config-dir "build"))
              (target-path (make-pathname (scripts-location module-name) "build")))
          (if (not (file-exists? source-path))
              (error 'build-command "unable to find build command of module"
                     module-name))
          (template-script source-path target-path)
          target-path
        ))
  ))


(define (test-commands module-name config)
  (or (alist-ref 'test-commands config)
      (alist-ref 'test-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'test-command "unable to find configuration directory of module"
                   module-name))
	(if (file-exists? (make-pathname config-dir "tests"))
	    (let ((tests-run-path (make-pathname config-dir "tests/run")))
	      (let ((source-paths 
		     (if (file-exists? tests-run-path)
			 (list tests-run-path)
			 (let ((flst (find-files (make-pathname config-dir "tests") 
						 limit: 1
						 test: file-execute-access?)))
			   (sort flst string<?)
			   ))
                 ))
		(let ((target-paths 
		       (map (lambda (x)
			      (make-pathname  (scripts-location module-name) 
					      (pathname-strip-directory x)))
			    source-paths)))
		  (for-each template-script source-paths target-paths)
		  target-paths
		  ))
	      ))
	))
  )


(define (plot-commands module-name config)
  (or (alist-ref 'plot-commands config)
      (alist-ref 'plot-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'plot-command "unable to find configuration directory of module" 
                   module-name))
	(let ((plots-run-path (make-pathname config-dir "plots")))
	  (let ((source-paths 
                 (list plots-run-path)))
            (let ((target-paths 
                   (map (lambda (x)
                          (make-pathname  (scripts-location module-name) 
                                          (pathname-strip-directory x)))
                        source-paths)))
              (for-each template-script source-paths target-paths)
              target-paths
              ))
          ))
      ))


(define (cleanup-command module-name config)
  (or (alist-ref 'cleanup-command config)
      (let ((config-dir (alist-ref 'config-path config)))
	(if (not config-dir)
	    (error 'cleanup-command "unable to find configuration directory of module" 
                   module-name))
	(let ((source-path (make-pathname config-dir "cleanup"))
              (target-path (make-pathname (scripts-location module-name) "cleanup")))
          (and (file-exists? source-path)
               (begin
                 (template-script source-path target-path)
                 target-path))
        ))
  ))


(define (build module-name build-dir local-version version lock-file log-file fetch-cmd build-cmd )
  (if (not (file-exists? lock-file))
      (call-with-output-file* log-file
	(lambda (out)
	  (run (mkdir -p ,build-dir)
               (touch ,lock-file))
	  (if (or (not local-version) (not (string=? version local-version)))
	      (let ((manifest0 (find-files build-dir)))
		(run- (,fetch-cmd ,module-name ,version ,build-dir >> ,log-file 2>&1  ))
                (let ((manifest1 (find-files build-dir)))
                  (run- (,build-cmd ,module-name ,build-dir >> ,log-file 2>&1 ))
                  (let (
                        (versions (call-with-input-file* (version-path) read))
                        (manifests (call-with-input-file* (manifest-path) read))
                        (manifest  (lset-difference string=? manifest1 manifest0))
                        )
                    (let ((versions1 (if (pair? versions) 
                                         (alist-update module-name version versions)
                                         (list (cons module-name version))))
                          (manifests1 (if (pair? manifests) 
                                          (alist-update module-name manifest manifests)
                                          (list (cons module-name manifest))))
                          )
                      (call-with-output-file* (version-path)
                                              (lambda (out) (write versions1 out )))
                      (call-with-output-file* (manifest-path)
                                              (lambda (out) (write manifests1 out )))
                      )
                    ))
                ))
	  (run (rm ,lock-file))
	  ))
      ))


(define (run-tests module-name build-dir version lock-file log-file cmds)
  (if (not (file-exists? lock-file))
      (call-with-output-file* log-file
        (lambda (out)				 
	  (run (touch ,lock-file))
	  (for-each (lambda (cmd) (run- (,cmd ,module-name ,build-dir >> ,log-file  2>&1  ))) cmds)
	  (run (rm ,lock-file))
	  ))
      ))


(define (make-plots module-name build-dir version lock-file log-file cmds)
  (if (not (file-exists? lock-file))
      (call-with-output-file* log-file
	(lambda (out)
	  (run (touch ,lock-file))
	  (for-each (lambda (cmd) (run- (,cmd ,module-name ,build-dir >> ,log-file  2>&1  ))) cmds)
	  (run (rm ,lock-file))
	  ))
      ))


(define (update-module module-name config)

  (if (not (file-exists? (version-path)))
      (let* ((path (version-path))
	     (dir (pathname-directory path)))
	(run- (mkdir -p ,dir) (touch ,path))))
              
  (if (not (file-exists? (manifest-path)))
      (let* ((path (manifest-path))
	     (dir (pathname-directory path)))
	(run- (mkdir -p ,dir) (touch ,path))))
              

  (let ((versions (call-with-input-file* (version-path) read)))

    (let (
          (local-version
           (and versions (pair? versions) (alist-ref module-name versions)))
          
	  (remote-version
           (string-trim-both
            (car
             (let ((v (ipipe (lambda (x) (read-lines x)) (,(revisions-command module-name config) ,module-name))))
               (if (null? v) (error 'update-module "cannot obtain version of module" module-name))
               v))
            ))
          )

      (let ((loc (build-location module-name remote-version)))
	(if (not (file-exists? loc))
	    (let ((build-lock-file (build-lock-path module-name remote-version))
		  (build-log-file (build-log-path module-name remote-version))
		  (test-lock-file (tests-lock-path module-name remote-version))
		  (test-log-file  (tests-log-path module-name remote-version))
		  (plot-lock-file (plots-lock-path module-name remote-version))
		  (plot-log-file  (plots-log-path module-name remote-version))
		  (process-lock-file (process-lock-path module-name remote-version))
		  )
	      (run (mkdir -p ,loc)
                   (touch ,build-log-file  ,test-log-file ,plot-log-file ,process-lock-file))
	      (process-fork
	       (lambda ()
		 (handle-exceptions exn
		    (begin (run (rm ,process-lock-file)) (signal exn))
		    (let ((cleanup (cleanup-command module-name config)))
		      (build module-name loc local-version remote-version 
			     build-lock-file build-log-file
			     (fetch-command module-name config)
			     (build-command module-name config))
		      (run-tests module-name loc remote-version 
				 test-lock-file test-log-file
				 (test-commands module-name config))
		      (make-plots module-name loc remote-version 
				  plot-lock-file plot-log-file
				  (plot-commands module-name config))
                      (if cleanup (run- (,cleanup)))
		      (run (rm ,process-lock-file))
		      (exit 0)
		      ))
		 ))
	      ))
	(list remote-version loc))
      ))
)

(define (modules-table meta-data meta-headers)
  (cadr 
   (fold 
    (lambda (module-meta ax)
      (match-let
       (((i lst) ax))
       (let* (
              (module-name  (car module-meta))
              (module-metav (cdr module-meta))
              (module-label (alist-ref 'label module-metav))
              )
         
         (list (+ 1 i) 
               (cons `(tr (@ (class ,(if (even? i) "even" "odd")))
                          (td ,(or module-label (sprintf "Module ~A" module-name))
                              " " ,(link (sprintf "module-log.html#~A" module-name) "(Logs)") 
                                        " " ,(link (sprintf "module-plot.html#~A" module-name) "(Plots)"))
                          . ,(map (lambda (mh) `(td ,(or (alist-ref mh module-metav) ""))) meta-headers))
                     lst))
         )))
    (list 0 '())
    meta-data)
   )
  )

(define (modules-page modules manifests)

  (define (meta-data< name)
    (lambda (x y)
      (let ((x-meta (cdr x)) (y-meta (cdr y)))
          (let ((x-prop (alist-ref name x-meta))
                (y-prop (alist-ref name y-meta)))
            (string<? (->string x-prop) (->string y-prop))
            ))
      ))


  (let* (
         (meta-data
          (map 
           (lambda (kv)
             (let* ((module-name (car kv))
                    (module-config (cdr kv))
                    (version.path (update-module module-name module-config))
                    (source-file (alist-ref module-name manifests))
                    )
               (cons module-name
                     (cons* `(version . ,(car version.path)) 
                            `(source . ,(link (pathname-strip-directory source-file)
                                              (pathname-strip-directory source-file)))
                            (alist-ref 'meta module-config)))
               ))
           modules))

         (meta-headers 
          (delete-duplicates 
            (fold (lambda (m ax) (fold (lambda (x ax) (cons ($ (car x)) ax)) ax (cdr m))) '() meta-data)
            equal?
            ))
         )
  `(
    (h1 "All modules")

    (table 
     (tr . ,(map (lambda (n href) `(th (a (@ (href ,href)) (b ,n)))) 
                 (cons "Module" meta-headers)
                 (cons "" (map (lambda (h) (sprintf "#orderedby~A" h)) meta-headers))
                 ))
     ,(modules-table meta-data meta-headers)
     )

    ,(map 
      (lambda (meta)
        (let ((sorted-meta-data (sort meta-data (meta-data< meta)))
              (key (sprintf "orderedby~A" meta)))
          `((h1 (a (@ (id ,key)) ,(sprintf "Modules ordered by property ~A" meta)))
            (table 
             (tr . ,(map (lambda (n href) `(th (a (@ (href ,href)) (b ,n))))
                         (cons "Module" meta-headers)
                         (cons "" (map (lambda (h) (sprintf "#orderedby~A" h)) meta-headers))
                         ))
             ,(modules-table sorted-meta-data meta-headers)
             )
            ))
        )
      (filter (lambda (x) (not (case x ((version source) #t) (else #f))))
              meta-headers))

    ))
  )
   

(define (module-log-page module-name module-config)
  (let (
        (module-label  (alist-ref 'label module-config))
        (version.path (update-module module-name module-config))
        )
    `((h1 ,(or module-label (sprintf "Module ~A" module-name )))
      (a (@ (name ,(->string module-name))))
      (p (ul (@ (id "module-menu")) 
             (li ,(link "index.html" "Back to index"))
             ))
      (p)
      (p ,(sprintf "The current version of ~A is ~A.~%" module-name (car version.path)))
      (p ,(link (sprintf "module-plot.html#~A" module-name) "Module plots"))
      (p ,(link (sprintf "module-build-log.html#~A" module-name)
               (sprintf "Module build log version ~A~%" 
                         (car version.path))))
      (p ,(link (sprintf "module-test-log.html#~A" module-name)
                (sprintf "Module test log version ~A~%" 
                         (car version.path))))
      (p ,(link (sprintf "module-plot-log.html#~A" module-name)
                (sprintf "Module plot log version ~A~%" 
                         (car version.path))))
      ))
  )

	

   
(define (copy-module-img-plots module-name module-loc module-dest)
  (let ((jpgpat  "(.*\\.[jJ][pP][eE]?[gG]$)")
	(pngpat  "(.*\\.[pP][nN][gG]$)")
        (svgpat  "(.*\\.[sS][vV][gG]$)")
        )
    (let ((pat (string-append jpgpat "|" pngpat "|" svgpat)))
      (let ((flst (find-files module-loc test: (regexp pat))))
        (for-each
         (lambda (f)
           (let ((fn (pathname-strip-directory f)))
             (run (cp ,f  ,(make-pathname module-dest fn)))))
         flst)))
    ))

   
(define (module-img-plots module-name module-loc module-dest module-config)
  (let (
        (module-label  (alist-ref 'label module-config))
        (jpgpat  "(.*\\.[jJ][pP][eE]?[gG]$)")
	(pngpat  "(.*\\.[pP][nN][gG]$)")
        (svgpat  "(.*\\.[sS][vV][gG]$)")
        )
    (let ((pat (string-append jpgpat "|" pngpat "|" svgpat)))
      (let ((flst (find-files module-loc test: (regexp pat))))
        `(
          (h1 ,(or module-label (sprintf "Module ~A" module-name )))
          (a (@ (name ,(->string module-name))))
          (p (ul (@ (id "module-menu")) 
                 (li ,(link "index.html" "Back to index"))
                 (li  "Module plots")
                 (li ,(link (sprintf "module-build-log.html#~A" module-name)
                            "Build log"))
                 (li ,(link (sprintf "module-test-log.html#~A" module-name)
                            "Test log"))
                 (li ,(link (sprintf "module-plot-log.html#~A" module-name)
                            "Plot log"))
                 ))
          ,(map (lambda (f) 
                  (let ((fn (pathname-strip-directory f)))
                    `(
                      (p (img (@ (src ,(make-pathname
                                        "figures" 
                                        (make-pathname (->string module-name) fn)))))))
                    )) 
                flst)
          )
        ))
    ))


(define (module-plot-page module-name module-config output-dir)
   (let* ((version.path (update-module module-name module-config))
          (plot-file-path (cadr version.path))
          (plot-file-dest (make-pathname output-dir (->string module-name))))
     (module-img-plots module-name plot-file-path plot-file-dest module-config))
   )


(define (copy-module-plots module-name module-config output-dir)
   (let* ((version.path (update-module module-name module-config))
          (plot-file-path (cadr version.path))
          (plot-file-dest (make-pathname output-dir (->string module-name))))
     (copy-module-img-plots module-name plot-file-path plot-file-dest))
   )



(define (module-source-page module-name module-config module-manifest output-dir)
   (let* ((version.path (update-module module-name module-config))
          (source-file-dest (make-pathname output-dir (->string module-name))))
     (map (lambda (f) 
            (let ((fn (pathname-strip-directory f)))
              `((a (@ (name ,(->string module-name))))
                (p (ul (@ (id "module-menu")) 
                       (li ,(link "index.html" "Back to index"))
                       (li ,(link (sprintf "module-plot.html#~A" module-name)
                                  "Module plots"))
                       (li ,(link (sprintf "module-build-log.html#~A" module-name)
                                  "Build log"))
                       (li ,(link (sprintf "module-test-log.html#~A" module-name)
                                  "Test log"))
                       (li ,(link (sprintf "module-plot-log.html#~A" module-name)
                                  "Plot log"))
                       ))
                (p ,(sprintf "Sources for module ~A:" module-name))
                (p ,(link f "Download file"))
                (pre . ,(intersperse (read-lines f) "\n")))
              ))
          module-manifest)
     ))


(define (copy-module-sources module-name module-manifest module-dest)
  (for-each
   (lambda (f)
     (let ((fn (pathname-strip-directory f)))
       (run (cp ,f  ,(make-pathname module-dest fn)))))
   module-manifest))
   

(define (module-build-log-page module-name module-config)
   (let ((version.path (update-module  module-name module-config)))
     `((a (@ (name ,(->string module-name))))
       (p (ul (@ (id "module-menu")) 
              (li ,(link "index.html" "Back to index"))
              (li ,(link (sprintf "module-plot.html#~A" module-name)
                         "Module plots"))
              (li "Build log")
              (li ,(link (sprintf "module-test-log.html#~A" module-name)
                         "Test log"))
              (li ,(link (sprintf "module-plot-log.html#~A" module-name)
                         "Plot log"))
              ))
       (p ,(sprintf "Build log for module ~A:" module-name))
       (pre . ,(intersperse (read-lines (build-log-path module-name (car version.path))) "\n")))
     )
  )

   
(define (module-test-log-page module-name module-config)
  (let ((version.path (update-module module-name module-config)))
    `((a (@ (name ,(->string module-name))))
      (p (ul (@ (id "module-menu")) 
             (li ,(link "index.html" "Back to index"))
             (li ,(link (sprintf "module-plot.html#~A" module-name)
                        "Module plots"))
             (li ,(link (sprintf "module-build-log.html#~A" module-name)
                        "Build log"))
             (li  "Test log")
             (li ,(link (sprintf "module-plot-log.html#~A" module-name)
                        "Plot log"))
             ))
      (p ,(sprintf "Test log for module ~A:" module-name))
      (pre . ,(intersperse (read-lines (tests-log-path module-name (car version.path))) "\n")))
    )
  )

   
(define (module-plot-log-page module-name module-config)
  (let ((version.path (update-module  module-name module-config)))
    `((a (@ (name ,(->string module-name))))
      (p (ul (@ (id "module-menu")) 
             (li ,(link "index.html" "Back to index"))
             (li ,(link (sprintf "module-plot.html#~A" module-name)
                        "Module plots"))
             (li ,(link (sprintf "module-build-log.html#~A" module-name)
                        "Build log"))
             (li ,(link (sprintf "module-test-log.html#~A" module-name)
                        "Test log"))
             (li "Plot log")
             ))
      (p ,(sprintf "Plot log for module ~A:" module-name))
      (pre . ,(intersperse (read-lines (plots-log-path module-name (car version.path))) "\n")))
    )
  )




(define default-layout
  '((xhtml-1.0-strict)
    (html
     (head
      (link (@ (href "site.css") (rel "stylesheet") (type "text/css"))) 
      (title ,($ 'title)))
     (body
      (h1 ,($ 'title))
      (div (@ (id "content")) (inject ,contents))))))

(define site-css
#<<EOF
.even { background-color: #CCC; }
.odd { background-color: #FFFFCC; }

#module-menu li { display: inline; list-style-type: none; padding-right: 20px; }

table th, table td {
  text-align: left;
  padding-right: 1em;
}

#content h1 { font-size: 95%; }

EOF
)


(define (pad-number n zeroes)
  (define (pad num len)
    (let ((str (if (string? num) num (number->string num))))
      (if (string-null? str)
          ""
          (if (>= (string-length str) len)
              str
              (string-pad str len #\0)))))
  (let ((len (string-length (->string n))))
    (if (= len zeroes)
        (number->string n)
        (pad n zeroes))))


(define (generate-report)

  (let* ((now         (seconds->local-time))
         (day         (pad-number (vector-ref now 3) 2))
         (month       (pad-number (+ 1 (vector-ref now 4)) 2))
         (year        (number->string (+ 1900 (vector-ref now 5))))
         (today-dir   (make-pathname (list year month) day))
         (report-dir  (make-pathname (output-location-prefix) today-dir))
         (sxml-dir    (make-pathname report-dir "src"))
         (figures-dir (make-pathname sxml-dir "figures"))
         (html-dir    (make-pathname report-dir "out"))
         )

    (run (mkdir -p ,report-dir ,sxml-dir ,figures-dir))

    (let ((module-pages
           (fold 
           (lambda (kv ax)
             (let* (
                    (module-name (car kv))
                    (module-config (cdr kv))
                    (module-label (alist-ref 'label module-config))
                    )
               
               (let recur ((version.path (update-module module-name module-config))
                           (module-pages ax))
                 (cond
                  ((file-exists? (process-lock-path module-name (car version.path)))
                   (begin
                     (sleep 5)
                     (recur version.path module-pages)))
                  (else
                   (let* ((module-figures-dir (make-pathname figures-dir (->string module-name)))
                          (module-sources-dir (make-pathname figures-dir (->string module-name)))
                          (manifests (call-with-input-file* (manifest-path) read))
                          (module-manifest (alist-ref module-name manifests))
                          )
                     (run (mkdir -p ,module-figures-dir))
                     (copy-module-plots module-name module-config figures-dir)
                     (copy-module-sources module-name module-manifest sxml-dir)
                     (match-let (((source-page log-page plot-page build-log-page test-log-page plot-log-page ) 
                                  module-pages))
                                (list
                                 (cons (module-source-page module-name module-config module-manifest html-dir) source-page)
                                 (cons (module-log-page module-name module-config) log-page)
                                 (cons (module-plot-page module-name module-config figures-dir) plot-page)
                                 (cons (module-build-log-page module-name module-config) build-log-page)
                                 (cons (module-test-log-page module-name module-config) test-log-page)
                                 (cons (module-plot-log-page module-name module-config) plot-log-page)
                                 )
                                ))
                   )
                  ))
               ))
           '(() () () () () ())
           (modules)))
          )
    
      `((report-dir    . ,report-dir)
        (sxml-dir      . ,sxml-dir)
        (html-dir      . ,html-dir)
        (figures-dir   . ,figures-dir)
        (module-pages  . ,module-pages)
        )
      ))
  )


(define (output-report results)

  (let* (
         (report-dir    (alist-ref 'report-dir results))
         (sxml-dir      (alist-ref 'sxml-dir results))
         (html-dir      (alist-ref 'html-dir results))
         (module-pages  (alist-ref 'module-pages results))
         (css-dir       (make-pathname report-dir "src"))
         (layouts-dir   (make-pathname report-dir "layouts"))
         (manifests     (call-with-input-file* (manifest-path) read))

         (output-manifests (map (lambda (x)
                                    (let ((module-name (car x)))
                                      (let ((fn (if (null? (cdr (cdr x)))
                                                    (pathname-strip-directory (car (cdr x)))
                                                    (sprintf "~A.tgz" module-name))))
                                        (cons module-name (make-pathname html-dir fn)))
                                      ))
                                  manifests))
         )
    
    (run (mkdir -p ,css-dir ,layouts-dir ,html-dir )
         (touch ,(make-pathname report-dir "hyde.scm"))
         )
      
    (call-with-output-file
        (make-pathname css-dir "site.css")
      (lambda (out)
        (display site-css out)
        ))
      
    (call-with-output-file
        (make-pathname layouts-dir "default.sxml")
      (lambda (out)
        (pp '() out)
        (display "`" out) 
        (pp default-layout out)
        ))
    
    (call-with-output-file
        (make-pathname sxml-dir "index.sxml")
      (lambda (out)
        (pp `((title "Testdrive module list")) out)
        (display "'" out) 
        (pp (modules-page (modules) output-manifests) out)
        ))
      
    (match-let (((source-page log-page plot-page build-log-page test-log-page plot-log-page )
                 module-pages))
             ;; output module pages
                 
             (call-with-output-file
                 (make-pathname sxml-dir "module-log.sxml")
               (lambda (out)
                 (pp `((title "Testdrive module logs")) out)
                 (display "'" out) 
                 (pp log-page out)
                 ))
             
             (call-with-output-file
                 (make-pathname sxml-dir "module-plot.sxml")
               (lambda (out)
                 (pp `((title "Testdrive module plots")) out)
                 (display "'" out) 
                 (pp plot-page out)
                 ))
             
             (call-with-output-file
                 (make-pathname sxml-dir "module-build-log.sxml")
               (lambda (out)
                 (pp `((title "Testdrive module build logs")) out)
                 (display "'" out) 
                 (pp build-log-page out)
                 ))
             
             (call-with-output-file
                 (make-pathname sxml-dir "module-test-log.sxml")
               (lambda (out)
                 (pp `((title "Testdrive module test logs")) out)
                 (display "'" out) 
                 (pp test-log-page out)
                 ))
             
             (call-with-output-file
                 (make-pathname sxml-dir "module-plot-log.sxml")
               (lambda (out)
                 (pp `((title "Testdrive module plot logs")) out)
                 (display "'" out) 
                 (pp plot-log-page out)
                 ))
             
             )
  ))

(define (main args)

  (let ((config-path
         (let ((path (get-environment-variable "TESTDRIVE_CONFIG")))
           (or path (and (pair? args) (car args))))))

    (load config-path)

    (let ((results (generate-report)))

      (output-report results)

      )
    ))


(main (command-line-arguments))
