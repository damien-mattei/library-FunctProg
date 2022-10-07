;; debug Macros and Functions
;; 
       


(define debug-mode #f)

(define debug-mode-save debug-mode)

(define debug-mode-save-lst '(#f))


;; save the debug mode and set it true
(define (debug-mode-on)
  (when (not debug-mode)
    ;;(display "debug-mode-on : ")
    (insert debug-mode debug-mode-save-lst)
    (set! debug-mode-save debug-mode)
    (set! debug-mode #t)))



(define (debug-mode-restore)
  (when (not (null? debug-mode-save-lst))
    (set! debug-mode (first debug-mode-save-lst))
    (set! debug-mode-save-lst (rest debug-mode-save-lst))))


(define (debug-mode-off)
  (when debug-mode
    ;;(display "debug-mode-on : ")
    (insert debug-mode debug-mode-save-lst)
    (set! debug-mode-save debug-mode)
    (set! debug-mode #f)))



;; (debug block) or (nodebug block)

(define-syntax debug
  (syntax-rules ()
    
    ((_ instruction)
     instruction)
    
    ((_ instruction ...)
     (let ()
       instruction
       ...))))


(define-syntax nodebug
  (syntax-rules ()

    ((_ instruction ...)
     '())
    
    ((_ instruction)
     '())))



;; display only if debug mode on for the functions below

(define-syntax debug-display
  (syntax-rules ()
    ((_ obj) (if debug-mode (display obj)))
    ((_ obj port) (if debug-mode (display obj port)))))


;; an enhanced newline that will only "new line" in debug mode LOL
(define-syntax debug-newline
  (syntax-rules ()
    ((_) (if debug-mode (newline)))))


(define-syntax debug-display-var-nl 
  (syntax-rules ()
    ((_ msg var) (if debug-mode (display-var-nl msg var)))))


;; debug with display-nl
(define-syntax debug-display-nl 
  (syntax-rules ()
    ((_ msg) (begin
	       ;;(display "debug.scm : debug-display-nl : debug-mode = ") ;; this has been added for debugging the debug macros !!! (and should be removed later)
	       ;;(display debug-mode)
	       ;;(newline)
	       (if debug-mode
		  (begin (display msg) 
			 (newline)))))))


;; continue with  display-msg-symb-nl
;; macros or function to display a variable with a message and add a newline
(define-syntax debug-display-msg-symb-nl 
  (syntax-rules ()
    ((_ msg symbl) (if debug-mode  
		       (begin
			 (display msg)
			 (display " ")
			 (display (symbol->string (quote symbl)))
			 (display " = ")
			 (display symbl)
			 (newline))))))


;; tired of rewriting debug macros starting from normal ones i wrote this one that encapsulate ANY one !!!
;; example(s):
;; (debug-only display-msg-var-nl "Sidonie : DBtoWebObserversKawa : work : (car wds-data-str-split) = " (car wds-data-str-split))
(define-syntax debug-only
  (syntax-rules ()

    ((_ fct-or-mac ...)
     ;;(begin
       ;;(display "debug.scm : debug-only : debug-mode = ") ;; this has been added for debugging the debug macros !!! (and should be removed later)
       ;;(display debug-mode)
       ;;(newline)
       (when debug-mode
	     (fct-or-mac ...)));)
    
    ((_ instruction)
     ;;(begin
       ;;(display "debug.scm : debug-only : debug-mode = ") ;; this has been added for debugging the debug macros !!! (and should be removed later)
       ;;(display debug-mode)
       ;;(newline)
       (when debug-mode
	     (instruction)))));)



;; debug or not a region
(define-syntax debug-region
  (syntax-rules ()
    
    ((_ instructions ...)
     (begin
       (debug-mode-on)
       (define rv (let ()
		    instructions
		    ...))
       (debug-mode-restore)
       (dv rv)
       rv))))


    
(define-syntax no-debug-region
  (syntax-rules ()
    
    ((_ instructions ...)
     (begin
       (debug-mode-off)
       (define rv (let ()
		    instructions
		    ...))
       (debug-mode-restore)
       rv))))



;; debug or not a region
(define-syntax debug-region-name
  (syntax-rules ()
    
    ((_ name instructions ...)
     (begin
       (debug-mode-on)
       (define rv (let ()
		    instructions
		    ...))
       (debug-mode-restore)
       (display-nl name)
       (dv rv)
       rv))))


    
(define-syntax no-debug-region-name
  (syntax-rules ()
    
    ((_ name instructions ...)
     (begin
       (debug-mode-off)
       (define rv (let ()
		    instructions
		    ...))
       (debug-mode-restore)
       rv))))
