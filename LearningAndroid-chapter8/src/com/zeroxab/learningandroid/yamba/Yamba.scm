;;; FIXME
;;; I don't know how to use require, so collections_utils.scm gets to
;;; be included here

;;; Returns a list of the elements of the given Enumeration.
(define (Enumeration->list (enum :: java.util.Enumeration)) :: list
  (let loop ((ls '()))
    (if (enum:hasMoreElements)
        (loop (cons (enum:nextElement) ls))
        (reverse ls))))

;;; Returns a list of the elements of the given Iterator.
(define (Iterator->list (iter :: java.util.Iterator)) :: list
  (let loop ((ls '()))
    (if (iter:hasNext)
        (loop (cons (iter:next) ls))
        (reverse ls))))

;;; Returns a list of the elements of the given Iterable.
(define (Iterable->list (it :: java.lang.Iterable)) :: list
  (Iterator->list (it:iterator)))

;;; Returns a list of the elements of the given array.
(define (Array->list (array :: Object)) :: list
  (when (*:isArray (array:getClass))
        (let* ((n :: int (java.lang.reflect.Array:getLength array))
               (result-vector :: vector (make-vector n)))
          (do ((i 0 (+ i 1)))
              ((= i n) (vector->list result-vector))
            (vector-set! result-vector i (array i))))))

;;; Converts the given object into a list.
(define ->list
  (case-lambda
   ((l :: list) l)
   ((l :: java.lang.Iterable) (Iterable->list l))
   ((l :: java.util.Iterator) (Iterator->list l))
   ((l :: java.util.Enumeration) (Enumeration->list l))
   ((l :: Object) (Array->list l))))

;;; Map for an Enumeration
(define (map-enumeration (f :: procedure)
                         (enum :: java.util.Enumeration))
  :: list
  (let loop ((ls '()))
    (if (enum:hasMoreElements)
        (loop (cons (f (enum:nextElement)) ls))
        (reverse ls))))

;;; Map for an Iterator
(define (map-iterator (f :: procedure)
                      (iter :: java.util.Iterator))
  :: list
  (let loop ((ls '()))
    (if (iter:hasNext)
        (loop (cons (f (iter:next)) ls))
        (reverse ls))))

;;; Map for an Iterable
(define (map-iterable (f :: procedure)
                      (it :: java.lang.Iterable))
  :: list
  (map-iterator f (it:iterator)))

;;; Map for an Array
(define (map-array (f :: procedure)
                   (obj :: Object))
  :: list
  (when (*:isArray (obj:getClass))
        (let* ((n :: int (java.lang.reflect.Array:getLength obj))
               (result-vector :: vector (make-vector n)))
          (do ((i 0 (+ i 1)))
              ((= i n) (vector->list result-vector))
            (vector-set! result-vector i (f (obj i)))))))

;;; For-each for an Enumeration
(define (for-each-enumeration (f :: procedure)
                              (enum :: java.util.Enumeration))
  (let loop ()
    (when (enum:hasMoreElements)
          (f (enum:nextElement))
          (loop))))

;;; For-each for an Iterator
(define (for-each-iterator (f :: procedure)
                           (iter :: java.util.Iterator))
  (let loop ()
    (when (iter:hasNext)
          (f (iter:next))
          (loop))))

;;; For-each for an Iterable
(define (for-each-iterable (f :: procedure)
                           (it :: java.lang.Iterable))
  (for-each-iterator f (it:iterator)))

;;; For-each for an Array
(define (for-each-array (f :: procedure)
                        (obj :: Object))
  (when (*:isArray (obj:getClass))
        (let ((n :: int (java.lang.reflect.Array:getLength obj)))
          (do ((i 0 (+ i 1)))
              ((= i n))
            (f (obj i))))))

;;; A version of map which works with lists, Enumerations, Iterators,
;;; Iterables, and Arrays.
(define better-map
  (case-lambda
   (((f :: procedure) (l :: list))
    (map f l))
   (((f :: procedure) (l :: java.util.Enumeration))
    (map-enumeration f l))
   (((f :: procedure) (l :: java.util.Iterator))
    (map-iterator f l))
   (((f :: procedure) (l :: java.lang.Iterable))
    (map-iterable f l))
   (((f :: procedure) (l :: Object))
    (map-array f l))
   (((f :: procedure) (l1 :: Object) (l2 :: Object) . rest)
    (apply map f (append (map ->list `(,l1 ,l2))
                         (map ->list rest))))))

;;; A version of for-each which works with lists, Enumerations,
;;; Iterators, Iterables, and Arrays.
(define better-for-each
  (case-lambda
   (((f :: procedure) (l :: list))
    (for-each f l))
   (((f :: procedure) (l :: java.util.Enumeration))
    (for-each-enumeration f l))
   (((f :: procedure) (l :: java.util.Iterator))
    (for-each-iterator f l))
   (((f :: procedure) (l :: java.lang.Iterable))
    (for-each-iterable f l))
   (((f :: procedure) (l :: Object))
    (for-each-array f l))
   (((f :: procedure) (l1 :: Object) (l2 :: Object) . rest)
    (apply for-each f (append (map ->list `(,l1 ,l2))
                              (map ->list rest))))))


;;;;;;

(require 'android-defs)
(require 'srfi-1)
(require 'syntax-utils)

(define-namespace Log "class:android.util.Log")

;; from kawa/testsuite/classes1.scm
(define-syntax (import-class form)
 (syntax-case form ()
  ((import-class fqcn)
   (let* ((cls :: java.lang.Class (eval (syntax fqcn)))
	  (name (string->symbol (java.lang.Class:getSimpleName cls))))
    #`(define-alias #,(datum->syntax-object form name) fqcn)))))

(define-alias Arrays  java.util.Arrays)
(define-alias Math java.lang.Math)
(define-alias InterruptedException java.lang.InterruptedException)
(define-alias Thread java.lang.Thread)

(define-alias Application   android.app.Application)
(define-alias Activity      android.app.Activity)
(define-alias Service       android.app.Service)
(define-alias Bitmap   android.graphics.Bitmap)
(define-alias Canvas   android.graphics.Canvas)
(define-alias Color    android.graphics.Color)
(define-alias Drawable android.graphics.drawable.Drawable)
(define-alias Resources            android.content.res.Resources)
(define-alias Context              android.content.Context)
(define-alias Intent               android.content.Intent)
(define-alias SharedPreferences    android.content.SharedPreferences)
(define-alias Handler   android.os.Handler)
(define-alias Bundle    android.os.Bundle)
(define-alias IBinder   android.os.IBinder)
(define-alias KeyEvent         android.view.KeyEvent)
(define-alias OnClickListener  android.view.View$OnClickListener)
(define-alias SurfaceHolder    android.view.SurfaceHolder)
(define-alias Menu             android.view.Menu)
(define-alias MenuItem         android.view.MenuItem)
(define-alias Toast       android.widget.Toast)
(define-alias Editable    android.text.Editable)
(define-alias TextWatcher android.text.TextWatcher)
(define-alias PreferenceActivity android.preference.PreferenceActivity)
(define-alias PreferenceManager  android.preference.PreferenceManager)

(define-alias FVector  gnu.lists.FVector)

(define-alias R com.zeroxab.learningandroid.yamba.R)

(define-alias Twitter winterwell.jtwitter.Twitter)

(define-syntax async-task
 (lambda (x)
  ;; TODO should add in the ability to bind (this) at the callsite to
  ;; (this-parent) so that I don't need an ugly let
  (syntax-case x ()
   ((async-task paramsT progressT resultT . r)
    (letrec
      ((process
	(lambda (form)
	 (syntax-case form (background cancel post pre progress publish-progress)
	  (((background name stmt ...) . rest)
	   ;; FIXME using object here instead of object1 seems to
	   ;; change the object macro at the toplevel in the
	   ;; interpreter, this looks like a bug
	   (cons #`((doInBackground (object1 ::Object[])) (@java.lang.Override) ::Object
		    (as resultT (let ((name (gnu.lists.FVector[paramsT] object1))) stmt ...)))
		 (process #`rest)))
	  (((cancel name stmt ...) . rest)
	   (cons #`((onCancelled (object1 ::Object)) (@java.lang.Override) ::void
		    (let ((name (as resultT object1))) stmt ...))
		 (process #`rest)))
	  (((post name stmt ...) . rest)
	   (cons #`((onPostExecute (object1 ::Object)) (@java.lang.Override) ::void
		    (let ((name (as resultT object1))) stmt ...))
		 (process #`rest)))
	  (((pre stmt ...) . rest)
	   (cons #`(onPreExecute (@java.lang.Override) ::void stmt ...)
		 (process #`rest)))
	  (((progress stmt ...) . rest)
	   (cons #`((onProgressUpdate (object1 ::Object[])) (@java.lang.Override) ::void
		    (let ((name (gnu.lists.FVector[progressT] object1)))
		     stmt ...))
		 (process #`rest)))
	  (((publish-progress stmt ...) . rest)
	   (cons #`((publishProgress (object1 ::Object[])) (@java.lang.Override) ::void
		    (let ((name (gnu.lists.FVector[progressT] object1)))
		     stmt ...))
		 (process #`rest)))
	  (() '())))))
     #`(object (android.os.AsyncTask) #,@(process #`r)))))))

(define-syntax simple-thread
 (lambda (x)
  ;; TODO should add in the ability to bind (this) at the callsite to
  ;; (this-parent) so that I don't need an ugly let
  (syntax-case x ()
   ((simple-thread stmts ...)
    #`(as Thread (object (Thread) ((run) ::void
				   stmts ...)))))))

(define-simple-class PrefActivity (PreferenceActivity)
 ((onCreate (savedInstanceState ::Bundle))
  (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)
  (addPreferencesFromResource R$xml:prefs)))

(define-simple-class YambaApplication (Application)
 (twitter          ::Twitter)
 (preferences      ::SharedPreferences)
 (service-running? ::boolean)
 ((onCreate)
  (invoke-special android.app.Application (this) 'onCreate)
  (set! preferences (PreferenceManager:getDefaultSharedPreferences (this)))
  (preferences:registerOnSharedPreferenceChangeListener
   (lambda ((preferences ::SharedPreferences) (key ::String))
    (set! twitter #!null))))
 ((onTerminate)
  (invoke-special android.app.Application (this) 'onTerminate))
 ((get-twitter) ::Twitter
  (synchronized (this)
		(when (equal? twitter #!null)
		 (set! twitter (Twitter:new (preferences:getString "username"  "student")
					    (preferences:getString "password"  "password")))
		 (twitter:setAPIRootUrl (preferences:getString "apiRoot" "http://yamba.marakana.com/api"))))
  twitter))

(define-simple-class StatusActivity (Activity <android.text.TextWatcher>)
 (edit-text     ::EditText)
 (update-button ::Button)
 (text-count    ::TextView)

 ((onCreate (savedInstanceState ::Bundle))
  (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)
  (setContentView R$layout:status)
  (set! edit-text (findViewById R$id:editText))
  (set! update-button (findViewById R$id:buttonUpdate))
  (update-button:setOnClickListener
   (lambda ((view ::View))
    ;; FIXME this is nasty
    (let* ((activity-this (this)))
     (*:execute
      (async-task
       String int String
       (background statuses
		   (try-catch
		    (begin (((as YambaApplication (getApplication)):get-twitter):setStatus (statuses 0))
			   (statuses 0))
		    (e winterwell.jtwitter.TwitterException
		       (Log:e "Yamba-StatusActivity" "Failed to set twitter status!")
		       (e:printStackTrace)
		       "Ooops, failed to post status!")))
       (progress values #!null)
       (post result ((Toast:makeText activity-this (as <String> result) Toast:LENGTH_LONG):show)))
      ((edit-text:getText):toString))
     (Log:d "Yamba-StatusActivity" "Clicked!"))))
  (set! text-count (findViewById R$id:textCount))
  (text-count:setText (140:toString))
  (text-count:setTextColor Color:GREEN)
  (edit-text:addTextChangedListener (this)))

 ((afterTextChanged (status-text ::Editable))
  (let ((count (- 140 (status-text:length))))
   (text-count:setText (format #f "~a" count))
   (text-count:setTextColor (cond ((< count 0) Color:RED)
				  ((< count 10) Color:YELLOW)
				  (else Color:GREEN)))))
 ((beforeTextChanged sequence start count after) #f)
 ((onTextChanged sequence start before count) #f)

 ((onCreateOptionsMenu (menu ::Menu))
  ((getMenuInflater):inflate R$menu:menu menu)
  #t)
 ((onOptionsItemSelected (item ::MenuItem))
  (cond ((equal? (item:getItemId) R$id:itemPrefs)
	 (startActivity (make <android.content.Intent> (this) PrefActivity)))
	((equal? (item:getItemId) R$id:itemServiceStart)
	 (startService (make <android.content.Intent> (this) UpdaterService)))
	((equal? (item:getItemId) R$id:itemServiceStop)
	 (stopService (make <android.content.Intent> (this) UpdaterService)))
	(else #f))
  #t))

(define update-delay 10000) ;; 10 seconds

(define-simple-class UpdaterService (Service)
 (run-flag ::boolean)
 (updater ::Thread)
 ((onBind (intent ::Intent)) #!null)
 ((onCreate) (invoke-special android.app.Service (this) 'onCreate)
  (Log:d "Yamba-UpdaterService" "Created")
  (set! updater
	(simple-thread
	 (let loop ()
	  (when run-flag
	   (Log:d "Yamba-UpdaterService" "Running")
	   (try-catch (begin (Log:d "Yamba-UpdaterService" "Updated Ran")
			     (better-for-each (lambda (status) (Log:d "Yamba-UpdaterService" status))
					      (((as YambaApplication (getApplication)):get-twitter):getFriendsTimeline)))
		      (e java.lang.InterruptedException
			 (Log:e "Yamba-UpdaterService" "Failed!")
			 (set! run-flag #f)))
	   (Thread:sleep update-delay)
	   (loop)))))
  (set! run-flag #f))
 ((onStartCommand (intent ::Intent) (flags ::int) (startId ::int))
  (invoke-special android.app.Service (this) 'onStartCommand intent flags startId)
  (set! (as YambaApplication (getApplication)):service-running? #t)
  (*:start updater)
  (set! run-flag #t)
  (Log:d "Yamba-UpdaterService" "Started")
  Service:START_STICKY)
 ((onDestroy)
  (set! run-flag #f)
  (set! (as YambaApplication (getApplication)):service-running? #f)
  (*:interrupt updater)
  (Log:d "Yamba-UpdaterService" "Destroyed")))
