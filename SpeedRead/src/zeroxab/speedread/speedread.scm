(require 'android-defs)
(require 'srfi-1)
(import (except (srfi :13 strings)
                string-hash))
(require 'regex)
(require 'syntax-utils)
(require 'srfi-69)

;; TODO Unicode " and --
;; TODO Size of punctuation is too small
;; TODO Long words are too slow, short words are a bit too fast
;; TODO Maybe color each letter
;; TODO Numbers are too fast

(define-alias Arrays  java.util.Arrays)
(define-alias Math java.lang.Math)
(define-alias InterruptedException java.lang.InterruptedException)
(define-alias Thread java.lang.Thread)
(define-alias Timer java.util.Timer)
(define-alias TimerTask java.util.TimerTask)
(define-alias Runnable java.lang.Runnable)
(define-alias BufferedReader java.io.BufferedReader)
(define-alias StringBuilder java.lang.StringBuilder)

(define-alias ScheduledThreadPoolExecutor java.util.concurrent.ScheduledThreadPoolExecutor)

(define-alias Application		android.app.Application)
(define-alias Activity			android.app.Activity)
(define-alias Service			android.app.Service)

(define-alias Bitmap			android.graphics.Bitmap)
(define-alias Canvas			android.graphics.Canvas)
(define-alias Color			android.graphics.Color)
(define-alias Drawable			android.graphics.drawable.Drawable)

(define-alias Context			android.content.Context)
(define-alias Intent                    android.content.Intent)
(define-alias Resources			android.content.res.Resources)
(define-alias SharedPreferences		android.content.SharedPreferences)
(define-alias ContentValues		android.content.ContentValues)

(define-alias Handler			android.os.Handler)
(define-alias Bundle			android.os.Bundle)
(define-alias IBinder			android.os.IBinder)
(define-alias PowerManager              android.os.PowerManager)
(define-alias WakeLock                  android.os.PowerManager$WakeLock)
(define-alias Parcelable                android.os.Parcelable)

(define-alias KeyEvent			android.view.KeyEvent)
(define-alias OnClickListener		android.view.View$OnClickListener)
(define-alias SurfaceHolder		android.view.SurfaceHolder)
(define-alias Menu			android.view.Menu)
(define-alias MenuItem			android.view.MenuItem)
(define-alias MotionEvent               android.view.MotionEvent)
(define-alias OnGestureListener         android.view.GestureDetector$OnGestureListener)
(define-alias GestureDetector           android.view.GestureDetector)

(define-alias ListView			android.widget.ListView)
(define-alias SimpleCursorAdapter       android.widget.SimpleCursorAdapter)
(define-alias Toast			android.widget.Toast)
(define-alias ViewBinder                android.widget.SimpleCursorAdapter$ViewBinder)
(define-alias ArrayAdapter              android.widget.ArrayAdapter)
(define-alias SimpleAdapter             android.widget.SimpleAdapter)
(define-alias OnItemClickListener       android.widget.AdapterView$OnItemClickListener)
(define-alias ProgressBar               android.widget.ProgressBar)
(define-alias SeekBar                   android.widget.SeekBar)
(define-alias OnSeekBarChangeListener   android.widget.SeekBar$OnSeekBarChangeListener)

(define-alias DateUtils 		android.text.format.DateUtils)
(define-alias Editable			android.text.Editable)
(define-alias TextWatcher		android.text.TextWatcher)
(define-alias Spannable		        android.text.Spannable)
(define-alias SpannableString		android.text.SpannableString)
(define-alias BackgroundColorSpan	android.text.style.BackgroundColorSpan)
(define-alias StyleSpan         	android.text.style.StyleSpan)

(define-alias PreferenceActivity	android.preference.PreferenceActivity)
(define-alias PreferenceManager		android.preference.PreferenceManager)
(define-alias BaseColumns		android.provider.BaseColumns)

(define-alias HttpGet                   org.apache.http.client.methods.HttpGet)
(define-alias BasicResponseHandler      org.apache.http.impl.client.BasicResponseHandler)
(define-alias DefaultHttpClient         org.apache.http.impl.client.DefaultHttpClient)
(define-alias HttpClient                org.apache.http.client.HttpClient)

(define-alias Uri                       android.net.Uri)

(define-alias TypedValue                android.util.TypedValue)

(define-alias FVector  gnu.lists.FVector)

(define-alias R zeroxab.speedread.R)

(define-namespace Log "class:android.util.Log")

;;; Manage tasks

(define-syntax (async-task stx)
 ;; TODO should add in the ability to bind (this) at the callsite to
 ;; (this-parent) so that I don't need an ugly let
 (syntax-case stx ()
  ((_ paramsT progressT resultT . r)
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
    #`(object (android.os.AsyncTask) #,@(process #`r))))))

(define-syntax (with-that stx)
 (syntax-case stx ()
  ((_ stmts ...) (with-syntax ((that (datum->syntax stx 'that)))
                  #'(let ((that (this))) stmts ...)))))

(define-syntax (thread x)
 (syntax-case x ()
  ((_ stmts ...)
   #`(with-that (as Thread (object (Thread) ((run) ::void stmts ...)))))))

(define-syntax (runnable x)
 (syntax-case x ()
   ((_ stmts ...)
    #`(with-that (as Runnable (object (Runnable) ((run) ::void stmts ...)))))))

(define-syntax (timer-task x)
 (syntax-case x ()
  ((_ stmts ...)
   #`(with-that (as TimerTask (object (TimerTask) ((run) ::void stmts ...)))))))

;;; Listeners

(define-syntax (on-item-click stx)
 (syntax-case stx ()
  ((_ obj stmts ...)
   (with-syntax ((adapter (datum->syntax stx 'adapter))
                 (view (datum->syntax stx 'view))
                 (position (datum->syntax stx 'position))
                 (id (datum->syntax stx 'id)))
    #`(obj:setOnItemClickListener
       (with-that
        (as OnItemClickListener
            (object (OnItemClickListener)
                    ((onItemClick adapter view position id) ::void
                     stmts ...)))))))))

(define-syntax (on-user-seek-bar-change stx)
 (syntax-case stx ()
  ((_ obj stmts ...)
   (with-syntax ((bar (datum->syntax stx 'bar))
                 (position (datum->syntax stx 'position))
                 (from-user (datum->syntax stx 'from-user)))
    #`(obj:setOnSeekBarChangeListener
       (with-that
        (as OnSeekBarChangeListener
            (object (OnSeekBarChangeListener)
                    ((onProgressChanged seek-bar position from-user) ::void
                     (when from-user stmts ...))
                    ((onStartTrackingTouch seek-bar) ::void #f)
                    ((onStopTrackingTouch seek-bar) ::void #f)))))))))

;;; Start activities

(define-syntax (simple-start-activity stx)
 (syntax-case stx ()
  ((_  obj activity arguments)
   #`(let ((intent ::android.content.Intent (make <android.content.Intent> obj activity)))
      ;; this may cause runtime errors if you're trying to put an
      ;; object that isn't supported by putExtra
      (with-compile-options warn-invoke-unknown-method: #f
                            (for-each (lambda (a) (intent:putExtra (car a) (cdr a))) arguments))
      (obj:startActivity intent)))))

;;; Basic file chooser using an external file manager

;; FIXME This doesn't work for some reason, so we need to initialize it manually
;; FIXME (string-hash "*file-chooser-request*") ends up not delivering requests
(define *file-chooser-request* 33)
(define (file-chooser (obj ::Activity))
 ;; TODO, why doesn't Intent work?
 (let ((intent <android.content.Intent>
               (make <android.content.Intent>
                (as String android.content.Intent:ACTION_GET_CONTENT))))
  (intent:setType "*/*")
  (intent:addCategory android.content.Intent:CATEGORY_OPENABLE)
  (try-catch
   (obj:startActivityForResult (intent:createChooser intent "Select a book to read") *file-chooser-request*)
   (ex <android.content.ActivityNotFoundException>
       ((Toast:makeText obj "No file manager, install one from the play store" Toast:LENGTH_SHORT):show)))))
(define (file-chooser-result f)
 (lambda ((obj ::Activity) (request ::int) (result ::int) (data ::Intent))
  (if (and (= request *file-chooser-request*) (= result obj:RESULT_OK))
      (begin (let ((uri ::Uri (data:getData)))
              (Log:i "speedread" (format #f "got back URI ~a ~a" (uri:toString) (uri:getPath)))
              (f uri))
             #t)
      #f)))

;;; Manage activity results

(define (multiple-activity-results (obj ::Activity) . fs)
 (lambda ((request ::int) (result ::int) (data ::Intent))
  (Log:d "multiple-activity-results" (format #f "~a ~a ~a" request result data))
  (let loop ((fs fs))
   (if (null? fs)
       #f
       (let ((r ((car fs) obj request result data)))
        (if r #t (loop (cdr fs))))))))

(define-simple-class SpeedReadApplication (Application)
 ((onCreate)
  (invoke-special android.app.Application (this) 'onCreate))
 ((onTerminate)
  (invoke-special android.app.Application (this) 'onTerminate)))

(define (next-word-indices (text ::String) offset)
 (car (regex-match-positions
       "((--|!|\\(|\\)|\\{|\\}|\\[|\\]|;|,|\\?|&|\\!|\\\"|”|“|‘|’|–|—|‒|―|\\.|\\:)|[^\\s]+?(?=(\\s|--|!|\\(|\\)|\\{|\\}|\\[|\\]|;|,|\\?|\\!|\\\"|”|“|‘|’|–|—|‒|―|\\.|\\:)))"
       text offset)))

(define (word-at (text ::String) offset)
 (let ((l (next-word-indices text offset)))
  (regex-replace "\\s" (text:substring (car l) (cdr l)) "")))

(define *wpm* #f)
(define *delay:start* 200)
(define *delay:word* #f)
(define *base-size* 8)

(define *comma-multiplier* 1.5)
(define *base-font-size* 2)
(define *bracket-multiplier* 1.5)
(define *number-multiplier* 4)
(define *stop/quote-multiplier* 1.5)
(define *unit-multiplier* 1)
(define *capitalized-multiplier* 2)
(define *length-exponent* 1.4)
(define *length-divisor* 15)
(define *length-bound* 8)

(define (capitalized? str)
 (and (> (string-length str) 0)
      (char-upper-case? (string-ref str 0))))

(define (word-delay str)
 (let* ((n (string-length str)))
  (inexact->exact
   (round (* (cond ((string->number str) (* *number-multiplier* *delay:word*))
		   ((member string '("(" ")" "{" "}" "[" "]"))
		    (* *bracket-multiplier* *delay:word*))
		   ((member string '(";" "&" "–" "—" "‒" "―" "--" ",")) (* *comma-multiplier* *delay:word*))
		   ((member string '( "?" "!" "\"" "”" "“" "‘" "’" "."))
		    (* *stop/quote-multiplier* *delay:word*))
		   ((= n 1) (* *unit-multiplier* *delay:word*))
		   (else (+ *delay:word*
			    (expt
			     (max 0 (* (* (/ *delay:word* *length-divisor*)
					  (- n *length-bound*))))
			     *length-exponent*))))
	     (if (capitalized? str) *capitalized-multiplier* 1))))))

(define (word-color string default)
 (cond ((member string '(";" "&" "–" "—" "‒" "―" "--" ",")) (Color:parseColor "#3380dE"))
       ((member string '("." "?" "\"" "”" "“" "‘" "’" "!")) (Color:parseColor "#DB1A1A"))
       ((member string '("(" "{" "[")) (Color:parseColor "#24a700"))
       ((member string '(")" "}" "]")) (Color:parseColor "#A10199"))
       (else default)))

(define (text-size string)
 (* (cond ((member string '("." "\"" ";" ","  "”" "“" "‘" "’")) 3)
          ((member string '("–" "—" "‒" "―" "--")) 2)
          ((member string '("&" "(" "{" "[" ")" "}" "]" "?" "!")) 1.5)
          (else 1))
    *base-size*))

(define-simple-class player (android.app.Activity android.view.GestureDetector$OnGestureListener)
 (speedread ::SpeedReadApplication)
 (timer-tick ::Runnable
             init: (runnable
                    (let ((text-view ::TextView (that:findViewById R$id:Viewer))
                          (word (word-at text offset)))
                     (when running? (timer:schedule (timer-task-thunk) (word-delay word)))
                     (text-view:setText (format #f "~a" word))
                     (text-view:setTextColor (as int (word-color word default-text-color)))
                     (text-view:setTextSize TypedValue:COMPLEX_UNIT_MM (text-size word))
                     (update-offset (cdr (next-word-indices text offset))))))
 ((timer-task-thunk) (timer-task (that:runOnUiThread timer-tick)))
 (timer ::Timer init: #!null)
 (text ::String)
 (offset ::int)
 (running? ::boolean init-value: #f)
 (wakelock ::WakeLock)
 (gesture-detector ::GestureDetector)
 (book-uri ::Uri)
 (progress-bar ::SeekBar)
 (default-text-color ::int)
 ((new-wakelock tag)
  (let ((power-manager ::PowerManager ((this):getSystemService POWER_SERVICE)))
   (power-manager:newWakeLock power-manager:SCREEN_BRIGHT_WAKE_LOCK tag)))
 ((onCreate (savedInstanceState ::Bundle))
  (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)
  (set! speedread (as SpeedReadApplication (getApplication)))
  (set! gesture-detector (make <android.view.GestureDetector> (this)))
  ((this):setContentView R$layout:player)
  (set! wakelock (new-wakelock "zeroxab.speedread.player_wake_lock_tag"))
  (set! progress-bar (findViewById R$id:ViewerLocation))
  (show-viewer)
  (set! *delay:word* (inexact->exact (round (/ (* 60000) *wpm*))))
  (set! default-text-color
        (Color:parseColor "#000000")
        ;; TODO Would be nice to get this from the theme
        ;; ((as TextView ((this):findViewById R$id:Viewer)):getTextColor)
        )
  (set! running? #t)
  (on-user-seek-bar-change progress-bar (update-offset position))
  (let* ((uri (as Uri (((getIntent):getExtras):getParcelable "book")))
         (previous-offset ::int (((getIntent):getExtras):getInt "offset"))
         (scheme (uri:getScheme)))
   (set! book-uri uri)
   (cond ((or (equal? scheme "http") (equal? scheme "https"))
          (with-that
           (*:execute
            (async-task
             String int String
             (background
              urls
              (let ((http-get ::HttpGet (make <org.apache.http.client.methods.HttpGet> (as String (vector-ref urls 0))))
                    (http-client ::HttpClient (make <org.apache.http.impl.client.DefaultHttpClient>))
                    (response-handler ::BasicResponseHandler (make <org.apache.http.impl.client.BasicResponseHandler>)))
               (http-client:execute http-get response-handler)))
             (progress values #!null)
             (post result (initialize-viewer result previous-offset)))
            (uri:toString))))
         ((equal? scheme "file")
          (with-that
           (*:execute
            (async-task
             String int String
             (background
              files
              (let* ((reader ::BufferedReader (make <java.io.BufferedReader>
                                               (make <java.io.FileReader>
                                                (as String (vector-ref files 0)))))
                     (sb (make <java.lang.StringBuilder>)))
               (let loop ()
                (let ((line (reader:readLine)))
                 (if (equal? line #!null)
                     (begin (reader:close) (sb:toString))
                     (begin (sb:append line) (if (equal? line "")
                                                 (sb:append "\n")
                                                 (sb:append " "))
                            (loop)))))))
             (progress values #!null)
             (post result (initialize-viewer result previous-offset)))
            (uri:getPath))))
         (else (error (format #f "Unknown scheme ~a for URI ~a" scheme (uri:toString)))))))
 ((initialize-viewer result previous-offset)
  (Log:i "speedread" (format #f "text ~a" (string-length result)))
  (set! text result)
  (set! timer (make <java.util.Timer>))
  (progress-bar:setMax (string-length result))
  (update-offset previous-offset)
  (show-overview))
 ((onDestroy) (invoke-special Activity (this) 'onDestroy))
 ((onResume) (invoke-special Activity (this) 'onResume)
  (stop-timer)
  (wakelock:acquire))
 ((onPause) (invoke-special Activity (this) 'onPause)
  (stop-timer)
  (wakelock:release)
  (let ((prefs ((getSharedPreferences *preferences:id* 0):edit))
        (uri-string (book-uri:toString)))
   (prefs:putString
    "book-list"
    (format #f "~s"
            (cons
             `(("title" . ,(book-uri:getLastPathSegment))
               ("uri" . ,uri-string)
               ("offset" . ,offset)
               ("length" . ,(string-length text))
               ("progress" . ,(inexact->exact (round (* 100 (/ offset (string-length text)))))))
             (remove (lambda (book) (equal? (cdr (assoc "uri" book)) uri-string))
                     (call-with-input-string
                       ((getSharedPreferences *preferences:id* 0):getString
                        "book-list" "()") read)))))
   (unless (prefs:commit)
    (error "Failed to add the book to the book-list"))))
 ((start-timer)
  (when (and (not running?) (not (equal? timer #!null)))
   (set! running? #t)
   (timer:schedule (timer-task-thunk) *delay:start*)
   (show-viewer)))
 ((show-viewer)
  ((findViewById R$id:ViewerLocation):setVisibility View:INVISIBLE)
  ((findViewById R$id:Overview):setVisibility View:INVISIBLE)
  ((findViewById R$id:leftArrow):setVisibility View:INVISIBLE)
  ((findViewById R$id:rightArrow):setVisibility View:INVISIBLE)
  ((findViewById R$id:goText):setVisibility View:INVISIBLE)
  ((findViewById R$id:Viewer):setVisibility View:VISIBLE))
 ((show-overview)
  ((findViewById R$id:ViewerLocation):setVisibility View:VISIBLE)
  ((findViewById R$id:Overview):setVisibility View:VISIBLE)
  ((findViewById R$id:leftArrow):setVisibility View:VISIBLE)
  ((findViewById R$id:rightArrow):setVisibility View:VISIBLE)
  ((findViewById R$id:goText):setVisibility View:VISIBLE)
  ((findViewById R$id:Viewer):setVisibility View:INVISIBLE))
 ((stop-timer)
  (set! running? #f)
  (show-overview))
 ((update-offset progress)
  (let ((location-view ::TextView (findViewById R$id:Location))
        (overview ::TextView (findViewById R$id:Overview))
        (center-offset 200))
   (set! offset progress)
   (location-view:setText (format #f "~a" offset))
   (let ((spannable ::SpannableString (make <android.text.SpannableString>
                                       (text:substring (max (- offset center-offset) 0)
                                                       (min (+ offset 1000) (string-length text))))))
    (spannable:setSpan (make <android.text.style.BackgroundColorSpan> Color:RED)
                       center-offset
                       (+ (string-length (word-at text offset)) center-offset)
                       0)
    (overview:setText spannable))
   (progress-bar:setProgress offset)))
 ((onSingleTapUp (me ::MotionEvent))
  (let ((location (/ (me:getX) ((findViewById R$id:playerRoot):getWidth))))
   (cond (running? (stop-timer) #t)
         ((< location 0.2) (update-offset (max 0 (- offset 200))) #t)
         ((> location 0.8) (update-offset (min (string-length text) (+ offset 200))) #t)
         ((and (< 0.3 location) (< location 0.7)) (start-timer) #t)
         (else #f))))
 ((onDown (me ::MotionEvent)) #f)
 ((onShowPress (me ::MotionEvent)) #f)
 ((onScroll (me1 ::MotionEvent) (me2 ::MotionEvent) (x ::float) (y ::float)) #f)
 ((onLongPress (me ::MotionEvent)) #f)
 ((onFling (me1 ::MotionEvent) (me2 ::MotionEvent) (x ::float) (y ::float)) #f)
 ((onTouchEvent (me ::MotionEvent)) (gesture-detector:onTouchEvent me)))

(define *preferences:id* "preferences")

(define-simple-class speedread (android.app.Activity)
 (speedread ::SpeedReadApplication)
 (go-button ::Button)
 (speed-bar ::SeekBar)
 ((onCreate (savedInstanceState ::Bundle))
  (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)
  (set! speedread (as SpeedReadApplication (getApplication)))
  (setContentView R$layout:main)
  ((findViewById R$id:go):setOnClickListener
   (lambda (view) (file-chooser (this))))
  (set! speed-bar (findViewById R$id:speedBar))
  (speed-bar:setMax 1000)
  (update-speed ((getSharedPreferences *preferences:id* 0):getInt "speed" 200))
  (on-user-seek-bar-change speed-bar (update-speed position)))
 ((update-speed speed)
  (set! *wpm* speed)
  ((as TextView (findViewById R$id:speed)):setText (format #f "Speed ~a wpm" speed))
  (speed-bar:setProgress speed)
  (let ((prefs ((getSharedPreferences *preferences:id* 0):edit)))
   (prefs:putInt "speed" speed)
   (unless (prefs:commit) (error "Failed to set speed"))))
 ((onActivityResult request result data)
  ((multiple-activity-results (this) (file-chooser-result (lambda (uri) (start-player uri 0))))
   request result data))
 ((start-player uri offset)
  (let ((intent <android.content.Intent> (make <android.content.Intent> (this) player)))
   (intent:putExtra (as String "book") (as Parcelable uri))
   (intent:putExtra (as String "offset") (as int offset))
   (startActivity intent)))
 ((onDestroy) (invoke-special Activity (this) 'onDestroy))
 ((onPause) (invoke-special Activity (this) 'onPause))
 ((onResume) (invoke-special Activity (this) 'onResume)
  (let ((books (map alist->hash-table
                    (call-with-input-string
                      ((getSharedPreferences *preferences:id* 0):getString
                       "book-list" "()") read))))
   (when (null? books)
    ((as TextView (findViewById R$id:recentlyReadHeading)):setText "No recently read books!"))
   (let ((list-view (as ListView (findViewById R$id:previousBooks))))
    (list-view:setAdapter
     (make <android.widget.SimpleAdapter> (this)
           books
           android.R$layout:simple_list_item_2
           (String[] "title" "progress")
           (int[] android.R$id:text1 android.R$id:text2)))
    (on-item-click list-view
                   (Log:i "speedread" (format #f "clicked: '~s'" (adapter:getItemAtPosition position)))
                   (start-player (Uri:parse (hash-table-ref (adapter:getItemAtPosition position) "uri"))
                                 (hash-table-ref (adapter:getItemAtPosition position) "offset")))))))
