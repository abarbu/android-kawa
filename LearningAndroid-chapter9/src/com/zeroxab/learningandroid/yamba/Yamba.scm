(require 'android-defs)
(require 'srfi-1)
(require 'syntax-utils)
(require <collections_utils>)

(define-namespace Log "class:android.util.Log")

;; from kawa/testsuite/classes1.scm
(define-syntax (import-class form)
 (syntax-case form ()
  ((import-class fqcn)
   (let* ((cls :: java.lang.Class (eval (syntax fqcn)))
	  (name (string->symbol (java.lang.Class:getSimpleName cls))))
    #`(define-alias ,(datum->syntax-object form name) fqcn)))))

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
(define-alias Context              android.content.Context)
(define-alias Intent               android.content.Intent)
(define-alias Resources            android.content.res.Resources)
(define-alias SharedPreferences    android.content.SharedPreferences)
(define-alias ContentValues        android.content.ContentValues)
(define-alias Handler   android.os.Handler)
(define-alias Bundle    android.os.Bundle)
(define-alias IBinder   android.os.IBinder)
(define-alias KeyEvent         android.view.KeyEvent)
(define-alias OnClickListener  android.view.View$OnClickListener)
(define-alias SurfaceHolder    android.view.SurfaceHolder)
(define-alias View             android.view.View)
(define-alias Menu             android.view.Menu)
(define-alias MenuItem         android.view.MenuItem)
(define-alias EditText    android.widget.EditText)
(define-alias Button      android.widget.Button)
(define-alias TextView    android.widget.TextView)
(define-alias Toast       android.widget.Toast)
(define-alias Editable    android.text.Editable)
(define-alias TextWatcher android.text.TextWatcher)
(define-alias PreferenceActivity android.preference.PreferenceActivity)
(define-alias PreferenceManager  android.preference.PreferenceManager)
(define-alias BaseColumns android.provider.BaseColumns)
(define-alias Cursor           android.database.Cursor)
(define-alias SQLiteDatabase   android.database.sqlite.SQLiteDatabase)
(define-alias SQLiteOpenHelper android.database.sqlite.SQLiteOpenHelper)

(define-alias FVector  gnu.lists.FVector)

(define-alias R com.zeroxab.learningandroid.yamba.R)

(define-alias Twitter winterwell.jtwitter.Twitter)

(define update-delay 10000) ;; 10 seconds

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
     #`(object (android.os.AsyncTask)
	       ,@(process #`r)))))))

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
 (status-data      ::StatusData)
 ((onCreate)
  (invoke-special android.app.Application (this) 'onCreate)
  (set! preferences (PreferenceManager:getDefaultSharedPreferences (this)))
  (set! status-data (make StatusData (this)))
  (preferences:registerOnSharedPreferenceChangeListener
   (lambda ((preferences ::SharedPreferences) (key ::String))
    (set! twitter #!null))))
 ((onTerminate)
  (invoke-special android.app.Application (this) 'onTerminate))
 ((get-twitter) ::Twitter
  (synchronized
   (this)
   (when (equal? twitter #!null)
    (set! twitter (Twitter:new (preferences:getString "username"  "student")
			       (preferences:getString "password"  "password")))
    (twitter:setAPIRootUrl (preferences:getString "apiRoot" "http://yamba.marakana.com/api"))))
  twitter)
 ((get-status-data) ::StatusData
  status-data)
 ((fetch-status-updates) ::int
  (Log:d "Yamba" "Fetching status")
  (let ((twitter (get-twitter)))
   (if (equal? twitter #!null)
       (begin (Log:e "Yamba" "Twitter not initialized!") 0)
       (try-catch
	(let* ((latest-status-at
		::long
		((get-status-data):get-latest-status-created-at-time))
	       (nr
		(count
		 (lambda (a) (equal? #t a))
		 (->list
		  (better-map
		   (lambda (status ::<winterwell.jtwitter.Status>)
		    (let* ((values (make ContentValues))
			   (created-at
			    (java.lang.Long ((status:getCreatedAt):getTime))))
		     (values:clear)
		     (values:put StatusData:C_ID status:id)
		     (values:put StatusData:C_CREATED_AT created-at)
		     ;; (values:put StatusData:C_SOURCE status:source)
		     (values:put StatusData:C_TEXT status:text)
		     (values:put StatusData:C_USER (status:getUser):name)
		     ((get-status-data):insert-or-ignore values)
		     (< latest-status-at created-at)))
		   (twitter:getFriendsTimeline))))))
	 (Log:d "Yamba added status update" nr)
	 nr)
	(e android.database.SQLException ;; java.lang.RuntimeException
	   (Log:e "Yamba-UpdaterService" e)
	   (Log:e "Yamba-UpdaterService" "Runtime!")
	   0))))))

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

(define-simple-class UpdaterService (Service)
 (updater ::Thread)
 (running? ::boolean)
 ((onBind (intent ::Intent)) #!null)
 ((onCreate) (invoke-special android.app.Service (this) 'onCreate)
  (Log:d "Yamba-UpdaterService" "Created")
  (set! updater
	(simple-thread
	 (try-catch
	  (let loop ()
	   (when running?
	    (Log:d "Yamba-UpdaterService" "Running")
	    (when (> ((as YambaApplication (getApplication)):fetch-status-updates) 0)
	     (Log:d "UpdateService" "New status"))
	    (Thread:sleep update-delay)
	    (loop)))
	  (e java.lang.InterruptedException
	     (Log:e "Yamba-UpdaterService" e)
	     (Log:e "Yamba-UpdaterService" "Failed!")
	     #f)))))
 ((onStartCommand (intent ::Intent) (flags ::int) (startId ::int))
  (invoke-special android.app.Service (this) 'onStartCommand intent flags startId)
  (unless running?
   (set! (as YambaApplication (getApplication)):service-running? #t)
   (set! running? #t)
   (*:start updater)
   (Log:d "Yamba-UpdaterService" "Started"))
  Service:START_STICKY)
 ((onDestroy)
  (set! (as YambaApplication (getApplication)):service-running? #f)
  (when running?
   (set! running? #f)
   (*:interrupt updater)
   (Log:d "Yamba-UpdaterService" "Destroyed"))))

(define-simple-class dbHelper (SQLiteOpenHelper)
 ((*init* (context_ ::Context))
  (invoke-special android.database.sqlite.SQLiteOpenHelper
 		  (this) '*init* context_ StatusData:DATABASE
		  #!null StatusData:VERSION))
 ((onCreate (db ::SQLiteDatabase))
  (let ((sql (string-append "create table "
			    StatusData:TABLE " ("
			    StatusData:C_ID " int primary key, "
			    StatusData:C_CREATED_AT " int, "
			    StatusData:C_USER " text, "
			    StatusData:C_TEXT " text)")))
   (*:execSQL db sql)
   (Log:d "Yamba-DB" (string-append "onCreated sql: " sql))))
 ((onUpgrade (db ::SQLiteDatabase) (oldVersion ::int) (newVersion ::int))
  ;; instead of an alter table we just drop and recreate
  (*:execSQL db (string-append "drop table if exists " StatusData:TABLE))
  (Log:d "Yamba-DB" "onUpdated")
  (onCreate db)))

(define-simple-class StatusData ()
 (DATABASE     ::String   allocation: 'static access: 'final init: "timeline.db")
 (VERSION      ::int      allocation: 'static access: 'final init: 1)
 (TABLE        ::String   allocation: 'static access: 'final init: "timeline")
 (C_ID         ::String   allocation: 'static access: 'final init: BaseColumns:_ID)
 (C_CREATED_AT ::String   allocation: 'static access: 'final init: "created_at")
 (C_TEXT       ::String   allocation: 'static access: 'final init: "text")
 (C_USER       ::String   allocation: 'static access: 'final init: "user")
 (C_SOURCE     ::String   allocation: 'static access: 'final init: "source")
 ;; (GET_ALL_ORDER_BY     ::String allocation: 'static access: 'final
 ;;      init: C_CREATED_AT + " DESC")
 ;; (MAX_CREATED_AT_COLUMNS ::String allocation: 'static access: 'final
 ;;      init: "max(" + C_CREATED_AT + ")")
 (db-helper    ::dbHelper allocation: 'static access: '(private final))
 ((*init* (context_ ::Context))
  (set! db-helper (make dbHelper context_)))
 ((insert-or-ignore (values ::ContentValues))
  (Log:d "StatusData insert of ignore" values)
  (let ((db ::SQLiteDatabase (db-helper:getWritableDatabase)))
   (try-finally
    (try-catch (db:insertWithOnConflict TABLE #!null values
					SQLiteDatabase:CONFLICT_IGNORE)
	       (e android.database.SQLException
		  (Log:e "Yamba-Updater SQL" e)
		  #f))
    (db:close))))
 ((get-status-updates)
  (let ((db ::SQLiteDatabase (db-helper:getReadableDatabase)))
   (try-finally
    ((db-helper:getReadableDatabase):query
     TABLE #!null #!null #!null #!null #!null
     (string-append C_CREATED_AT " DESC"))
    (db:close))))
 ((get-latest-status-created-at-time)
  (let ((db ::SQLiteDatabase (db-helper:getReadableDatabase)))
   (try-finally
    (let ((cursor ::Cursor
		  (db:query TABLE
			    (String[] (string-append "max(" C_CREATED_AT ")"))
			    #!null #!null #!null #!null #!null)))
     (try-finally
      (if (cursor:moveToNext) (cursor:getLong 0) java.lang.Long:MIN_VALUE)
      (cursor:close)))
    (db:close))))
 ((get-status-text-by-id (id ::long))
  (let ((db ::SQLiteDatabase (db-helper:getReadableDatabase)))
   (try-finally
    (let ((cursor ::Cursor (db:query TABLE
				     (String[] C_TEXT)
				     (string-append C_ID "=" id)
				     #!null #!null #!null #!null)))
     (try-finally
      (if (cursor:moveToNext) (cursor:getString 0) #!null)
      (cursor:close)))
    (db:close)))))
