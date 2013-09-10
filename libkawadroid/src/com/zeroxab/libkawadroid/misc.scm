(require 'android-defs)
(require 'srfi-1)
(import (except (srfi :13 strings)
                string-hash))
(require 'regex)
(require 'syntax-utils)
(require 'srfi-69)

(provide 'libkawadroid)

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
