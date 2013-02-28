(require 'android-defs)
(require 'srfi-1)

(define-namespace Log "class:android.util.Log")

(define-alias Math java.lang.Math)

(define-alias Bitmap   android.graphics.Bitmap)
(define-alias Canvas   android.graphics.Canvas)
(define-alias Color    android.graphics.Color)
(define-alias Drawable android.graphics.drawable.Drawable)
(define-alias Resources            android.content.res.Resources)
(define-alias Context              android.content.Context)
(define-alias Intent               android.content.Intent)
(define-alias SharedPreferences    android.content.SharedPreferences)
(define-alias Activity android.app.Activity)
(define-alias Handler android.os.Handler)
(define-alias Bundle  android.os.Bundle)
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


(define-alias R com.zeroxab.learningandroid.yamba.R)

(define-alias Twitter winterwell.jtwitter.Twitter)

(define-simple-class PrefActivity (PreferenceActivity)
 ((onCreate (savedInstanceState ::Bundle))
  (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)
  (addPreferencesFromResource R$xml:prefs)))

(define-simple-class StatusActivity (Activity <android.view.View$OnClickListener>
					      <android.text.TextWatcher>)
 (edit-text     ::EditText)
 (update-button ::Button)
 (twitter       ::Twitter)
 (text-count    ::TextView)
 (preferences   ::SharedPreferences)

 ((onCreate (savedInstanceState ::Bundle))
  (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)
  (setContentView R$layout:status)

  (set! edit-text (findViewById R$id:editText))
  (set! update-button (findViewById R$id:buttonUpdate))
  (update-button:setOnClickListener (this))

  (set! text-count (findViewById R$id:textCount))
  (text-count:setText (140:toString))
  (text-count:setTextColor Color:GREEN)
  (edit-text:addTextChangedListener (this))

  (set! preferences (PreferenceManager:getDefaultSharedPreferences (this)))
  (preferences:registerOnSharedPreferenceChangeListener
   (lambda ((preferences ::SharedPreferences) (key ::String))
    (set! twitter #!null))))

 ((get-twitter)
  (when (equal? twitter #!null)
   (set! twitter (Twitter:new (preferences:getString "username"  "")
			      (preferences:getString "password"  "")))
   (twitter:setAPIRootUrl (preferences:getString "apiRoot" "http://yamba.marakana.com/api")))
  twitter)

 ((onClick (view ::View))
  (let* ( ;; FIXME this is nasty
	 (activity-this (this))
	 (post-to-twitter
	  (object (android.os.AsyncTask)
		  ((doInBackground (statuses ::java.lang.Object[])) (@java.lang.Override) ::Object
		   (try-catch (begin ((get-twitter):setStatus (statuses 0))
 		   		     (statuses 0))
 		   	      (e winterwell.jtwitter.TwitterException
 		   		 ((Toast:makeText activity-this
 		   				  "Ooops, failed to set twitter status!"
 		   				  Toast:LENGTH_SHORT):show)
 		   		 (Log:e "Yamba-StatusActivity" "Failed to set twitter status!")
 		   		 (e:printStackTrace)
 		   		 "Failed to post")))
 		  ((onProgressUpdate values) (@java.lang.Override) ::void
 		   #!null)
 		  ((onPostExecute result) (@java.lang.Override)
 		   ((Toast:makeText activity-this (as <String> result) Toast:LENGTH_LONG):show)))))
   (*:execute post-to-twitter ((edit-text:getText):toString))
   (Log:d "Yamba-StatusActivity" "Clicked!")))

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
	(else #f))
  #t))
