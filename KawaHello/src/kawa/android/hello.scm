(require 'android-defs)

(define-namespace Log "class:android.util.Log")

(activity hello1
  (on-create-view
   (Log:i "kawa" "i")
   (Log:d "kawa" "d")
   (Log:e "kawa" "e")
   (android.widget.TextView
    (this)
    text: "Hello, Android from Kawa Scheme!")))

(activity hello2
  (on-create-view
   (android.widget.LinearLayout
    (this)
    orientation: android.widget.LinearLayout:VERTICAL
    view:
    (android.widget.TextView
     (this)
     text: "Hello, Android from Kawa Scheme!")
    view:
    (android.widget.Button
     (this)
     text: "Click Me!"
     on-click-listener: (lambda (v)
			 ((android.widget.Toast:makeText
			   (this)
			   "Clicked!"
			   android.widget.Toast:LENGTH_LONG):show))))))

(activity hello
  (on-create-view
    (define mTts
      (android.speech.tts.TextToSpeech
	(this)
	(lambda (i) ())))
    (android.widget.LinearLayout (this)
      orientation: android.widget.LinearLayout:VERTICAL
      view:
	(android.widget.TextView (this)
	  text: "Enter the text to speak")
      view:
	(android.widget.EditText (this)
	  id: 101)
      view:
	(android.widget.Button (this)
	  text: "Speak!"
	  on-click-listener:
	    (lambda (v)
	      (mTts:speak
		((as <android.widget.EditText>
		  ((this):findViewById 101)):getText)
		android.speech.tts.TextToSpeech:QUEUE_FLUSH
		#!null))))))
