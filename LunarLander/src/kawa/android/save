(require 'android-defs)

;; TODO inspect cast!
;; TODO I don't know how to have overloaded functions in kawa, setState1 and setState2
;; FIXME This is really ugly, apparently 'as' doesn't support primitive types

(define-namespace Log "class:android.util.Log")

(define-alias Bitmap android.graphics.Bitmap)
(define-alias Canvas android.graphics.Canvas)
(define-alias Drawable android.graphics.drawable.Drawable)
(define-alias Bundle android.os.Bundle)
(define-alias SurfaceHolder android.view.SurfaceHolder)
(define-alias KeyEvent android.view.KeyEvent)
(define-alias Resources android.content.res.Resources)
(define-alias Context android.content.Context)
(define-alias Handler android.os.Handler)
(define-alias Math java.lang.Math)

;; Difficulty setting constants
(define-constant DIFFICULTY_EASY   ::int 0)
(define-constant DIFFICULTY_HARD   ::int 1)
(define-constant DIFFICULTY_MEDIUM ::int 2)

;; Physics constants
(define-constant PHYS_DOWN_ACCEL_SEC   ::int 35)
(define-constant PHYS_FIRE_ACCEL_SEC   ::int 80)
(define-constant PHYS_FUEL_INIT        ::int 60)
(define-constant PHYS_FUEL_MAX         ::int 100)
(define-constant PHYS_FUEL_SEC         ::int 10)
(define-constant PHYS_SLEW_SEC         ::int 120) ; degrees/second rotate
(define-constant PHYS_SPEED_HYPERSPACE ::int 180)
(define-constant PHYS_SPEED_INIT       ::int 30)
(define-constant PHYS_SPEED_MAX        ::int 120)

;; State-tracking constants
(define-constant STATE_LOSE    ::int 1)
(define-constant STATE_PAUSE   ::int 2)
(define-constant STATE_READY   ::int 3)
(define-constant STATE_RUNNING ::int 4)
(define-constant STATE_WIN     ::int 5)

;; Goal condition constants
(define-constant TARGET_ANGLE          ::int 18)     ;  > this angle means crash
(define-constant TARGET_BOTTOM_PADDING ::int 17)     ;  px below gear
(define-constant TARGET_PAD_HEIGHT     ::int 8)      ;  how high above ground
(define-constant TARGET_SPEED          ::int 28)     ;  > this speed means crash
(define-constant TARGET_WIDTH          ::double 1.6) ;  width of target

;; UI constants (i.e. the speed & fuel bars)
(define-constant UI_BAR         ::int 100) ; width of the bar(s)
(define-constant UI_BAR_HEIGHT  ::int 10) ; height of the bar(s)
(define-constant KEY_DIFFICULTY ::string "mDifficulty")
(define-constant KEY_DX         ::string "mDX")

(define-constant KEY_DY         ::string "mDY")
(define-constant KEY_FUEL       ::string "mFuel")
(define-constant KEY_GOAL_ANGLE ::string "mGoalAngle")
(define-constant KEY_GOAL_SPEED ::string "mGoalSpeed")
(define-constant KEY_GOAL_WIDTH ::string "mGoalWidth")

(define-constant KEY_GOAL_X         ::string "mGoalX")
(define-constant KEY_HEADING        ::string "mHeading")
(define-constant KEY_LANDER_HEIGHT  ::string "mLanderHeight")
(define-constant KEY_LANDER_WIDTH   ::string "mLanderWidth")
(define-constant KEY_WINS           ::string "mWinsInARow")

(define-constant KEY_X ::string "mX")
(define-constant KEY_Y ::string "mY")


(define-simple-class LunarThread (java.lang.Thread)
 (backgroundImage ::Bitmap)
 (canvas-height ::int) (canvas-width ::int)
 (crashedImage ::Drawable)
 (difficulty ::int)
 ;; velocity
 (dX ::double) (dY ::double)
 (engineFiring ::boolean)
 (firingImage ::Drawable)
 (fuel ::double)
 ;; Allowed angle.
 (goalAngle ::int)
 ;;  Allowed speed.
 (goalSpeed ::int)
 ;;  Width of the landing pad.
 (goalWidth ::int)
 ;;  X of the landing pad.
 (goalX ::int)
 ;;  Message handler used by thread to interact with TextView
 (handler ::Handler)
 ;;  * Lander heading in degrees, with 0 up, 90 right. Kept in the range
 ;;  * 0..360.
 (heading ::double)
 ;;  Pixel height of lander image.
 (landerHeight ::int)
 ;;  What to draw for the Lander in its normal state
 (landerImage ::Drawable)
 ;;  Pixel width of lander image.
 (landerWidth ::int)
 ;;  Used to figure out elapsed time between frames
 (lastTime ::long)
 ;;  Paint to draw the lines on screen.
 (linePaint ::<android.graphics.Paint>)
 ;;  "Bad" speed-too-high variant of the line color.
 (linePaintBad ::<android.graphics.Paint>)
 ;;  The state of the game. One of READY, RUNNING, PAUSE, LOSE, or WIN
 (mode ::int)
 ;;  Currently rotating, -1 left, 0 none, 1 right.
 (rotating ::int)
 ;;  Indicate whether the surface has been created & is ready to draw
 (run? ::boolean)
 ;;  Scratch rect object.
 (scratchRect ::<android.graphics.RectF>)
 ;;  Handle to the surface manager object we interact with
 (surfaceHolder ::SurfaceHolder)
 ;;  Number of wins in a row.
 (winsInARow ::int)
 ;;  X of lander center.
 (x ::double)
 ;;  Y of lander center.
 (y ::double)
 (context ::Context)
 ((*init* (surfaceHolder_ ::SurfaceHolder) (context_ ::Context) (handler_ ::Handler))
  (set! canvas-height 1)
  (set! canvas-width 1)
  (set! run? #f)

  ;; get handles to some important objects
  (set! surfaceHolder surfaceHolder_)
  (set! handler handler_)
  (set! context context_)

  (let ((res ::Resources (invoke context 'getResources)))
   (set! landerImage (res:getDrawable com.zeroxab.lunarlander.R$drawable:lander_plain))
   (set! firingImage (res:getDrawable com.zeroxab.lunarlander.R$drawable:lander_firing))
   (set! crashedImage (res:getDrawable com.zeroxab.lunarlander.R$drawable:lander_crashed))
   ;; load background image as a Bitmap instead of a Drawable b/c
   ;; we don't need to transform it and it's faster to draw this way
   (set! backgroundImage (android.graphics.BitmapFactory:decodeResource res com.zeroxab.lunarlander.R$drawable:earthrise)))

  ;; Use the regular lander image as the model size for all sprites
  (set! landerWidth (landerImage:getIntrinsicWidth))
  (set! landerHeight (landerImage:getIntrinsicHeight))

  ;; // Initialize paints for speedometer
  (set! linePaint (android.graphics.Paint))
  (linePaint:setAntiAlias #t)
  (linePaint:setARGB 255 0 255 0)

  (set! linePaintBad (android.graphics.Paint))
  (linePaintBad:setAntiAlias #t)
  (linePaintBad:setARGB 255 120 180 0)

  (set! scratchRect (android.graphics.RectF 0 0 0 0))

  (set! winsInARow 0)
  (set! difficulty DIFFICULTY_MEDIUM)

  ;; // initial show-up of lander (not yet playing)
  (set! x landerWidth)
  (set! y (* 2 landerHeight))
  (set! fuel PHYS_FUEL_INIT)
  (set! dX 0)
  (set! dY 0)
  (set! heading 0)
  (set! engineFiring #t))
 ((doStart) ::void
  (synchronized surfaceHolder
		;; First set the game for Medium difficulty
		(set! fuel PHYS_FUEL_INIT)
		(set! engineFiring #f)
		;; cast!
		(set! goalWidth (as int (* landerWidth TARGET_WIDTH)))
		(set! goalSpeed TARGET_SPEED)
		(set! goalAngle TARGET_ANGLE)
		(let ((speedInit PHYS_SPEED_INIT))
		 ;; Adjust difficulty params for EASY/HARD
		 (cond
		  ((equal? difficulty DIFFICULTY_EASY)
		   (set! fuel (* fuel (/ 3 2)))
		   (set! goalWidth (* goalWidth (/ 4 3)))
		   (set! goalSpeed (* goalSpeed (/ 3 2)))
		   (set! goalAngle (* goalAngle (/ 4 3)))
		   (set! speedInit (* speedInit (/ 3 4))))
		  ((equal? difficulty DIFFICULTY_HARD)
		   (set! fuel (* fuel (/ 7 8)))
		   (set! goalWidth (* goalWidth (/ 3 4)))
		   (set! goalSpeed (* goalSpeed / 7 8))
		   (set! speedInit (* speedInit / 4 3))))

		 ;; pick a convenient initial location for the lander sprite
		 (set! x (/ canvas-width 2))
		 (set! y (- canvas-height (/ landerHeight 2)))

		 ;; start with a little random motion
		 (set! dY (* (Math:random) (- speedInit)))
		 (set! dX (* (Math:random) 2 (- speedInit speedInit)))
		 (set! heading 0))

		;; Figure initial spot for landing, not too near center
		(let loop ()
		 ;; cast!
		 (set! goalX (* (Math:random) (- canvas-width goalWidth)))
		 (unless (> (Math:abs (- goalX (- x (/ landerWidth 2))))
			    (/ canvas-height 6))
		  (loop)))

		(set! lastTime (+ (java.lang.System:currentTimeMillis) 100))
		(setState1 STATE_RUNNING)))

 ;; Pauses the physics update & animation.
 ((pause) ::void (synchronized surfaceHolder
			       (when (equal? mode STATE_RUNNING)
				(setState1 STATE_PAUSE))))

 ;; Restores game state from the indicated Bundle. Typically called when
 ;; the Activity is being restored after having been previously
 ;; destroyed.
 ;; @param savedState Bundle containing the game state
 ((restoreState (savedState ::Bundle))
  (synchronized surfaceHolder
		(setState1 STATE_PAUSE)
		(set! rotating 0)
		(set! engineFiring #f)

		(set! difficulty (savedState:getInt KEY_DIFFICULTY))
		(set! x (savedState:getDouble KEY_X))
		(set! y (savedState:getDouble KEY_Y))
		(set! dX (savedState:getDouble KEY_DX))
		(set! dY (savedState:getDouble KEY_DY))
		(set! heading (savedState:getDouble KEY_HEADING))

		(set! landerWidth (savedState:getInt KEY_LANDER_WIDTH))
		(set! landerHeight (savedState:getInt KEY_LANDER_HEIGHT))
		(set! goalX (savedState:getInt KEY_GOAL_X))
		(set! goalSpeed (savedState:getInt KEY_GOAL_SPEED))
		(set! goalAngle (savedState:getInt KEY_GOAL_ANGLE))
		(set! goalWidth (savedState:getInt KEY_GOAL_WIDTH))
		(set! winsInARow (savedState:getInt KEY_WINS))
		(set! fuel (savedState:getDouble KEY_FUEL))))

 ;; Dump game state to the provided Bundle. Typically called when the
 ;; Activity is being suspended.
 ;; @return Bundle with this view's state
 ((saveState (map ::Bundle)) ::Bundle
  (synchronized surfaceHolder
		(unless (equal? map #!null)
		 (map:putInt KEY_DIFFICULTY (java.lang.Integer:valueOf difficulty))
		 (map:putDouble KEY_X (java.lang.Double:valueOf x))
		 (map:putDouble KEY_Y (java.lang.Double:valueOf y))
		 (map:putDouble KEY_DX (java.lang.Double:valueOf dX))
		 (map:putDouble KEY_DY (java.lang.Double:valueOf dY))
		 (map:putDouble KEY_HEADING (java.lang.Double:valueOf heading))
		 (map:putInt KEY_LANDER_WIDTH (java.lang.Integer:valueOf landerWidth))
		 (map:putInt KEY_LANDER_HEIGHT (java.lang.Integer:valueOf landerHeight))
		 (map:putInt KEY_GOAL_X (java.lang.Integer:valueOf goalX))
		 (map:putInt KEY_GOAL_SPEED (java.lang.Integer:valueOf goalSpeed))
		 (map:putInt KEY_GOAL_ANGLE (java.lang.Integer:valueOf goalAngle))
		 (map:putInt KEY_GOAL_WIDTH (java.lang.Integer:valueOf goalWidth))
		 (map:putInt KEY_WINS (java.lang.Integer:valueOf winsInARow))
		 (map:putDouble KEY_FUEL (java.lang.Double:valueOf fuel))))
  map)

 ((run)
  (let loop ()
   (when run?
    (let ((c ::Canvas #!null))
     (try-finally
      (begin
       (set! c (surfaceHolder:lockCanvas #!null))
       (synchronized surfaceHolder
		     (when (equal? mode STATE_RUNNING) (updatePhysics))
		     (doDraw c)))
      ;; do this in a finally so that if an exception is thrown
      ;; during the above, we don't leave the Surface in an
      ;; inconsistent state
      (unless (equal? c #!null)
       (surfaceHolder:unlockCanvasAndPost c)))
     (loop)))))

 ;; sets the current difficulty.
 ((setDifficulty (difficulty_ ::int))
  (synchronized surfaceHolder
		(set! difficulty difficulty_)))

 ;; sets if the engine is currently firing.
 ((setFiring (firing ::boolean))
  (synchronized surfaceHolder
		(set! engineFiring firing)))

 ;; Used to signal the thread whether it should be running or not.
 ;; Passing true allows the thread to run; passing false will shut it
 ;; down if it's already running. Calling start() after this was most
 ;; recently called with false will result in an immediate shutdown.
 ;;
 ;; @param b true to run, false to shut down

 ((setRunning (b ::boolean))
  (set! run? b))

 ;; Sets the game mode. That is, whether we are running, paused, in the
 ;; failure state, in the victory state, etc.
 ;; @see #setState(int, CharSequence)
 ;; @param mode one of the STATE_* constants
 ((setState1 (mode ::int)) ::void
  (synchronized surfaceHolder
		(setState mode "")))

 ;; Sets the game mode. That is, whether we are running, paused, in the
 ;; failure state, in the victory state, etc.
 ;; @param mode one of the STATE_* constants
 ;; @param message string to add to screen or null
 ((setState (mode_ ::int) (message ::string)) ::void
  (define (send-message text view)
   (let ((msg ::android.os.Message (handler:obtainMessage))
	 (b ::Bundle (Bundle)))
    (b:putString "text" text)
    (b:putInt "viz" view)
    (msg:setData b)
    (handler:sendMessage msg)))
  (synchronized
   surfaceHolder
   (set! mode mode_)
   (if (equal? mode STATE_RUNNING)
       (send-message "" android.view.View:INVISIBLE)
       (begin
	(set! rotating 0)
	(set! engineFiring #f)
	(let* ((res ::Resources (context:getResources))
	       (str (string-append
		     (cond ((equal? mode STATE_READY)
			    (res:getText com.zeroxab.lunarlander.R$string:mode_ready))
			   ((equal? mode STATE_PAUSE)
			    (res:getText com.zeroxab.lunarlander.R$string:mode_pause))
			   ((equal? mode STATE_LOSE)
			    (res:getText com.zeroxab.lunarlander.R$string:mode_lose))
			   ((equal? mode STATE_WIN)
			    (string-append
			     (res:getText com.zeroxab.lunarlander.R$string:mode_win_prefix)
			     (number->string winsInARow)
			     " "
			     (res:getText com.zeroxab.lunarlander.R$string:mode_win_suffix))))
		     (if (equal? message "")
			 ""
			 (string-append "\n " message)))))
	 (when (equal? mode STATE_LOSE) (set! winsInARow 0))
	 (send-message str android.view.View:VISIBLE))))))


 ;; Callback invoked when the surface dimensions change.
 ((setSurfaceSize (width ::int) (height ::int))
  (synchronized surfaceHolder
		(set! canvas-width width)
		(set! canvas-height height)))

 ;; Resumes from a pause.
 ((unpause)
  (synchronized surfaceHolder
		(set! lastTime (+ (java.lang.System:currentTimeMillis)100)))
  (setState1 STATE_RUNNING))

 ;; Handles a key-down event.
 ;; @param keyCode the key that was pressed
 ;; @param msg the original event object
 ;; @return true
 ((doKeyDown (keyCode ::int) (msg ::KeyEvent))
  (synchronized surfaceHolder
		(let ((okStart
		       (member keyCode (list KeyEvent:KEYCODE_DPAD_UP
					     KeyEvent:KEYCODE_DPAD_DOWN
					     KeyEvent:KEYCODE_S))))
		 (cond ((and okStart (member mode (list STATE_READY STATE_LOSE STATE_WIN)))
			(doStart) #t)
		       ((and okStart (member mode (list STATE_PAUSE)))
			(unpause) #t)
		       ((member mode (list STATE_RUNNING))
			(cond ((member keyCode (list KeyEvent:KEYCODE_DPAD_CENTER
						     KeyEvent:KEYCODE_SPACE))
			       (setFiring #t) #t)
			      ((member keyCode (list KeyEvent:KEYCODE_DPAD_LEFT
						     KeyEvent:KEYCODE_Q))
			       (set! rotating -1) #t)
			      ((member keyCode (list KeyEvent:KEYCODE_DPAD_RIGHT
						     KeyEvent:KEYCODE_W))
			       (set! rotating 1) #t)
			      ((member keyCode (list KeyEvent:KEYCODE_DPAD_UP))
			       (pause) #t)
			      (else #f)))
		       (else #f)))))

 ;; Handles a key-up event.
 ;; @param keyCode the key that was pressed
 ;; @param msg the original event object
 ;; @return true if the key was handled and consumed, or else false

 ((doKeyUp (keyCode ::int) (msg ::KeyEvent))
  (synchronized surfaceHolder
		(if (equal? mode STATE_RUNNING)
		    (cond ((member keyCode (list KeyEvent:KEYCODE_DPAD_CENTER
						 KeyEvent:KEYCODE_SPACE))
			   (setFiring #f) #t)
			  ((member keyCode (list KeyEvent:KEYCODE_DPAD_LEFT
						 KeyEvent:KEYCODE_Q
						 KeyEvent:KEYCODE_DPAD_RIGHT
						 KeyEvent:KEYCODE_W))
			   (set! rotating 0) #t)
			  (else #f))
		    #f)))

 ((doDraw (canvas ::Canvas))
  (canvas:drawBitmap backgroundImage
		     (<android.graphics.Rect>:new
		      0 0
		      (backgroundImage:getWidth)
		      (backgroundImage:getHeight))
		     (<android.graphics.Rect>:new
		      0 0
		      canvas-width canvas-height)
		     #!null)
  ;; Draw the fuel gauge
  (let ((fuelWidth ::int (* UI_BAR (/ fuel PHYS_FUEL_MAX))))
   (scratchRect:set 4 4 (+ 4 fuelWidth) (+ 4 UI_BAR_HEIGHT))
   (canvas:drawRect scratchRect linePaint))

  ;; Draw the speed gauge, with a two-tone effect
  (define (draw-bar width paint)
   (scratchRect:set (+ 4 UI_BAR 4) 4 (+ 4 UI_BAR 4 width) (+ 4 UI_BAR_HEIGHT))
   (canvas:drawRect scratchRect paint))
  (let* ((speed ::double (Math:sqrt (+ (* dX dX) (* dY dY))))
	 (speedWidth ::int (/ (* UI_BAR speed) PHYS_SPEED_MAX)))
   (if (<= speed goalSpeed)
       (draw-bar speedWidth linePaint)
       (begin
	;; Draw the bad color in back, with the good color in front of it
	(draw-bar speedWidth linePaintBad)
	(let ((goalWidth ::int (/ (* UI_BAR goalSpeed) PHYS_SPEED_MAX)))
	 (draw-bar goalWidth linePaint)))))

  ;; Draw the landing pad
  (canvas:drawLine goalX
		   (+ 1 (- canvas-height TARGET_PAD_HEIGHT))
		   (+ goalX goalWidth)
		   (+ 1 (- canvas-height TARGET_PAD_HEIGHT))
		   linePaint)

  (let ((yTop ::int (- canvas-height (+ y (/ landerHeight 2))))
	(xLeft ::int (- x (/ landerWidth 2)))
	(image ::Drawable (cond ((equal? mode STATE_LOSE) crashedImage)
				(engineFiring             firingImage)
				(else                     landerImage))))
   ;; Draw the ship with its current rotation
   (canvas:save)
   ;; FIXME This is really ugly, apparently 'as' doesn't support primitive types
   (canvas:rotate (let ((heading ::float heading))
		   heading)
		  (let ((x ::float x))
		   x)
		  (- canvas-height (let ((y ::float y))
				    y)))
   (image:setBounds xLeft yTop (+ xLeft landerWidth) (+ yTop landerHeight))
   (image:draw canvas)
   (canvas:restore)))

 ;; Figures the lander state (x, y, fuel, ...) based on the passage of
 ;; realtime. Does not invalidate(). Called at the start of draw().
 ;; Detects the end-of-game and sets the UI to the next state.
 ((updatePhysics)
  (let* ((now ::long (java.lang.System:currentTimeMillis))
	 (elapsed ::double (/ (- now lastTime) 1000.0)))

   ;; Do nothing if mLastTime is in the future.
   ;; This allows the game-start to delay the start of the physics
   ;; by 100ms or whatever.
   (unless (> lastTime now)
    (unless (= rotating 0)
     ;; mRotating -- update heading
     (set! heading (modulo (+ heading (* rotating (* PHYS_SLEW_SEC elapsed))) 360)))

    ;; Base accelerations -- 0 for x, gravity for y
    (let ((ddx ::double 0.0)
	  (ddy ::double (- (* PHYS_DOWN_ACCEL_SEC elapsed))))
     (when engineFiring
      (let* ((elapsedFiring ::double elapsed)
	     (fuelUsed ::double (* elapsedFiring PHYS_FUEL_SEC)))

       ;; tricky case where we run out of fuel partway through the
       ;; elapsed
       (when (> fuelUsed fuel)
	(set! elapsedFiring (* (/ fuel fuelUsed) elapsed))
	(set! fuelUsed fuel)
	;; Oddball case where we adjust the "control" from here
	(set! engineFiring #f))
       (set! fuel (- fuel fuelUsed))

       ;; have this much acceleration from the engine
       (let* ((accel ::double (* PHYS_FIRE_ACCEL_SEC elapsedFiring))
	      ;; taking 0 as up, 90 as to the right
	      ;; cos(deg) is ddy component, sin(deg) is ddx component
	      (radians ::double (/ (* 2 Math:PI heading) 360)))
	(set! ddx (* (Math:sin radians) accel))
	(set! ddy (+ ddy (* (Math:cos radians) accel))))))

     (let ((dxOld ::double dX) (dyOld ::double dY))
      ;; figure speeds for the end of the period
      (set! dX (+ dX ddx))
      (set! dY (+ dY ddy))
      ;; figure position based on average speed during the period
      (set! x (+ x (* elapsed (/ (+ dX dxOld) 2))))
      (set! y (+ y (* elapsed (/ (+ dY dyOld) 2)))))

     (set! lastTime now)

     ;; Evaluate if we have landed ... stop the game
     (let ((yLowerBound ::double
			(- (+ TARGET_PAD_HEIGHT (/ landerHeight 2))
			   TARGET_BOTTOM_PADDING)))
      (when (<= y yLowerBound)
       (set! y yLowerBound)

       (let ((res ::Resources (context:getResources))
	     (speed ::double (Math:sqrt (+ (* dX dX) (* dY dY))))
	     (onGoal ::boolean
		     (and (<= goalX (- x (/ landerWidth 2)))
			  (<= (+ x (/ landerWidth 2)) (+ goalX goalWidth)))))
	;; "Hyperspace" win -- upside down, going fast,
	;; puts you back at the top.
	(cond ((and onGoal
		    (< (Math:abs (- heading 180)) goalAngle)
		    (> speed PHYS_SPEED_HYPERSPACE))
	       ;; STATE_WIN
	       (set! winsInARow (+ winsInARow 1))
	       (doStart))
	      ((not onGoal)
	       (setState STATE_LOSE (res:getText com.zeroxab.lunarlander.R$string:message_off_pad)))
	      ((not (or (<= heading goalAngle) (>= heading (- 360 goalAngle))))
	       (setState STATE_LOSE (res:getText com.zeroxab.lunarlander.R$string:message_bad_angle)))
	      ((> speed goalSpeed)
	       (setState STATE_LOSE (res:getText com.zeroxab.lunarlander.R$string:message_too_fast)))
	      (else
	       (setState1 STATE_WIN)
	       (set! winsInARow (+ winsInARow 1))))))))))))

(define-simple-class LunarView (android.view.SurfaceView <android.view.SurfaceHolder$Callback>)
 (statusText ::android.widget.TextView)
 (thread ::LunarThread)
 ((*init* (context_ ::<android.content.Context>)  (attrs_ ::<android.util.AttributeSet>))
  (invoke-special <android.view.SurfaceView> (this) '*init* context_ attrs_)
  (let ((holder (getHolder)))
   (holder:addCallback (this))
   (set! thread (LunarThread:new holder
				 context_
				 (object (<android.os.Handler>)
				 	 ((handleMessage (m ::android.os.Message)) ::void
				 	  (statusText:setVisibility ((m:getData):getInt "viz"))
				 	  (statusText:setText ((m:getData):getString "text"))))))
   (setFocusable #t)))
 ((getThread) thread)
 ;; Standard override to get key-press events.
 ((onKeyDown (keyCode ::int) (msg ::KeyEvent))
  (thread:doKeyDown keyCode msg))
 ;; Standard override for key-up. We actually care about these, so we can
 ;; turn off the engine or stop rotating.
 ((onKeyUp (keyCode ::int) (msg ::KeyEvent))
  (thread:doKeyUp keyCode msg))
 ;; Standard window-focus override. Notice focus lost so we can pause on
 ;; focus lost. e.g. user switches to take a call.
 ((onWindowFocusChanged (hasWindowFocus ::boolean))
  (unless hasWindowFocus (thread:pause)))
 ;; Installs a pointer to the text view used for messages.
 ((setTextView (textView ::android.widget.TextView))
  (set! statusText textView))
 ;; Callback invoked when the surface dimensions change.
 ((surfaceChanged (holder ::SurfaceHolder) (format ::int) (width ::int) (height ::int))
  (thread:setSurfaceSize width height))
 ;; Callback invoked when the Surface has been created and is ready to be used.
 ((surfaceCreated (holder ::SurfaceHolder))
  (thread:setRunning #t)
  (thread:start))
 ;; Callback invoked when the Surface has been destroyed and must no longer
 ;; be touched. WARNING: after this method returns, the Surface/Canvas must
 ;; never be touched again!
 ((surfaceDestroyed (holder ::SurfaceHolder))
  ;; we have to tell thread to shut down & wait for it to finish, or else
  ;; it might touch the Surface after we return and explode
  (thread:setRunning #f)
  (let loop ()
   (try-catch
    (thread:join)
    (e java.lang.InterruptedException (loop))))))

(define-constant MENU_EASY ::int 1)
(define-constant MENU_HARD ::int 2)
(define-constant MENU_MEDIUM ::int 3)
(define-constant MENU_PAUSE ::int 4)
(define-constant MENU_RESUME ::int 5)
(define-constant MENU_START ::int 6)
(define-constant MENU_STOP ::int 7)

(define-simple-class LunarLander (android.app.Activity)
 (lunarThread ::LunarThread)
 (lunarView ::LunarView)
 ((onCreate (savedInstanceState ::Bundle))
  (invoke-special android.app.Activity (this) 'onCreate savedInstanceState)

  ;; tell system to use the layout defined in our XML file
  (setContentView com.zeroxab.lunarlander.R$layout:main)

  ;; get handles to the LunarView from XML, and its LunarThread
  (set! lunarView (as LunarView (findViewById com.zeroxab.lunarlander.R$id:lunar)))
  (set! lunarThread (lunarView:getThread))

  ;; give the LunarView a handle to the TextView used for messages
  (lunarView:setTextView (as <android.widget.TextView>
			     (findViewById com.zeroxab.lunarlander.R$id:text)))

  (if (equal? #!null savedInstanceState)
      (begin
       (lunarThread:setState1 STATE_READY)
       (Log:w "LunarLander" "SIS is null"))
      (begin
       (lunarThread:restoreState savedInstanceState)
       (Log:w "LunarLander" "SIS is non-null"))))

 ;; Invoked during init to give the Activity a chance to set up its Menu.
 ;; @param menu the Menu to which entries may be added
 ;; @return true
 ((onCreateOptionsMenu (menu ::<android.view.Menu>))
  (invoke-special android.app.Activity (this) 'onCreateOptionsMenu menu)
  (menu:add 0 MENU_START 0 com.zeroxab.lunarlander.R$string:menu_start)
  (menu:add 0 MENU_STOP 0 com.zeroxab.lunarlander.R$string:menu_stop)
  (menu:add 0 MENU_PAUSE 0 com.zeroxab.lunarlander.R$string:menu_pause)
  (menu:add 0 MENU_RESUME 0 com.zeroxab.lunarlander.R$string:menu_resume)
  (menu:add 0 MENU_EASY 0 com.zeroxab.lunarlander.R$string:menu_easy)
  (menu:add 0 MENU_HARD 0 com.zeroxab.lunarlander.R$string:menu_hard)
  (menu:add 0 MENU_MEDIUM 0 com.zeroxab.lunarlander.R$string:menu_medium)
  #t)

 ;; Invoked when the user selects an item from the Menu.
 ;; @param item the Menu entry which was selected
 ;; @return true if the Menu item was legit (and we consumed it), false
 ;;         otherwise
 ((onOptionsItemSelected (item ::<android.view.MenuItem>))
  (case (item:getItemId)
   ((MENU_START) (lunarThread:doStart) #t)
   ((MENU_STOP)
    (lunarThread:setState STATE_LOSE
			  (getText com.zeroxab.lunarlander.R$string:message_stopped))
    #t)
   ((MENU_PAUSE) (lunarThread:pause) #t)
   ((MENU_RESUME) (lunarThread:unpause) #t)
   ((MENU_EASY) (lunarThread:setDifficulty DIFFICULTY_EASY) #t)
   ((MENU_MEDIUM) (lunarThread:setDifficulty DIFFICULTY_MEDIUM) #t)
   ((MENU_HARD) (lunarThread:setDifficulty DIFFICULTY_HARD) #t)
   (else #f)))

 ;; Invoked when the Activity loses user focus.
 ((onPause)
  (invoke-special android.app.Activity (this) 'onPause)
  ((lunarView:getThread):pause))

 ;; Notification that something is about to happen, to give the Activity a
 ;; chance to save state.
 ;; @param outState a Bundle into which this Activity should save its state
 ((onSaveInstanceState (outState ::Bundle))
  (invoke-special android.app.Activity (this) 'onSaveInstanceState outState)
  (lunarThread:saveState outState)
  (Log:w "LunarLander" "SIS called")))
