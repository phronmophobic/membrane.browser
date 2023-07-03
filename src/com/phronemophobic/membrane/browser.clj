(ns com.phronemophobic.membrane.browser
  (:require [membrane.ui :as ui]
            [membrane.skia :as skia]
            [com.phronemophobic.membrane.browser.impl :as impl]
            [com.rpl.specter :as spec]
            [clojure.java.io :as io]
            [membrane.basic-components :as basic]
            [com.phronemophobic.gen2 :as gen2]
            [membrane.component :refer
             [defui defeffect make-app]]
            [com.phronemophobic.cef :as cef
             :refer [print-doc
                     cef-string]]
            [com.phronemophobic.cinterop :as cinterop]
            [clojure.core.async :refer [go
                                        put!
                                        chan
                                        <!
                                        timeout
                                        dropping-buffer
                                        promise-chan
                                        close!
                                        alts!
                                        thread]
             :as async]
            )
  (:import java.util.concurrent.Executors
           java.util.concurrent.TimeUnit
           com.sun.jna.Pointer
           com.sun.jna.Memory)
  )


(gen2/import-cef-classes)

(def glfw @#'skia/glfw)

(defmacro glfw-call [ret fn-name & args]
  `(.invoke ^com.sun.jna.Function
            (.getFunction ^com.sun.jna.NativeLibrary glfw ~(name fn-name))
            ~ret
            (to-array (vector ~@args))))

(def post-empty-event @#'skia/glfw-post-empty-event)

(def ^:no-doc main-class-loader @clojure.lang.Compiler/LOADER)
(def ^:no-doc void Void/TYPE)

(defn ^:private long->pointer [n]
  (tech.v3.datatype.ffi.Pointer. n))

(def skialib @#'skia/membraneskialib)

(skia/defc skia_bgra8888_draw skialib Void/TYPE [skia-resource buffer width height row-bytes])
(defn skia-bgra8888-draw [resource buffer width height row-bytes]
  (skia_bgra8888_draw resource buffer (int width) (int height) (int row-bytes)))

(skia/defc skia_direct_bgra8888_buffer skialib Pointer [buf width height row-bytes])
(defn skia-direct-bgra8888-buffer [buf width height row-bytes]
  (skia_direct_bgra8888_buffer buf (int width) (int height) (int row-bytes)))

(skia/defc skia_cleanup skialib Void/TYPE [skia-resource])

(skia/defc skia_draw_surface skialib Void/TYPE [destination source])


(skia/defc skia_browser_buffer skialib Pointer [width height])
(defn skia-browser-buffer [width height]
  (skia_browser_buffer (int width) (int height)))

(skia/defc skia_browser_update skialib Void/TYPE [skia-resource dirty-rects-count dirty-rects buffer width height])
(defn skia-browser-update [resource dirty-rects-count dirty-rects buffer width height]
  (skia_browser_update resource (int dirty-rects-count) dirty-rects buffer (int width) (int height)))

(skia/defc skia_browser_draw skialib Void/TYPE [skia-resource buffer width height])
(defn skia-browser-draw [resource buffer width height]
  (skia_browser_draw resource buffer (int width) (int height)))

(def ^:no-doc
  objlib (com.sun.jna.NativeLibrary/getInstance "CoreFoundation"))
(skia/defc dispatch_sync_f objlib void [queue context work])
(skia/defc dispatch_async_f  objlib void [queue context work])

(skia/defc cef_task_runner_get_for_current_thread @cinterop/cef CefTaskRunner)

(defonce ^:no-doc
  callbacks (atom []))

(def ^:no-doc main-class-loader @clojure.lang.Compiler/LOADER)

(defn skia-draw [dispatch! $browser-info paint-type nrects rects buffer width height]
  (when (zero? paint-type)
    (let [browser-info (dispatch! :get $browser-info)]

      (locking (:draw-lock browser-info)
        (if (:resource browser-info)
          (if (and (= width (:width browser-info))
                   (= height (:height browser-info)))
            (when (pos? (.intValue nrects))
              (skia-browser-update (:resource browser-info) (.intValue nrects) rects buffer width height)
              (dispatch! :update $browser-info update :browser-id (fnil inc 0)))
            (do
              (dispatch! :update
                         $browser-info
                         dissoc
                         :resource)
              (skia_cleanup (:resource browser-info))
              (skia-draw dispatch! $browser-info paint-type nrects rects buffer width height)))
          (let [resource (skia-browser-buffer width height)
                browser-info {:resource resource
                              :width width
                              :height height}]
            (skia-browser-draw resource buffer width height)
            (dispatch! :update $browser-info update :browser-id (fnil inc 0))

            (dispatch! :update $browser-info merge browser-info)
            (post-empty-event))))))
  ;; always return nil. don't leak cache
  nil)

(defrecord Browser [browser browser-id focused? width height resource draw-lock]
  ui/IOrigin
  (-origin [_]
    [0 0])


  ui/IMouseMove
  (-mouse-move [elem pos]
    (when browser
      (.sendMouseMoveEvent (.getHost ^CefBrowser browser) 
                           (cef/map->mouse-event
                            {:x (first pos)
                             :y (second pos)})
                           0)
      
      ))
  
  ui/IMouseEvent
  (-mouse-event [elem pos button mouse-down? mods]
    (when browser
      (.sendMouseClickEvent (.getHost ^CefBrowser browser) 
                            (cef/map->mouse-event
                             {:x (first pos)
                              :y (second pos)})
                            button
                            (if mouse-down?
                              0
                              1)
                            1)
      ))

  ui/IScroll
  (-scroll [elem delta mpos]
    (when browser
      (.sendMouseWheelEvent (.getHost ^CefBrowser browser) 
                            (cef/map->mouse-event
                             {:x (first mpos)
                              :y (second mpos)})
                            (first delta)
                            (second delta))
      ))

  ui/IHasKeyPress
  (has-key-press [this]
    focused?)
  ui/IKeyPress
  (-key-press [elem k]
    (when (and browser focused?)
      (let [c (if (keyword? k)
                (if (= k :enter)
                  \return
                  (char (get skia/keycodes k)))
                (.charAt k 0))]
        (.sendKeyEvent (.getHost browser)
                       (cef/map->key-event
                        {:type 3
                         :modifiers 0
                         :character c
                         :unmodified-character c})))))

  ui/IHasKeyEvent
  (has-key-event [this]
    focused?)
  ui/IKeyEvent
  (-key-event [elem key code action mods]
    (when (and focused? browser)
      (when (#{:press :release :repeat}
             action)
        (let [key-event {:type (case action
                                 :press 1
                                 :release 2
                                 :repeat 1
                                 )
                         :modifiers mods
                         :native-key-code code
                         :character (char key)
                         :unmodified-character (char key)}]
          (.sendKeyEvent (.getHost ^CefBrowser browser)
                         (cef/map->key-event
                          key-event)))))
    )
  

  ui/IBounds
  (-bounds [this]
    [width height])

  skia/IDraw
  (draw [this]
    (when draw-lock
      (locking draw-lock
        (when resource
          (skia_draw_surface skia/*skia-resource* resource))))))

(defn truncate [s n]
  (subs s 0 (min (count s) n)))

(def post-empty-event @#'skia/glfw-post-empty-event)

(defui browser-view [{:keys [
                             browser-info
                             translate-delta
                             resize-delta
                             ^:membrane.component/contextual focus]}]
  
  (let [{:keys [browser
                browser-id
                width height
                x y
                resource
                draw-lock]} browser-info
        focused? (= focus
                    browser)]
    [
     (ui/translate
      x y
      [
       (ui/wrap-on
        :mouse-down
        (fn [handler pos]
          (cons
           [:set $focus browser]
           (handler pos)))
        (ui/vertical-layout
         (ui/horizontal-layout
          (ui/on
           :mouse-down
           (fn [mpos]
             [[::begin-diff
               $translate-delta
               (fn [[dx dy]]
                 [[:update $x + dx]
                  [:update $y + dy]])]])
           (ui/filled-rectangle [0.5 0.5 0.5]
                                10 10))
          (basic/button {:text "close"
                         :on-click
                         (fn []
                           [[::close-browser browser]])}))
         [(ui/bordered 
           0
           (Browser. browser browser-id focused? width height resource draw-lock))
          (ui/translate width height
                        (ui/on
                         :mouse-down
                         (fn [mpos]
                           [[::begin-diff
                             $resize-delta
                             (fn [[dx dy]]
                               [[::resize-browser
                                 browser [(+ dx width) (+ dy height)]]])]])
                         (ui/filled-rectangle [0.5 0.5 0.5]
                                              10 10)))]))

       (when-let [[dw dh] resize-delta]
         (ui/with-style ::ui/style-stroke
           (ui/rectangle (+ width dw)
                         (+ height dh))))])
     (when-let [[dx dy] translate-delta]
       (ui/with-style ::ui/style-stroke
        (ui/translate (+ x dx) (+ y dy)
                      (ui/rectangle width height))))]))


(defui diff-container [{:keys [body
                               delta*
                               diff-intents-f
                               mdown]}]
  (let [body (if delta*
               (ui/on
                :mouse-up
                (fn [[mx my]]
                  (into
                   (diff-intents-f [(- mx (nth mdown 0)) (- my (nth mdown 1))])
                   [[:set delta* nil]
                    [:set $delta* nil]
                    [:set $diff-intents-f nil]
                    [:set $mdown nil]]))
                :mouse-move
                (fn [[mx my]]
                  [[:set delta* [(- mx (nth mdown 0)) (- my (nth mdown 1))]]])
                body)
               ;; else
               (ui/wrap-on
                :mouse-down
                (fn [handler mpos]
                  (let [intents (handler mpos)]
                    (if-let [[_ $resize-delta f :as begin-diff]
                             (some #(when (= ::begin-diff (first %))
                                      %)
                                   intents)]
                      (into
                       [[:set $delta* $resize-delta]
                        [:set $diff-intents-f f]
                        [:set $mdown mpos]]
                       (remove #(= % begin-diff))
                       intents)
                      ;; else
                      intents)))
                body))]
    body))


(defui browser-test [{:keys [browsers]}]
  (let [[cw ch :as container-size] (:membrane.stretch/container-size context)]
   (ui/wrap-on
    :mouse-down
    (fn [handler mpos]
      (let [intents (handler mpos)]
        (if (seq intents)
          intents
          [[::add-browser $browsers mpos]])))
    (diff-container
     {:$body nil
      :body
      (ui/fixed-bounds
       container-size
       (into [(ui/label (count browsers))]
             (map (fn [browser-id]
                    (let [browser-info (get browsers browser-id)]
                      (browser-view {:browser-info browser-info}))))
             (keys browsers)))}))))

(def glGetError #'skia/glGetError)

(defonce ^:private work-chan (chan 20))
(defonce browser-app-state
  (atom {:info {}
         :browsers {}
         :workf (fn []
                  (loop []
                    (let [work (async/poll! work-chan)]
                      (when work
                        (work)
                        (recur)))))}))
(let [atm @#'cef/prepared-environment]
  (defn prepared? []
    @atm))

(defn- run-helper [window-chan workf]
  (with-local-vars [windows #{}]
    (letfn [(init []
              (if (not= 1 (glfw-call Integer/TYPE glfwInit))
                false
                (do
                  (.setContextClassLoader (Thread/currentThread) main-class-loader)
                  (#'skia/fix-press-and-hold!)
                  ;; (glfw-call void glfwWindowHint GLFW_COCOA_RETINA_FRAMEBUFFER (int 0))

                  ;;
                  (do
                    (glfw-call void glfwWindowHint skia/GLFW_CONTEXT_VERSION_MAJOR (int 3))
                    (glfw-call void glfwWindowHint skia/GLFW_CONTEXT_VERSION_MINOR (int 2))
                    ;; 3.2+ only
                    (glfw-call void glfwWindowHint skia/GLFW_OPENGL_PROFILE skia/GLFW_OPENGL_CORE_PROFILE)
                    ;; Required on Mac
                    (glfw-call void glfwWindowHint skia/GLFW_OPENGL_FORWARD_COMPAT skia/GL_TRUE))

                  (glfw-call void glfwWindowHint skia/GLFW_VISIBLE (int 0))

                  true)))
            (add-windows! []
              (loop [window (async/poll! window-chan)]
                (when window
                  (var-set windows (conj (var-get windows) (skia/init! window)))
                  (recur (async/poll! window-chan)))))
            (wait-events []
              (glfw-call void glfwWaitEventsTimeout (double 0.5))
              #_(glfw-call void glfwWaitEvents )
              #_(glfw-call void glfwPollEvents)
              #_(java.lang.Thread/sleep 30))
            (close-windows! []
              (let [ws (var-get windows)
                    to-close (filter skia/should-close? ws)]
                (when (seq to-close)
                  (run! skia/cleanup! to-close)
                  (var-set windows (reduce disj ws to-close)))))
            (cleanup []
              (glfw-call Void/TYPE glfwTerminate))
            ]

      (try
        (when (init)
          (add-windows!)
          (loop []
            (wait-events)

            ;; clear gl errors. :-/
            (glGetError)

            (add-windows!)

            (close-windows!)

            (when workf
              (workf))

            (run! skia/repaint!
                  (var-get windows))



            (when (seq (var-get windows))
              (recur))))
        (catch Exception e
          (println e))

        (finally
          (cleanup))))))

(defonce window-chan (chan 1))
(defn run [view-fn workf]
  (let [options {:include-container-info true}]
    (async/>!! window-chan (skia/map->GlfwSkiaWindow
                            (merge
                             {:view-fn
                              (if (:include-container-info options)
                                view-fn
                                (fn [_] (view-fn)))}
                             options)))
    (async/thread
      (@#'skia/dispatch-sync!
       (fn []
         (try
           (run-helper window-chan workf)
           (catch Throwable e
             (println e))))))
    nil))



(defeffect ::goto [browser url]
  (impl/goto browser url))

(declare dispatch-thread)
(defn -main [& args]

  (def browser-app (make-app #'browser-test browser-app-state))
  

  (run browser-app
    (:workf @browser-app-state))
  ,
  )



(defeffect ::resize-browser [browser size]
  (impl/resize browser size))

(defeffect ::start-browser [$browsers browser-id]
  (let [$browser-info [$browsers (spec/keypath browser-id)] ]
   (impl/create-browser [400 400]
                        ;;"http://blog.phronemophobic.com/"
                        "https://phoboslab.org/xtype/"
                        (fn [work]
                          (async/>!! work-chan work)
                          (post-empty-event))
                        {:on-after-created
                         (fn [browser]
                           (dispatch! :update $browser-info assoc :browser browser)
                           (prn "created" ))
                         :on-before-close
                         (fn [browser]
                           (dispatch! :update $browser-info dissoc :browser)
                           (dispatch! :update $browsers dissoc browser-id)
                           (prn "before close" ))
                         :on-paint
                         (fn [browser paint-type nrects rects buffer width height]
                           (prn "painting")
                           (skia-draw dispatch! $browser-info paint-type nrects rects buffer width height))})))

(defeffect ::add-browser [$browsers [x y]]
  (let [browser-id (random-uuid)]
   (dispatch! :update $browsers assoc browser-id {:x x
                                                  :y y
                                                  :width 400
                                                  :height 400
                                                  :draw-lock (Object.)
                                                  :browser-id 0
                                                  :resource nil
                                                  })
   (dispatch! ::start-browser $browsers browser-id)))

(defeffect ::close-browser [browser]
  (impl/close browser))

(comment

  (create-browser [400 400]
                  "http://www.google.com"
                  dispatch-main
                  {:on-schedule-message-pump-work
                   (fn [delay]
                     (prn "scheduling!" delay)
                     (async/put! debounce-chan delay))
                   :on-after-created
                   (fn [browser]
                     (prn "created" ))
                   :on-before-close
                   (fn [browser]
                     (prn "before close" ))
                   :on-paint
                   (fn [browser paint-type nrects rects buffer width height]
                     (prn "painting"))})
  
  ,)



