(ns com.phronemophobic.membrane.browser.impl
  "This namespace provides a simplified interface to clj-cef."
  (:require [membrane.ui :as ui]
            [membrane.skia :as skia
             :refer [glfw-call]
             ]
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


(defn ^:private queue
  ([] clojure.lang.PersistentQueue/EMPTY))

(defonce ^:private initialized? (atom false))
(defonce ^:private create-host-queue (atom (queue)))
(defonce ^:private browsers (atom {}))

(defn ^:private enqueue-create-host [f]
  (swap! create-host-queue conj f))

(defn ^:private drain-create-host-queue []
  (let [[old new] (swap-vals! create-host-queue empty)]
    (doseq [f old]
      (f))
    (when (seq old)
      (cef/cef-do-message-loop-work))))


(defonce ^:private
  debounce-chan (async/chan
                 (async/sliding-buffer 1)))

(defn ^:private default-timeout []
  (async/timeout 30))


(defn start-debounce-thread [dispatch-main]
  (async/thread
    (prn "entering dispatch thread")
    (try
      (loop [to nil]
        (let [[delay port] (async/alts!!
                            (if to
                              [to debounce-chan]
                              [debounce-chan]))]
          (if (or (= port to)
                  (<= delay 0))
            (do (dispatch-main cef/cef-do-message-loop-work)
                (recur (default-timeout)))
            ;; else
            (recur (async/timeout delay)))))
      (catch Throwable e
        (clojure.pprint/pprint e))
      (finally
        (prn "Exiting dispatch thread"))))
  nil)


(defn initialize [dispatch-main
                  {:keys [remote-debugging-port
                          cache-path]}]
  (let [[old _] (reset-vals! initialized? true)]
    (if old
      ;; already initialized, just drain queue
      (drain-create-host-queue)

      ;; else initialize and then drain queue
      (let [browser-process-handler
            (cef/map->browser-process-handler
             {:on-context-initialized
              (fn [bph]
                (drain-create-host-queue))
              :on-schedule-message-pump-work
              (fn [this delay]
                (async/put! debounce-chan delay))})
            app
            (cef/map->app
             {:get-browser-process-handler
              (fn [app]
                browser-process-handler)})


            target-dir cef/default-target-dir]
        (start-debounce-thread dispatch-main)

        (cef/cef-initialize (cef/map->main-args)
                            (cef/map->settings
                             (merge
                              {:framework-dir-path (.getAbsolutePath (io/file target-dir "Chromium Embedded Framework.framework"))
                               :browser-subprocess-path (.getAbsolutePath (io/file target-dir "ceflib Helper"))
                               :main-bundle-path (.getAbsolutePath target-dir)
                               :locales-dir-path (.getAbsolutePath (io/file target-dir "locales"))
                               :windowless-rendering-enabled 1
                               :external-message-pump 1}
                              (when cache-path
                                {:cache-path (.getCanonicalPath (io/as-file cache-path))})
                              (when remote-debugging-port
                                {:remote-debugging-port remote-debugging-port})))
                            app
                            nil))))
  nil)



(defn- start-browser*
  "Must be called on the main thread."
  [[initial-width initial-height] url dispatch-main
   {:keys [on-after-created
           on-before-close
           on-paint
           on-paint+content-scale
           remote-debugging-port]
    :as opts}]

  (cef/prepare-environment!)

  (let [browser-settings (cef/map->browser-settings)

        last-content-scale (volatile! nil)
        life-span-handler
        (cef/map->life-span-handler
         {:on-after-created
          (fn [this browser]
            (swap! browsers assoc (.getIdentifier browser)
                   {:browser browser
                    :content-scale 1
                    :width initial-width
                    :height initial-height})
            (when on-after-created
              (on-after-created browser))
            (dispatch-main cef/cef-do-message-loop-work))
          :on-before-close
          (fn [this browser]
            (try
              (when on-before-close
                (on-before-close browser))
              (finally
                (swap! browsers dissoc (.getIdentifier browser)))))})

        render-handler
        (cef/map->render-handler
         (merge
          {:get-view-rect
           (fn [handler browser rect]
             (let [{:keys [width height]} (get @browsers  (.getIdentifier browser))]
               (set! (.width rect) width)
               (set! (.height rect) height)))
           :on-paint
           (fn [handler browser paint-type nrects rects buffer width height]
             (when on-paint+content-scale
               (on-paint+content-scale browser @last-content-scale paint-type nrects rects buffer width height))
             (when on-paint
               (on-paint browser paint-type nrects rects buffer width height)))}
          {:get-screen-info
           (fn [handler browser screen-info]
             (let [{:keys [content-scale]} (get @browsers  (.getIdentifier browser))]
               (set! (.device_scale_factor screen-info) content-scale)
               (vreset! last-content-scale content-scale))
             ;; we modified screen-info (see docs)
             (int 1))}))
        client
        (cef/map->client
         {:get-life-span-handler
          (fn [client]
            life-span-handler)
          :get-render-handler
          (fn [client]
            render-handler)})

        window-info (cef/map->window-info
                     {:windowless-rendering-enabled 1})]
    (enqueue-create-host
     #(cef/cef-browser-host-create-browser window-info client url browser-settings nil nil))
    ;; idempotent. will drain queue on initialize
    (initialize dispatch-main opts)))


(defn start-browser
  [[initial-width initial-height :as initial-size] url dispatch-main
   {:keys [on-after-created
           on-before-close
           on-paint
           remote-debugging-port]
    :as opts}]
  (dispatch-main #(start-browser*
                   initial-size
                   url
                   dispatch-main
                   opts)))

(defn resize
  "Given an opaque browser, tries to resize it to [w h]. 
  Errors are ignored if `browser` is no longer valid.
  
  Returns nil."
  ([browser [w h]]
   (resize browser [w h] 1))
  ([browser [w h] content-scale]
   (let [bid (.getIdentifier browser)
         browser-host (.getHost browser)]
     (swap! browsers update bid
            assoc
            :content-scale content-scale
            :width w
            :height h)
     (.notifyScreenInfoChanged browser-host)
     (.wasResized browser-host))
   nil))


(defn goto
  "Given an opaque browser, tries to navigate to url.
  Errors are ignored if `browser` is no longer valid.
  `url` should be a string."
  [browser url]
  nil)

cef/map->browser
cef/map->browser-host

(defn close
  "Closes and frees resources for opaque `browser`.
  Errors are ignored if `browser` is no longer valid."
  ([browser]
   (close browser 0))
  ([browser force-close?]
   (-> browser
       (.getHost)
       (.closeBrowser (if force-close?
                        1
                        0))))
  #_(dispatch-main #(-> browser
                        (.getHost)
                        (.tryCloseBrowser))))

(defn list-browsers
  "Returns current valid browsers."
  []
  (into
   []
   (map :browser)
   (vals @browsers)))

(defn create-browser
  "Asychronously creates a new browser host. "
  [[initial-width initial-height :as initial-size]
   url
   dispatch-main
   {:keys [:on-after-created
           :on-before-close
           :on-paint]
    :as opts}]

  (start-browser initial-size
                 url
                 dispatch-main
                 opts)
  nil)


(comment
  (require '[membrane.skia :as skia])
  (def dispatch-main #'skia/dispatch-sync!)

  (defonce debounce-chan (async/chan
                     (async/sliding-buffer 1)))

  (defn default-timeout []
    (async/timeout 30))

  (async/thread
    (try
      (loop [;; anonymous chan for first iteration
             ;; want to wait until debouce-chan
             ;; send it's first message
             to (chan)]
        (let [[delay port] (async/alts!! [to debounce-chan])]
          (if (or (= port to)
                  (<= delay 0))
            (do (dispatch-main cef/cef-do-message-loop-work)
                (recur (default-timeout)))
            ;; else
            (recur (async/timeout delay)))))
      (catch Throwable e
        (clojure.pprint/pprint e))
      (finally
        (prn "Exiting dispatch thread"))))

  (create-browser [400 400]
                  "http://www.google.com"
                  dispatch-main
                  {:on-after-created
                   (fn [browser]
                     (prn "created" ))
                   :on-before-close
                   (fn [browser]
                     (prn "before close" ))
                   :on-paint
                   (fn [browser paint-type nrects rects buffer width height]
                     (prn "painting"))})
  
  ,)

