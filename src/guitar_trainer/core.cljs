(ns ^:figwheel-hooks guitar-trainer.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

;; define your app data so that it doesn't get over-written on reload
(defonce !app-state (atom {}))

(defn create-audio-context []
  (let [constructor (or js/window.AudioContext
                        js/window.webkitAudioContext)]
    (constructor.)))

(defn get-audio-context! [app-state]
  (swap! app-state assoc :audio-context (create-audio-context)))

(defn enumerate-inputs+ []
  (-> (js/navigator.mediaDevices.enumerateDevices)
      (.then (fn [devices]
               (->> (js->clj devices {:keywordize-keys true})
                    (map (fn [device]
                           {:label (.-label device)
                            :device-id (aget device "deviceId")})))))))

(defn get-inputs! [app-state]
  (-> (enumerate-inputs+)
      (.then #(swap! app-state assoc :input-devices %))))


(defn get-audio-input+ [device-id]
  (-> (js/navigator.mediaDevices.getUserMedia (clj->js {:audio {"deviceId" device-id}
                                                        :video false}))))

(defn process-audio
  [audio-processing-event]
  (let [input-buffer (aget audio-processing-event "inputBuffer")
        output-buffer (aget audio-processing-event "outputBuffer")
        ;; FIXME converting the TypedArray like this might be too expensive
        input-data (js->clj (js/Array.from (.getChannelData input-buffer 0)))
        output-data (.getChannelData output-buffer 0)]
    (println "> num of chans " (.-numberOfChannels input-buffer)
             ">  input data first " (first input-data))

    (swap! !app-state assoc :input-avg
           (/ (apply + input-data)
              (count input-data))
           :timestampe (.getTime (js/Date.)))))

(defn init! [app-state]
  (get-audio-context! app-state)
  (get-inputs! app-state)
  (-> (get-audio-input+ "default")
      (.then (fn [stream]
               (let [context (get @app-state :audio-context)
                     source (.createMediaStreamSource context stream)
                     processor (.createScriptProcessor context 4096 1 1)]
                 (.connect source processor)
                 (.connect processor (.-destination context))
                 (set! (.-onaudioprocess processor)
                       process-audio))))))

(init! !app-state)

(defn get-app-element []
  (gdom/getElement "app"))

(defn main-ui [app-state]
  [:div
   [:h1 "Guitar Trainer"]
   [:p (str @app-state)]])

(defn mount [el]
  (reagent/render-component [main-ui !app-state] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
