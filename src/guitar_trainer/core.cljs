(ns ^:figwheel-hooks guitar-trainer.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

;; define your app data so that it doesn't get over-written on reload
(defonce !app-state (atom {:input-devices '()
                           :current-input nil
                           :audio-context nil}))

(defn set-current-input [device-id]
  (swap! !app-state assoc :current-input device-id))

(defn current-input []
  (:current-input @!app-state))

(defn set-input-devices [input-devices]
  (swap! !app-state assoc :input-devices input-devices))

(defn create-audio-context []
  (let [constructor (or js/window.AudioContext
                        js/window.webkitAudioContext)]
    (constructor.)))

(defn current-audio-context []
  (:audio-context @!app-state))

(defn get-audio-context []
  (swap! !app-state assoc :audio-context (create-audio-context)))

(defn enumerate-devices+ []
  (-> (js/navigator.mediaDevices.enumerateDevices)
      (.then (fn [devices]
               (->> (js->clj devices {:keywordize-keys true})
                    (map (fn [device]
                           {:label (.-label device)
                            :device-id (aget device "deviceId")
                            :kind (.-kind device)})))))))

(defn get-input-devices+ []
  (-> (enumerate-devices+)
      (.then (fn [devices]
               (let [input-devices (filter #(= "audioinput" (:kind %)) devices)]
                 (set-input-devices input-devices)
                 input-devices)))))

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
           :timestamp (.getTime (js/Date.)))))

(defn init! []
  (get-audio-context)
  (-> (get-input-devices+)
      (.then #(set-current-input (first %)))))

(defn get-app-element []
  (gdom/getElement "app"))

(defn input-selector-ui
  [{:keys [current-input input-devices]}]
  [:select {:value current-input
            :on-change #(set-current-input (.. % -target -value))}
   (for [{:keys [label device-id]} input-devices]
     ^{:key device-id}
     [:option {:value device-id} label])])

(defn reset-audio-processing!
  [_]
  ;; TODO close exisiting resources
  (-> (get-audio-input+ (current-input))
      (.then (fn [stream]
               (let [context (current-audio-context)
                     source (.createMediaStreamSource context stream)
                     processor (.createScriptProcessor context 4096 1 1)]
                 (.connect source processor)
                 (.connect processor (.-destination context))
                 (set! (.-onaudioprocess processor)
                       process-audio)))))
  )

(defn start-ui []
  [:button {:on-click reset-audio-processing!}
   "Start"])

(defn main-ui []
  (let [{:keys [] :as app-state} @!app-state]
    [:div
     [:h1 "Guitar Trainer"]
     [input-selector-ui app-state]
     [start-ui]
     [:p (str app-state)]]))

(defn mount [el]
  (reagent/render-component [main-ui] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (init!)
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
