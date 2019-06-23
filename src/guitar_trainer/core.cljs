(ns ^:figwheel-hooks guitar-trainer.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]))

(def note-names ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])

(defn frequency->midi-note [frequency]
  (+ 69
     (js/Math.round
      (* 12
         (/ (js/Math.log (/ frequency 440))
            (js/Math.log 2))))))

(defn frequency->note-name [frequency]
  (let [midi-note (frequency->midi-note frequency)]
    (str (get note-names (mod midi-note 12)) " (MIDI: " midi-note ")")))

;; define your app data so that it doesn't get over-written on reload
(defonce !app-state (atom {:input-devices '()
                           :current-input nil
                           :audio-context nil
                           :analyser nil
                           :current-pitch "--"}))

(defn set-current-input [device-id]
  (swap! !app-state assoc :current-input device-id))

(defn set-input-devices [input-devices]
  (swap! !app-state assoc :input-devices input-devices))

(defn set-current-pitch [note frequency]
  (swap! !app-state assoc :current-pitch {:note note
                                          :frequency frequency}))

(defn set-analyser [analyser]
  (swap! !app-state assoc :analyser analyser))

(defn current-input []
  (:current-input @!app-state))

(defn current-audio-context []
  (:audio-context @!app-state))

(defn current-analyser []
  (:analyser @!app-state))

(defn create-audio-context []
  (let [constructor (or js/window.AudioContext
                        js/window.webkitAudioContext)]
    (constructor.)))

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
  (-> (js/navigator.mediaDevices.getUserMedia (clj->js {:audio {"deviceId" device-id}}))))

(defn input-selector-ui
  [{:keys [current-input input-devices]}]
  [:select {:value current-input
            :on-change #(set-current-input (.. % -target -value))}
   (for [{:keys [label device-id]} input-devices]
     ^{:key device-id}
     [:option {:value device-id} label])])

(defn find-fundamental-freq-from-floats [buffer sample-rate]
  (let [best-r (atom 0)
        best-k (atom -1)
        k-start 8                                     ;; frame start distance
        k-end 1000
        r-threshold 0.9                               ;; threshold for early exit
        r-success 0.0025                              ;; threshold for success case
        continue (atom true)
        buffer (js->clj (js/Array.from buffer))
        n (/ (count buffer) 2)                        ;; buffer size / 2
        first-frame (take n buffer)
        rms (js/Math.sqrt (/ (reduce (fn [rms x]
                                       (+ rms
                                          (* x x)))
                                     0
                                     buffer)
                             (count buffer)))]
    (when (> rms 0.01)
      (doseq [k (range (inc k-end) k-start -1)
              :while @continue]
        (let [second-frame (drop k buffer)
              frame-pairs (map (fn [b1 b2] [b1 b2])
                               first-frame
                               second-frame)
              sum (reduce (fn [sum [b1 b2]]
                            (+ sum
                               (js/Math.abs
                                (- b1
                                   b2))))
                          0
                          frame-pairs)
              r (- 1 (/ sum n))]
          (when (> r @best-r)
            (reset! best-r r)
            (reset! best-k k))
          #_(when (> r r-threshold)
              (println ">>> found above threshold" r r-threshold)
              (reset! continue false))))
      (println "max/min " (apply max first-frame)
               (apply min first-frame)
               " , freq: "
               (/ sample-rate @best-k)))
    (when (> @best-r r-success)
      (/ sample-rate @best-k))))

(defn find-fundamental-freq-from-bytes [buffer sample-rate]
  (let [k-start 20                                    ;; frame start distance
        k-end 1000
        r-threshold 0.02                              ;; threshold for early exit
        r-success 0.001                               ;; threshold for success case
        normalise #(/ (- % 128) 128)
        buffer (map normalise (js/Array.from buffer))
        n (/ (count buffer) 2)                        ;; buffer size / 2
        first-frame (take n buffer)
        [best-r best-k] (loop [k k-start
                               best-r 0
                               best-k -1
                               second-frame (drop k buffer)]
                          (let [frame-pairs (map *
                                                 first-frame
                                                 second-frame)
                                sum (apply + frame-pairs)
                                r (/ sum n)]
                            (cond
                              (> k k-end)
                              [best-r best-k]

                              (> r r-threshold)
                              (do
                                (println ">>> found above threshold" r r-threshold)
                                [r k])

                              (> r best-r)
                              (recur (inc k) r k (rest second-frame))

                              :else
                              (recur (inc k) best-r best-k (rest second-frame)))))]

    (when (> best-r r-success)
      (/ sample-rate best-k))))

(defn find-closest-note [fundamental-freq]
  {:note (frequency->note-name fundamental-freq)})

(defn detect-pitch []
  (let [audio-context (current-audio-context)
        analyser (current-analyser)
        buffer (js/Uint8Array. (.-fftSize analyser))
        _ (.getByteTimeDomainData analyser buffer)
        fundamental-freq (find-fundamental-freq-from-bytes buffer (.-sampleRate audio-context))
        ;buffer (js/Float32Array. (.-fftSize analyser))
        ;_ (.getFloatTimeDomainData analyser buffer)
        ;fundamental-freq (find-fundamental-freq-from-floats buffer (.-sampleRate audio-context))
        ]
    (if fundamental-freq
      (let [{:keys [note frequency]} (find-closest-note fundamental-freq)
            ;; TODO add distance from target here
            ]
        (set-current-pitch note fundamental-freq))
      (do
        (set-current-pitch nil fundamental-freq)))
    (swap! !app-state assoc
           :timestamp (.getTime (js/Date.)))
    (js/window.requestAnimationFrame detect-pitch)))

(defn reset-audio-processing!
  [_]
  (println ">>> resetting audio processing")
  ;; TODO close exisiting resources
  (when-not (current-audio-context)
    (get-audio-context))
  (-> (get-audio-input+ (current-input))
      (.then (fn [stream]
               (let [context (current-audio-context)
                     source (.createMediaStreamSource context stream)
                     analyser (.createAnalyser context)]
                 (set-analyser analyser)
                 (set! (.-fftSize analyser) 2048)
                 (.connect source analyser)
                 (.connect source (.-destination context))
                 (detect-pitch))))))

(defn start-ui []
  [:button {:on-click reset-audio-processing!}
   "Start"])

(defn note-ui [{:keys [note frequency]}]
  (let [default-message "--"]
    [:div
     [:h2 "Current note: " (or note default-message)]
     [:h2 "Current frequency: " (or frequency default-message)]]))

(defn main-ui []
  (let [{:keys [current-pitch] :as app-state} @!app-state]
    [:div
     [:h1 "Guitar Trainer"]
     [input-selector-ui app-state]
     [start-ui]
     [note-ui current-pitch]
     #_[:p (str app-state)]]))

(defn mount [el]
  (reagent/render-component [main-ui] el))

(defn get-app-element []
  (gdom/getElement "app"))

(defn init! []
  (-> (get-input-devices+)
      (.then #(set-current-input (first %)))))

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
