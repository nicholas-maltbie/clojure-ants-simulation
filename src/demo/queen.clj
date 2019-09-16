(ns demo.queen
  (:require [demo.ant :as ant]
            [demo.util :refer [bound rank-by roulette]]
            [demo.world :as world]
            [demo.domain :as domain]))

(defn can-spawn-ant [place]
  (and
    (>= (get-in place [:queen :held-food]) (get-in place [:queen :spawn-threshold]))
    (not (:ant place))
  )
)

(defn trail [place]
  (update place :pher #(if (:home place) % (inc %))))

(defn move [from-place to-place]
  [(trail (dissoc from-place :queen))
   (assoc to-place :queen (:queen from-place))])

(defn ant-loop [location state]
  (Thread/sleep (get-in state [:config :ant-sleep-ms]))
  (dosync
   (send-off *agent* ant-loop state)
   (let [world (:world state)
         place @(get-in world location)
         new-places (flatten [(ant/behave (:config state) world place)])]
     (doseq [new-place new-places]
       (ref-set (get-in world (:location new-place)) new-place))
     (-> new-places last :location))))

(defn spawn-ant [place]
  (let [needed (get-in place [:queen :spawn-threshold])
        state (get-in place [:queen :spawn-state])
        updated (assoc (update-in place [:queen :held-food] (fn [val] (- val needed)))
          :ant (domain/build-ant 
            {
              :dir (rand-int 8)
              :life (+ (get-in state [:config :base-life]) (rand-int (get-in state [:config :life-random]))) 
              :energy (get-in state [:config :start-enery])
              :hunger-threshold (get-in state [:config :hunger-threshold])
              :agent (agent (:location place))
              :food-energy (get-in state [:config :food-energy])
            }
          )
        )]
      (println "Spawning a new ant")
      (send-off (get-in updated [:ant :agent]) ant-loop state)
      updated
  )
)

(defn take-food [place]
  (-> place (update :food dec) (update-in [:queen :held-food] inc)))

(defn turn [place amount]
  (update-in place [:queen :dir] (comp (partial bound 8) +) amount))

(def rank-by-pher (partial rank-by :pher))
(def rank-by-home (partial rank-by #(if (:home %) 1 0)))
(def rank-by-food (partial rank-by :food))
(def homing (juxt rank-by-home rank-by-pher))

(defn rand-behavior [config world behavior place]
  (let [[ahead ahead-left ahead-right :as nearby] (world/nearby-places config world (:location place) (get-in place [:queen :dir]))
        ranks (apply merge-with + (behavior nearby))
        actions [#(move % ahead) #(turn % -1) #(turn % 1)]
        index (roulette [(if (not (:home ahead)) 0 (ranks ahead))
                         (ranks ahead-left)
                         (ranks ahead-right)])]
    ((actions index) place)))

(defn behave [config world place]
  (let [[ahead & _] (world/nearby-places config world (:location place) (get-in place [:queen :dir]))]
    (if (:home place)
      (cond
        (can-spawn-ant place) (spawn-ant place)
        (pos? (:food place)) (take-food place)
        :else (rand-behavior config world homing place)
      )
      (cond
        (:home ahead) (move place ahead)
        :else (rand-behavior config world homing place))
    )
  )
)
