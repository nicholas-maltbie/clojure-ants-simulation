(ns demo.ant
  (:require [demo.util :refer [bound rank-by roulette]]
            [demo.world :as world]))

(defn drop-food [place]
  (-> place (update :food inc) (update :ant dissoc :food)))

(defn trail [place]
  (update place :pher #(if (:home place) % (inc %))))

(defn move [from-place to-place]
  [(trail (dissoc from-place :ant))
   (assoc to-place :ant (:ant from-place))])

(defn take-food [place]
  (-> place (update :food dec) (assoc-in [:ant :food] true)))

(defn turn [place amount]
  (update-in place [:ant :dir] (comp (partial bound 8) +) amount))

; function to age an ant at a location
; will decrement the life of an ant at a given location
(defn age [place] 
  (update-in place [:ant :life] dec))

; function for an ant in a location to use some of its energy
; will decrement the energy of an ant at a given location
(defn use-energy [place]
  (update-in place [:ant :energy] dec))

; function for an ant to eat the food it is currently carrying
; the ant will dissoc the food it is carrying and gain energy
(defn eat-food [place]
  (let [eats (get-in place [:ant :food-energy])]
    (-> place (update-in [:ant :energy] (fn [val] (+ val eats))) (update :ant dissoc :food))
  )
)

; function to check if an ant at a given location is hungery
; an ant will be hungry if its energy is less than its hunger threshold
(defn is-hungry [place]
  (<= (get-in place [:ant :energy]) (get-in place [:ant :hunger-threshold])))

; function to age an ant and have it use up a point of energy
(defn do-step [place]
  (age (use-energy place)))

; kill an ant at a given location
(defn die [place]
  (println "Ant is dead")
  (dissoc place :ant))

; function to check if an ant is still alive
(defn alive [place]
    (and 
        (> (get-in place [:ant :life]) 0)
        (> (get-in place [:ant :energy]) 0)))

(def rank-by-pher (partial rank-by :pher))
(def rank-by-home (partial rank-by #(if (:home %) 1 0)))
(def rank-by-food (partial rank-by :food))
(def foraging (juxt rank-by-food rank-by-pher))
(def homing (juxt rank-by-home rank-by-pher))
(def turn-around #(turn % 4))

(defn rand-behavior [config world behavior place]
  (let [[ahead ahead-left ahead-right :as nearby] (world/nearby-places config world (:location place) (get-in place [:ant :dir]))
        ranks (apply merge-with + (behavior nearby))
        actions [#(move % ahead) #(turn % -1) #(turn % 1)]
        index (roulette [(if (:ant ahead) 0 (ranks ahead))
                         (ranks ahead-left)
                         (ranks ahead-right)])]
    ((actions index) place)))

(defn do-action [config world place]
  (let [[ahead & _] (world/nearby-places config world (:location place) (get-in place [:ant :dir]))]
    (if (is-hungry place)
      (if (get-in place [:ant :food])
        (eat-food place)
        (cond
          (pos? (:food place)) (-> place take-food eat-food)
          (pos? (:food ahead)) (move place ahead)
          :else (rand-behavior config world foraging place)
        )
      )
      (if (get-in place [:ant :food])
        (cond
          (:home place) (-> place drop-food turn-around)
          (and (:home ahead) (not (:ant ahead))) (move place ahead)
          :else (rand-behavior config world homing place))
        (cond
          (and (pos? (:food place)) (not (:home place))) (-> place take-food turn-around)
          (and (pos? (:food ahead)) (not (:home ahead))) (move place ahead)
          :else (rand-behavior config world foraging place)
        )
      )
    )
  )
)
    
(defn behave [config world place]
  (let [used-place (do-step place)]
    (cond
      (alive used-place) (do-action config world used-place)
      :else (die used-place)
    )
  )
)
