(ns demo.ui.queen
  (:require [demo.ui.core :as ui]
            [demo.util :as util]))

(def directions
  {0 [2 0 2 4]
   1 [4 0 0 4]
   2 [4 2 0 2]
   3 [4 4 0 0]
   4 [2 4 2 0]
   5 [0 4 4 0]
   6 [0 2 4 2]
   7 [0 0 4 4]})

(defn queen-color [queen]
  (if (:food queen) :purple :blue))

(defn next-loc [dir loc config]
  (-> dir directions (util/delta (util/scale loc (:scale config)))))

(defn render-queen [queen img config x y]
  (ui/make-line img {:color (queen-color queen)
                     :border (next-loc (:dir queen) [x y] config)}))
