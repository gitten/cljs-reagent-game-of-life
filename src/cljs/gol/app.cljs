(ns gol.app
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]))


(def generation (atom 0))
(def world-width (atom 50))
(def world-height (atom 30))

(def pixel-width (atom 15))

(def running? (atom false))


;;;;
;;;; World Definitions
;;;;


(defn make-world [world-state]
  (into []
        (map vector
             world-state
             (for [y (range @world-height)
                   x (range @world-width)]
               [x y]))))


(def init-world
  (make-world (replicate (* @world-width @world-height) 0)))


(defn rand-world []
  (let [max-pop (* @world-width @world-height)]
    (make-world (repeatedly max-pop #(rand-int 2)))))

(def world (atom rand-world))


;;;;
;;;; World Processing Definitions
;;;;

;; index->xy-cord : Number -> Number -> Vec(pair)
(defn index->xy-cord [idx world-width world-height]
  (let [x (mod idx world-width)
        y (quot idx world-height)]
    [x, y]))


;; xy-cord->index : Vec -> Number -> Number
(defn xy-cord->index [[x y] world-width]
  (+ x (* y world-width)))


;; valid-naybor? : Number -> Number -> Number -> Bool
(defn valid-naybor? [x y world-width world-height]
  (and (< x world-width)
       (< y world-height)
       (>= x 0)
       (>= y 0)))


;; get-naybor-xys : Vec(pair) -> Number -> Seq
(defn get-naybor-indices [[x y] world-width world-height]
  (let [x-transform [-1 0 1 -1 1 -1 0 1]
        y-transform [-1 -1 -1 0 0 1 1 1]
        xy-transform (map vector x-transform y-transform)]
    (for [[xt yt] xy-transform
          :let [naybor-x (+ x xt)
                naybor-y (+ y yt)]
          :when (valid-naybor? naybor-x
                               naybor-y
                               world-width
                               world-height)]
      (xy-cord->index [naybor-x naybor-y] world-width))))


;; the-reckoning : Seq(indices) -> Vec(world) -> Number
(defn the-reckoning [naybor-indices world]
  (letfn [(count-heads [indices world acc]
            (if (empty? indices) acc
                (let [[cell-population _] (world (first indices))]
                    (recur (rest indices)
                           world
                           (+ acc cell-population)))))]
    (count-heads naybor-indices world 0)))


;; god-finger: Number -> Number -> Number
(defn god-finger [target-population naybor-population]
  (let [still-living? (and (= 1 target-population)
                           (= 2 naybor-population))
        birth? (= 3 naybor-population)]
    (if (or still-living? birth?) 1 0)))


;; hand-of-god : Vec(cell)-> Vec(world) -> Number -> Vec(cell)
(defn hand-of-god [world-cell world naybor-indices]
  (let [cell-pop (world-cell 0)
        xy-cord (world-cell 1)
        naybor-pop (the-reckoning naybor-indices world)]
    [(god-finger cell-pop naybor-pop) xy-cord]))


;; a-new-world : Vec(world) -> Number -> Number -> Vec(world)
(defn a-new-world [world world-width world-height]
  (into []
        (for [cell world
              :let
              [naybors (get-naybor-indices (get cell 1)
                                           world-width
                                           world-height)]]
          (hand-of-god cell world naybors))))


;;;;
;;;; Rendering Components
;;;;

(defn toggle-life [current-state index world x y]
  (assoc world
         index
         [(if (= "white" current-state) 1 0)
          [x y]]))


(defn set-pixel [index a-world px-width]
  (let [[state [x y]] (@a-world index)
        alive? (if (= state 1) "black" "white")]
   [:rect
    {:x (* x @px-width)
     :y (* y @px-width)
     :width @px-width
     :height @px-width
     :fill alive?
     :on-click
     #(reset! a-world (toggle-life alive? index @a-world x y))}]))


(defn population [a-world]
  (let [max-pop (* @world-width @world-height)]
    [:svg
     (for [index (range max-pop)]
       ^{:key index} [set-pixel index a-world pixel-width])]))


(defn x-grid-lines []
  [:svg
   (for [x (range (+ 1 @world-width))
        :let [x-val (* x @pixel-width)]]
         [:line {:x1 x-val
                 :x2 x-val
                 :y1 0
                 :y2 (* @pixel-width @world-height)
                 :stroke-width 1
                 :stroke :grey}])])


(defn y-grid-lines []
  [:svg
   (for [y (range (+ 1 @world-height))
        :let [y-val (* y @pixel-width)]]
         [:line {:x1 0
                 :x2 (* @pixel-width @world-width)
                 :y1 y-val
                 :y2 y-val
                 :stroke-width 1
                 :stroke :grey}])])


(defn world-view [a-world]
  [:div
   [:svg {:width (* @world-width @pixel-width)
          :height (* @world-height @pixel-width)}
    [population a-world]
    [x-grid-lines]
    [y-grid-lines]]])


(defn generation-stepper [world-ratom]
  [:button
   {:on-click #(reset! world-ratom (a-new-world @world-ratom @world-width @world-height))}
   "Step Generation"])


(defn run-state [world-ratom]
  (let [next-state (if @running? "Pause?" "Start?(notworking)")]
    [:button
     {:on-click #(swap! running? not)}
     next-state]))


(defn clear-world [world-ratom]
  [:button
   {:on-click #(reset! world-ratom init-world)}
   "CLEAR"])


(defn random-world [world-ratom]
  [:button
   {:on-click #(reset! world-ratom (rand-world))}
   "Random Population"])


(defn generation-controller [world-ratom]
  [:div "Simulation Controlls:"
   [:div
    [run-state world-ratom]
    [generation-stepper world-ratom]
    [random-world world-ratom]
    [clear-world world-ratom]]])


(defn app-view [world]
  [:div
   [generation-controller world]
   [world-view world]])


(defn init []
  (reagent/render-component [app-view world]
                            (.getElementById js/document "container")))


(defn more-button
  [counter]
  [:button
   {:class "button-class"
    :on-click #(swap! counter inc)}
   "more"])


