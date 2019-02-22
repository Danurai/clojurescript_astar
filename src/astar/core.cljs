(ns astar.core
  (:require 
    [astar.grids :as grids]
    [astar.maps  :as maps]
    [astar.model :refer [appstate]]
    [reagent.core :as r]))
    
(enable-console-print!) 
    
(def ^:const size 15)
(def ^:const center [10 10])

(defn- get_paths [ orig goal ]
  (let [path_fn grids/astar] ;breadth_first breadth_first_early greedy_breadth_first uniform_cost astar
    (path_fn orig goal (:board @appstate))))

                
(swap! appstate assoc :board maps/map1  ;terrainmap map1 blockedmap
                      :paths nil
                      :path_home nil)
(swap! appstate assoc :orig (-> @appstate :board :start))

(defn- mousemove [ evt ]
  (let [target (.-target evt)
        de     (.-documentElement js/document)
        mx     (-> (.-clientX evt) (- (.-offsetLeft target)) (+ (.-scrollLeft de)))
        my     (-> (.-clientY evt) (- (.-offsetTop  target)) (+ (.-scrollTop  de)))
        cell   (grids/get_pixel_cell center [mx my] size)
        [x y]  cell]
    (if (and 
          (< -1 x (-> @appstate :board :width))
          (< -1 y (-> @appstate :board :height))
          (not= cell (:cell @appstate)))
          (let [paths (get_paths (:orig @appstate) cell)]
            (swap! appstate assoc :cell cell
                                  :paths paths
                                  :path_home (grids/path_to_cell cell paths)
                                  )))))
                                  
;(defn- mouseclick [ evt ]
;  (let [[x y] (:cell @appstate)]
;    (if (and (< -1 x (-> @appstate :board :width)) (< -1 y (-> @appstate :board :height)))
;        (if (some #(= (:cell @appstate) %) (-> @appstate :board :blocked))
;          (let [cell (:cell @appstate)
;                paths (get_paths (:orig @appstate) cell)]
;            (swap! appstate assoc-in [:board :blocked] (remove #(= cell %) (-> @appstate :board :blocked)))
;            (swap! appstate assoc :paths paths
;                                  :path_home (grids/path_to_cell cell paths)))
;          (do 
;            (swap! appstate update-in [:board :blocked] conj (:cell @appstate))
;            (swap! appstate dissoc :paths :path_home))))))
      
(defn- mouseclick [ evt ]
  (let [[x y] (:cell @appstate)]
    (if (and (< -1 x (-> @appstate :board :width)) (< -1 y (-> @appstate :board :height)))
        (swap! appstate assoc :orig (:cell @appstate)
                              :paths (get_paths (:orig @appstate) (:cell @appstate))
                              :path_home nil))))

(defn- draw_grid [ ctx origin cellsize celldims ]
  (doseq [x (-> celldims first range) y (-> celldims second range)]
    (grids/draw_cell ctx origin cellsize [x y])))
    
(defn- draw_line [ ctx orig grid ]
  (set! (.-strokeStyle ctx) "#DDDDDD")
  (set! (.-lineWidth ctx) "5")
  (.beginPath ctx)
  (let [[x y] (grids/get_cell_pixel orig center size)]
    (.moveTo ctx x y))
  (let [[x y] (grids/get_cell_pixel grid center size)]
    (.lineTo ctx x y))
  (.stroke ctx)
  ; draw points
  (set! (.-fillStyle ctx) "#EF0000")
  (set! (.-strokeStyle ctx) "#FFF")
  (set! (.-lineWidth ctx) "1")
  (doseq [cell (grids/vec_linedraw_noround orig grid)]
    (.beginPath ctx)
    (let [[x y] (grids/get_cell_pixel cell center size)]
      (.arc ctx x y (/ size 10) 0 (* 2 Math/PI))
      (.fill ctx)
      (.stroke ctx)))
  
  )
  
(defn- draw_path [ ctx orig size path ] 
  (let [coords (map #(grids/get_cell_pixel % orig size) path)]
    (.beginPath ctx)
    (let [[x y] (first coords)]
      (.moveTo ctx x y))
    (doseq [[x y] (rest coords)]
      (.lineTo ctx x y))
    (set! (.-strokeStyle ctx) "rgba(100,0,100,0.5)")
    (set! (.-lineWidth ctx) "5")
    (.stroke ctx)))
      
(defn draw_page [ canvas ]
  (let [ctx (.getContext canvas "2d")
        w   (.-clientWidth  canvas) 
        h   (.-clientHeight canvas)]
    (.clearRect ctx 0 0 w h)
    
    (draw_grid ctx center size (-> @appstate :board vals))
    (draw_path ctx center size (:path_home @appstate))
    ;(draw_line ctx (:orig @appstate) (:cell @appstate))
    
    ))
    
(defn astarclass [ ]
  (let [dom-node (r/atom nil)]
    (r/create-class
     {:component-did-update
        (fn [ this ]
          (draw_page (.getElementById js/document "drawing")))
          ;(draw_page (.-firstChild @dom-node)))
      :component-did-mount
        (fn [ this ]
          (reset! dom-node (r/dom-node this)))
      :reagent-render
        (fn [ ]
          @appstate
          [:div.container.my-1
            [:canvas#drawing.border (if-let [node @dom-node] 
             {:width "1000px" 
              :height "600px"
              :on-mouse-move mousemove
              :on-click      mouseclick
              :on-mouse-out  #(swap! appstate dissoc :mx :my)})]
            ;[:div (str @appstate)]
            ])})))


(r/render-component [astarclass] (.getElementById js/document "app"))