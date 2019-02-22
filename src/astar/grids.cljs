(ns astar.grids
  (:require 
    [astar.model :refer [appstate]]
    [astar.htmlcolours :refer [col_to_rgb]]
    [tailrecursion.priority-map :refer [priority-map priority-map-by]]))
    
(def ^:const directions (reverse [[0 -1] [-1 0] [0 1] [1 0]])) ;even, reverse for odd

(defn- grid_add [ a b ]
  (mapv #(+ %1 %2) a b))
(defn- grid_neighbours [ grid ]
  (let [neighbours (if (zero? (mod (apply + grid) 2)) directions (reverse directions))] ; hack direction order for zig-zag paths
    (mapv #(grid_add grid (nth neighbours %)) (range 4))))  
  
; AStar
(defn- in-bounds? [ cell graph ] 
  (let [{:keys [width height]} graph
        [x y]                  cell]
    (and (<= 0 x (dec width)) (<= 0 y (dec height)))))
    
(defn- passable? [ cell graph ]
  (not-any? #(= cell %) (:blocked graph)))
  
(defn- not_yet_visited [ neighbours visited graph ]
  (->> neighbours 
       (filter #(in-bounds? % graph))
       (filter #(passable?  % graph))
       (remove #(some? (visited %)))
       vec))

(defn breadth_first [ start goal graph ]
  (loop [frontier  [start]
         came_from {start false}]    ; Origin {[x x] false}
    (if (empty? frontier)
        came_from
        (let [fronts (not_yet_visited (-> frontier first grid_neighbours) came_from graph)]
          (recur (apply conj (-> frontier rest vec) fronts) (apply merge came_from (map #(hash-map % (first frontier)) fronts)))))))

(defn breadth_first_early [ start goal graph ]
  (loop [frontier  [start]
         came_from {start false}]    ; Origin {[x x] false}
    (if (or (empty? frontier)
            (= goal (first frontier)))
        came_from
        (let [fronts (not_yet_visited (-> frontier first grid_neighbours) came_from graph)]
          (recur (apply conj (-> frontier rest vec) fronts) (apply merge came_from (map #(hash-map % (first frontier)) fronts)))))))
          

(defn- best_path? [from_cell to_cell cost_so_far graph]
  (let [old_cost (get cost_so_far to_cell)
        new_cost (+ (get cost_so_far from_cell) (-> graph :costs (get to_cell 1)))]
    (or (nil? old_cost) (< new_cost old_cost))))
    
(defn- priority_visited [ cell cost_so_far graph ]
  (->> (grid_neighbours cell)
       (filter #(in-bounds? % graph))
       (filter #(passable? % graph))
       ;; Filter out {cell cost} if cost >= cost_so_far
       (filter #(best_path? cell % cost_so_far graph))       
       (map #(priority-map % (+ (get cost_so_far cell 1) (get (:costs graph) % 1))))
       reverse
       (apply conj)))
         
(defn uniform_cost [start goal graph]
; FIXME - out of bounds and blocked start should return empty {}
  (loop [frontier (priority-map start 0)
         came_from (hash-map start false)
         cost_so_far (hash-map start 0)]
    (if (or (empty? frontier)
            (= goal (-> frontier first key)))
        came_from
        (let [cell      (-> frontier first key)
              front     (priority_visited cell cost_so_far graph)
              new_frontier (dissoc frontier cell)]
          (recur (if (empty? front) new_frontier (conj new_frontier front))
                 (apply merge came_from (map #(hash-map (key %) cell) front))
                 (if (empty? front) cost_so_far (conj cost_so_far front)))))))

(defn- heuristic 
  "Manhattan distance on a square grid"
  ; return abs(a.x - b.x) + abs(a.y - b.y)
  [ a b ]
    (apply + (map #(-> %1 (- %2) Math/abs) a b)))

(defn- greedy_priority_visited [ cell goal came_from graph ]
  (->> (grid_neighbours cell)
       (remove #(some? (came_from %)))
       (filter #(in-bounds? % graph))
       (filter #(passable? % graph))
       (map #(priority-map % (heuristic % goal)))
       reverse
       (apply conj)))
       
(defn greedy_breadth_first [ start goal graph ]
; FIXME - out of bounds and blocked start should return empty {}
  (if (and (in-bounds? goal graph) (passable? goal graph))
    (loop [frontier (priority-map start 0)
           came_from (hash-map start false)]
      (if (or (= goal (-> frontier first key))
              (empty? frontier))
          came_from
          (let [cell      (-> frontier first key)
                front     (greedy_priority_visited cell goal came_from graph)
                new_frontier (dissoc frontier cell)]
            (recur (if (empty? front) new_frontier (conj new_frontier front))
                   (apply merge came_from (map #(hash-map (key %) cell) front))
                   ))))))
                   
(defn astar [start goal graph]
; FIXME - out of bounds and blocked start should return empty {}
  (if (and (in-bounds? goal graph) (passable? goal graph))
    (loop [frontier (priority-map start 0)
           came_from (hash-map start false)
           cost_so_far (hash-map start 0)]
      (if (or (= goal (-> frontier first key))
              (empty? frontier))
          came_from
          (let [cell          (-> frontier first key)
                new_costs     (priority_visited cell cost_so_far graph)
                fronts        (apply merge (map (fn [[k v]] (priority-map k (+ v (heuristic k goal)))) new_costs))
                new_frontier  (dissoc frontier cell)]
            (recur (if (empty? fronts) new_frontier (conj new_frontier fronts))
                   (apply merge came_from (map #(hash-map (key %) cell) new_costs))
                   (if (empty? new_costs) cost_so_far (conj cost_so_far new_costs))))))))
              
(defn path_to_cell [ cell paths ]
  (if (->> cell (get paths) some?)
    (loop [path [cell]]
      (let [from (last path)
            step (get paths from)]
        (if (or (nil? step) (false? step))
            (reverse path)
            (recur (conj path step)))))))
    
(defn- round_cell [ cell ]
  (map #(Math/round %) cell))
  
(defn get_cell_pixel [ cell orig size ]
  (map #(-> %1 (* size) (+ (/ size 2)) (+ %2)) cell orig))
(defn get_pixel_cell [ orig pixel size ]
  (round_cell
    (map #(/ (reduce - [%1 %2 (/ size 2)]) size) pixel orig)))
     
(defn grid_distance_diags [ a b ]
  (apply Math/max
    (map #(-> %1 (- %2) Math/abs) a b)))
(defn grid_distance_nodiags [ a b ]
  (reduce +
    (map #(-> %1 (- %2) Math/abs) a b)))
    
(defn- lerp [ a b t ]
  (-> b (- a) (* t) (+ a)))
(defn- vec_lerp [ a b t ]
  (map #(lerp (nth a %) (nth b %) t) (-> a count range)))
(defn- vec_linedraw [ a b ]
  (let [N  (grid_distance_diags a b)]
    (map #(round_cell (vec_lerp a b (-> 1 (/ N) (* %)))) (range (inc N)))))
(defn vec_linedraw_noround [ a b ]
  (let [N  (grid_distance_diags a b)]
    (map #(vec_lerp a b (-> 1 (/ N) (* %))) (range (inc N)))))
    
(defn- fill_text [ ctx orig size cell text ]
  (set! (.-fillStyle ctx) (col_to_rgb :darkslategray))
  (set! (.-textAlign ctx) "center")
  (set! (.-textBaseline  ctx) "middle")
  (set! (.-font      ctx) "14px Sans-Serif")
  (let [[x y] (get_cell_pixel cell orig size)]
    (.fillText ctx text x y)))
    
;(defn- grid_symbol [ cell ]
;  (let [[ax ay] cell
;        [bx by] (get (:bfs @appstate) cell)]
;     ;(str bx " " by)))
;     (if (> bx ax)
;        ">"
;        (if (< bx ax)
;            "<"
;            (if (< by ay)
;              "^"
;              (if (> by ay)
;                  "v"
;                  "."))))))

(defn- path_cost [ path board ]
  (apply + (map #(get (:costs board) % 1) (rest path))))
    
(defn draw_cell [ ctx orig size cell ] ; context [0 0] 40 [3 2]
  (let [[x y] (get_cell_pixel cell orig size)]
    (set! (.-fillStyle ctx) 
      (if (= (:orig @appstate) cell)
          (col_to_rgb :white)
          (if (some #(= cell %) (-> @appstate :board :blocked))
              (col_to_rgb :darkslategray)
              (case (get (-> @appstate :board :costs) cell)
                10 (col_to_rgb :mediumblue)
                5 (col_to_rgb :darkgreen)
                3 (col_to_rgb :darkseagreen)
                (col_to_rgb :gainsboro)))))
    
    (.beginPath ctx)
    (.rect ctx (-> x (- (/ size 2))) (-> y (- (/ size 2))) size size)
    (.fill ctx)
    
    (set! (.-strokeStyle ctx) (col_to_rgb :white))
    (set! (.-lineWidth ctx) "1")    
    (.stroke ctx)
    
    ;(let [d (-> cell (path_to_cell (:paths @appstate)) (path_cost (:board @appstate)))]
    ;  (if (> d 0) (fill_text ctx orig size cell d)))
    ))