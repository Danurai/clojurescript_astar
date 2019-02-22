(ns astar.model
  (:require
    [reagent.core :as r]))
    
(def appstate (r/atom {:orig [0 0] :board {:width 10 :height 10}}))

