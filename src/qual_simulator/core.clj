(ns qual-simulator.core
  (:gen-class)
  (:require [clojure.core.async :refer [chan >!! <!! close!]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn mc-simulate [times simulate-once output-channel]
  (future
    (dotimes [_ times]
      (>!! output-channel (simulate-once)))
    (close! output-channel)))

(defn random-match-result [match-with-probabilities]
  (let [rnd-percent (inc (rand-int 100))
        home-team (first match-with-probabilities)
        away-team (nth match-with-probabilities 1)
        home-win-prob (nth match-with-probabilities 2)
        draw-prob (nth match-with-probabilities 3)
        away-win-prob (nth match-with-probabilities 4)]
    (cond
      (not= 100 (+ home-win-prob draw-prob away-win-prob))
      (throw (IllegalArgumentException. "Probabilities don't sum to 100%"))
      (<= rnd-percent home-win-prob)
      {:home home-team :home-points 3 :away away-team :away-points 0}
      (<= rnd-percent (+ home-win-prob draw-prob))
      {:home home-team :home-points 1 :away away-team :away-points 1}
      :else
      {:home home-team :home-points 0 :away away-team :away-points 3})))

(defn simulate-match [standings-before match-with-probabilities]
  (let [match-result (random-match-result match-with-probabilities)
        home-team (:home match-result)
        away-team (:away match-result)
        home-team-points-before (home-team standings-before)
        away-team-points-before (away-team standings-before)]
    (-> standings-before
        (assoc home-team (+ home-team-points-before (:home-points match-result)))
        (assoc away-team (+ away-team-points-before (:away-points match-result))))))

(defn simulate-qual-once [initial-standings matches-with-probabilities]
  (let [standings-with-points (reduce simulate-match 
                                      initial-standings 
                                      matches-with-probabilities)
        sorted-standings (sort-by last > standings-with-points)]
    (map first sorted-standings)))

(defn accumulate-to-distrib [acc current-result]
  (loop [current-acc acc
         remaining-teams current-result
         current-position 1]
    (if (empty? remaining-teams)
      current-acc
      (let [this-team (first remaining-teams)
            teams-tail (rest remaining-teams)
            new-position-val (inc (get-in current-acc [this-team current-position]))]
        (recur (assoc-in current-acc [this-team current-position] new-position-val)
               teams-tail
               (inc current-position))))))

(defn reduce-results [result-count results-channel]
  (let [results-seq (for [x (range 0 result-count)]
                      (<!! results-channel))
        distrib-seed {:greece {1 0 2 0 3 0 4 0 5 0 6 0}
                      :hungary {1 0 2 0 3 0 4 0 5 0 6 0}
                      :romania {1 0 2 0 3 0 4 0 5 0 6 0}
                      :finland {1 0 2 0 3 0 4 0 5 0 6 0}
                      :northern-ireland {1 0 2 0 3 0 4 0 5 0 6 0}
                      :faroe-islands {1 0 2 0 3 0 4 0 5 0 6 0}}
        distrib (reduce accumulate-to-distrib distrib-seed results-seq)]
    distrib))

(defn fmap [a-map f]
  (reduce (fn [acc [k v]] (assoc acc k (f v))) {} a-map))

(defn distribution-to-percentage [result-count dist]
  (fmap dist (fn [x] (/ (float x) result-count))))

(defn distributions-to-percentages [result-count dists]
  (fmap dists (fn [team-dist] (distribution-to-percentage result-count team-dist))))

(defn print-team-dist [[team-name team-dist]]
  (let [sorted-placements (sort-by first team-dist)]
    (str team-name " ::: " (apply str sorted-placements))))

(defn print-results [placement-dists]
  (map print-team-dist placement-dists))

(defn mc-simulate-qual [initial-standings matches-with-probabilities]
  (let [results-channel (chan 2)
        times-to-run 10000]
    (mc-simulate times-to-run 
                 (fn [] (simulate-qual-once initial-standings matches-with-probabilities))
                 results-channel)
    (print-results (distributions-to-percentages times-to-run 
                                                 (reduce-results times-to-run results-channel)))))

(def standings {:greece 0
                :hungary 0
                :romania 0
                :finland 0
                :northern-ireland 0
                :faroe-islands 0})

(def matches [[:hungary :northern-ireland 70 20 10]
              [:faroe-islands :finland 3 12 85]
              [:greece :romania 70 20 10]
              [:romania :hungary 60 27 13]
              [:finland :greece 23 32 45]
              [:northern-ireland :faroe-islands 88 10 2]
              [:faroe-islands :hungary 3 13 84]
              [:finland :romania 32 33 35]
              [:greece :northern-ireland 85 12 3]
              [:greece :faroe-islands 96 3 1]
              [:hungary :finland 35 33 32]
              [:romania :northern-ireland 79 17 4]
              [:northern-ireland :finland 20 30 50]
              [:romania :faroe-islands 95 4 1]
              [:hungary :greece 21 31 48]
              [:finland :hungary 41 33 26]
              [:northern-ireland :romania 11 24 65]
              [:faroe-islands :greece 2 7 91]
              [:faroe-islands :northern-ireland 8 22 70]
              [:greece :finland 78 16 6]
              [:hungary :romania 29 32 39]
              [:finland :faroe-islands 93 6 1]
              [:northern-ireland :hungary 21 31 48]
              [:romania :greece 29 33 38]
              [:hungary :faroe-islands 92 7 1]
              [:northern-ireland :greece 8 20 72]
              [:romania :finland 70 20 10]
              [:faroe-islands :romania 2 8 90]
              [:finland :northern-ireland 69 21 10]
              [:greece :hungary 80 15 5]])

(def example-sim-result [:greece 
                         :finland 
                         :romania 
                         :hungary 
                         :northern-ireland 
                         :faroe-islands])
