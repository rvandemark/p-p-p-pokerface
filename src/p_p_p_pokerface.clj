(ns p-p-p-pokerface)

(def ranks ["2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"])

(defn rank [card]
  (+ (.indexOf ranks (str (first card))) 2))

(defn suit [card]
  (str (second card)))

(defn sorted [hand]
  (sort (map rank hand)))

(defn combo-freq [hand]
  (let [ranks-map (map rank hand)
        rank-freqs-map (frequencies ranks-map)
        rank-freqs-vals (vals rank-freqs-map)]
    (frequencies rank-freqs-vals)))

(defn increments-by-1 [cards]
  (let [sort (sorted cards)]
    (apply = 1 (map - (rest sort) sort))))

(defn combo-freq-size? [hand size]
  (contains? (set (keys (combo-freq hand))) size))

(defn pair? [hand]
  (combo-freq-size? hand 2))

(defn three-of-a-kind? [hand]
  (combo-freq-size? hand 3))

(defn four-of-a-kind? [hand]
  (combo-freq-size? hand 4))

(defn flush? [hand]
  (= (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= (get (combo-freq hand) 2) 2))

(defn straight? [hand]
  (if (= (rank (last hand)) 14)
    (and (= (rank (first hand)) 2) (increments-by-1 (take 4 hand)))
    (increments-by-1 hand)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0
    ))
