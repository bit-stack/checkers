(ns checkers.core)

(defn create-checkerboard
  "creates a checkerboard.
   On large checkboards, a map implementation would be more memory efficient,
   but I assume this will never be used in such cases where memory would be a concern.
   I also assume this will not be used with non-sensical checkerboards dimensions
   (0 by 0, 1 by 0, -10 by -10, etc."
  [num-rows num-cols]
  (let [row (apply vector (take num-cols (repeat nil)))]
    (apply vector (take num-rows (repeat row)))))

(defn set-in-checkerboard
  "sets the value of the position given by row/column to val"
  [checkerboard row col val]
  (assoc checkerboard row
                      (assoc (checkerboard row) col val)))

(defn get-piece
  "returns the piece at the current row-col position in the checkerboard"
  [checkerboard row col]
  ((checkerboard row) col))

(defn enemy-of
  "returns the enemy piece of piece"
  [piece]
  (cond
    (= piece :white) :black
    (= piece :black ) :white
    :else nil))

(defn valid-pos?
  "returns true if the position exists in the checkerboard"
  [checkerboard row col]
  (let [num-rows (count checkerboard)
        num-cols (count (checkerboard 0))]
    (and (not-any? neg? [row col])
         (< row num-rows)
         (< col num-cols))))

(defn jump-pos
  "Gets the theoretical position the piece would end up in if it jumped the enemy piece.
   Assumes the pieces are in adjacent positions (i.e. piece can jump the enemy piece)"
  [[row col] [enemy-row enemy-col]]
  [(+ enemy-row (- enemy-row row))
   (+ enemy-col (- enemy-col col))])

(defn adjacent-enemies-of
  "returns the positions of all adjacent enemies of the piece (if there is any) at row col
   in the format [[row col] ...]"
  [checkerboard row col]
  (let [enemy-piece (enemy-of (get-piece checkerboard row col))]
    (into [] (filter (fn [[r c]] (and (valid-pos? checkerboard r c)
                                      (= enemy-piece (get-piece checkerboard r c))))
                     [[(dec row) (inc col)]
                      [(dec row) (dec col)]
                      [(inc row) (inc col)]
                      [(inc row) (dec col)]]))))

(defn possible-jumps-of
  "Gets the possible jumps of the piece at the row-column"
  [checkerboard row col]
  (if-let [piece (get-piece checkerboard row col)]
    (into [] (filter (fn [[r c]] (and (valid-pos? checkerboard r c)
                                      (nil? (get-piece checkerboard r c))))
                     (map (partial jump-pos [row col]) (adjacent-enemies-of checkerboard row col))))
    []))

(def test-checkerboard0
  (-> (create-checkerboard 10 10)
      (set-in-checkerboard 5 5 :white)
      (set-in-checkerboard 4 4 :black)
      (set-in-checkerboard 3 3 :black)))

(def test-checkerboard1
  (-> (create-checkerboard 10 10)
      (set-in-checkerboard 5 5 :white)
      (set-in-checkerboard 4 4 :black)
      (set-in-checkerboard 6 6 :white)
      (set-in-checkerboard 3 3 :black)
      (set-in-checkerboard 6 4 :black)))

(def test-checkerboard2
  (-> (create-checkerboard 10 10)
      (set-in-checkerboard 0 1 :white)
      (set-in-checkerboard 1 0 :black)
      (set-in-checkerboard 2 1 :white)
      (set-in-checkerboard 9 8 :black)
      (set-in-checkerboard 8 9 :white)))

(def test-checkerboard3
  (-> (create-checkerboard 10 10)
      (set-in-checkerboard 5 5 :black)
      (set-in-checkerboard 4 4 :white)
      (set-in-checkerboard 6 6 :white)
      (set-in-checkerboard 6 4 :white)
      (set-in-checkerboard 4 6 :white)
      (set-in-checkerboard 3 3 :black)))

(def test-checkerboard4
  (-> (create-checkerboard 10 10)
      (set-in-checkerboard 5 5 :white)))


(defn -main [& args]
  ;; Check that nil doesn't have any possible jumps
  (assert (= [] (possible-jumps-of test-checkerboard0 7 7)))

  ;; Check lonely piece doesn't have any jumps
  (assert (= [] (possible-jumps-of test-checkerboard4 5 5)))

  ;; Check that jumps to already occupied positions are not possible
  (assert (= [] (possible-jumps-of test-checkerboard0 5 5)))

  ;; Corner check
  (assert (= [] (possible-jumps-of test-checkerboard2 0 1)))
  (assert (= [] (possible-jumps-of test-checkerboard2 8 9)))
  (assert (= [] (possible-jumps-of test-checkerboard2 9 8)))
  (assert (= [[3 2]] (possible-jumps-of test-checkerboard2 1 0)))

  ;; Overall check for correctness
  (assert (= [[7 3]] (possible-jumps-of test-checkerboard1 5 5)))
  (assert (= (set [[7 7] [3 7] [7 3]]) (set (possible-jumps-of test-checkerboard3 5 5))))

  (println "All tests passed!"))


