(ns drop.core
  (:require [clojure.string :refer [lower-case]])
  (:import (java.net URLEncoder)))

(def games (atom {}))

;; Pure internal game list manipulations

(defn diag [cs]
  "Major diagonal of a vector of vectors"
  (loop [cs cs
         res []]
    (if-let [cs (seq (filter seq cs))]
      (recur (doall (map next (nthrest cs 1)))
             (conj res (first (first cs))))
      res)))

(defn make-major [b row col]
  "Slice board to place row and col on major diagonal"
  (cond
    (> col row) (drop (- col row) b)
    (> row col) (map #(drop (- row col) %) b)
    :default b))

(defn win [b r c t]
  "Check last move for a win"
  (let [row (nth (apply map vector b) r)
        col (nth b r)
        f-diag (diag (make-major b r c))
        b-diag (diag (make-major
                      (reverse b) r (- (dec (count b)) c)))
        pred (if (odd? t) odd? even?)]
    (some #(every? (fn [x] (and (pred x) (pos? x))) (vec %))
          (apply concat (map #(partition 4 1 %)
                             (list row col f-diag b-diag))))))

;; STM Exceptions

(defn v-dimensions [rows cols]
  "Validate dimensions"
  (when (not (and (number? rows)
                  (number? cols)))
    (throw (ex-info "Dimensions must be numeric"
                    {:type ::ex-type, :cause :non-numeric})))
  (when (or (< rows 4)
            (< cols 4))
    (throw (ex-info "Game must have at least 4 rows and columns"
                    {:type ::ex-type, :cause :too-small})))
  true)

(defn v-player-name [name]
  "Validate player name lengths"
  (when (zero? (count name))
    (throw (ex-info "Player name cannot be null"
                    {:type ::ex-type, :cause :bad-player})))
  true)

(defn v-names-differ [one two]
  "Validate different players"
  (when (= one two)
    (throw (ex-info "Player names must differ"
                    {:type ::ex-type, :cause :same-player})))
  true)

(defn v-game [s]
  "Validate game slug exists"
  (let [g (get @games s)]
    (when (nil? g)
      (throw (ex-info "Game does not exist"
                      {:type ::ex-type, :cause :bad-game})))
    g))

(defn pure-make-game [one two rows cols]
  "Make a new game structure"
  {:one one
   :two two
   :rows rows
   :cols cols
   :state :in-progress
   :turn 1
   :board (into []
                (map
                 vec
                 (partition
                  cols
                  (map
                   (constantly 0)
                   (range
                    (* rows cols))))))})

(defn make-game [one two rows cols]
  "Validation and STM wrapper for new game"
  (v-dimensions rows cols)
  (v-player-name one)
  (v-player-name two)
  (v-names-differ one two)
  (let [slug-root (URLEncoder/encode (str
                                      (lower-case one)
                                      "-"
                                      (lower-case two)) "UTF-8")
        slug (loop [i 0]
               (let [slug (keyword (str slug-root (when (pos? i) (str "-" i))))]
                 (if (contains? @games slug)
                   (recur (inc i))
                   slug)))
        game (pure-make-game one two rows cols)]
    (swap! games assoc-in [slug] (ref game))
    slug))

(defn pure-move [g col]
  "In game g, drop token in column col"
  (when (not= (:state g) :in-progress)
    (throw (ex-info "Game is not in play"
                    {:type :ex-type, :cause :game-done})))
  (when (or (< col 0)
            (> col (dec (:cols g))))
    (throw (ex-info "Column out of range "
                    {:type :ex-type, :cause :bad-column})))

  (let [r (nth (:board g) col)
        _ (when (not (zero? (first r)))
            (throw (ex-info "Column is full"
                            {:type :drop-exception, :cause :full-column})))
        t (take-while zero? r)
        nt (count t)
        turn (:turn g)
        b (assoc (:board g) col
                 (assoc (nth (:board g) col) (dec nt) turn))
        w (win b (dec nt) col turn)]

    (let [t (inc turn)]
      (merge g {:board b
                :turn t}
             (when (> t (* (:rows g) (:cols g)))
               {:state :done})
             (when-let [w w]
               {:state :done
                :winner (if (odd? turn)
                          (:one g) (:two g))})))))
(defn pure-quit [g]
  "Quit game g. Update state and turn."
  (when (not= (:state g) :in-progress)
    (throw (ex-info "Game is not in play"
                    {:type :ex-type, :cause :game-done})))
  (let [turn (:turn g)]
    (let [t (inc turn)]
      (merge g {:state :done
                :turn t
                :quitter (if (odd? turn)
                           (:one g) (:two g))}))))

(defn move-wrapper [gref col]
  "Wrap pure-move for STM"
  (try
    (dosync
     (alter gref pure-move col))
    (catch Exception e
      (let [d (ex-data e)]
        (if (= :drop-exception (:type d))
          (do
            #_(println "detectable exception caught")
            (:cause d))
          (do
            #_(println "non-detectable exception caught, re-throwing")
            (throw e)))))))

(defn quit-wrapper [gref]
  "Wrap pure-quit for STM"
  (try
    (dosync
     (alter gref pure-quit))
    (catch Exception e
      (let [d (ex-data e)]
        (if (= :drop-exception (:type d))
          (do
            #_(println "detectable exception caught")
            (:cause d))
          (do
            #_(println "non-detectable exception caught, re-throwing")
            (throw e)))))))

(defn find-move [b x]
  "In board b, find Column for move x. Return g.cols if not found"
  (reduce (fn [i c]
            (if-let [x (some #(= x %) c)]
              (reduced i)
              (inc i)))
          0 b))

(defn moves
  "In board b, return a range of moves"
  ([b from]
   (moves b from from))

  ([b from to]
   (when (and (pos? from)
              (pos? to)
              (>= to from))
     (into [] (map #(find-move b %) (range from (inc to)))))))

;; Format internals for API-shaped return

(defn list-games []
  "Return in-progress games"
  (vec (filter
        #(not (nil? %))
        (map
         (fn [x]
           (when (= :in-progress (get @(second x) :state))
             (first x))) @games))))

(defn game-status [s]
  "Return status for game slug s"
  (let [g (get @games s)]
    (if (not (nil? g))
      (merge
       (when-let [w (get @g :winner)]
         {:winner w})
       {:players [(:one @g) (:two @g)]
        :state (-> (:state @g)
                   clojure.string/upper-case
                   (subs 1))})
      nil)))

(defn game-move [s idx]
  "Return move idx for game slug s"
  (let [g (get @games s)]
    (if (not (nil? @g))
      (do
        (if-let [q (:quitter @g)]
          (if (and (= idx (dec (:turn @g)))
                   (= (:state @g) :done))
            {:type "QUIT"
             :player q}))
        (if (and  (< 0 idx)
                  (> (:turn @g) idx))
          {:column (find-move (:board @g) idx)
           :type "MOVE"
           :player (if (odd? idx)
                     (:one @g)
                     (:two @g))}
          :no-game))
      :no-game)))

(defn list-moves [s from to]
  "Return range of moves for game slug s"
  (let [g (get @games s)]
    (if (nil? @g)
      :no-game
      (let [from (if (or (nil? 1)
                         (< from 1)) 1 from)
            to (if (or (nil? to)
                       (> to (:turn @g)))
                 (dec (:turn @g))
                 to)
            m (moves (:board @g) from to)]
        (if (nil? m)
          :no-game
          (merge
           {:one (:one @g)
            :two (:two @g)
            :turn (:turn @g)
            :cols (:cols @g)
            :to to
            :moves m}
           (if-let [q (:quitter @g)]
             {:quitter q})))))))

(defn post-move [s p c]
  "Player p drops a token in column c in game slug s"
  (let [g (get @games s)]
    (if (or (nil? @g)
            (not (or (= (lower-case p) (lower-case (:one @g)))
                     (= (lower-case p) (lower-case (:two @g))))))
      :no-game
      (if (or (and (odd? (:turn @g))
                   (= (lower-case p) (lower-case (:one @g))))
              (and (even? (:turn @g))
                   (= (lower-case p) (lower-case (:two @g)))))
        (move-wrapper g c)
        :wrong-player))))

(defn quit-game [s p]
  "Player p quits game slug s"
  (let [g (get @games s)]
    (if (or (nil? @g)
            (not (or (= (lower-case p) (lower-case (:one @g)))
                     (= (lower-case p) (lower-case (:two @g))))))
      :no-game
      (if (or (and (odd? (:turn @g))
                   (= (lower-case p) (lower-case (:one @g))))
              (and (even? (:turn @g))
                   (= (lower-case p) (lower-case (:two @g)))))
        (quit-wrapper g)
        :wrong-player))))
