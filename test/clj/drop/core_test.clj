(ns drop.core-test
  (:require [drop.core :refer :all]
            [clojure.test :refer [deftest is testing]]))

(def known-moves '[[0 0 0 0 3]
                   [0 0 6 5 2]
                   [0 0 0 7 1]
                   [0 0 0 0 4]])

(defn pure-detectable-catcher [f & args]
  "Catch and return app domain exceptions"
  (try
    (apply f args)
    (catch Exception e
      (let [d (ex-data e)]
        (if (= :c4-exception (:type d))
          (:cause d)
          nil)))))

(defn blank []
  "Return a new four by four game"
  {:one "One"
   :two "Two"
   :rows 4
   :cols 4
   :state :in-progress
   :turn 1
   :board (into []
                (map
                 vec
                 (partition
                  4
                  (map
                   (constantly 0)
                   (range
                    (* 4 4))))))})

(defn play [& args]
  "Reducer makes a pure move and returns state, merging domain errors"
  (let [a (first args)]
    (try
      (apply pure-move args)
      (catch Exception e
        (let [d (ex-data e)]
          (if (= :c4-exception (:type d))
            (merge a {:cause d})
            nil))))))

;; (reduce play (blank) (shuffle (flatten (repeatedly 4 (partial range 4)))))

(deftest t-slices
  "Test stateless matrix transformations"
  (testing "diag"
    (is (= (diag '[[1 0 0 0]
                   [0 1 0 0]
                   [0 0 1 0]
                   [0 0 0 1]]) '[1 1 1 1]) "square")

    (is (= (diag '[[1 0 0 0]
                   [0 1 0 0]
                   [0 0 1 0]
                   [0 0 0 1]
                   [2 2 2 2]]) '[1 1 1 1]) "non-square")
    (is (= (diag '[[1]]) '[1]) "minimal"))

  (testing "make-major"
    (is (= (make-major '[[0 1 0 0 0]
                         [0 0 1 0 0]
                         [0 0 0 1 0]
                         [0 0 0 0 1]] 2 1)
           '((1 0 0 0)
             (0 1 0 0)
             (0 0 1 0)
             (0 0 0 1))) "more rows than columns")

    (is (= (make-major '[[0 0 0 0 0]
                         [1 0 0 0 0]
                         [0 1 0 0 0]
                         [0 0 1 0 0]] 1 2)
           '([1 0 0 0 0]
             [0 1 0 0 0]
             [0 0 1 0 0])) "more columns than rows")

    (let [bo '[[1 0 0 0]
               [0 1 0 0]
               [0 0 1 0]
               [0 0 0 1]]]
      (is (= (make-major bo 2 2) bo) "equal columns and rows"))))

#_(deftest t-make-game
    (testing "make-game"
      (testing "same players")
      (testing "small dimensions")
      (testing "names too short")
      )
    )

(deftest t-win
  "Test win detection in every direction"
  (testing "odd win detection"
    (is (true? (win '[[0 1 0 0]
                      [0 1 0 0]
                      [0 1 0 0]
                      [0 1 0 0]] 1 1 1)) "row win")
    (is (true? (win '[[0 0 0 0]
                      [0 0 0 0]
                      [1 1 1 1]
                      [0 0 0 0]] 2 1 1)) "column win")
    (is (true? (win '[[0 0 0 0 0]
                      [0 0 0 0 0]
                      [0 1 1 1 1]
                      [0 0 0 0 0]] 2 1 1)) "buried column win")
    (is (true? (win '[[1 0 0 0]
                      [0 1 0 0]
                      [0 0 1 0]
                      [0 0 0 1]] 1 1 1)) "forward diagonal win")
    (is (true? (win '[[0 0 0 0 0 0]
                      [0 1 0 0 0 0]
                      [0 0 1 0 0 0]
                      [0 0 0 1 0 0]
                      [0 0 0 0 1 0]
                      [0 0 0 0 0 0]] 1 1 1)) "buried forward diagonal win")
    (is (true? (win '[[0 0 0 1]
                      [0 0 1 0]
                      [0 1 0 0]
                      [1 0 0 0]] 2 1 1)) "backward diagonal win"))
  (testing "even win detection"
    (is (true? (win '[[2 2 2 2]
                      [0 0 0 0]
                      [0 0 0 0]
                      [0 0 0 0]] 0 1 2)) "column win"))
  (testing "non-win detection"
    (is (nil? (win '[[0 1 0 0]
                     [0 2 0 0]
                     [0 1 0 0]
                     [0 1 0 0]] 1 1 1)) "row non-win")
    (is (nil? (win '[[0 0 0 0]
                     [0 0 0 0]
                     [1 1 2 1]
                     [0 0 0 0]] 2 1 1)) "column non-win")
    (is (nil? (win '[[1 0 0 0]
                     [0 1 0 0]
                     [0 0 1 0]
                     [0 0 0 2]] 1 1 1)) "forward diagonal non-win")
    (is (nil? (win '[[0 0 0 0 0 0]
                     [0 1 0 0 0 0]
                     [0 0 2 0 0 0]
                     [0 0 0 1 0 0]
                     [0 0 0 0 1 0]
                     [0 0 0 0 0 0]] 1 1 1)) "buried forward diagonal non-win")
    (is (nil? (win '[[0 0 0 1]
                     [0 0 2 0]
                     [0 1 0 0]
                     [1 0 0 0]] 2 1 1)) "backward diagonal non-win")))

(deftest t-pure-move
  "Test game mutations"
  (let [g (blank)]
    (testing "board mutation"
      (testing "empty column"
        (is (= 1 (last (nth (:board (pure-move g 1)) 1))) "column preserved")
        (is (= 1 (last (nth (:board (pure-move g 2)) 2))) "column preserved"))
      (testing "non-empty column"
        (is (= 3 (nth
                  (nth (:board
                        (pure-move (pure-move (pure-move g 2) 2) 2)) 2) 1))
            "same column")
        (is (= 3 (nth
                  (nth (:board
                        (pure-move (pure-move (pure-move g 2) 1) 2)) 2) 2))
            "different column")))
    (is (= :full-column (pure-detectable-catcher
                         (fn [] (-> (pure-move g 2)
                                    (pure-move 2)
                                    (pure-move 2)
                                    (pure-move 2)
                                    (pure-move 2)))))
        "full column")
    (is (= :game-done (pure-detectable-catcher
                       (fn [] (pure-move (assoc-in g [:state] :done) 2))))
        "game done")
    (is (= :bad-column (pure-detectable-catcher
                        (fn [] (pure-move g -1))))
        "column too low")
    (is (= :bad-column (pure-detectable-catcher
                        (fn [] (pure-move g 5))))
        "column too high")
    (is (= :full-column (pure-detectable-catcher
                         (fn [] (-> (pure-move g 2)
                                    (pure-move 2)
                                    (pure-move 2)
                                    (pure-move 2)
                                    (pure-move 2)))))
        "draw detection"))
  )

(deftest t-game-state
  (testing "odd player out of turn")
  (testing "even player out of turn")
  (testing "known state"))

(deftest t-find-move
  (is (and (= (find-move known-moves 1) 2)
           (= (find-move known-moves 2) 1)
           (= (find-move known-moves 3) 0)
           (= (find-move known-moves 4) 3)
           (= (find-move known-moves 5) 1)
           (= (find-move known-moves 6) 1)
           (= (find-move known-moves 7) 2))))

(deftest t-moves
  (is (= (count (moves known-moves 3)) 1) "one argument should return one move")
  (is (and (nil? (moves known-moves 0))
           (nil? (moves known-moves 0 0))
           (nil? (moves known-moves 0 3))
           (nil? (moves known-moves 3 0))) "asking for move zero returns nil")
  (is (nil? (moves known-moves 3 1)) "from greater than to returns nil")
  (is (= (moves known-moves 3 6) '[0 3 1 1]) "known board"))

(deftest t-symmetry
  (let [ms (shuffle (flatten (repeatedly 4 (partial range 4))))
        r (reduce play (blank) ms)
        b (:board r)
        t (dec (:turn r))
        f (moves b 1 t)]
    (is (= (take t ms) f) "input moves equal found moves")))

(deftest t-concurrency
  (testing "multithreaded moves"))

(deftest t-random
  (let [r (reduce play (blank)
                  (shuffle (flatten (repeatedly 4 (partial range 4)))))]
    (is (or (:winner r)
            (and (= (:state r) :done)
                 (= (:turn r) 17))) "must win or draw")))

;;;  no ai yet

(defn rtest [r f]
  (try
    (dosync (alter r f))
    (catch ArithmeticException e
      (println "Math error"))
    (catch Exception e
      (let [d (ex-data e)]
        (if (= :detectable (:type d))
          (do
            (println "detectable exception caught")
            (:cause d))
          (println "non-detectable exception caught"))))))

(flatten (repeatedly 4 (partial range 4)))
