(ns main
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn pool
  [m]
  (apply concat
         (for [[p {:keys [cv n]}] m]
           (for [_ (range n)] {:p p :cv cv :hits 0}))))

(defn gen-attackers
  ([pool starting-force-n]
   (mapcat (fn [strategy]
             (gen-attackers pool starting-force-n strategy))
           [:lowest-p :highest-p]))
  ([pool starting-force-n replacement-strategy]
   (->> (combo/permutations pool)
     (map (juxt (partial take starting-force-n)
                (partial drop starting-force-n)))
     (map (fn [[a b]] [(sort-by (juxt :p :cv) a)
                       (sort-by (juxt :p :cv) b)]))
     (distinct)
     (map (fn [[a b]] [a b replacement-strategy])))))

(def airstrikes [[{:p 1/6 :cv 3 :hits 0}]
                 [{:p 2/6 :cv 3 :hits 0}]
                 [{:p 3/6 :cv 3 :hits 0}]])

(def attackers [;; 4 triple-fire (tf) + 12 double-fire (df) (axis only)
                [{:p 3/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}]

                ;; 4 tf + 8 df + 4 sf (axis only)
                [{:p 3/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}
                 {:p 1/6 :cv 4 :hits 0}]

                ;; 16 df
                [{:p 2/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}]

                ;; 4 single-fire (sf) + 12 df
                [{:p 1/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}
                 {:p 2/6 :cv 4 :hits 0}]

                ;; 3 sf + 9 df (soviets typically)
                [{:p 1/6 :cv 3 :hits 0}
                 {:p 2/6 :cv 3 :hits 0}
                 {:p 2/6 :cv 3 :hits 0}
                 {:p 2/6 :cv 3 :hits 0}]])

(def defenders
  (concat
    ;; cases:
    ;; - 16df (4 blocks)
    ;; - 8df + 8sf (4 blocks)
    ;; - 4df + 12sf (4 blocks)
    ;; - 16sf (4 blocks)
    ;; - 12df (4 blocks)
    ;; - 9df + 3sf (4 blocks)
    ;; - 6df + 6sf (4 blocks)
    ;; - 6df + 3sf (3 blocks)
    ;; - 3df + 9sf (4 blocks)
    ;; - 3df + 6sf (3 blocks)
    ;; - 3df + 3sf (2 blocks)
    ;; - 12df (3 blocks)
    ;; - 9df (3 blocks)
    ;; - 8df (2 blocks)
    ;; - 6df (2 blocks)
    ;; - 4df (1 block)
    ;; - 3df (1 block)
    ;; - 4df + 4df (2 blocks)
    ;; - 4df + 8sf (3 blocks)
    ;; - 8df + 4sf (3 blocks)
    ;; - 12df + 4sf (4 blocks)

    (mapcat concat
            (for [cv [3 4]
                  x (range 1 5)]
              (for [y (range (- 5 x))]
                (pool {2/6 {:cv cv :n x}
                       1/6 {:cv cv :n y}}))))

    ;; cases:
    ;; - 12sf (3 blocks)
    ;; - 9sf (3 blocks)
    ;; - 8sf (2 blocks)
    ;; - 6sf (2 blocks)
    ;; - 4sf (1 block)
    ;; - 3sf (1 block)
    (for [cv [3 4]
          x (range 1 4)]
      (pool {1/6 {:cv cv :n x}}))

    ;; case: 16sf (4 blocks)
    [(pool {1/6 {:cv 4 :n 4}})]

    ;; case: 4 tf + 12 df (4 blocks)
    [(pool {3/6 {:cv 4 :n 1}
            2/6 {:cv 4 :n 3}})]

    ;; case: 4 tf + 8 df + 4 sf (4 blocks)
    [(pool {3/6 {:cv 4 :n 1}
            2/6 {:cv 4 :n 2}
            1/6 {:cv 4 :n 1}})]))

(def hits-required-for-full-step [1 2])

(def replacement-strategies
  {:lowest-p (juxt (comp + :p) (comp + :cv))
   :highest-p (juxt (comp - :p) (comp + :cv))})

(def attackers-2+2
  [;; 4 triple-fire (tf) + 12 double-fire (df) (axis only)
   [[{:p 3/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :lowest-p]
   [[{:p 3/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :highest-p]

   [[{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 3/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :lowest-p]
   [[{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 3/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :highest-p]

   ;; 4 tf + 8 df + 4 sf (axis only)
   [[{:p 3/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 1/6 :cv 4 :hits 0}]
    :lowest-p]
   [[{:p 3/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 1/6 :cv 4 :hits 0}]
    :highest-p]

   [[{:p 1/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 3/6 :cv 4 :hits 0}]
    :lowest-p]
   [[{:p 1/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 3/6 :cv 4 :hits 0}]
    :highest-p]

   [[{:p 1/6 :cv 4 :hits 0}
     {:p 3/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :lowest-p]
   [[{:p 1/6 :cv 4 :hits 0}
     {:p 3/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :highest-p]

   [[{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 1/6 :cv 4 :hits 0}
     {:p 3/6 :cv 4 :hits 0}]
    :lowest-p]
   [[{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 1/6 :cv 4 :hits 0}
     {:p 3/6 :cv 4 :hits 0}]
    :highest-p]

   ;; 16 df
   [[{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :lowest-p]
   [[{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :highest-p]

   ;; 4 single-fire (sf) + 12 df
   [[{:p 1/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :lowest-p]
   [[{:p 1/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :highest-p]

   [[{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 1/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :lowest-p]
   [[{:p 2/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    [{:p 1/6 :cv 4 :hits 0}
     {:p 2/6 :cv 4 :hits 0}]
    :highest-p]

   ;; 3 sf + 9 df (soviets typically)
   [[{:p 1/6 :cv 3 :hits 0}
     {:p 2/6 :cv 3 :hits 0}]
    [{:p 2/6 :cv 3 :hits 0}
     {:p 2/6 :cv 3 :hits 0}]
    :lowest-p]
   [[{:p 1/6 :cv 3 :hits 0}
     {:p 2/6 :cv 3 :hits 0}]
    [{:p 2/6 :cv 3 :hits 0}
     {:p 2/6 :cv 3 :hits 0}]
    :highest-p]

   [[{:p 2/6 :cv 3 :hits 0}
     {:p 2/6 :cv 3 :hits 0}]
    [{:p 1/6 :cv 3 :hits 0}
     {:p 2/6 :cv 3 :hits 0}]
    :lowest-p]
   [[{:p 2/6 :cv 3 :hits 0}
     {:p 2/6 :cv 3 :hits 0}]
    [{:p 1/6 :cv 3 :hits 0}
     {:p 2/6 :cv 3 :hits 0}]
    :highest-p]])

(def scenarios-2+2
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers-2+2
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

(def scenarios-1
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy]
        (concat (gen-attackers [{:p 1/6 :cv 4 :hits 0}] 1 nil)
                (gen-attackers [{:p 2/6 :cv 4 :hits 0}] 1 nil)
                (gen-attackers [{:p 3/6 :cv 4 :hits 0}] 1 nil)
                (gen-attackers [{:p 1/6 :cv 3 :hits 0}] 1 nil)
                (gen-attackers [{:p 2/6 :cv 3 :hits 0}] 1 nil)
                (gen-attackers [{:p 3/6 :cv 3 :hits 0}] 1 nil))
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

(def scenarios-1+2
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy]
        (concat (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 3/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 3/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 3/6 :cv 4 :hits 0}]
                               1))
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

(def scenarios-1+3
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy]
        (concat (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 3/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 3/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 3/6 :cv 4 :hits 0}]
                               1)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 3/6 :cv 4 :hits 0}]
                               1))
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

(def scenarios-2+1
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy]
        (concat (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}]
                               2)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}]
                               2)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}]
                               2)
                (gen-attackers [{:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}]
                               2)
                (gen-attackers [{:p 2/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 3/6 :cv 4 :hits 0}]
                               2)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 2/6 :cv 4 :hits 0}
                                {:p 3/6 :cv 4 :hits 0}]
                               2)
                (gen-attackers [{:p 1/6 :cv 4 :hits 0}
                                {:p 1/6 :cv 4 :hits 0}
                                {:p 3/6 :cv 4 :hits 0}]
                               2))
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

#_(def scenarios-2+3
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

#_(def scenarios-2+4
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

#_(def scenarios-3+1
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

#_(def scenarios-3+2
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

#_(def scenarios-3+3
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

#_(def scenarios-3+4
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

(def scenarios-4-no-reinforcement
  (for [airstrike airstrikes
        attacker attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required nil nil]))

#_(def scenarios-4+1
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

#_(def scenarios-4+2
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

#_(def scenarios-4+3
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

#_(def scenarios-4+4
  (for [airstrike airstrikes
        [attacker reinforcement replacement-strategy] attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required reinforcement replacement-strategy]))

(defn sort-by-next-victim
  ([f pool]
   (sort-by f pool))
  ([pool]
   (sort-by-next-victim (juxt (comp - :hits) (comp - :cv) (comp + :p)) pool)))

(defn dices-thrown
  "Returns `{p n}` for `pool` where p is the dice's probability and n the amount
   of dices."
  [pool]
  (reduce
    (fn [acc {:keys [p cv] :as _block}]
      (update acc p #((fnil + 0) % cv)))
    {}
    pool))

(defn eliminated?
  [block]
  (= (:cv block) 0))

(defn deal-hit
  [block hits-required-for-full-step]
  (when (some? block)
    (let [block (update block :hits inc)]
      (if (<= hits-required-for-full-step (:hits block))
        (let [block (-> block
                      (update :cv dec)
                      (update :hits (constantly 0)))]
          (when (not (eliminated? block))
            block))
        block))))

(defn reinforce-pool
  [pool reinforcement replacement-strategy]
  (let [pool (sort-by-next-victim
               (replacement-strategies replacement-strategy)
               pool)
        reinforced-pool (concat pool reinforcement)]
    (if (> (count reinforced-pool) 4)
      (drop (max 0 (- (count reinforced-pool) 4)) reinforced-pool)
      reinforced-pool)))

(defn reset-hits
  [pool]
  (map (fn [block] (assoc block :hits 0)) pool))

(defn subtract
  "Deal `hits` to such a block in `pool` that has either been already dealt a
   hit, or has the highest CV value, or in case of a tie is the weakest
   defending unit in terms of firepower. For every full step of `hits-required-for-full-step`
   reduce `:cv` from block."
  [pool hits hits-required-for-full-step]
  #_(prn ::subtract pool hits hits-required-for-full-step)
  (reduce
    (fn [pool _]
      (let [pool (sort-by-next-victim pool)]
        (if-let [block (deal-hit (first pool) hits-required-for-full-step)]
          (concat [block] (rest pool))
          (rest pool))))
    pool
    (range hits)))

(defn dice-roll
  "How many hits (statistically) for a dice roll based on given `strategy`:
   - :ceil (typically for calculating an optimistic attacker result)
   - :floor (typically used for calculating the defender's dice throws)"
  [pool strategy]
  #_(prn ::dice-roll pool strategy)
  (let [hits (reduce
               (fn [hits [p n]]
                 (+ hits (* p n)))
               0
               (dices-thrown pool))]
    (case strategy
      :ceil (Math/ceil hits)
      :floor (Math/floor hits)
      :debug hits
      (throw (ex-info "invalid strategy" {})))))

(defn cv-steps
  [pool]
  (->> (dices-thrown pool)
    vals
    (apply +)))

(defn calc-hits
  ([pool n hits-required]
   (min
     (cv-steps pool)
     (calc-hits n hits-required)))
  ([n hits-required]
   (Math/floor (/ n hits-required))))

(defn simulate
  "Simulates a given scenario given as a triplet of maps containing pool of
   blocks with a given combat value `:cv` (denoting amount of dice thrown) and
   probability `:p` (to denote the successful values of a dice throw). Simulates
   the first and second combat turn of a blitz attack using Eastfront II rules†.

   † Rules generally: Airstrike dice are resolved for attacker only and resolved
     first, hits are subtracted from defender's force pool (subtractions are
     made to attacker's benefit). After that, defender resolves all throwable
     dice with remaining force pool, hits are subtracted from the attacker's
     force pool (to attacker's benefit). Finally, attacker resolves all dice,
     hits are subtracted from defender's force pool."
  [[airstrike-start
    attacker-start
    defender-start
    hits-required-for-full-step
    reinforcement
    replacement-strategy
    :as scenario]]
  #_(prn ::scenario scenario)
  ;; 1st battle turn
  (let [hits-req hits-required-for-full-step
        airstrike-1st-hits (dice-roll airstrike-start :ceil)
        defender-1st (subtract defender-start airstrike-1st-hits hits-required-for-full-step)
        defender-1st-hits (dice-roll defender-1st :floor)
        attacker-1st (subtract attacker-start defender-1st-hits 1)
        attacker-1st-hits (dice-roll attacker-1st :ceil)
        defender-1st-final (subtract defender-1st attacker-1st-hits hits-required-for-full-step)
        airstrike-1st-reduced (subtract airstrike-start 1 1)]
    #_(prn ::1st-battle-turn [airstrike-1st-reduced attacker-1st defender-1st-final])
    ;; 2nd battle turn
    (let [airstrike-2nd-start (reset-hits airstrike-1st-reduced)
          attacker-2nd-start (cond-> (reset-hits attacker-1st)
                               reinforcement (reinforce-pool reinforcement
                                                             replacement-strategy))
          defender-2nd-start (reset-hits defender-1st-final)
          airstrike-2nd-hits (dice-roll airstrike-2nd-start :ceil)
          defender-2nd (subtract defender-2nd-start airstrike-2nd-hits hits-required-for-full-step)
          defender-2nd-hits (dice-roll defender-2nd :floor)
          attacker-2nd (subtract attacker-2nd-start defender-2nd-hits 1)
          attacker-2nd-hits (dice-roll attacker-2nd :ceil)
          defender-2nd-final (subtract defender-2nd attacker-2nd-hits hits-required-for-full-step)
          airstrike-2nd-reduced (subtract airstrike-1st-reduced 1 1)
          hits-dealt-1st-agg (calc-hits attacker-start defender-1st-hits 1)
          hits-taken-1st-agg (calc-hits defender-start
                                        (+ airstrike-1st-hits
                                           attacker-1st-hits)
                                        hits-req)
          hits-dealt-2nd-agg (calc-hits attacker-2nd-start defender-2nd-hits 1)
          hits-taken-2nd-agg (calc-hits defender-2nd-start
                                        (+ airstrike-2nd-hits
                                           attacker-2nd-hits)
                                        hits-req)]
      #_(prn ::2nd-battle-turn [airstrike-2nd-reduced attacker-2nd defender-2nd-final])
      {:scenario scenario
       :1st {:airstrike airstrike-1st-reduced
             :attacker attacker-1st
             :defender defender-1st-final
             :hits {:airstrike airstrike-1st-hits
                    :attacker attacker-1st-hits
                    :defender defender-1st-hits}
             :agg {:hits-dealt hits-dealt-1st-agg
                   :hits-taken hits-taken-1st-agg}}
       :2nd {:airstrike airstrike-2nd-reduced
             :attacker attacker-2nd
             :defender defender-2nd-final
             :hits {:airstrike airstrike-2nd-hits
                    :attacker attacker-2nd-hits
                    :defender defender-2nd-hits}
             :agg {:hits-dealt hits-dealt-2nd-agg
                   :hits-taken hits-taken-2nd-agg}}
       :agg {:hits-dealt (+ hits-dealt-1st-agg hits-dealt-2nd-agg)
             :hits-taken (+ hits-taken-1st-agg hits-taken-2nd-agg)}})))

(defn wrap-simulation
  [scenarios]
  (->> scenarios
    (map (fn [scenario] {scenario (simulate scenario)}))
    (mapcat vals)))

(defn firepower-explained
  [pool]
  (str/join " " (map (fn [[p n]]
                       (str n (case p
                                1/6 "SF"
                                2/6 "DF"
                                3/6 "TF"
                                (throw (ex-info "invalid value" {:p p})))))
                     ;; TF > DF > SF
                     (sort #(compare %2 %1) (dices-thrown pool)))))

(defn explain-attacker
  [[airstrike attacker defender hits-required reinforcement :as scenario]]
  (format "%s %s"
          (cond-> (firepower-explained attacker)
            reinforcement (str " + " (firepower-explained reinforcement)))
          (case (-> airstrike first :cv)
            1 "I"
            2 "II"
            3 "III"
            (throw (ex-info "invalid value" {:airstrike airstrike})))))

(defn explain
  [[airstrike attacker defender hits-required reinforcement :as scenario]]
  (format (case hits-required
            2 "%s (double defense) vs %s"
            1 "%s vs %s"
            (throw (ex-info "invalid value" {:hits-required hits-required
                                             :scenario scenario})))
          (firepower-explained defender)
          (explain-attacker scenario)))

(defn explain-defender
  [[airstrike attacker defender hits-required :as scenario]]
  (format (case hits-required
            2 "%s (double defense)"
            1 "%s"
            (throw (ex-info "invalid value" {:hits-required hits-required
                                             :scenario scenario})))
          (firepower-explained defender)))

(defn explain-replacement-strategy
  [[airstrike attacker defender hits-required reinforcements replacement-strategy]]
  (case replacement-strategy
    :lowest-p "Infantry first"
    :highest-p "Armor first"
    ""))

(defn to-csv
  [scenarios hq-activation]
  (assert (#{:regular :blitz} hq-activation))
  (let [scope (case hq-activation
                :regular [:1st :agg]
                :blitz [:agg])
        result-scope (case hq-activation
                       :regular :1st
                       :blitz :2nd)]
    (->> scenarios
      (wrap-simulation)
      ;; group together different airstrike (SF, DF, TF) scenarios
      (group-by (juxt #(conj [] (-> %
                                  (get-in [:scenario 0 0])
                                  (select-keys [:cv])))
                      #(get-in % [:scenario 1])
                      #(get-in % [:scenario 2])
                      #(get-in % [:scenario 3])
                      #(get-in % [:scenario 4])
                      #(get-in % [:scenario 5])))
      ;; by airstrike: SF < DF < TF
      (sort-by #(get-in % [:scenario #_"based on strength of airstrike" 0 0 :p]))
      (map (fn [[general-scenario simulations]]
             (with-meta (concat [(explain general-scenario)]
                                [(explain-defender general-scenario)]
                                [(explain-attacker general-scenario)]
                                [(explain-replacement-strategy general-scenario)]
                                (map (fn [simulation]
                                       (get-in simulation (concat scope [:hits-taken])))
                                     simulations)
                                (map (fn [simulation]
                                       (get-in simulation (concat scope [:hits-dealt])))
                                     simulations)
                                (map (fn [simulation]
                                       (firepower-explained (get-in simulation [result-scope :defender])))
                                     simulations)
                                (map (fn [simulation]
                                       (firepower-explained (get-in simulation [result-scope :attacker])))
                                     simulations))
                        {:general-scenario general-scenario})))
      (sort-by
        (juxt
          ;; armor-dropped first strategy grouped at the bottom
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits reinforcement replacement-strategy
                   :as general-scenario] (-> entry meta :general-scenario)]
              (case replacement-strategy
                :lowest-p 2
                :highest-p 1
                0)))
          ;; double defense grouped at the bottom
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits
                   :as general-scenario] (-> entry meta :general-scenario)]
              (case hits
                2 1
                1 2
                (throw (ex-info "unknown value" hits)))))
          ;; sum of defender TF cvs
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits
                   :as general-scenario] (-> entry meta :general-scenario)]
              (get (dices-thrown defender) 3/6)))
          ;; sum of defender DF cvs
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits
                   :as general-scenario] (-> entry meta :general-scenario)]
              (get (dices-thrown defender) 2/6)))
          ;; sum of defender cvs
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits
                   :as general-scenario] (-> entry meta :general-scenario)]
              (apply + (map second (dices-thrown defender)))))
          ;; sum of attacker TF cvs
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits
                   :as general-scenario] (-> entry meta :general-scenario)]
              (get (dices-thrown attacker) 3/6)))
          ;; sum of attacker DF cvs
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits
                   :as general-scenario] (-> entry meta :general-scenario)]
              (get (dices-thrown attacker) 2/6)))
          ;; sum of attacker cvs
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits
                   :as general-scenario] (-> entry meta :general-scenario)]
              (apply + (map second (dices-thrown attacker)))))
          ;; sum of reinforcement TF cvs
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits reinforcement
                   :as general-scenario] (-> entry meta :general-scenario)]
              (get (dices-thrown reinforcement) 3/6)))
          ;; sum of reinforcement DF cvs
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits reinforcement
                   :as general-scenario] (-> entry meta :general-scenario)]
              (get (dices-thrown reinforcement) 2/6)))
          ;; sum of reinforcement cvs
          (fn [entry]
            (let [[_airstrike-strength attacker defender hits reinforcement
                   :as general-scenario] (-> entry meta :general-scenario)]
              (apply + (map second (dices-thrown reinforcement))))))
        #(compare %2 %1))
      (distinct)
      ;; soviets don't have TF
      (remove #(->> % first (re-find #"TF.+TF"))))))

(defn to-csv!
  [scenarios scope label]
  (with-open [writer (io/writer (format "simulate-%s.%s.csv"
                                        (name scope)
                                        (name label)))]
    (csv/write-csv writer
                   (concat [["Scenario"
                             "Defender"
                             "Attacker"
                             "Replacement strategy"
                             "Hits taken (SF)"
                             "Hits taken (DF)"
                             "Hits taken (TF)"
                             "Hits dealt (SF)"
                             "Hits dealt (DF)"
                             "Hits dealt (TF)"
                             "Defender result (SF)"
                             "Defender result (DF)"
                             "Defender result (TF)"
                             "Attacker result (SF)"
                             "Attacker result (DF)"
                             "Attacker result (TF)"]]
                           (to-csv scenarios scope)))))

(comment
  (do
    (to-csv! scenarios-4-no-reinforcement :regular :4)
    (to-csv! scenarios-4-no-reinforcement :blitz :4)
    (to-csv! scenarios-1 :regular :1)
    (to-csv! scenarios-1 :blitz :1)
    (to-csv! scenarios-2+1 :regular :2+1)
    (to-csv! scenarios-2+1 :blitz :2+1)
    (to-csv! scenarios-2+2 :regular :2+2)
    (to-csv! scenarios-2+2 :blitz :2+2)
    (to-csv! scenarios-1+2 :regular :1+2)
    (to-csv! scenarios-1+2 :blitz :1+2)
    (to-csv! scenarios-1+3 :regular :1+3)
    (to-csv! scenarios-1+3 :blitz :1+3)))