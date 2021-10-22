(ns main
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn pool
  [m]
  (apply concat
         (for [[p {:keys [cv n]}] m]
           (for [_ (range n)] {:p p :cv cv :hits 0}))))

(def airstrikes [[{:p 1/6 :cv 3 :hits 0}]
                 [{:p 2/6 :cv 3 :hits 0}]
                 [{:p 3/6 :cv 3 :hits 0}]])

(def attackers [;; 16 double-fire (df)
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
    ))

(def scenarios
  (for [airstrike airstrikes
        attacker attackers
        defender defenders]
    [airstrike attacker defender]))

(defn sort-by-next-victim
  [pool]
  (sort-by (juxt (comp - :hits) (comp - :cv) (comp + :p)) pool))

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
  [[airstrike-start attacker-start defender-start :as scenario]]
  (let [hits-required-for-full-step 1]                      ; todo
    #_(prn ::scenario scenario)
    ;; 1st battle turn
    (let [airstrike-1st-hits (dice-roll airstrike-start :ceil)
          defender-1st (subtract defender-start airstrike-1st-hits hits-required-for-full-step)
          defender-1st-hits (dice-roll defender-1st :floor)
          attacker-1st (subtract attacker-start defender-1st-hits hits-required-for-full-step)
          attacker-1st-hits (dice-roll attacker-1st :ceil)
          defender-1st-final (subtract defender-1st attacker-1st-hits hits-required-for-full-step)
          airstrike-1st-reduced (subtract airstrike-start 1 1)]
      #_(prn ::1st-battle-turn [airstrike-1st-reduced attacker-1st defender-1st-final])
      ;; 2nd battle turn
      (let [airstrike-2nd-start (reset-hits airstrike-1st-reduced)
            attacker-2nd-start (reset-hits attacker-1st)
            defender-2nd-start (reset-hits defender-1st-final)
            airstrike-2nd-hits (dice-roll airstrike-2nd-start :ceil)
            defender-2nd (subtract defender-2nd-start airstrike-2nd-hits hits-required-for-full-step)
            defender-2nd-hits (dice-roll defender-2nd :floor)
            attacker-2nd (subtract attacker-2nd-start defender-2nd-hits hits-required-for-full-step)
            attacker-2nd-hits (dice-roll attacker-2nd :ceil)
            defender-2nd-final (subtract defender-2nd attacker-2nd-hits hits-required-for-full-step)
            airstrike-2nd-reduced (subtract airstrike-1st-reduced 1 1)]
        #_(prn ::2nd-battle-turn [airstrike-2nd-reduced attacker-2nd defender-2nd-final])
        {:scenario scenario
         :1st {:airstrike airstrike-1st-reduced
               :attacker attacker-1st
               :defender defender-1st-final
               :hits {:airstrike airstrike-1st-hits
                      :attacker attacker-1st-hits
                      :defender defender-1st-hits}}
         :2nd {:airstrike airstrike-2nd-reduced
               :attacker attacker-2nd
               :defender defender-2nd-final
               :hits {:airstrike airstrike-2nd-hits
                      :attacker attacker-2nd-hits
                      :defender defender-2nd-hits}}
         :agg {:hits-dealt (+ defender-1st-hits
                              defender-2nd-hits)
               :hits-taken (+ airstrike-1st-hits
                              airstrike-2nd-hits
                              attacker-1st-hits
                              attacker-2nd-hits)}}))))

(def simulations
  (->> scenarios
    (map (fn [scenario] {scenario (simulate scenario)}))
    (mapcat vals)))

(comment
  (with-open [writer (io/writer "out-file.csv")]
    (csv/write-csv writer
                   [["" "Hits taken" "Hits dealt"]
                    ["" "SF" "" "DF" "" "TF" ""]
                    ["16DF vs 16DF B III" 7 9 9 11 9 12]]))
  )