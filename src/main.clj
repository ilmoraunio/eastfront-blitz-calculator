(ns main
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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
      (pool {1/6 {:cv cv :n x}}))))

(def hits-required-for-full-step [1 2])

(def scenarios
  (for [airstrike airstrikes
        attacker attackers
        defender defenders
        hits-required hits-required-for-full-step]
    [airstrike attacker defender hits-required]))

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
  [[airstrike-start attacker-start defender-start hits-required-for-full-step
    :as scenario]]
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
                            attacker-2nd-hits)}})))

(def simulations
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

(defn explain
  [[airstrike attacker defender hits-required :as scenario]]
  (format (case hits-required
            2 "%s vs %s Blitz %s (double defense)"
            1 "%s vs %s Blitz %s"
            (throw (ex-info "invalid value" {:hits-required hits-required
                                             :scenario scenario})))
          (firepower-explained defender)
          (firepower-explained attacker)
          (case (-> airstrike first :cv)
            1 "I"
            2 "II"
            3 "III"
            (throw (ex-info "invalid value" {:airstrike airstrike})))))

(defn to-csv
  [simulations]
  (->> simulations
    ;; group together different airstrike (SF, DF, TF) scenarios
    (group-by (juxt #(conj [] (-> %
                                (get-in [:scenario 0 0])
                                (select-keys [:cv])))
                    #(get-in % [:scenario 1])
                    #(get-in % [:scenario 2])
                    #(get-in % [:scenario 3])))
    ;; by airstrike: SF < DF < TF
    (sort-by #(get-in % [:scenario #_"based on strength of airstrike" 0 0 :p]))
    (map (fn [[general-scenario simulations]]
           (with-meta (concat [(explain general-scenario)]
                    (mapcat (fn [simulation]
                              [(get-in simulation [:agg :hits-taken])
                               (get-in simulation [:agg :hits-dealt])])
                            simulations))
                      {:general-scenario general-scenario})))
    (sort-by
      (juxt
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
        ;; double defense follows always
        (fn [entry]
          (let [[_airstrike-strength attacker defender hits
                 :as general-scenario] (-> entry meta :general-scenario)]
            (case hits
              2 1
              1 2
              (throw (ex-info "unknown value" hits))))))
      #(compare %2 %1))
    (distinct)))

(defn to-csv!
  [simulations]
  (with-open [writer (io/writer "out-file.csv")]
    (csv/write-csv writer
                   (concat [["" "Hits taken" "Hits dealt"]
                            ["" "SF" "" "DF" "" "TF" ""]]
                           (to-csv simulations)))))