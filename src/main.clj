(ns main)

(def airstrikes-v2 [[{:p 1/6 :cv 3 :hits 0}]
                    [{:p 2/6 :cv 3 :hits 0}]
                    [{:p 3/6 :cv 3 :hits 0}]])

(def attackers-v2 [[{:p 2/6 :cv 4 :hits 0}
                    {:p 2/6 :cv 4 :hits 0}
                    {:p 2/6 :cv 4 :hits 0}
                    {:p 2/6 :cv 4 :hits 0}]])

(def defenders-v2 [[{:p 2/6 :cv 4 :hits 0}
                    {:p 2/6 :cv 4 :hits 0}
                    {:p 2/6 :cv 4 :hits 0}
                    {:p 2/6 :cv 4 :hits 0}]])

(def scenarios
  (for [airstrike airstrikes-v2
        attacker attackers-v2
        defender defenders-v2]
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
   Provides the results in following format:

   ```
   [[pool]  ; after 1st battle turn
    [pool]] ; after 2nd battle turn
   ```

   † Rules generally: Airstrike dice are resolved for attacker only and resolved
     first, hits are subtracted from defender's force pool (subtractions are
     made to attacker's benefit). After that, defender resolves all throwable
     dice with remaining force pool, hits are subtracted from the attacker's
     force pool (to attacker's benefit). Finally, attacker resolves all dice,
     hits are subtracted from defender's force pool."
  [[airstrike-start attacker-start defender-start :as _scenario]]
  (let [hits-required-for-full-step 1]                      ; todo
    #_(prn ::scenario _scenario)
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
      (let [airstrike-2nd-hits (dice-roll airstrike-1st-reduced :ceil)
            defender-2nd (subtract defender-1st-final airstrike-2nd-hits hits-required-for-full-step)
            defender-2nd-hits (dice-roll defender-2nd :floor)
            attacker-2nd (subtract attacker-1st defender-2nd-hits hits-required-for-full-step)
            attacker-2nd-hits (dice-roll attacker-2nd :ceil)
            defender-2nd-final (subtract defender-2nd attacker-2nd-hits hits-required-for-full-step)
            airstrike-2nd-reduced (subtract airstrike-1st-reduced 1 1)]
        #_(prn ::2nd-battle-turn [airstrike-2nd-reduced attacker-2nd defender-2nd-final])
        {:1st [airstrike-1st-reduced attacker-1st defender-1st-final]
         :2nd [airstrike-2nd-reduced attacker-2nd defender-2nd-final]}))))

(comment
  (map (fn [scenario] {scenario (simulate scenario)}) scenarios))