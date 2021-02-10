(ns p-p-p-pokerface)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card]
  (let [value (get card 0)]
    (if (Character/isDigit value) (Integer/parseInt (str value)) (get replacements value))))


(defn suit [[_, suit]]
  (str suit))

(defn
  nums-greater-than [nums, num] (filter (fn [x] (> x num)) (vals (frequencies nums)))
  )

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (not (empty? (nums-greater-than ranks 1)))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (not (empty? (nums-greater-than ranks 2)))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (not (empty? (nums-greater-than ranks 3)))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= 1 (count (frequencies suits)))))

(defn full-house? [hand]
  (let [suits (map rank hand)
        suit-count (vals (frequencies suits))]
    (and (not= nil (some #{3} suit-count)) (not= nil (some #{2} suit-count)))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        occurrences (vals (frequencies ranks))
        value (filter (fn [x] (> x 1)) occurrences)]
    (= 2 (count value))))

(defn straight? [hand]
  (let [ranks (map rank hand)
       min-rank (apply min ranks)
       max-rank (apply max ranks)
        target (range min-rank (+ max-rank 1))]
    (or (= (sort ranks) [2 3 4 5 14]) (= target (sort ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matching-checkers (filterv (fn [checker] ((first checker) hand)) checkers)
        hand-vals (mapv (fn [[_, val]] val) matching-checkers)]
    (apply max hand-vals)))
