(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (>= (apply max freq) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (>= (apply max freq) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (>= (apply max freq) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (apply = suits)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (every? #(or (= % 2) (= % 3)) freq)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freq (vals (frequencies ranks))]
    (= '(1 2 2)  (sort freq))))

(defn straight? [hand]
 (let [ranks (map rank hand)
       sorted-ranks (sort ranks)
       inv-sorted-ranks (sort (replace {14 1} ranks))]
   (or (= (take 5 (iterate inc (first sorted-ranks))) sorted-ranks)
       (= (take 5 (iterate inc (first inv-sorted-ranks))) inv-sorted-ranks))))

(defn straight-flush? [hand]
  (let [ranks (map rank hand)]
    (and (straight? hand) (flush? hand))))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [[bool _]] bool) 
                                   (map (fn [[f score]] [(f hand) score]) checkers))))
    ))
