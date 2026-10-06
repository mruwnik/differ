(ns differ.client.board-stats
  "Pure helpers for the board throughput strip: formatting and chart geometry.")

(def series
  "Charted series in fixed categorical order (reference palette slots 1-2,
   validated as an adjacent pair on a light surface)."
  [{:key :done :label "Done" :color "#2a78d6"}
   {:key :reverted :label "Reverted" :color "#eb6834"}])

(defn active?
  "True when any transition was recorded in the stats window."
  [stats]
  (boolean (some pos? (vals (:totals stats)))))

(defn last-bucket-count
  "Count of `kind` in the most recent bucket (0 when absent)."
  [stats kind]
  (get (last (:buckets stats)) kind 0))

(defn format-rate
  "Per-hour rate: two decimals below 1, one below 10, whole numbers above."
  [x]
  (cond
    (nil? x) "–"
    (zero? x) "0"
    (< x 1) (.toFixed x 2)
    (< x 10) (.toFixed x 1)
    :else (str (js/Math.round x))))

(defn format-minutes
  "Duration in minutes as 45m / 3.5h / 2.1d."
  [m]
  (cond
    (nil? m) "–"
    (< m 60) (str (js/Math.round m) "m")
    (< m 1440) (str (.toFixed (/ m 60) 1) "h")
    :else (str (.toFixed (/ m 1440) 1) "d")))

(defn nice-max
  "Clean y-axis maximum >= n: n itself up to 5, else the next 1/2/5 x 10^k."
  [n]
  (if (<= n 5)
    (max 1 n)
    (let [mag (js/Math.pow 10 (js/Math.floor (js/Math.log10 n)))
          f (/ n mag)]
      (* mag (cond (<= f 1) 1 (<= f 2) 2 (<= f 5) 5 :else 10)))))

(defn bar-path
  "SVG path for a column from baseline `y0` up `h` px, square at the
   baseline with rounded top corners (radius capped by width and height)."
  [x y0 w h]
  (let [r (min 4 (/ w 2) h)
        top (- y0 h)]
    (str "M" x "," y0
         "L" x "," (+ top r)
         "Q" x "," top " " (+ x r) "," top
         "L" (- (+ x w) r) "," top
         "Q" (+ x w) "," top " " (+ x w) "," (+ top r)
         "L" (+ x w) "," y0 "Z")))

(defn chart-bars
  "Grouped columns, one group per bucket with a bar per `series` entry
   separated by `gap` px. Zero counts produce no bar. Returns
   {:y-max n :bars [{:bucket i :key k :color c :x :w :h :count}]}."
  [buckets {:keys [slot-width bar-width gap plot-height]}]
  (let [y-max (nice-max (reduce max 0 (for [b buckets s series] (get b (:key s) 0))))
        group-width (+ (* bar-width (count series)) (* gap (dec (count series))))
        inset (/ (- slot-width group-width) 2)]
    {:y-max y-max
     :bars (vec (for [[i b] (map-indexed vector buckets)
                      [j s] (map-indexed vector series)
                      :let [n (get b (:key s) 0)]
                      :when (pos? n)]
                  {:bucket i
                   :key (:key s)
                   :color (:color s)
                   :count n
                   :x (+ (* i slot-width) inset (* j (+ bar-width gap)))
                   :w bar-width
                   :h (* plot-height (/ n y-max))}))}))
