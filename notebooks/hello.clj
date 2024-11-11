{:nextjournal.clerk/visibility {:code :hide :result :hide}}

(ns hello
  {:nextjournal.clerk/toc true}
  (:require [meta-csv.core :as csv]
            [nextjournal.clerk :as clerk]
            [tech.v3.dataset :as ds])
  (:import [java.time LocalDateTime]
           [java.util Locale]
           [java.time.temporal WeekFields]
           [java.time.format DateTimeFormatter]))

(defn parse-date
  "Parse the Strava date string format to Java's LocalDateTime object"
  [date-string]
  (let [format  "MMM d, uuuu, h:m:s a"
        formatter (DateTimeFormatter/ofPattern format)]
    (LocalDateTime/parse date-string formatter)))

(defn get-week [local-date-time]
  (let [week-fields (WeekFields/of Locale/US)
        week-of-year (.weekOfWeekBasedYear week-fields)]
    (.get local-date-time week-of-year)))

(defn round-single [f]
  (-> f (* 10) Math/floor (/ 10)))

(defn group-subseqs [s]
  (let [subseqs (reduce (fn [accum next]
                          (if (= 1 (- next (:prev accum)))
                            (-> accum
                                (assoc :current-group (conj (:current-group accum) next))
                                (assoc :prev next))
                            (-> accum
                                (assoc :groups (conj (:groups accum) (:current-group accum)))
                                (assoc :current-group [next])
                                (assoc :prev next))))
                        {:groups [] :current-group [(first s)] :prev (first s)} (rest s))]
    (-> subseqs (assoc :groups (conj (:groups subseqs) (:current-group subseqs))) :groups)))

{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Analyzing my 2024 Strava Cycling Data

;; I recently got into road cycling. I would even say that I am a _serious_ cyclist.
;; That said, 2024 was the first year where I could complete a full* season of training 
;; and participate in my first event. With all my rides uploaded to Strava, I wondered
;; if I could gleam any insight from the data and use it to set goals for and improve my
;; 2025 cycling season.

;; _\* - it wasn't exactly a full season as we shall later see_

;; Some questions I set out to find the answer to:
;;  - Was I consistent?
;;  - Were there any strange patterns?
;;  - Did I improve?
;;  - Did I have varied rides?
;;  - How can I have more long duration rides?

;; ## The data

;; Instead of using Strava's API, I exported my account's data archive. This gave me
;; an `activities.csv` file containing all the activities I've ever uploaded to Strava 
;; as well as a directory containing the individual `fit` files for each activity.

;; Strava includes many columns (89 to be exact) and a lot of them are irrelevant 
;; to this analysis.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def raw-data (ds/->dataset (csv/read-csv "activities.csv")))
(keys raw-data)

;; ### Cleaning the data

;; As part of the data cleaning process
;;  - the irrelevant columns (e.g. _"Number of Runs"_) are removed,
;;  - only non-commute, ride activities from 2024 are selected,
;;  - activities with missing data are removed,
;;  - additional columns which facilitate grouping are added and
;;  - the speeds are converted from _m/s_ to _km/h_.

;; The data now looks like this:

^{:nextjournal.clerk/visibility {:code :fold :result :hide}}
(def dataset
  (let [cols ["Activity Date"
              "Distance"
              "Moving Time"
              "Max Speed"
              "Average Speed"
              "Elevation Gain"
              "Max Heart Rate"
              "Average Heart Rate"
              "Filename"]
        renamed-cols (-> cols
                         (assoc (.indexOf cols "Activity Date") "ActivityDate")
                         (assoc (.indexOf cols "Moving Time") "MovingTime")
                         (assoc (.indexOf cols "Max Speed") "MaxSpeed")
                         (assoc (.indexOf cols "Average Speed") "AvgSpeed")
                         (assoc (.indexOf cols "Elevation Gain") "ElevationGain")
                         (assoc (.indexOf cols "Max Heart Rate") "MaxHeartRate")
                         (assoc (.indexOf cols "Average Heart Rate") "AvgHeartRate"))]
    (-> raw-data

        ; Remove commutes and get only rides from 2024
        (ds/filter #(and (= 0.0 (get % "Commute"))
                         (= "Ride" (get % "Activity Type"))
                         (= 2024 (-> (get % "Activity Date") parse-date .getYear))))

        ; Remove activities where average heart rate and distance data is missing
        (ds/filter #(and (some? (get % "Average Heart Rate"))
                         (not= 0.0 (get % "Distance"))))

        ; Get only relevant cols
        (ds/select-columns cols)

        ; Rename cols
        (ds/rename-columns renamed-cols)

        ; Add Week, Month, DayOfYear column and convert speed from m/s to km/h
        (ds/row-map (fn [{:strs [ActivityDate MaxSpeed AvgSpeed MovingTime]}]
                      (let [parsed (parse-date ActivityDate)]
                        {"Week" (get-week parsed)
                         "MovingTime" (float (/ MovingTime 60 60))
                         "MaxSpeed" (round-single (* MaxSpeed 3.6))
                         "AvgSpeed" (round-single (* AvgSpeed 3.6))}))))))

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/table {::clerk/page-size 5} dataset)

;; The number of activities in the cleaned dataset:

(-> dataset ds/rows count)

;; There are a few key metrics missing. For example power data, _rate
;; of perceived exertion_ and also generally how strong I felt about 
;; a work out. I did not have the equipment to record these data nor
;; did I have the foresight to do so. That being said, the current data
;; for 2025.

;; ## Consistent Rides

;; ### Overview

^{:nextjournal.clerk/visibility {:code :fold}}
(let [; group data by week
      week-group (-> dataset (ds/group-by-column "Week"))

      ; get all week numbers
      week-nums (keys week-group)

      ; get number of rides per week
      week-counts (map #(-> % ds/rows count) (vals week-group))

      ; get total duration of rides per week in hours
      week-duration (map #(reduce + (get % "MovingTime")) (vals week-group))

      ; prepare labels for plotting
      labels [{:x 20 :y 8.5 :xanchor "left" :text "Elbow fracture" :showarrow false}
              {:x 35.25 :y 8.5 :xanchor "left" :text "Deutschland Tour" :showarrow false}
              {:x 39.25 :y 7.5 :xanchor "left" :text "Vacation" :showarrow false}]

      ; prepare breakpoints for plotting
      lines [{:type "line" :x0 19.5 :y0 0 :x1 19.5 :y1 9 :line {:width 1 :dash "dot"}}
             {:type "line" :x0 35 :y0 0 :x1 35 :y1 9 :line {:width 1  :color "MediumVioletRed"}}
             {:type "line" :x0 39 :y0 0 :x1 39 :y1 9 :line {:width 1 :dash "dot"}}]

      ;prepare data for plotting
      data [{:x week-nums :y week-counts :name "Number of Rides" :type "bar"}
            {:x week-nums :y week-duration :name "Duration (Hours)" :type "bar"}]]
  (clerk/plotly {:data data
                 :layout {:title "Weekly Rides"
                          :annotations labels
                          :shapes lines
                          :plot_bgcolor "Snow"
                          :xaxis {:title {:text "Week Number"}}}}))

;; The question of how consistent I was in my riding was pretty easy to answer
;; even if I didn't have any data. The answer was **no**. 

;; In the chart above, showing the weekly number of rides I did as well as the 
;; volume in duration, there is a glaring big gap between week 20 and week 32, 
;; almost 3 months. I had broken my elbow then. The general prognosis of a fracture
;; is 8-12 weeks before life could be back to normal was pretty spot on because
;; I started riding again 12 weeks after the fracture. In fact, I started that week
;; with 2 rides totalling just under 2 hours, which was double what I did during the
;; start of the season.

;; This was because I had been training for my first race, the Deutschland Tour.
;; I could only train for 3 weeks leading up to the event. After the event
;; also saw a drastic decline in weekly volume, which was due to fatigue, but
;; also due to the fact that I was preparing to go on a 2-week vacation in China.
;; I didn't want to injure myself again!

;; But how consistent was I in the weeks that I _was able_ to train?

;; ### Measuring consistency

;; In this section, we look at several statistics which quantifies consistency for 
;; use in comparison.

;; #### Consecutive Weeks

;; In the table below, my longest week streak is 12 weeks. Which equates to 3 training
;; blocks, if the average length of a training block is 4 weeks. It looks alright at
;; first, but with the second longest streak being 5 weeks only (less than half of the
;; longest), it shows that I wasn't consistently pulling off training blocks.

^{:nextjournal.clerk/visibility {:code :fold}}
(let [; get all unique week numbers
      week-nums (-> dataset (ds/unique-by-column "Week") (get "Week"))

      ; segment weeks into groups of sequential weeks i.e. no jumps
      week-subseqs (group-subseqs week-nums)

      ; convert each subgroup into its size i.e. count
      conseq-week-counts (sort > (map count week-subseqs))

      longest-consecutive-weeks (first conseq-week-counts)
      second-longest-conseq-weeks (second conseq-week-counts)]
  (clerk/table {:head ["Metric" "Number of consecutive weeks"]
                :rows [["Longest" longest-consecutive-weeks]
                       ["Second longest" second-longest-conseq-weeks]]}))

;; #### Distribution of ride frequencies

;; A quarter (**23.8%**) of my weeks had **5 rides** and also only **1 ride**. 
;; Consistency for me means riding more than **3 times a week**. This accounts 
;; for **48% of my weeks**. _Less than half!_ If I were to be more lenient to include 
;; weeks with more than 2 rides a week, it would still only account for 62% of 
;; my weeks.

^{:nextjournal.clerk/visibility {:code :fold}}
(let [; group data by week
      week-groups (-> dataset (ds/group-by-column "Week"))

      ; get number of rides per week
      week-counts (map #(-> % ds/rows count) (vals week-groups))

      ; get total number of weeks
      total-weeks (count week-counts)

      ; get the frequency distribution of weekly count
      week-count-distrib (reduce (fn [acc cur]
                                   (if (nil? (get acc cur))
                                     (assoc acc cur 1)
                                     (update acc cur inc))) {} week-counts)

      ; prepare data for plotting
      data (sort (fn [a b] (< (:num a) (:num b)))
                 (map (fn [e] {:x [(round-single (* (/ (val e) total-weeks) 100))]
                               :y [""]
                               :num (key e)
                               :name (str (key e) " rides/wk.")
                               :orientation "h"
                               :type "bar"}) week-count-distrib))]
  (clerk/plotly {:data data
                 :layout {:title "Frequency distribution - Number of rides per week"
                          :height 310 :barmode "stack"}}))

;; #### Weekly differences in ride volume 

;; This statistic also corroborates the fact that I am not consistent. It shows
;; that my ride volume in hours vary on average from one week to the next by almost **3 hours**.
;; If we follow the principle of progressive loading, we would want this to ideally
;; be at around **0.5 hours** (the amount of time we increase weekly ride volume).

^{:nextjournal.clerk/visibility {:code :fold}}
(let [; group data by week
      week-group (-> dataset (ds/group-by-column "Week"))

      ; get number of rides per week
      week-counts (map #(-> % ds/rows count) (vals week-group))

      ; get total duration of rides per week in hours
      week-duration (map #(reduce + (get % "MovingTime")) (vals week-group))

      ; function to get the sequential difference in a series
      x->dx (fn [xs] (reduce (fn [a c]
                               (->> (last a) (- c) Math/abs (conj a)))
                             [(first xs)] (rest xs)))

      ;; function to get the average of a collection
      avg (fn [s] (/ (reduce + s) (count s)))
      ;; function to get the median of a collection
      median (fn [s] (let [sorted (sort s)
                           n (count sorted)
                           m (if (even? n) (/ n 2) (Math/ceil (/ n 2)))]
                       (nth sorted (dec m))))

      mean-weekly-diff-dur (-> week-duration x->dx avg round-single)
      mean-weekly-diff-count (-> week-counts x->dx avg round-single)
      median-weekly-diff-dur (-> week-duration x->dx median round-single)
      median-weekly-diff-count (-> week-counts x->dx median)]
  (clerk/table {:head ["Metric" "Mean weekly difference" "Median weekly difference"]
                :rows [["Duration (hours)" mean-weekly-diff-dur median-weekly-diff-dur]
                       ["Count" mean-weekly-diff-count median-weekly-diff-count]]}))

;; ### Key Findings in Consistency

;; - The cycling season was **split into 3** due to an injury and a 2 week vacation
;; - The **longest consecutive week streak** was 12, a big jump from the second 
;;   longest: 5
;; - Less than half of the weeks spent training had **more than 3 rides**
;; - Riding volume varies greatly from week to week with an average 
;;   **weekly difference of 2.9 hours or 2.2 rides**
