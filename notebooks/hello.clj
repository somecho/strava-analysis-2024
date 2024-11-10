{:nextjournal.clerk/visibility {:code :hide :result :hide}}

(ns hello
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
                         (assoc (.indexOf cols "Activity Date")
                                "ActivityDate")
                         (assoc (.indexOf cols "Moving Time")
                                "MovingTime")
                         (assoc (.indexOf cols "Max Speed")
                                "MaxSpeed")
                         (assoc (.indexOf cols "Average Speed")
                                "AvgSpeed")
                         (assoc (.indexOf cols "Elevation Gain")
                                "ElevationGain")
                         (assoc (.indexOf cols "Max Heart Rate")
                                "MaxHeartRate")
                         (assoc (.indexOf cols "Average Heart Rate")
                                "AvgHeartRate"))]
    (-> raw-data
        ; Remove commutes and get only rides from 2024
        (ds/filter #(and (= 0.0 (get % "Commute"))
                         (= "Ride" (get % "Activity Type"))
                         (= 2024 (-> (get % "Activity Date") parse-date .getYear))))
        ; Remove activities where heart rate and distance data is missing
        (ds/filter #(and (some? (get % "Average Heart Rate"))
                         (some? (get % "Max Heart Rate"))
                         (not= 0.0 (get % "Distance"))))
        ; Get only relevant cols
        (ds/select-columns cols)
        ; Rename cols
        (ds/rename-columns renamed-cols)
        ; Add Week, Month, DayOfYear column and convert speed from m/s to km/h
        (ds/row-map (fn [{:strs [ActivityDate MaxSpeed AvgSpeed]}]
                      (let [parsed (parse-date ActivityDate)]
                        {"Week" (get-week parsed)
                         "Month" (.getMonthValue parsed)
                         "DayOfYear" (.getDayOfYear parsed)
                         "MaxSpeed" (round-single (* MaxSpeed 3.6))
                         "AvgSpeed" (round-single (* AvgSpeed 3.6))}))))))

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/table {::clerk/page-size 5} dataset)

;; The number of activities in the cleaned dataset:

(-> dataset ds/rows count)

;; Those of you who are also into cycling will notice that a key metric
;; is missing: power data. As of the end of this year's season, I do not
;; yet have a power meter, so some metrics will be hard to objectively
;; extrapolate, such as performance improvement. Another metric that is 
;; also missing is _rate of perceived exertion_ or _RPE_. 
