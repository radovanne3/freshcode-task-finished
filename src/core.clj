(ns core)

(def all-patients
  '({:firstname "Adam"
     :lastname  "Smith"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Joseph"
     :lastname  "Goodman"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Werner"
     :lastname  "Ziegler"
     :diagnosis "COVID-19"
     :treated   false}
    {:firstname "Boris"
     :lastname  "Henry"
     :diagnosis "Healthy"
     :treated   false}
    {:firstname "Johnny"
     :lastname  "Grayhand"
     :diagnosis "COVID-76"
     :treated   false}))

;; workaround --find explanation to do better?
(defn treated? [param] param)
(defn disease-name [param] param)

(defn group-data
  "Function for filtering patient-group based on input vector"
  [patients cond]
  (group-by #(select-keys % cond) patients))


(defn body
  "Body function that will run on every patient-group"
  [[k v]]
  (let [patients-group v
        disease-name (:diagnosis k)
        treated? (:treated k)]
    (println " processing patients with " disease-name
             ", that " (if treated? "WERE" "WERE NOT") " treated")

    (println " the count of patients is " (count patients-group))
    (println " patients' lastnames are: " (clojure.string/join ", " (map :lastname patients-group)))

    ; let's return a count of patients for each group
    (count patients-group)
    ))


(defmacro factor-group
  [patients group-data cond-vec body]
  `(let [patients-group# (~group-data ~patients ~cond-vec)]
     (map #(body %) patients-group#)))


(factor-group all-patients group-data [treated? :treated disease-name :diagnosis] body)























