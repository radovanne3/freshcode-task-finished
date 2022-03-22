(ns core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))


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

;; specs for all-patients coll-of-maps
;; beginning
(s/def ::all-patients-spec
  (s/coll-of (s/keys :req-un [::firstname ::lastname ::diagnosis ::treated])))

(s/def ::firstname string?)
(s/def ::lastname string?)
(s/def ::diagnosis string?)
(s/def ::treated boolean?)

(s/valid? ::all-patients-spec all-patients)
;; end


;; workaround --find explanation to do better?
(defn treated? [param] param)
(defn disease-name [param] param)
(defn first-name [param] param)
(defn last-name [param] param)

;; specs for treated?, disease-name, first-name, lastname functions ( should I use :pre :post in this case? , code repetition? )
;; beginning
(s/fdef treated?
        :args (s/cat :param keyword?))

(s/fdef disease-name
        :args (s/cat :param keyword?))

(s/fdef first-name
        :args (s/cat :param keyword?))

(s/fdef last-name
        :args (s/cat :param keyword?))



(stest/instrument 'core/treated?)
(stest/instrument 'core/disease-name)
(stest/instrument 'core/first-name)
(stest/instrument 'core/last-name)
;; end


(defn group-data
  "Function for filtering patient-group based on input vector,
  In case of  ~empty vector supplied as cond we will create similar structure for output value"
  [patients cond]
  (if (>= (count cond) 2)
    (group-by #(select-keys % cond) patients)
    (let [interleave-param (map (fn [x] (select-keys x [:treated :diagnosis :firstname :lastname])) patients)]
      (partition 2 (interleave interleave-param (vec patients))))))


;; spec for group-data function
;; beginning
(s/fdef group-data
        :args (s/cat :patients ::all-patients-spec :cond vector?))

(stest/instrument 'core/group-data)
;; end


(defn body
  "Body function that will run on every patient-group"
  [[k v]]
  (let [patients-group v
        disease-name (cond
                       (= (:diagnosis k) nil) (map :diagnosis v)
                       :else (:diagnosis k))
        treated? (cond
                   (= (:treated k) nil) (map :treated v)
                   :else (:treated k))]
    (println " processing patients with " disease-name
             ", that " (if treated? "WERE" "WERE NOT") " treated")

    (println " the count of patients is " (count patients-group))
    (println " patients' lastnames are: " (clojure.string/join ", " (map :lastname patients-group)))

    ; let's return a count of patients for each group
    (count patients-group)))

;; spec for body function
;; beginning

(s/fdef body
        :args (s/coll-of (s/cat :key map? :val vector?)))

(stest/instrument 'core/body)
;; end


(defmacro factor-group
  [patients group-data cond-vec body]
  `(let [patients-group# (~group-data ~patients ~cond-vec)]
     (map #(~body %) patients-group#)))






























