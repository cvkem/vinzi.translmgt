(ns vinzi.translmgt.excel
  (:use [clojure.pprint])
  (:require [clojure
              [edn :as edn]
              [set :as set]
              [string :as str]]
            [clojure.java
             [io :as io]]
            [debug-repl.debug-repl :as dr]
            [vinzi.clj-excel.core :as ce]
            [vinzi.tools
             [vCsv :as vCsv]]))



(defn write-as-xls 
  "Write data (sequential of similar hashmaps) to excel-file with path 'fPath'."
  [fPath data columnOrder]
  (let [wb (ce/workbook)  ;; new/empty work-book
        _ (println "wb=" wb "  of type " (type wb))
        sheets (ce/sheets wb)
        s1   (first sheets)
        _ (println "s1=" s1  "  of type " (type s1))
        dataArray (if (seq columnOrder)
                    (vCsv/map-seq-to-csv data columnOrder :sortHeader false)
                    (vCsv/map-seq-to-csv data))]
    (ce/merge-rows wb 0 dataArray)
    (ce/save wb fPath) 
  ))


(def ConcatenateMark "&|&")
(def ReConcatenateMark #"&\|&")

(def OrderedKeys [:path :key :linenr :base-lang-duplic :base-lang-value :value :status :comments])

(defn write-as-excel
  [fPath data]
  ;;  concatenate comment to a single string (excel does not support vectors.
  (let [data (map #(assoc % :comments (str/join ConcatenateMark (map str (:comments %)))) data)]
    (write-as-xls fPath data OrderedKeys)))


(defn read-from-excel
  "Read the files from excel and split comments again."
  [fPath]
  (let [wb (ce/workbook fPath)
        data (->> (ce/get-lazy-sheet wb 0)
                  (vCsv/csv-to-map )
                  ;; split comments when string is not empty
                  (map #(if-let [c (:comments %)]
                          (assoc % :comments (str/split c ReConcatenateMark))
                          %) ))]
    data))




