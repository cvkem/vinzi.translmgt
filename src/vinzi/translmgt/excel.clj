(ns vinzi.translmgt.excel
  (:use [clojure.pprint])
  (:require [clojure
              [edn :as edn]
              [set :as set]
              [string :as str]]
            [clojure.java
             [io :as io]]
        ;;    [debug-repl.debug-repl :as dr]
            [vinzi.tools
             [vCsv :as vCsv]
             [vExcel :as vExcel]]))



(defn write-as-xls 
  "Write data (sequential of similar hashmaps) to excel-file with path 'fPath'."
  [fPath data columnOrder]
  (let [wb (vExcel/workbook)  ;; new/empty work-book
        dataArray (if (seq columnOrder)
                    (vCsv/map-seq-to-csv data columnOrder :sortHeader false)
                    (vCsv/map-seq-to-csv data))]
    (vExcel/merge-rows wb 0 0 dataArray)
    (vExcel/save wb fPath) 
  ))


(def ConcatenateMark "&|&")
(def ReConcatenateMark #"&\|&")

(def OrderedKeys [:path :key :linenr :base-lang-duplic :base-lang-value :value :status :comments])

(defn write-as-excel
  "Write the data representing a represenation as a new excel file."
  [fPath data]
  ;;  concatenate comment to a single string (excel does not support vectors.
  (let [data (map #(assoc % :comments (str/join ConcatenateMark (map str (:comments %)))) data)]
    (write-as-xls fPath data OrderedKeys)))


(defn read-from-excel
  "Read the files from excel and split comments again."
  [fPath]
  (let [wb (vExcel/workbook fPath)
        parse-num (fn [v]
                    ;; outputs first matching type long, double or nil
                    (when (seq v)   ;; empty string --> nil
                      (let [v (Double/parseDouble v)
                            vl (long v)]
                        (if (= (- v vl) 0.0)
                          vl
                          v))))
        data (->> (vExcel/get-lazy-sheet wb 0)
                  (vCsv/csv-to-map )
                  ;; split comments when string is not empty
                  (map #(if-let [cmt (:comments %)]
                          (let [cmt (str/split cmt ReConcatenateMark)
                                cmt (if (= cmt [""])  
                                      []    ;; replace empty array by empty array
                                      cmt)]
                            (assoc % :comments cmt))
                          %) )
                  ;; type conversions
                  (map #(assoc %  :linenr (parse-num (:linenr %))
                                  :base-lang-duplic (parse-num (:base-lang-duplic %))) ))] 
    data))




