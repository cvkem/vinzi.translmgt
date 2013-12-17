(ns vinzi.translmgt.core
  (:use [clojure.pprint])
  (:require [clojure
              [string :as str]]
            [clojure.java
             [io :as io]]
            [debug-repl.debug-repl :as dr]
            [vinzi.tools 
             [vDateTime :as vDate]
             [vExcept :as vExcept]
	     [vFile :as vFile]
             [vProperties :as vProps]]))

(def Props "translMgt.properties")

(def DefBase (let [ymd (vDate/get-ymd-date)]
               (str/join "-" (map ymd [:year :month :day]))))


(defn notification 
  "Entry point for generating notifications." 
  [& args]
  (apply println args))

(defn warning 
  "Entry point for generating warnings." 
  [& args]
  (apply notification (concat (list "WARNING") args)))



(defn get-properties 
  "Read a properties file and return the data as a sequence of maps
   with keys (ordinal_nr, key, value, comments)."
  [fName]
  (let [lpf "(get-properties): "
        emptyProp {:key nil :value nil :comments []}
        res (with-open [inp (io/reader fName)]
              (loop [lines (line-seq inp)
                     currProp emptyProp
                     cumm    [] ]
                ;;(println "cumm now is: " cumm "  and currProp=" currProp)
                (if (seq lines)
                  (let [line (str/trim (first lines)) ;; spaces stripped
                        lines (rest lines)]
                    (if (seq line)
                      (if (.startsWith line "#") 
                        (let [currProp (assoc currProp :comments
                                        (conj (:comments currProp) line))] 
                          (recur lines currProp cumm)) ;; comment added
                        (let [[k & vs] (str/split line #"=")
                              vs (str/join "" vs)
                              prop (assoc currProp :key k :value vs)
                              cumm (conj cumm prop)]
                           (recur lines emptyProp cumm))) ;; property
                      (recur lines currProp cumm))) ;;ignore empty lines
                  (if (= currProp emptyProp)
                    cumm
                    (conj cumm currProp)))))
        res (map #(assoc %1 :ordinal_nr %2) res (rest (range)))]
     res))


;; let global proberties
(let [props (if (vFile/file-exists Props)
   	     (let [p (vProps/read-properties Props)]
                (notification (with-out-str (pprint p)))
                p)
             (do
               (warning "Could not find the property-file: " Props)
               {}))
      {:keys [git_repo_home translation_folder 
              base_language translations]} props
      translations (str/split translations #",")]


  (defn get-properties-file 
    "Extend the properties with a relative filename."
    [fName]
    (let [path (apply str (drop (count git_repo_home) fName))] 
      (map #(assoc % :file path) (get-properties fName))))


  (defn get-language-params 
    "Get the folder containing the language-package. 
     This function returns a hashmap with:
       - packageFolder:  folder containing the language pack
       - packageTail:  tail of the propertiesfiles for this language
       - translFolder: folder containing the translations" 
    [lang base]
    (let [base (or base DefBase)
          _ (dr/debug-repl)
          transFld (vFile/filename git_repo_home translation_folder lang) ]
      {:packageFolder (vFile/filename git_repo_home lang) 
       :packageTail  (str "_" lang ".properties")
       :translFolder transFld
       :translFile   (vFile/filename transFld (str base "_translation.edn"))
       }))


  (defn new-translation [[lang base & rst]]
    (let [lpf "(new-translation): "
          _ (when-not lang
              (vExcept/throw-except lpf "required first parameter"
                                    " <lang> missing."))
         ; _ (dr/debug-repl)
          {:keys [packageFolder packageTail
                  translFolder translFile]} (get-language-params lang base)
           ]
      (notification " Generate language " lang " from source " packageFolder
            "\n\tand emitting result to " translFile )
      (vFile/ensure-dir-exists translFile)
      (let [pFiles (->> packageFolder
                    (vFile/list-files )
                   (filter #(.endsWith (.getName %) packageTail)))]
        (doseq [f pFiles]
          (println (.getCanonicalPath f)))
        (->> pFiles
             (map #(.getCanonicalPath %) )
             (map get-properties-file )
             (apply concat ))
      )
  ))

  (defn check-updates 
    [lang prevBase newBase]
    )

  (defn expand-translation
    [lang base]
    )

  (defn help []
   (notification "Usage:   <command> <arg1> ...<argn>")
   (notification "Properties of the translation are stored in " Props)
   (notification "Where command is:")
   (notification (str " - new-translation <lang> [base]: "
                      " Store translation <lang> "
                       "to the  git_repo_home/ folder as defined in " 
                       Props
                       " where [base] is prefix of the output-file."
                       " if no [base] is given the current date is used."))
    (notification (str "expand-translation <lang> [base]:"
                       " Expands the language-pack <lang> to the set of" 
                       " individual properties paths/files. Existing files"
                       " are overwritten."))
    (notification (str "check-updates <lang> [prevbase] [newbase]:"
                       " Check for updates in the base language and move"
                       " the updates to the translation <lang>" 
                       " So new keys are added and for existing keys it is "
                       " checked whether there are changes in the base"
                       " language. Each language item gets a status-flag"
                       " with value New, Modified, Unmodified"))
    (notification (str "sort-translation <lang> <sort> [base]:"
                        " sort the translation and overwrite the existing"
                        " file with [base]. The <sort> should have on of"
                        " the values status-origKey, status-origValue,"
                        " origKey-status, origVal-status (case-insensitive)"
                        " The check-updates uses ordering origKey-status"
                        " Such that items with the same key are shown"
                        " next to one another (focus on consistent"
                        " translation of the same key)"
                        )))

) ;; let global properties

(defn -main [& args]
   
   (if (> (count args) 0)
     (case (first args)
        "new-translation" (apply new-translation (rest args))
;;                 (concat (list (props :base_language) "")(rest args)))
        "expand-translation" (apply expand-translation (rest args))
        "check-updates"       (apply check-updates (rest args))
        (do
          (warning "Incorrect command: " (first args))
          (help)))
     (do 
       (warning "Provide a command as first argument")
       (help))))

