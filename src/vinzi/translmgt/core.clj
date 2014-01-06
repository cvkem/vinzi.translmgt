(ns vinzi.translmgt.core
  (:use [clojure.pprint]
        [vinzi.tools [vSql :only [qs]]])
  (:require [clojure
              [edn :as edn]
              [set :as set]
              [string :as str]]
            [clojure.java
             [io :as io]]
            [debug-repl.debug-repl :as dr]
            [vinzi.tools 
             [vDateTime :as vDate]
             [vEdn :as vEdn]
             [vExcept :as vExcept]
	     [vFile :as vFile]
             [vProperties :as vProps]
             [vRelation :as vRel]]
            ))

(def Props "translMgt.properties")

(def TransEdn "translation.edn")    

;;(def DefBase (let [ymd (vDate/get-ymd-date)]
;;               (str/join "-" (map ymd [:year :month :day]))))
(def DefBase "")


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
                              vs   (->> vs
                                        (str/join "" )
                                        (#(str/replace % "<TRANSLATE ME>" "") ))
                              prop (assoc currProp :key (str/trim k) :value vs)
                              cumm (conj cumm prop)]
                           (recur lines emptyProp cumm))) ;; property
                      (recur lines currProp cumm))) ;;ignore empty lines
                  (if (= currProp emptyProp)
                    cumm
                    (conj cumm currProp)))))
        res (map #(assoc %1 :ordinal_nr %2) res (rest (range)))]
     res))


(defn write-properties 
  "Write the props data to a (plain) property file."
  [fName transl]
  (let [transl (sort-by :ordinal_nr transl)
        item-str (fn [{:keys [comments key value]}]
                   (let [comments (if (seq comments)
                                    (str (str/join "\n" comments) \newline)
                                    "")
                         kv (str key \= (qs value))]
                     (str comments kv)))
;;        _ (dr/debug-repl)
        content (->> transl
                     (map item-str )
                     (str/join "\n" ))]
    (vFile/ensure-dir-exists fName)
    (spit fName content)))


;; let global proberties
(let [props (if (vFile/file-exists Props)
   	     (let [p (vProps/read-properties Props)]
                (notification (with-out-str (pprint p)))
                p)
             (do
               (warning "Could not find the property-file: " Props)
               {}))
      {:keys [git_repo_home translation_folder data_folder
              base_language base_language_path
              translations]} props
      translations (str/split translations #",")]


  (defn get-properties-file 
    "Extend the properties with a relative filename."
    [packageFolder fName]
    {:pre [(.startsWith fName packageFolder)]}
    (let [dropCnt (-> (count packageFolder)
                      (#(if (.endsWith packageFolder vFile/FileSep)
                          %
                          (inc %)) ))
          relPath (apply str (drop dropCnt fName))] 
      (when-not (seq relPath)
        (println "computed relPath=" relPath "  for fName="fName " in packageFolder="packageFolder))
      (map #(assoc % :path relPath) (get-properties fName))))



  (defn get-checked-lang-params 
    "Get the folder containing the language-package. 
     This function returns a hashmap with:
       - packageFolder:  folder containing the language pack
       - packageTail:  tail of the propertiesfiles for this language
       - translFolder: folder containing the translations
      Als ensures the translation folder exists." 
    ([lang] (get-checked-lang-params lang nil))
    ([lang base]
      (let [lpf "(get-checked-lang-params): "
            base (or base DefBase)
            base (if (seq base)
                   (str base "_") "")
            _ (when-not lang
                (vExcept/throw-except lpf "required first parameter"
                                      " <lang> missing."))
            packageFolder (if (= lang base_language)
                            base_language_path
                            (vFile/filename git_repo_home data_folder lang))
            transFld (vFile/filename git_repo_home translation_folder lang)]
        (when-not (vFile/dir-exists packageFolder)
          (vExcept/throw-except lpf "Folder " packageFolder 
                                " does not exist"))
        ;; create folder if it does not exists yet
        (.mkdirs (io/file transFld))
        {:packageFolder packageFolder
         :packageTail  (str "_" lang ".properties")
         :translFolder transFld
         :translFile   (vFile/filename transFld (str base TransEdn))
         })))


  (defn read-language-from-properties 
    "Read a translation from a series of property-files."
    [{:keys [packageFolder packageTail]}]
    (let [lpf "(read-language-from-properties): "]
      (let [pFiles (->> packageFolder
                    (vFile/list-files )
                   (filter #(.endsWith (.getName %) packageTail)))]
        (doseq [f pFiles]
          (println (.getCanonicalPath f)))
        (->> pFiles
             (map #(.getCanonicalPath %) )
             (map (partial get-properties-file packageFolder) )
             (#(do (pprint (first %))
                   %))
        ;    (map get-properties)
             (apply concat )))))


  (defn read-translation 
    "Read the (current) language translation."
    [translFile]
    (vEdn/read-edn-file translFile))
;;    (edn/read-string (slurp translFile)))


  (defn write-translation
    "Write a translation to an edn file."
    [translFile transl]
    (println "Writing to file: " translFile)
    (spit translFile (with-out-str (pprint transl))))


  (defn new-translation 
    "Create a new translation file."
    [[lang base & rst]]
    (let [lpf "(new-translation): "
          {:keys [packageFolder packageTail
                  translFolder translFile] :as lPars} 
               (get-checked-lang-params lang base)]
      (when (vFile/file-exists translFile)
        (vExcept/throw-except lpf " File " translFile 
                              " exists. Can not overwrite it"))
      (let [transl (read-language-from-properties lPars)]
        (write-translation translFile transl))))

   (defn all-new-translations
     "Generate a new translation for all languages that exist in the data-folder"
     []
     (let [lpf "(all-new-translations): "]
       (println "To be implemented!")
       ))

  (defn check-updates 
    "Compare a base-language to the current language and update the status-flags.
     This function returns a new translation (with actual status-flags and added
     new items.)"
    [[lang]]
    (let [pars (get-checked-lang-params lang)
          bPars (get-checked-lang-params base_language)
          joinKeys [:path :key]
          report-count #(do (println %1 "seq has " (count %2) " elements") 
                           %2)
          currLang (->  (read-translation (:translFile pars))
                       ((partial report-count "curr" ))
                       (vRel/split-recs joinKeys :curr))
          renamePath (fn [r] (assoc r :path 
                               (str/replace (:path r) 
                                 (re-pattern (str "_" base_language)) 
                                                    (str "_" lang)))) 
          baseLang (-> (read-translation (:translFile bPars))
                       ((partial report-count "baseLang" ))
                       (vRel/split-recs joinKeys :base)
                       (#(map renamePath %)))
          update-status (fn [{:keys [path key base curr] :as rec}]
                          (let [nRec (if (nil? base)
                                      (assoc curr :status "REMOVED")
                                        (if (nil? curr)
                                          (assoc base :base-lang-value (:value base)
                                            :value (str (:value base) " <TRANSLATE>")
                                            :status "TRANSLATE")
                                          (let [{:keys [status value base-lang-value]} curr]
                                            (if (not= (:value base) base-lang-value)
                                              (let [status (if base-lang-value
                                                             "UPDATE"
                                                             "TRANSLATE")]
                                                (assoc curr 
                                                      :base-lang-value (:value base)
                                                      :value (str value " <" status ">")
                                                       :status status))
                                              (if (re-find #"<UPDATE>|<TRANSLATE>"
                                                           value) ;; retain status
                                                (assoc rec :status "ok")
                                                rec)))))]
                            (assoc nRec :path path :key key)))
          _ (do
              (pprint (first currLang))
              (pprint (first baseLang))
              (report-count "curr-split" currLang)
              (report-count "base-split" baseLang)
              (print "and first from base-language")
              (def B (take 10 (sort-by #(vec (map % joinKeys)) baseLang)))
              (def C (take 10 (sort-by #(vec (map % joinKeys)) currLang)))
              )
          updatedLang (->> (set/join currLang baseLang)

                       ((partial report-count "joined" ))
                           (map update-status ))]
      (println "Updated language has: " (count updatedLang) " elements.")
    (write-translation (:translFile pars) updatedLang)))


  (def TestEmit "/tmp/plp")

  (defn expand-translation
    "Read the edn-file and expand it to the series of property-files (language files)."
    [[lang base & rst]]
    (let [lpf "(expand-translation): "
          {:keys [packageFolder packageTail
                  translFolder translFile] :as lPars} 
               (get-checked-lang-params lang base)]
      (when-not (vFile/file-exists translFile)
        (vExcept/throw-except lpf " File " translFile 
                              " does not exists. Can not expand."))
      (let [transl (edn/read-string (slurp translFile))
            _ (def TRANS transl)
            transl (->> transl 
                        (group-by :path ))
            basePath (if (seq TestEmit) TestEmit packageFolder)]
        (.mkdirs (io/file basePath))
        (def T1 transl)
        (doseq [[fName tr] transl]
          (let [fullPath (vFile/filename basePath fName)] 
            (def Tc tr)
            (println "emit to file: " fullPath)
;;            (println " value tr of type: " (type tr))
          (write-properties fullPath tr))))))


(defn help []
   (notification "Usage:   <command> <arg1> ...<argn>")
   (notification "Where command is:")
   (notification (str "\n - new-translation <lang> [base]: \n"
                      " Store translation <lang> "
                       " to the  <git_repo_home>/<data_folder> folder as defined in " 
                       Props
                       " where [base] is prefix of the output-file."
                       " if no [base] is given an empty string is used"
                       " (git used for versioning).\n"
                       " The translation is written to "
                       " <git_repo_home>/<translations>/<lang>/" TransEdn ))
   (notification (str "\n - all-new-translations : \n"
                       "  For all folders in <git_repo_home>/<data_folder>"
                       "  create a translation if it does not exist yet")) 
    (notification (str "\n - expand-translation <lang> [base]:\n"
                       " Expands the language-pack <lang> to the set of" 
                       " individual properties paths/files. Existing files"
                       " are overwritten."))
    (notification (str "\n -  check-updates <lang> [prevbase] [newbase]:\n"
                       " Check for updates in the base language and move"
                       " the updates to the translation <lang>" 
                       " So new keys are added and for existing keys it is "
                       " checked whether there are changes in the base"
                       " language. Each language item gets a status-flag"
                       " with value New, Modified, Unmodified"))
    (notification (str "\n - sort-translation <lang> <sort> [base]:\n"
                        " sort the translation and overwrite the existing"
                        " file with [base]. The <sort> should have on of"
                        " the values status-origKey, status-origValue,"
                        " origKey-status, origVal-status (case-insensitive)"
                        " The check-updates uses ordering origKey-status"
                        " Such that items with the same key are shown"
                        " next to one another (focus on consistent"
                        " translation of the same key)"
                        ))
   (notification "\nProperties of the translation are stored in " Props)
    )

) ;; let global properties

(defn -main [& args]
   
   (if (> (count args) 0)
     (case (first args)
        "new-translation" (new-translation (rest args))
        "all-new-translations" (all-new-translations (rest args))
;;                 (concat (list (props :base_language) "")(rest args)))
        "expand-translation" (expand-translation (rest args))
        "check-updates"       (check-updates (rest args))
        "help"     (apply help (rest args))
        (do
          (warning "Incorrect command: " (first args))
          (help)))
     (do 
       (warning "Provide a command as first argument")
       (help))))

