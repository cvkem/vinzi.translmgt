(ns vinzi.translmgt.core
  (:use [clojure.pprint]
        [vinzi.tools [vSql :only [qs]]])
  (:require [clojure
              [edn :as edn]
              [set :as set]
              [string :as str]]
            [clojure.java
             [io :as io]]
            [vinzi.translmgt.excel :as xls]
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

(def StoreExcel true)

(def TransFile (if StoreExcel
                 "translation.xls"
                 "translation.edn"))

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
   with keys (linenr, key, value, comments)."
  [fName]
  (let [lpf "(get-properties): "
        _ (println lpf fName)
        emptyProp {:key nil :value nil :comments []}
        res (with-open [inp (io/reader fName)]
              (loop [lines (line-seq inp)
                     currProp emptyProp
                     cumm    [] 
                     linenr  1]
                ;;(println "cumm now is: " cumm "  and currProp=" currProp)
                (if (seq lines)
                  (let [line (first lines) ;; spaces stripped
                        lines (rest lines)]
                    (if (seq line)
                      (if (.startsWith line "#")  ;; TODO:  (re-find #"^\s*#")
                        (let [currProp (assoc currProp :comments
                                        (conj (:comments currProp) line))] 
                          (recur lines currProp cumm (inc linenr))) ;; comment added
                        (let [[k & vs] (str/split line #"=")
                              vs   (->> vs
                                        (str/join "" )
                                        (#(str/replace % "<TRANSLATE ME>" "") ))
                              prop (assoc currProp :key (str/trim k) 
                                                   :value vs
                                                   :linenr linenr)
                              cumm (conj cumm prop)]
                           (recur lines emptyProp cumm (inc linenr)))) ;; property
                      (recur lines currProp cumm (inc linenr)))) ;;ignore empty lines
                  ;; add current item when nog empty
                  (if (= currProp emptyProp)
                    cumm
                    (conj cumm currProp)))))
        ;; remove items without a key
        res (filter #(seq (:key %)) res)
;;        res (map #(assoc %1 :ordinal_nr %2) res (rest (range)))
        remove-duplic (fn [res]
                       (let [res (group-by :key res)
                             res (map #(sort-by :linenr %) (vals res))
                             select-last (fn [items]
                                           (when (> (count items) 1)
                                             (println "multiple items with key: " (:key (first items)) 
                                                      " on lines: " (str/join "," (map :linenr items)) " Keeping item:")
                                             (pprint (last items)))
                                           (last items))
                             res (doall (map select-last res))]
                         res))
        res (remove-duplic res)]
     res))


(defn write-properties 
  "Write the props data to a (plain) property file."
  [fName transl]
  (let [transl (sort-by :linenr transl)
        item-str (fn [{:keys [comments key value]}]
                   (let [comments (if (seq comments)
                                    (str (str/join "\n" comments) \newline)
                                    "")
                         kv (str key \= value)]
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
         :translFile   (vFile/filename transFld (str base TransFile))
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
    (if StoreExcel
      (xls/read-from-excel translFile)
      (->> translFile 
           (vEdn/read-edn-file )
           (map #(dissoc % :DUPLICATE) ))))


  ;; order used when writing to file (all tekst mapped to lower-case)
  (def translOrderFunc (fn [rec]
                         (str/join "::" (map #(str/lower-case (% rec)) [:base-lang-value :path]))))

  (defn write-translation
    "Write a translation to an edn file."
    [translFile transl]
    {:pre [(string? translFile) (sequential? transl)]}
    (println "Writing to file: " translFile)
    (let [transl (sort-by translOrderFunc transl)] 
      (time (if StoreExcel
              (do
                (println "Be patient. Building a new excel workbook might take a minute ...")
                (xls/write-as-excel translFile transl)) 
              (spit translFile (with-out-str (pprint transl)))))
      (println "Translation written.")))



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



  (defn fix-translation 
    "TEMPORARY FIX: Create a new translation file. Temporary code to merge in missing items for nl-language.
       NOTE: This function contains code to find duplicate base-language values and code to 
       automatically translate when strings appear multiple times in the base language
       (marked with flag AUTO)."
    [[lang base & rst]]
    (let [lpf "(fix-translation): "
          {:keys [packageFolder packageTail
                  translFolder translFile] :as lPars} 
               (get-checked-lang-params lang base)]
      (let [joinKeys [:path :key]
            keyFn   #(str (:path %) "::" (:key %))
            trProps (->> (read-language-from-properties lPars)
                         (map #(assoc %
                                      :status "NEW-PROP"
                                      :base-lang-value (:value %)) )
                         (#(zipmap (map keyFn %) %) ))
            trEdn   (->> (read-translation translFile)
                         (#(zipmap (map keyFn %) %) ))
            tr      (->> (into trProps trEdn)
                         (vals ))
            ;; comparison on lower-case and after trimming spaces
            trg     (-> (group-by #(str/lower-case (str/trim (str (:base-lang-value %)))) tr)
                        (vals ))
            fix-group (fn [trs]
                        (if (<= (count trs) 1)
                          trs
                        (let [prt-f  #(= (:status %) "NEW-PROP")
                              edn (remove prt-f trs)
                              edn-tr (set (map :value edn))
                              cnt    (count edn-tr)
                              prp (filter prt-f trs)]
                         (if (>= cnt 1)
                           (let [trn (first edn-tr)
                                 status (if (= cnt 1) 
                                          "NEW-PROP-AUTO-TRANSLATE"
                                          (str "NEW-PROP-AUTO-OPTIONS" cnt))
                                 prp (map #(assoc % :value trn :status status) prp)]
                             (when (> cnt 1)
                               (println (count edn-tr) " translations of " (:base-lang-value (first edn)) ":" 
                                       (str/join ";" edn-tr) " Using: " trn))
                             (map #(assoc % :base-lang-duplic (count trs)) (concat edn prp)))  ;; return fixed list
                             trs))))  ;; return unmodified
            trgf     (->> (map fix-group trg)
                         (apply concat ))
                     ]
        (write-translation (str translFile ".fix") trgf)
         {:trProps trProps
          :trEdn   trEdn
          :tr      tr
          :trg     trg
          :trgf    trgf}
        )))


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
    (println "Processing language: " lang)
    (println " ==================  WAARSCHUWING ================================\n"
             "  code gebruikt INNER JOIN, en lines die niet in de base-language \n"
             " bestaan worden weggegooid ipv gemarkeerd!!\n"
             " Pas vRel aan zodat er een OUTER-JOIN in zit (relational)"
             "===================================================================")
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
                                            :value (str (:value base) "<T>")
                                            :status "TRANSLATE")
                                        (let [{:keys [status value base-lang-value]} curr]
                                          (if (not= (:value base) base-lang-value)
                                            (let [status (if base-lang-value
                                                           "UPDATE"
                                                           "TRANSLATE")]
                                              (assoc curr 
                                                      :base-lang-value (:value base)
                                                      :value (str value "<" (first status) ">")
                                                       :status status))
                                            (if (re-find #"<U>|<T>|<UPDATE>|<TRANSLATE>" value) 
                                                curr ;; retain status
                                                (assoc curr :status "ok"))))))]
                            (assoc nRec :path path :key key)))
;          _ (do
;              (pprint (first currLang))
;              (pprint (first baseLang))
;              (report-count "curr-split" currLang)
;              (report-count "base-split" baseLang)
;              (print "and first from base-language")
;              (def B (take 10 (sort-by #(vec (map % joinKeys)) baseLang)))
;              (def C (take 10 (sort-by #(vec (map % joinKeys)) currLang)))
;              )
          updatedLang (->> (set/join currLang baseLang)
                           ((partial report-count "joined" ))
                           (map update-status ))
          mark-duplicates (fn [tr]
                            (let [tr (vals (group-by #(map % [:path :key]) tr))
                                  mark-duplic (fn [x]
                                                (if (= (count x) 1)
                                                  x
                                                  (map #(assoc %1 :DUPLICATE %2) x (rest (range)))))]
                              (apply concat (map mark-duplic tr))))
          updatedLang (mark-duplicates updatedLang)
          ]
      (println "Updated language has: " (count updatedLang) " elements.")
    (write-translation (:translFile pars) updatedLang)))


 ;; (def TestEmit "/tmp/plp")
  (def TestEmit nil)

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
      (let [transl (read-translation translFile)
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
                       " <git_repo_home>/<translations>/<lang>/" TransFile ))
   (notification (str "\n - all-new-translations : \n"
                       "  For all folders in <git_repo_home>/<data_folder>"
                       "  create a translation if it does not exist yet" 
                       " NOTE: to be implemented"))
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
    (notification (str "\n -  prune-file <lang> <filename> <filename2> ...:\n"
                       " Remove one or more  files from the translation "
                       " and remove the files from the output-folder too. "
                       " NOTE: to be implemented"))
;    (notification (str "\n - sort-translation <lang> <sort> [base]:\n"
;                        " sort the translation and overwrite the existing"
;                        " file with [base]. The <sort> should have on of"
;                        " the values status-origKey, status-origValue,"
;                        " origKey-status, origVal-status (case-insensitive)"
;;                        " The check-updates uses ordering origKey-status"
;                        " Such that items with the same key are shown"
;                        " next to one another (focus on consistent"
;                        " translation of the same key)"
   (notification "\nProperties of the translation are stored in " Props)
    )

) ;; let global properties

(defn -main [& args]
   (println "Received args: " (str/join "," args)) 
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

