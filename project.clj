(defproject vinzi.translmgt "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
		[org.clojure/tools.logging "0.2.6"]
                 ;; needed only to compile vinzi.tools
                [org.clojure/java.jdbc "0.2.3"]
  		[org.slf4j/slf4j-api "1.6.5"]
  		[ch.qos.logback/logback-core "1.0.6"]
  		[ch.qos.logback/logback-classic "1.0.6"]
                 [vinzi/vinzi.tools "0.2.0-SNAPSHOT"]])
