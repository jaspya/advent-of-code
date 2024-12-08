#!/usr/bin/env bb

(require '[babashka.cli :as cli]
         '[babashka.fs :as fs]
         '[babashka.http-client :as http])

(def cli-options {:year {:default 2024 :coerce :long}
                  :day {:coerce :long}})

(defn print-required-env-help
  []
  (println "Store you Advent of Code session cookie value in AOC_SESSION\n")
  (println "export AOC_SESSION=\"\""))

(defn print-required-args-help
  []
  (println "Missing arguments, year and/or day.\n")
  (println "Call with `--year 2024 --day 1`"))

(defn input-path
  [day]
  (str "2024/clojure/resources/day" day ".input"))

(defn download-input
  [year day session]
  (let [url (str "https://adventofcode.com/" year "/day/" day "/input")
        response (http/get url {:headers {"Cookie" (str "session=" session)}})]
    (spit (input-path day) (:body response))))

(defn start
  []
  (let [{:keys [year day]} (cli/parse-opts *command-line-args* {:spec cli-options})
        session (System/getenv "AOC_SESSION")]
    (cond (fs/exists? (input-path day))
          (println "Input file already exists")

          (and year day session)
          (download-input year day session)

          (not session)
          (print-required-env-help)

          :else
          (print-required-args-help))))

(start)
