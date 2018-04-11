(ns fulcro-spec.impl.test
  (:require [cljs.test :as ct]))

;; ================================================================================
;; ENV
;; ================================================================================

(defonce tests-ref (atom {:namespaces {}}))

(defn register-test [test-ns test-name test-var]
  ;; register by name so reloading replaces the old test
  (swap! tests-ref assoc-in [:namespaces test-ns :vars test-name] test-var))

(defn register-fixtures [test-ns type fix]
  (swap! tests-ref assoc-in [:namespaces test-ns :fixtures type] fix))

(defn get-tests []
  (get @tests-ref :namespaces))

(defn get-test-ns-info [ns]
  {:pre [(symbol? ns)]}
  (get-in @tests-ref [:namespaces ns]))

(defn get-test-namespaces
  "returns all the registered test namespaces and symbols
   use (get-test-ns-info the-sym) to get the details"
  []
  (-> @tests-ref (:namespaces) (keys)))

(defn get-test-count []
  (->> (for [{:keys [vars] :as test-ns} (-> @tests-ref (:namespaces) (vals))]
         (count vars))
    (reduce + 0)))

;; ================================================================================
;; Replacement functions for test
;; ================================================================================

(declare test-ns-block)

(defn run-tests-block
  "Like test-vars, but returns a block for further composition and
  later execution."
  [env namespaces]

  (let [summary
        (volatile!
          {:test 0 :pass 0 :fail 0 :error 0
           :type :summary})

        merge-counters
        (fn []
          (vswap!
            summary
            (partial merge-with +)
            (:report-counters (ct/get-current-env))))]

    (-> [(fn [] (ct/set-env! env))]
      (into (->> namespaces
              (mapcat (fn [ns]
                        (-> (test-ns-block env ns)
                          (conj merge-counters))))))
      (conj (fn []
              (ct/report @summary)
              (ct/report (assoc @summary :type :end-run-tests))
              (ct/clear-env!))))))

(defn run-tests
  ([]
   (run-tests (ct/empty-env)))
  ([env]
   (run-tests env (get-test-namespaces)))
  ([env namespaces]
   (ct/run-block (run-tests-block env namespaces))))

(defn run-all-tests
  "Runs all tests in all namespaces; prints results.
  Optional argument is a regular expression; only namespaces with
  names matching the regular expression (with re-matches) will be
  tested."
  ([] (run-all-tests (ct/empty-env) nil))
  ([re] (run-all-tests (ct/empty-env) re))
  ([re env]
   (run-tests env
     (->> (get-test-namespaces)
       (filter #(or (nil? re)
                  (re-matches re (str %))))
       (into [])))))

(defn test-all-vars-block [ns]
  (let [env (ct/get-current-env)
        {:keys [fixtures each-fixtures vars] :as test-ns}
        (get-test-ns-info ns)]

    (-> [(fn []
           (when (nil? env)
             (ct/set-env! (ct/empty-env)))
           (when-let [fix (:once fixtures)]
             (ct/update-current-env! [:once-fixtures] assoc ns fix))
           (when-let [fix (:each fixtures)]
             (ct/update-current-env! [:each-fixtures] assoc ns fix)))]

      (into (ct/test-vars-block
              (->> vars                                     ;; vars is {test-name test-var}
                (vals)
                (sort-by #(-> % meta :line)))))
      #_(conj (fn []
                (when (nil? env)
                  (ct/clear-env!)))))))

(defn test-all-vars
  "Calls test-vars on every var with :test metadata interned in the
  namespace, with fixtures."
  [ns]
  (ct/run-block
    (-> (test-all-vars-block ns)
      (conj (fn []
              (ct/report {:type :end-test-all-vars :ns ns}))))))

(defn test-ns-block
  "Like test-ns, but returns a block for further composition and
  later execution.  Does not clear the current env."
  ([env ns]
   {:pre [(symbol? ns)]}
   [(fn []
      (ct/set-env! env)
      (ct/do-report {:type :begin-test-ns, :ns ns})
      ;; If the namespace has a test-ns-hook function, call that:
      ;; FIXME: must turn test-ns-hook into macro so it registers itself instead of just calling a defn
      (ct/block (test-all-vars-block ns)))
    (fn []
      (ct/do-report {:type :end-test-ns, :ns ns}))]))

(defn test-ns
  "If the namespace defines a function named test-ns-hook, calls that.
  Otherwise, calls test-all-vars on the namespace.  'ns' is a
  namespace object or a symbol.

  Internally binds *report-counters* to a ref initialized to
  *initial-report-counters*.  "
  ([ns] (test-ns (ct/empty-env) ns))
  ([env ns]
   (ct/run-block
     (concat (test-ns-block env ns)
       [(fn []
          (ct/clear-env!))]))))

