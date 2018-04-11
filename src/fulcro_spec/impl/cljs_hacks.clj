(ns fulcro-spec.impl.cljs-hacks
  "Hacks new definitions of deftest and use-fixtures into cljs"
  (:require
    [cljs.test]
    [cljs.analyzer]))

(in-ns 'cljs.test)

(defmacro deftest
  "Defines a test function with no arguments.  Test functions may call
  other tests, so tests may be composed.  If you compose tests, you
  should also define a function named test-ns-hook; run-tests will
  call test-ns-hook instead of testing all vars.
  Note: Actually, the test body goes in the :test metadata on the var,
  and the real function (the value of the var) calls test-var on
  itself.
  When cljs.analyzer/*load-tests* is false, deftest is ignored."
  [name & body]
  (when cljs.analyzer/*load-tests*
    `(do
       (def ~(vary-meta name assoc :test `(fn [] ~@body))
         (fn [] (cljs.test/test-var (.-cljs$lang$var ~name))))

       (let [the-var# (var ~name)]
         (set! (.-cljs$lang$var ~name) the-var#)
         (fulcro-spec.impl.test/register-test (quote ~(symbol (str *ns*))) (quote ~name) the-var#)))))

(defmacro use-fixtures [type & fns]
  {:pre [(contains? #{:once :each} type)]}
  `(fulcro-spec.impl.test/register-fixtures (quote ~(symbol (str *ns*))) ~type [~@fns]))
