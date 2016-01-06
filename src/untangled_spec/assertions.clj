(ns untangled-spec.assertions)

(defn fn-assert-expr [msg [f arg :as form]]
  `(let [arg# ~arg
         result# (~f arg#)]
     {:type (if result# :pass :fail)
      :assertion ~msg :message ~msg
      :actual arg# :expected '~f}))

(defn eq-assert-expr [msg [act exp :as form]]
  `(let [act# ~act
         exp# ~exp
         result# (= act# exp#)]
     {:type (if result# :pass :fail)
      :assertion ~msg :message ~msg
      :actual act# :expected exp#}))

(defn exception-matches? [msg e exp-type & [re f f+]]
  (->> (cond
         (some-> (ex-data e) :type (= ::internal))
         {:type :error :message (.getMessage e)
          :actual e :expected "it to throw"}

         (not= exp-type (type e))
         {:type :fail :actual (type e) :expected exp-type
          :message "exception did not match type"}

         (and re (not (re-find re (.getMessage e))))
         {:type :fail :actual (.getMessage e) :expected (str re)
          :message "exception's message did not match regex"}

         (and f (not (f e)))
         {:type :fail :actual e :expected f+
          :message "checker function failed"}

         :else {:type :passed :actual "act" :expected "exp"})
       (merge {:assertion msg
               :throwable e})))

(defn throws-assert-expr [msg [cljs? should-throw exp-type & [re f]]]
  `(try ~should-throw
        (throw (ex-info (str "Expected an '" '~exp-type "' to be thrown!")
                        {:type ::internal}))
        (catch ~(if (not cljs?) (symbol "Throwable") (symbol "js" "Object"))
          e# (exception-matches? ~msg e# ~exp-type ~re ~f '~f))))

(defn assert-expr [disp-key msg form]
  (case (str disp-key)
    "call"    (fn-assert-expr     msg (rest form))
    "eq"      (eq-assert-expr     msg (rest form))
    "throws?" (throws-assert-expr msg (rest form))
    :else {:type :fail :message "ELSE" :actual "BAD" :expected ""}))

(defn triple->assertion [cljs? [left arrow expected]]
  (let [prefix (if cljs? "cljs.test" "clojure.test")
        is (symbol prefix "is")
        msg (str left " " arrow " " expected)]
    (case arrow
      =>
      (let [actual left]
        `(~is (= ~actual ~expected)
              ~msg))

      =fn=>
      (let [checker expected
            arg left]
        `(~is (~'call ~checker ~arg)
              ~msg))

      =throws=>
      (let [should-throw left
            criteria expected]
        `(~is (~'throws? ~cljs? ~should-throw ~@criteria)
              ~msg))

      (throw (ex-info "invalid arrow" {:arrow arrow})))))
