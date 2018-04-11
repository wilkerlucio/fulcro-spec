(ns fulcro-spec.core
  (:require-macros
    [fulcro-spec.core]
    [fulcro-spec.impl.cljs-hacks])
  (:require
    [cljs.test :include-macros true]
    [fulcro-spec.assertions]
    [fulcro-spec.impl.test]
    [fulcro-spec.async]
    [fulcro-spec.runner] ;;side effects
    [fulcro-spec.selectors]
    [fulcro-spec.stub]))
