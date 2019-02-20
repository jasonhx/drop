(ns drop.interceptors
  (:require [reitit.coercion :as coercion]
            [reitit.spec :as rs]
            [reitit.impl :as impl]))

(defn my-coerce-exceptions-interceptor
  "Interceptor for handling coercion exceptions.
    Expects a :coercion of type `reitit.coercion/Coercion`
    and :parameters or :responses from route data, otherwise does not mount."
  []
  {:name ::coerce-exceptions
   :compile (fn [{:keys [coercion parameters responses]} _]
              (if (and coercion (or parameters responses))
                {:error (fn [ctx ex]
                          (let [data (ex-data (or (:error ctx)
                                                  ex))]
                            (if-let [status
                                     (case (:type data)
                                       :reitit.coercion/request-coercion 400
                                       :reitit.coercion/response-coercion 500
                                       nil)]
                              (let [response
                                    {:status status,
                                     :body nil
                                     #_["coercion error"]
                                     #_(coercion/encode-error data)}]
                                (-> ctx
                                    (assoc :response response)
                                    (assoc :error nil)))
                              ctx)))}))})
