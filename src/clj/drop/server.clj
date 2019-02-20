(ns drop.server
  (:require [io.pedestal.http :as server]
            [reitit.pedestal :as pedestal]
            [reitit.ring :as ring]
            [reitit.http :as http]
            [reitit.swagger :as swagger]
            [reitit.swagger-ui :as swagger-ui]
            [reitit.http.coercion :as http-coercion]
            [reitit.coercion.spec :as spec-coercion]
            [reitit.http.interceptors.parameters :as parameters]
            [reitit.http.interceptors.muuntaja :as muuntaja]
            [reitit.http.interceptors.multipart :as multipart]
            [reitit.http.interceptors.dev :as dev]
            [clojure.spec.alpha :as s]
            [clojure.core.async :as a]
            [clojure.java.io :as io]
            [muuntaja.core :as m]
            [drop.core :as dc]
            [drop.interceptors :as di]
            [drop.websokets :as dw])
  (:gen-class))

(s/def ::start     integer?)
(s/def ::until     integer?)

(s/def ::column    integer?)

(s/def ::rows      integer?)
(s/def ::columns   integer?)

(s/def ::move-idx  integer?)

(s/def ::gameId    keyword?)

(s/def ::player-id string?)
#_(s/def ::player-id (s/and string?
                            #(not (= "moves" (clojure.string/lower-case %)))))
(s/def ::players   (s/coll-of ::player-id :distinct true :count 2 :into []))

(s/def ::new-game  (s/keys :req-un [::players ::rows ::columns]))

(def router
  (pedestal/routing-interceptor
   (http/router
    [["/swagger.json"
      {:get {:no-doc true
             :swagger {:info {:title "drop_token"
                              :description "drop server"}}
             :handler (swagger/create-swagger-handler)}}]

     ["/chsk"
      {:swagger {:tags ["websocket"]}
       :get {:handler (fn [req]
                        (dw/ring-ajax-get-or-ws-handshake req))}
       :post {:handler (fn [req]
                         (dw/ring-ajax-post req))}}]

     ["/drop_token"
      {:get {:summary "List all in-progress games"
             ;; :responses {200 {:description "List of in-progress games"
             ;;                  :schema {:type object}}}
             :handler (fn [_]
                        {:status 200
                         :body {:games (dc/list-games)}})}
       :post {:summary "Create a new game"
              :parameters {:body ::new-game}
              ;; :responses {200 {:body {(s/keys :req-un [::gameId])}}}
              :handler (fn [{{{:keys [rows columns players]} :body} :parameters}]
                         (let [g (dc/make-game (first players)
                                               (second players)
                                               rows
                                               columns)]
                           {:status 200
                            :body {:gameId g}}))}}]

     ["/drop_token/:gameId"
      {:get {:summary "Get game state"
             :parameters {:path (s/keys :req-un [::gameId])}
             :handler (fn [{{{:keys [gameId]} :path} :parameters}]
                        (let [g (dc/game-status gameId)]
                          (if (nil? g)
                            {:status 404}
                            {:status 200
                             :body g})))}}]

     ["/drop_token/:gameId/moves/:move-idx"
      {:get {:summary "Return specific move"
             :parameters {:path (s/keys :req-un [::gameId ::move-idx])}
             :handler (fn [{{{:keys [gameId move-idx]} :path} :parameters}]
                        (let [g (dc/game-move gameId move-idx)]
                          (if (= :no-game g)
                            {:status 404}
                            {:status 200
                             :body g})))}}]

     ["/drop_token/:gameId/:player-id"
      {:get {:summary "Get (sub) list of game moves"
             :parameters {:path (s/keys :req-un [::gameId ::player-id])
                          :query (s/keys :opt-un [::start ::until])}
             :handler (fn [{{{player-id :player-id gameId :gameId} :path
                            {start :start until :until} :query}
                           :parameters}]
                        (if (= player-id "moves")
                          (let [g (dc/list-moves gameId start until)]
                            (if (= :no-game g)
                              {:status 404}
                              {:status 200
                               :body
                               {:moves
                                (let [c (:cols g)
                                      m (into []
                                              (map-indexed
                                               (fn [idx x]
                                                 (if (>= x c)
                                                   {:type "QUIT"
                                                    :player (:quitter g)}
                                                   {:type "MOVE"
                                                    :player (if (odd? idx)
                                                              (:one g)
                                                              (:two g))
                                                    :column x}))
                                               (:moves g)))]
                                  (if (and (>= (:to g) (:turn g))
                                           (contains? g :quitter))
                                    (conj m {:type "QUIT"
                                             :player (:quitter g)})
                                    m))}}))))}

       :post {:summary "Post a move"
              :responses {200 {:body {:move string?}}}
              :parameters {:path (s/keys :req-un [::gameId ::player-id])
                           :body (s/keys :req-un [::column]) }
              :handler (fn [{{{column :column} :body
                             {player-id :player-id gameId :gameId} :path}
                            :parameters}]
                         (let [g (dc/post-move gameId player-id column)]
                           (case g
                             :no-game {:status 404}
                             :wrong-player {:status 409}
                             (:game-done
                              :full-column
                              :bad-column) {:status 400}
                             {:status 200
                              :body {:move (str
                                            (-> (str gameId)
                                                (subs 1)) "/" player-id "/"
                                            (dec (:turn g))) }} )))}

       :delete {:summary "Quit game"
                :parameters {:path (s/keys :req-un [::gameId ::player-id])}
                :handler (fn [{{{:keys [gameId player-id]} :path} :parameters}]
                           (case (dc/quit-game gameId player-id)
                             :no-game {:status 404}
                             :wrong-player {:status 409}
                             :game-done {:status 410}
                             {:status 202})
                           )}}]]
    { ;;:reitit.interceptor/transform dev/print-context-diffs
     :data {:coercion spec-coercion/coercion
            :muuntaja m/instance
            :interceptors [ ;; query-params & form-params
                           (parameters/parameters-interceptor)
                           ;; content-negotiation
                           (muuntaja/format-negotiate-interceptor)
                           ;; encoding response body
                           (muuntaja/format-response-interceptor)
                           ;; decoding request body
                           (muuntaja/format-request-interceptor)
                           ;; coercing response bodys
                           #_(di/my-coerce-exceptions-interceptor)
                           (http-coercion/coerce-response-interceptor)
                           ;; coercing request parameters
                           (http-coercion/coerce-request-interceptor)
                           ;; multipart
                           (multipart/multipart-interceptor)]}})

   ;; optional default ring handler (if no routes have matched)
   (ring/routes
    (swagger-ui/create-swagger-ui-handler
     {:path "/"
      :config {:validatorUrl nil}})
    (ring/create-resource-handler)
    (ring/create-default-handler))))

(defn start []
  (-> {:env :dev
       ::server/type :immutant
       ::server/port 3000
       ::server/join? false
       ;; no pedestal routes
       ::server/routes []
       ;; allow serving the swagger-ui styles & scripts from self
       ::server/secure-headers {:content-security-policy-settings
                                {:default-src "'self'"
                                 :style-src "'self' 'unsafe-inline'"
                                 :script-src "'self' 'unsafe-inline'"}}}
      (io.pedestal.http/default-interceptors)
      ;; use the reitit router
      (pedestal/replace-last-interceptor router)
      #_(io.pedestal.http/dev-interceptors)
      (io.pedestal.http/create-server)
      (io.pedestal.http/start))
  (println "server running in port 3000"))

(defn- main []
  (println "Drop starting")
  (start))

(comment
  (start))
