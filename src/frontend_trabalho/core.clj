(ns frontend-trabalho.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(defn fetch-food-data [food-name]
  (let [url (str "http://localhost:3000/api/calorias/" food-name)
        response (client/get url {:as :json})]
    (if (= 200 (:status response))
      (:body response)
      nil)))

(defn print-food-options [data]
  (println "Opcoes encontradas:")
  (run! (fn [[idx item]]
          (println (str (inc idx) ". " (:descricao item) " - " (:calorias item) " calorias")))
        (map-indexed vector data)))

(defn save-food-data [food-name calories]
  (let [url "http://localhost:3000/registro-alimentacao"
        body (json/generate-string {:alimento food-name :calorias calories})
        response (client/post url {:body body
                                   :headers {"Content-Type" "application/json"
                                             "Accept" "application/json"}})]
    (if (= 200 (:status response))
      (println "Alimento registrado com sucesso!")
      (println "Erro ao registrar alimento!"))))

(defn select-food [data]
  (print "Escolha o numero do alimento: ")
  (flush)
  (let [choice (read)
        idx (dec choice)
        selected (nth data idx nil)]
    (if selected
      (do
        (let [calorias (int (read-string (str (:calorias selected))))]
          (println "Voce escolheu:" (:descricao selected) "com" calorias "calorias.")
          (save-food-data (:descricao selected) calorias))
      (println "Opcao invalida.")))))

(defn get-food-data [food-name]
  (let [data (fetch-food-data food-name)]
    (if (and (sequential? data) (not (empty? data)))
      (do
        (print-food-options data)
        (select-food data))
      (println "Nenhum alimento encontrado."))))

(defn get-exercise-data []
  (let [url (str "http://localhost:3000/exercicios")
        response (client/get url {:as :json})]
        (if (= 200 (:status response))
        )))

(defn menu []
  (println "Bem-vindo ao UniNutri!")
  (println "1. Registrar Alimentacao")
  (println "2. Registrar Exercicio")
  (println "3. Sair")
  (print "Escolha uma opcao: ")
  (flush)
  (let [option (read)]
    (cond
      (= option 1)
      (do
        (println "Registrar Alimentacao")
        (print "Digite o nome do alimento: ")
        (flush)
        (let [food-name (read)]
          (get-food-data food-name))
        (recur))

      (= option 2)
      (do
        (println "Registrar Exercicio")
        (flush)
        (let [exercise-name (read)]
          (print "Digite a duração em minutos: ")
          (flush)
          (let [duration (read)]
            ))
        (recur))

      (= option 3)
      (println "Saindo do programa...")

      :else
      (do
        (println "Opcao invalida!")
        (recur)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (menu))
