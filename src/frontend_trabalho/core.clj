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
          (save-food-data (:descricao selected) calorias)))
      (println "Opcao invalida."))))

(defn get-food-data [food-name]
  (let [data (fetch-food-data food-name)]
    (if (and (sequential? data) (not (empty? data)))
      (do
        (print-food-options data)
        (select-food data))
      (println "Nenhum alimento encontrado."))))

(defn fetch-exercise-data [exercise-name weight duration]
  (let [url (str "http://localhost:3000/api/exercicios/" exercise-name)
        params (cond-> {}
        weight (assoc :weight weight)
        duration (assoc :duration duration))
        response (client/get url {:query-params params :as :json})]
    (if (= 200 (:status response))
      (:body response)
      nil)))

(defn print-exercise-options [data]
  (println "Opcoes de exercicios encontradas:")
  (run! (fn [[idx exercise]]
   (println (str (inc idx) ". " (:nome-pt exercise) 
    " - " (:total_calories exercise) " calorias totais"
    " (" (:calories_per_hour exercise) " cal/hora)")))
    (map-indexed vector data)))

(defn save-exercise-data [exercise-data original-name]
  (let [url "http://localhost:3000/registro-exercicio"
        body (json/generate-string {:exercise exercise-data :original_name original-name})
        response (client/post url {:body body
        :headers {"Content-Type" "application/json"
        "Accept" "application/json"}})]
    (if (= 200 (:status response))
      (println "Exercicio registrado com sucesso!")
      (println "Erro ao registrar exercicio!"))))

(defn select-exercise [data original-name]
  (print "Escolha o numero do exercicio: ")
  (flush)
  (let [choice (read)
        idx (dec choice)
        selected (nth data idx nil)]
    (if selected
      (do
        (println "Voce escolheu:" (:nome-pt selected) 
                 "com" (:total_calories selected) "calorias totais.")
        (save-exercise-data selected original-name))
      (println "Opcao invalida."))))

(defn get-exercise-info []
  (print "Digite o peso (em libras, deixe vazio para 160): ")
  (flush)
  (let [weight-input (read-line)]
    (print "Digite a duracao em minutos (deixe vazio para 60): ")
    (flush)
    (let [duration-input (read-line)
          weight (if (empty? weight-input) nil (Integer/parseInt weight-input))
          duration (if (empty? duration-input) nil (Integer/parseInt duration-input))]
      [weight duration])))

(defn get-exercise-data [exercise-name]
  (let [[weight duration] (get-exercise-info)
        data (fetch-exercise-data exercise-name weight duration)]
    (if (and (sequential? data) (not (empty? data)))
      (do
        (print-exercise-options data)
        (select-exercise data exercise-name))
      (println "Nenhum exercicio encontrado ou erro na busca."))))

(defn menu []
  (println "Bem-vindo ao UniNutri!")
  (println "1. Registrar Alimentacao")
  (println "2. Registrar Exercicio") 
  (println "3. Ver Alimentos Salvos")
  (println "4. Ver Exercicios Salvos")
  (println "5. Sair")
  (print "Escolha uma opcao: ")
  (flush)
  (let [option-str (read-line)
      option (try (Integer/parseInt option-str) (catch Exception _ 0))]
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
        (print "Digite o nome do exercicio: ")
        (flush)
        (let [exercise-name (read-line)]
          (when (not (empty? exercise-name))
            (get-exercise-data exercise-name)))
        (recur))
      
      (= option 3)
      (do
        (println "Alimentos Salvos:")
        (try
          (let [response (client/get "http://localhost:3000/alimentos-salvos" {:as :json})]
            (if (= 200 (:status response))
              (let [foods (:body response)]
                (if (empty? foods)
                  (println "Nenhum alimento salvo ainda.")
                  (run! (fn [food] 
                        (println (str "- " (:alimento food) ": " (:calorias food) " calorias")))
                        foods)))
              (println "Erro ao buscar alimentos salvos.")))
          (catch Exception e
            (println "Erro ao conectar com o servidor.")))
        (recur))
      
      (= option 4)
      (do
        (println "Exercicios Salvos:")
        (try
          (let [response (client/get "http://localhost:3000/exercicios-salvos" {:as :json})]
            (if (= 200 (:status response))
              (let [exercises (:body response)]
                (if (empty? exercises)
                  (println "Nenhum exercicio salvo ainda.")
                  (run! (fn [exercise]
                          (println (str "- " (:nome exercise) 
                                   " (original: " (:nome-original exercise) "): "
                                  (:total-calorias exercise) " calorias totais, "
                                  (:calorias-por-hora exercise) " cal/hora")))
                        exercises)))
              (println "Erro ao buscar exercicios salvos.")))
          (catch Exception e
            (println "Erro ao conectar com o servidor.")))
        (recur))
      
      (= option 5)
      (println "Saindo do programa...")
      
      :else
      (do
        (println "Opcao invalida!")
        (recur)))))

(defn -main
  "UniNutri - Sistema de registro de alimentacao e exercicios"
  [& args]
  (menu))