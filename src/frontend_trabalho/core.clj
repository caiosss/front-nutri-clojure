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

(defn fetch-exercise-data [exercise-name duration]
  (let [url (str "http://localhost:3000/api/exercicios/" exercise-name)
        params (cond-> {}
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
    (print "Digite a duracao em minutos (deixe vazio para 60): ")
    (flush)
    (let [duration-input (read-line)
      duration (if (empty? duration-input) nil (Integer/parseInt duration-input))]
        [nil duration]))

(defn get-exercise-data [exercise-name]
  (let [[_ duration] (get-exercise-info)
        data (fetch-exercise-data exercise-name duration)]
    (if (and (sequential? data) (not (empty? data)))
      (do
        (print-exercise-options data)
        (select-exercise data exercise-name))
      (println "Nenhum exercicio encontrado ou erro na busca."))))

(defn display-calorie-balance [total-calorias]
  (cond
    (< total-calorias 0)
    (do
      (println (str "Parabens! Voce gastou mais calorias do que consumiu."))
      (println (str "Voce ja gastou " (Math/abs total-calorias) " calorias a mais do que consumiu.")))
    
    (> total-calorias 0)
    (println (str "Voce consumiu mais calorias do que gastou: " total-calorias " kcal."))
    
    (= total-calorias 0)
    (println "Perfeitamente equilibrado. Como deve ser. Calorias consumidas = calorias gastas. ou seja 0")))

(defn menu []
  (println "0. Cadastrar Usuario (obrigatório antes de usar o sistema)")
  (println "1. Registrar Alimentacao")
  (println "2. Registrar Exercicio") 
  (println "3. Ver Alimentos Salvos")
  (println "4. Ver Exercicios Salvos")
  (println "5. Ver Total de Calorias Consumidas e Gastas")
  (println "6. Sair")
  (print "Escolha uma opcao: ")
  (flush)
  (let [option-str (read-line)
        option (try (Integer/parseInt option-str) (catch Exception _ -1))]
    
    (cond
      (= option 0)
      (do
        (println "Cadastro de Usuario")
        (print "Digite o nome do usuario: ") (flush)
        (let [user-name (read-line)]
          (print "Digite o email do usuario: ") (flush)
          (let [user-email (read-line)]
            (print "Digite a senha do usuario: ") (flush)
            (let [user-password (read-line)]
              (print "Digite o peso (libras): ") (flush)
              (let [user-weight (read-line)]
                (print "Digite a altura (cm): ") (flush)
                (let [user-height (read-line)]
                  (print "Digite o sexo (M/F): ") (flush)
                  (let [user-sex (read-line)]
                    (when (and (not (empty? user-name))
                               (not (empty? user-email))
                               (not (empty? user-password))
                               (not (empty? user-weight))
                               (not (empty? user-height))
                               (not (empty? user-sex)))
                    (try
                      (let [response (client/post "http://localhost:3000/registro-usuario"
                                                  {:body (json/generate-string {:nome user-name
                                                                                :email user-email
                                                                                :senha user-password
                                                                                :peso user-weight
                                                                                :altura user-height
                                                                                :sexo user-sex})
                                                   :headers {"Content-Type" "application/json"
                                                             "Accept" "application/json"}})]
                        (if (= 200 (:status response))
                          (println "Usuario cadastrado com sucesso!")
                          (println "Erro ao cadastrar usuario.")))
                      (catch Exception e
                        (println "Já há um usuário cadastrado."))))
                  (println "Pressione Enter para continuar...") 
                  (read-line))))))
          (recur)))

      (= option 1)
      (do
        (println "Registrar Alimentacao")
        (print "Digite o nome do alimento: ") (flush)
        (let [food-name (read-line)]
          (when (not (empty? food-name))
            (try
              (get-food-data food-name)
              (catch Exception e
          (println "É necessário cadastrar um usuário antes de registrar alimentos.")))))
        (println "Pressione Enter para continuar...") (read-line)
        (recur))

      (= option 2)
      (do
        (println "Registrar Exercicio")
        (print "Digite o nome do exercicio: ") (flush)
        (let [exercise-name (read-line)]
          (when (not (empty? exercise-name))
            (try
              (get-exercise-data exercise-name)
              (catch Exception e
          (println "É necessário cadastrar um usuário antes de registrar exercícios.")))))
        (println "Pressione Enter para continuar...") (read-line)
        (println " ")
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
            (println "É necessário cadastrar um usuário antes de visualizar alimentos.")))
        (println "Pressione Enter para continuar...") (read-line)
        (println " ")
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
            (println "É necessário cadastrar um usuário antes de visualizar exercícios.")))
        (println "Pressione Enter para continuar...") (read-line)
        (recur))

      (= option 5)
      (do
        (println "Total de Calorias Consumidas e Gastas")
        (try
          (let [response (client/get "http://localhost:3000/calorias-total" {:as :json})]
            (if (= 200 (:status response))
              (let [{:keys [total-calorias]} (:body response)]
                (display-calorie-balance total-calorias))
              (println "Erro ao buscar total de calorias.")))
          (catch Exception e
            (println "É necessário cadastrar um usuário antes de visualizar o total de calorias.")))
        (println "Pressione Enter para continuar...") (read-line)
        (recur))

      (= option 6)
      (println "Saindo do programa...")

      :else
      (do
        (println "Opcao invalida!")
        (println "Pressione Enter para continuar...") (read-line)
        (recur)))))

(defn -main
  "UniNutri - Sistema de registro de alimentacao e exercicios"
  [& args]
  (println "Bem-vindo ao UniNutri! - Sistema de registro de alimentacao e exercicios")
  (menu))
