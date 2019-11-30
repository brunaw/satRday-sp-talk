# `tidyverse` para Aprendizado de Máquina
library(tidyverse)
library(ranger)
data <- dials::Chicago

data_tibble <- rep(list(data), 10) %>% 
  enframe(name = 'index', value = 'data')
data_tibble


treino_teste <- function(data){
  data %>% 
    mutate(base = ifelse(runif(n()) > 0.75, "teste", "treino")) %>% 
    split(.$base) %>% 
    purrr::map(~select(.x, -.data[["base"]]))
}


data_tibble <- data_tibble %>% 
  mutate(treino_teste = purrr::map(data, treino_teste))

data_tibble


modelagem <- function(treino, 
                      mtry = NULL, 
                      num.trees = NULL, 
                      coef.reg = 1, 
                      formula = ridership ~ .) {
  
  ranger::ranger(formula, 
                 data = treino, 
                 num.trees = num.trees,
                 mtry = mtry, 
                 importance = "impurity", 
                 regularization = list(coef.reg = coef.reg))
}


modelos <- list(
  arvores = list(mtry = ncol(data) - 1, num.trees = 1, coef.reg = 1),
  bagging = list(mtry = ncol(data) - 1, num.trees = 100, coef.reg = 1), 
  floresta = list(mtry = sqrt(ncol(data) - 1), num.trees = 100, coef.reg = 1),
  floresta_regularizada07 =  list(mtry = (ncol(data) - 1)/2, num.trees = 100, coef.reg = 0.7),
  floresta_regularizada02 =  list(mtry = (ncol(data) - 1)/2, num.trees = 100, coef.reg = 0.2)) %>% 
  enframe(name = 'modelo', value = 'parametros')

data_tibble <- data_tibble %>% 
  crossing(modelos) %>% 
  arrange(modelo)



treinando_models <- data_tibble %>% 
  mutate(
    full_parametros = 
      map2(parametros, map(treino_teste, "treino"), 
           ~list_modify(.x, treino = .y)),
    modelo_treinado = invoke_map(modelagem, full_parametros))


eqm <- function(modelo, teste){
  pp <- predict(modelo, teste)
  sqrt(mean((pp$predictions - teste$ridership)^2))
}


numero_variaveis <- function(modelo){
  sum(modelo$variable.importance > 0)
}


resultados <- treinando_models %>% 
  mutate(
    eqm = map2_dbl(.x = modelo_treinado,
                   .y = map(treino_teste, "teste"), 
                   ~eqm(modelo = .x, teste = .y)),
    numero_variaveis = map_int(modelo_treinado, numero_variaveis), 
    rsquared = map_dbl(modelo_treinado, "r.squared"))

resultados %>% 
  select(modelo, eqm, numero_variaveis, rsquared) %>% 
  group_by(modelo) %>% 
  summarise_all(mean)


resultados %>% 
  select(modelo, eqm, numero_variaveis, rsquared) %>% 
  ggplot(aes(x = eqm)) +
  facet_wrap(~modelo) +
  geom_density(fill = "#f5c04a") +
  labs(y = "Densidade", x = "MSQR") +
  theme_classic() 
  
