library(readxl)
library(jsonlite)
library(dplyr)
# Importando dados de 2013 -----------
acolhimento_2013 <- read_excel("./input/Censo SUAS 2013/Acolhimento/Censo_SUAS_2013_Acolhimento_RH_Divulgação.xlsx",
                               sheet = 1)
POP_2013 <- read_excel("./input/Censo SUAS 2013/Centro POP/Censo_SUAS_2013_CentroPOP_RH_Divulgação.xlsx",
                               sheet = 1)
cons_estad_2013 <- read_excel("./input/Censo SUAS 2013/Conselho Estadual/Censo_SUAS_2013_Conselho_Estadual_RH_Divulgação.xlsx",
                               sheet = 1)
cons_mun_2013 <- read_excel("./input/Censo SUAS 2013/Conselho Municipal/Censo_SUAS_2013_Conselho_Municipal_RH_Divulgação.xlsx",
                               sheet = 1)
CRAS_2013 <- read_excel("./input/Censo SUAS 2013/CRAS/Censo_SUAS_2013_CRAS_RH_Divulgação.xlsx",
                               sheet = 1)
CREAS_2013 <- read_excel("./input/Censo SUAS 2013/CREAS/Censo_SUAS_2013_CREAS_RH_Divulgação.xlsx",
                               sheet = 1)

estados <- fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/estados")

estados <- select(estados,id,sigla)

municipios <- fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/municipios")

municipios <- select(municipios,id,nome) %>% 
        mutate(id = as.numeric(substr(id,1,6)))