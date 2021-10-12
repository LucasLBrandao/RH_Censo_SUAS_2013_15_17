library(readxl)
library(jsonlite)
library(dplyr)
# Importando dados de 2015 -----------
acolhimento_2015 <- read_excel("./input/Censo SUAS 2015/Unidades de Acolhimento/Unidades de Acolhimento_Recursos Humanos_divulgação.xlsx",
                               sheet = 1)
POP_2015 <- read_excel("./input/Censo SUAS 2015/Centro POP/Censo-SUAS_2015_Centro_POP_RH_divulgação.xlsx",
                               sheet = 1)
cons_estad_2015 <- read_excel("./input/Censo SUAS 2015/Conselho Estadual/Censo SUAS 2015_Conselho Estadual_RH_divulgação.xlsx",
                               sheet = 1)
cons_mun_2015 <- read_excel("./input/Censo SUAS 2015/Conselho Municipal/Censo SUAS 2015_Conselho Municipal_RH_Divulgação.xlsx",
                               sheet = 1)
CRAS_2015 <- read_excel("./input/Censo SUAS 2015/CRAS/CensoSUAS_2015_CRAS_RH_divulgacao.xlsx",
                               sheet = 1)
CREAS_2015 <- read_excel("./input/Censo SUAS 2015/CREAS/CensoSUAS2015_CREAS_RH_Divulgação.xlsx",
                               sheet = 1)
DIA_2015 <- read_excel("./input/Censo SUAS 2015/Centro DIA/CensoSUAS_2015_CentroDIA_RH_divulgacao.xlsx",
                       sheet = 1)
conviv_2015 <- read_excel("./input/Censo SUAS 2015/Centro de Convivência/CensoSUAS_2015_Convivencia_RH_divulgacao.xlsx",
                          sheet = 1)

estados <- fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/estados")

estados <- select(estados,id,sigla)

municipios <- fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/municipios")

municipios <- select(municipios,id,nome) %>% 
        mutate(id = as.numeric(substr(id,1,6)))