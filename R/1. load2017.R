library(readxl)
library(jsonlite)
library(dplyr)
# Importando dados de 2017 -----------
acolhimento_2017 <- read_excel("./input/Censo SUAS 2017/Unidades_Acolhimento/CENSO SUAS 2017 Acolhimento RH divulgação.xlsx",
                               sheet = 1)
POP_2017 <- read_excel("./input/Censo SUAS 2017/Centro_POP/Censo SUAS 2017_Centro POP_RH_divulgação.xlsx",
                               sheet = 1)
cons_estad_2017 <- read_excel("./input/Censo SUAS 2017/Conselho_Estadual/Censo SUAS_RH_Conselhos Estaduais.xlsx",
                               sheet = 1)
cons_mun_2017 <- read_excel("./input/Censo SUAS 2017/Conselho_Municipal/Censo SUAS 2017_RH_Conselho Municipal.xlsx",
                               sheet = 1)
CRAS_2017 <- read_excel("./input/Censo SUAS 2017/CRAS/Censo SUAS 2017_CRAS_RH_divulgacao.xlsx",
                               sheet = 1)
CREAS_2017 <- read_excel("./input/Censo SUAS 2017/CREAS/Censo SUAS 2017_CREAS_RH_divulgacao.xlsx",
                               sheet = 1)
DIA_2017 <- read_excel("./input/Censo SUAS 2017/Centro DIA/CensoSUAS2017_CentroDIA_RH_divulgacao.xlsx",
                       sheet = 1)
conviv_2017 <- read_excel("./input/Censo SUAS 2017/Centro_Convivencia/CensoSUAS2017_Convivencia_RH_divulgacao.xlsx",
                          sheet = 1)
familia_2017 <- read_excel("./input/Censo SUAS 2017/Família Acolhedora/Censo SUAS 2017_Família Acolhedora_RH_divulgação.xlsx",
                          sheet = 1)

estados <- fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/estados")

estados <- select(estados,id,sigla)

municipios <- fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/municipios")

municipios <- select(municipios,id,nome) %>% 
        mutate(id = as.numeric(substr(id,1,6)))