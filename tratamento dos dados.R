library(tidyverse)
library(readxl)
library(jsonlite)


# Importando dados de 2013 -----------
acolhimento_2013 <- read_excel("./Censo SUAS 2013/Acolhimento/Censo_SUAS_2013_Acolhimento_RH_Divulgação.xlsx",
                               sheet = 1)
POP_2013 <- read_excel("./Censo SUAS 2013/Centro POP/Censo_SUAS_2013_CentroPOP_RH_Divulgação.xlsx",
                               sheet = 1)
cons_estad_2013 <- read_excel("./Censo SUAS 2013/Conselho Estadual/Censo_SUAS_2013_Conselho_Estadual_RH_Divulgação.xlsx",
                               sheet = 1)
cons_mun_2013 <- read_excel("./Censo SUAS 2013/Conselho Municipal/Censo_SUAS_2013_Conselho_Municipal_RH_Divulgação.xlsx",
                               sheet = 1)
CRAS_2013 <- read_excel("./Censo SUAS 2013/CRAS/Censo_SUAS_2013_CRAS_RH_Divulgação.xlsx",
                               sheet = 1)
CREAS_2013 <- read_excel("./Censo SUAS 2013/CREAS/Censo_SUAS_2013_CREAS_RH_Divulgação.xlsx",
                               sheet = 1)

estados <- fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/estados")

estados <- select(estados,id,sigla)

municipios <- fromJSON("https://servicodados.ibge.gov.br/api/v1/localidades/municipios")

municipios <- select(municipios,id,nome) %>% 
        mutate(id = as.numeric(substr(id,1,6)))

# Selecionando colunas -------
acolhimento_2013 <- acolhimento_2013 %>% 
        select(UF,IBGE,IBGE7,Município,Sexo = q38_3,
               Escolaridade = q38_4,
               Profissão = q38_5,
               Vínculo = q38_6,
               Função = q38_7) %>% 
        mutate(alocação = "Centro de acolhimento")
POP_2013 <- POP_2013 %>% 
         select(UF=uf,IBGE=municipio,Sexo = q46_3,
               Escolaridade = q46_6,
               Profissão = q46_7,
               Vínculo = q46_8,
               Função = q46_9,
               Idade = D46_2) %>% 
        mutate(alocação = "Centro POP") %>% 
        left_join(municipios, by = c("IBGE" = "id")) %>% 
        rename(Município = nome)
cons_estad_2013 <- cons_estad_2013 %>% 
         select(sig.UF = UF,Sexo = q67_3,
               Escolaridade = q67_4,
               Função = q67_6,
               Idade = D67_2) %>% 
        mutate(alocação = "Conselho estadual") %>% 
        left_join(estados, by = c("sig.UF"="id")) %>% 
        rename(UF = sigla) %>% 
        select(-sig.UF)
cons_mun_2013 <- cons_mun_2013 %>%  
        select(UF,IBGE,IBGE7,Município,Sexo = q67_3,
               Escolaridade = q67_4,
               Função = q67_6,
               Idade = D67_2) %>% 
        mutate(alocação = "Conselho município")
CRAS_2013 <- CRAS_2013 %>% 
        select(UF=uf,IBGE,Município,Sexo = q63_5,
               Escolaridade = q63_6,
               Profissão = q63_7,
               Vínculo = q63_8,
               Função = q63_9,
               Idade = D63_2) %>% 
        mutate(alocação = "CRAS")
CREAS_2013 <- CREAS_2013 %>% 
        select(UF=uf,
               IBGE=municipio,
               IBGE7=CD_IBGE7,
               Município = Cidade,
               Sexo = q58_3,
               Escolaridade = q58_8,
               Profissão = q58_9,
               Vínculo = q58_10,
               Função = q58_11,
               Idade = D58_2) %>% 
        mutate(alocação = "CREAS")
SUAS_2013 <- bind_rows(
        acolhimento_2013,
          POP_2013,
          cons_estad_2013,
          cons_mun_2013,
          CRAS_2013,
          CREAS_2013)

SUAS_2013 <- SUAS_2013 %>% mutate(
        Escolaridade = toupper(str_replace(Escolaridade,"Ensino ","")),
        Profissão = ifelse(Profissão == "Outras formações de nível superior",
                           "Outro profissional de nivel superior",Profissão),
        Vínculo = case_when(Vínculo == "Cargo público comissionado" ~ "Comissionado" ,
                            Vínculo == "Empregado (CLT)"~"Empregado Público (CLT)" ,
                            Vínculo == "Servidor Estatutário"~"Servidor Público Estatutário",
                            Vínculo == "Servidor Temporário"~"Servidor Público Temporário" ,
                            Vínculo %in% c("Trabalhador de Empresa prestadora de serviços" ,
                                           "Trabalhador de Empresa/Cooperativa/Entidade Prestadora de Serviços")~"Terceirizado" ,
                            TRUE ~ as.character(Vínculo)),
        Função = case_when(Função == "Apoio administrativo"~"Apoio Administrativo",
                           Função == "Cuidador residente (“mãe/pai social”)"~"Cuidador",
                           Função == "Educador (a) Social"~ "Educador/Orientador Social" ,
                           Função == "Estagiario (a)" ~ "Estagiário (a)",
                           Função == "Outro"~ "Outros",
                           Função == "Serviços gerais (limpeza/conservação)"~ "Serviços Gerais" ,
                           TRUE ~ as.character(Função))) %>% 
        mutate(UF = as.factor(UF),
               Sexo = as.factor(Sexo),
               Escolaridade = as.factor(Escolaridade),
               Profissão = as.factor(Profissão),
               Vínculo = as.factor(Vínculo),
               Função = as.factor(Função),
               alocação = as.factor(alocação))


sapply(SUAS_2013, levels)

# Conselhos estaduais não está indicado o municípios
# Conselhos municipais e estaduais não estão indicados os vínculos
# Centro de acolhimento não tem idade e conselho município falta muitos
# Conselho estadual e municipal não tem profissão
# Centro POP, Conselho estadual e CRAS não tem IBGE7


SUAS_2013 %>% filter(is.na(IBGE7)) %>% summary()
        