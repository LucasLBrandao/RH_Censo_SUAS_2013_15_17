library(tidyverse)

source("./R/1. load2017.R",encoding = "UTF-8")

# Selecionando colunas -------


acolhimento_2017_dadosgerais <- read_excel("./input/Censo SUAS 2017/Unidades_Acolhimento/Censo Suas 2017 Unidades de Acolhimento_divulgação.xlsx",
                               sheet = 1) %>% select(id_acolhimento,q3,q7)
acolhimento_2017 <- acolhimento_2017 %>% 
        select(id_acolhimento,
               UF,
               IBGE,
               IBGE7,
               Município = Municipio_2013,
               Sexo = q42.3,
               Escolaridade = q42.9,
               Profissão = q42.10,
               Vínculo = q42.11,
               Função = q42.12,
               Idade = D42.2,
               Faixa_idade = D42.2.categoria) %>% 
        left_join(acolhimento_2017_dadosgerais) %>% 
        rename(esfera = q7,
               gov = q3) %>% 
        mutate(alocação = "Centro de acolhimento",
               proteção = "PSE de alta complexidade")

CREAS_2017_dadosgerais <- read_excel("./input/Censo SUAS 2017/CREAS/Censo SUAS 2017_CREAS_divulgacao.xlsx",
                               sheet = 1) %>% select(NU_identificador,q2)

POP_2017 <- POP_2017 %>% 
         select(id_creas_pop,
                UF,
               IBGE,
               IBGE7,
               Município = Municipio_2013,
               Sexo = q44.3,
               Escolaridade = q44.7,
               Profissão = q44.8,
               Vínculo = q44.9,
               Função = q44.10,
               Idade = D44.2,
               Faixa_idade = D44.2categoria) %>% 
        left_join(CREAS_2017_dadosgerais, by = c("id_creas_pop" = "NU_identificador")) %>% 
        rename(esfera = q2) %>% 
        mutate(alocação = "Centro POP",
               Idade = as.numeric(Idade),
               proteção = "PSE de média complexidade",
               gov = "Governamental") %>% 
        select( -id_creas_pop)
 
cons_estad_2017 <- cons_estad_2017 %>% 
         select(UF = UF_n,
                Sexo = q81.3,
               Escolaridade = d81.5,
               Função = q81.7,
               Idade,
               Faixa_idade = Faixas.etarias) %>% 
        mutate(alocação = "Conselho estadual",
               esfera = "Estadual",
               gov = "Governamental")


cons_mun_2017 <- cons_mun_2017 %>%  
        select(UF,
               IBGE,
               IBGE7,
               Município = Municipio_2013,
               Sexo = q81.3,
               Escolaridade = q81.5,
               Função = q81.7,
               Idade,
               Faixa_idade = Faixas.etarias) %>% 
        mutate(alocação = "Conselho município",
               esfera = "Municipal",
               gov = "Governamental")


CRAS_2017 <- CRAS_2017 %>% 
        select(UF,
               IBGE,
               IBGE7,
               Município = Municipio_2013,
               Sexo = q52.3,
               Escolaridade = q52.9,
               Profissão = q52.10,
               Vínculo = q52.11,
               Função = q52.12,
               Idade = D52.2,
               Faixa_idade = D52.2.FaixasEtarias) %>% 
        mutate(Idade = as.numeric(Idade),
               alocação = "CRAS",
               esfera = "Municipal",
               proteção = "PS básica",
               gov = "Governamental")


CREAS_2017 <- CREAS_2017 %>% 
        select(NU_identificador,
               UF,
               IBGE,
               IBGE7,
               Município = Municipio_2013,
               Sexo = q62.3,
               Escolaridade = q62.6,
               Profissão = q62.7,
               Vínculo = q62.8,
               Função = q62.9,
               Idade = D62_2,
               Faixa_idade = D62_2_categoria) %>% 
        left_join(CREAS_2017_dadosgerais) %>% 
        rename(esfera = q2) %>% 
        mutate(alocação = "CREAS",
               proteção = "PSE de média complexidade",
               gov = "Governamental")

DIA_2017_dadosgerais <- read_excel("./input/Censo SUAS 2017/Centro DIA/CensoSUAS2017_CentroDIA_DG_divulgacao.xlsx",
                       sheet = 1) %>% select(NU_IDENTIFICADOR,q1,q4)

DIA_2017 <- DIA_2017 %>% 
        select(NU_IDENTIFICADOR,
               UF,
               IBGE,
               IBGE7,
               Município = Municipio_2013,
               Sexo = q35_sexo,
               Escolaridade = q35_esc,
               Profissão = q35_prof,
               Vínculo = q35_vinc,
               Função = q35_func,
               Idade = d35_idade,
               Faixa_idade = d35_idade_categoria) %>%
        left_join(DIA_2017_dadosgerais) %>% 
        rename(esfera = q1,
               gov = q4) %>% 
        mutate(alocação = "Centro DIA",
               proteção = "diversos") %>% 
        select(-NU_IDENTIFICADOR)

conviv_2017_dadosgerais <- read_excel("./input/Censo SUAS 2017/Centro_Convivencia/CensoSUAS2017_Convivencia_DG_divulgacao.xlsx",
                          sheet = 1) %>% select(NU_IDENTIFICADOR,q1,q2)

conviv_2017 <- conviv_2017 %>% 
        select(NU_IDENTIFICADOR,
               UF,
               IBGE,
               IBGE7,
               Município = Municipio_2013,
               Sexo = q31_sexo,
               Escolaridade = q31_esc,
               Profissão = q31_prof,
               Vínculo = q31_vinc,
               Função = q31_func,
               Idade = d31_idade,
               Faixa_idade = d31_idade_categoria) %>% 
        left_join(conviv_2017_dadosgerais) %>% 
        rename(esfera = q1,
               gov = q2) %>% 
        mutate(alocação = "Centro de convivência",
               proteção = "PS básica") %>% 
        select(-NU_IDENTIFICADOR)

familia_2017_dadosgerais <- read_excel("./input/Censo SUAS 2017/Família Acolhedora/Censo SUAS 2017_Família Acolhedora_divulgação.xlsx",
                          sheet = 1) %>% select(NºIDENTIFICADOR,q.6,q.2)

familia_2017 <- familia_2017 %>% 
         select(NºIDENTIFICADOR,
                UF,
               IBGE,
               IBGE7,
               Município = Municipio_2013,
               Sexo = q.30.3,
               Escolaridade = q.30.9,
               Profissão = q.30.10,
               Vínculo = q.30.11,
               Função = q.30.12,
               Faixa_idade = D30.2.FaixasEtarias) %>% 
        left_join(familia_2017_dadosgerais) %>% 
        rename(esfera = q.6,
               gov = q.2) %>% 
        mutate(alocação = "Família acolhedora",
               proteção = "PSE de alta complexidade") %>% 
        select(-NºIDENTIFICADOR)
        
# Unindo os dados --------

SUAS_2017 <- bind_rows(
        acolhimento_2017,
          POP_2017,
          cons_estad_2017,
          cons_mun_2017,
          CRAS_2017 ,
          CREAS_2017,
          conviv_2017,
          DIA_2017,
        familia_2017)


# Limpando os dados -----

categorias <- readRDS("./output/SUAS-2013.rds")

categorias <- sapply(categorias, levels)

SUAS_2017 <- SUAS_2017 %>% mutate(UF = as.factor(UF),
               Sexo = as.factor(Sexo),
               Escolaridade = as.factor(Escolaridade),
               Profissão = as.factor(Profissão),
               Vínculo = as.factor(Vínculo),
               Função = as.factor(Função),
               alocação = as.factor(alocação),
               Faixa_idade = as.factor(Faixa_idade))

sapply(SUAS_2017, levels)

SUAS_2017 <- SUAS_2017 %>% mutate(
        
        Escolaridade = case_when(Escolaridade == "Nível fundamental" ~ "Fundamental Completo",
                                 Escolaridade == "Nível médio" ~ "Médio completo",
                                 Escolaridade == "Nível Superior" ~ "Superior Completo",
                                 TRUE ~ as.character(Escolaridade)) %>% toupper(),
        
        Sexo = ifelse(Sexo %in% c("F","FEMININO"),"Feminino",
                      ifelse(Sexo %in% c("M","MASCULINO"), "Masculino",
                             as.character(Sexo))),
        
        Profissão = case_when(Profissão  == "Outra formação de nível superior" ~ "Outro profissional de nivel superior",
                              Profissão == "Antropologo" ~ "Antropólogo",
                              Profissão == "Cientista político" ~ "Cientista Político",
                              Profissão == "Profissional de nível médio" ~ "Profissional de nivel medio" ,
                              Profissão == "Sociologo" ~ "Sociólogo",
                              TRUE ~ as.character(Profissão)),
        
        Vínculo = case_when(
                            Vínculo %in% c("Empregado Público Celetista - CLT",
                                           "Empregado Público Celetista – CLT",
                                           "Empregado Público Celetista",
                                           "Emprega Público (CLT)" ) ~ "Empregado Público (CLT)",
                            Vínculo == "Sem vínculo" ~ "Sem Vínculo",
                            Vínculo %in% c("Empregado Celetista do Setor Privado",
                                           "Empregado Celetista do setor privado - CLT") ~ "Empregado (CLT)",
                            Vínculo %in% c("Servidor Estatutário",
                                           "Servidor/Estatutário")~"Servidor Público Estatutário",
                            Vínculo == "Servidor Temporário"~"Servidor Público Temporário" ,
                            Vínculo %in% c("Trabalhador de Empresa / Cooperativa / Entidade Prestadora de Serviços",
                                           "Trabalhador de Empresa, Cooperativa ou Entidade Prestadora de Serviços",
                                           "Trabalhador de empresa/cooperativa/entidade prestadora de serviço",     
                                           "Trabalhador de Empresa/Cooperativa/Entidade Prestadora de Serviços")~"Terceirizado" ,
                            TRUE ~ as.character(Vínculo)),
        
        Função = case_when(Função == "Apoio administrativo"~"Apoio Administrativo",
                           Função %in% c("Cuidador(a) Residente","Cuidador(a)") ~ "Cuidador",
                           Função %in% c("Educador (a) Social",
                                         "Educador Social",
                                         "Orientador/Educador(a) Social" ) ~ "Educador/Orientador Social" ,
                           Função %in% c("Serviços Gerais (limpeza, conservação, motoristas, etc)" ,
                                         "Serviços Gerais (limpeza, conservação, motoristas, etc.)") ~ "Serviços Gerais" ,
                           Função == "Técnico(a) de Nível Médio" ~ "Técnico(a) de nível médio",
                           Função %in% c("Técnico(a) de Nível Superior",
                                         "Técnico de nível superior") ~ "Técnico(a) de nível superior",
                           TRUE ~ as.character(Função))) %>% 
        
        
        mutate(UF = as.factor(UF),
               Sexo = as.factor(Sexo),
               Escolaridade = as.factor(Escolaridade),
               Profissão = as.factor(Profissão),
               Vínculo = as.factor(Vínculo),
               Função = as.factor(Função),
               alocação = as.factor(alocação),
               Faixa_idade = as.factor(Faixa_idade))


sapply(SUAS_2017, levels)


#   Verificando tipo dos dados
sapply(SUAS_2017,class)

# Verificando e ajustando os valores de idade
library(assertive)
assert_all_are_in_closed_range(SUAS_2017$Idade[!is.na(SUAS_2017$Idade)],lower = 15,upper = 120)

idade_media <- SUAS_2017 %>% group_by(Faixa_idade) %>% 
        summarize(media_idade = floor(mean(Idade,na.rm = T)))

SUAS_2017 <- SUAS_2017 %>% 
        mutate(Idade = ifelse(Idade < 14,idade_media$media_idade[7],
                       ifelse(Idade > 113,idade_media$media_idade[12],
                        Idade)))


# Arrumando os níveis dos fatores
SUAS_2017 <- SUAS_2017 %>% mutate(Escolaridade = factor(Escolaridade, levels = c("SEM ESCOLARIDADE",
                                                                    "FUNDAMENTAL INCOMPLETO",
                                                                    "FUNDAMENTAL COMPLETO",
                                                                    "MÉDIO INCOMPLETO",
                                                                    "MÉDIO COMPLETO",
                                                                    "SUPERIOR INCOMPLETO",
                                                                    "SUPERIOR COMPLETO",
                                                                    "ESPECIALIZAÇÃO",
                                                                    "MESTRADO",
                                                                    "DOUTORADO")),
                     Vínculo = factor(Vínculo, levels = c("Sem Vínculo",
                                                          "Outro vínculo não permanente",
                                                          "Voluntário",
                                                          "Terceirizado",
                                                          "Empregado (CLT)",
                                                          "Empregado Público (CLT)",
                                                          "Servidor Público Temporário",
                                                          "Comissionado",
                                                          "Servidor Público Estatutário" )))
# Verificando valores faltantes
#Municípios
# Conselhos estaduais não está indicado, 
SUAS_2017 %>% filter(is.na(IBGE)) %>% summary()

#Profissão
# Conselho estadual e municipal não tem profissão, CRAs e centro acolhimento e centro convivência tem alguns faltantes
SUAS_2017 %>% filter(is.na(Profissão)) %>% summary()
SUAS_2017 <- SUAS_2017 %>% mutate(Profissão = as.factor(ifelse(! alocação %in% c("Conselho estadual","Conselho município") &
                                                     is.na(Profissão),
                                                     "Sem formação profissional",
                                                     as.character(Profissão))))



#Vínculos
# Conselho municio e estadual não tem essa informações, centro de acolimento e centro de convivência muito pouco
SUAS_2017 %>% filter(is.na(Vínculo)) %>% summary()


#Idade
# Poucas observações não apresentam a informações da idade
SUAS_2017 %>% filter(is.na(Idade)) %>% summary()

SUAS_2017_s_conselho <- SUAS_2017 %>% filter(!str_detect(alocação,"Conselho"))

# Adicionando a coluna de ano
SUAS_2017 <- SUAS_2017 %>% mutate(ano = 2017)
#Exportando arquivos
saveRDS(SUAS_2017,"./output/SUAS-2017.rds")
saveRDS(SUAS_2017_s_conselho,"./output/SUAS-2017-s-conselho.rds")
