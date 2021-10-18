library(tidyverse)

source("./R/1. load2015.R",encoding = "UTF-8")

# Selecionando colunas -------


acolhimento_2015 <- acolhimento_2015 %>% 
        select(id_acolhimento,
               UF,
               IBGE,
               IBGE7,
               Município = Municipio_2013,
               Sexo = q44_3,
               Escolaridade = q44_8,
               Profissão = q44_9,
               Vínculo = q44_10,
               Função = q44_11,
               Idade = D44_2,
               Faixa_idade = D44_2_categoria,
               esfera = q1,
               gov = q5) %>% 
        mutate(alocação = "Centro de acolhimento",
               proteção = "PSE de alta complexidade")

CREAS_2015_dadosgerais <- read_excel("./input/Censo SUAS 2015/CREAS/CensoSUAS2015_CREAS_DadosGerais_Divulgação.xlsx",
                               sheet = 1) %>% select(NU_identificador,q2)

POP_2015 <- POP_2015 %>% 
         select(UF,
               IBGE,
               IBGE7,
               Município = Municipio,
               Sexo = q39_3,
               Escolaridade = q39_6,
               Profissão = q39_7,
               Vínculo = q39_8,
               Função = q39_9,
               Idade = D39_2,
               Faixa_idade = D39_2categoria) %>% 
        mutate(alocação = "Centro POP",
               Idade = as.numeric(Idade),
               proteção = "PSE de média complexidade",
               esfera = "Municipal",
               gov = "Governamental")
        #left_join(municipios, by = c("IBGE" = "id")) %>% 
        #rename(Município = nome)


cons_estad_2015 <- cons_estad_2015 %>% 
         select(UF,
                Sexo = q63_3,
               Escolaridade = q63_4,
               Função = q63_6,
               Vínculo = q63_7,
               Idade = D63_2,
               Faixa_idade = D63_2_categoria) %>% 
        mutate(alocação = "Conselho estadual",
               esfera = "Estadual",
               gov = "Governamental" )


cons_mun_2015 <- cons_mun_2015 %>%  
        select(UF,
               IBGE,
               IBGE7,
               Município,
               Sexo = q63_3,
               Escolaridade = q63_4,
               Função = q63_6,
               Vínculo = q63_7,
               Idade = D63_2,
               Faixa_idade = D63_2_categoria) %>% 
        mutate(alocação = "Conselho município",
               esfera = "Municipal",
               gov = "Governamental")


CRAS_2015 <- CRAS_2015 %>% 
        select(UF,
               IBGE = CD_IBGE,
               IBGE7,
               Município,
               Sexo = q55_3,
               Escolaridade = q55_6,
               Profissão = q55_7,
               Vínculo = q55_8,
               Função = q55_9,
               Idade = d55_2,
               Faixa_idade = d55_2_categoria) %>% 
        mutate(Idade = as.numeric(Idade),
               alocação = "CRAS",
               esfera = "Municipal",
               proteção = "PS básica",
               gov = "Governamental")




CREAS_2015 <- CREAS_2015 %>% 
        select(NU_identificador,
               UF,
               IBGE=IBGE6,
               IBGE7,
               Município,
               Sexo = q47_3,
               Escolaridade = q47_8,
               Profissão = q47_9,
               Vínculo = q47_10,
               Função = q47_11,
               Idade = D47_2,
               Faixa_idade = D47_2_categoria) %>% 
        left_join(CREAS_2015_dadosgerais) %>% 
        rename(esfera = q2) %>% 
        mutate(alocação = "CREAS",
               proteção = "PSE de média complexidade",
               gov = "Governamental") %>% 
        select(-NU_identificador)

conviv_2015_dadosgerais <- read_excel("./input/Censo SUAS 2015/Centro de Convivência/CensoSUAS_2015_Convivencia_DadosGerais_divulgacao.xlsx",
                                      sheet = 1) %>% select(NU_IDENTIFICADOR,q2)

conviv_2015 <- conviv_2015 %>% 
        select(NU_IDENTIFICADOR,
               UF,
               IBGE,
               IBGE7,
               Município,
               Sexo = q31_3,
               Escolaridade = q31_8,
               Profissão = q31_9,
               Vínculo = q31_10,
               Função = q31_11,
               Idade = d31_2,
               Faixa_idade = d31_2_categoria) %>% 
        left_join(conviv_2015_dadosgerais) %>% 
        rename(gov = q2) %>% 
        mutate(alocação = "Centro de convivência",
               esfera = "Municipal",
               proteção = "PS Básica")


DIA_2015_dadosgerais <- read_excel("./input/Censo SUAS 2015/Centro DIA/CensoSUAS_2015_CentroDIA_dadosgerais_divulgacao.xlsx",
                                   sheet = 1) %>% select(NU_IDENTIFICADOR,q6)

DIA_2015 <- DIA_2015 %>% 
        select(NU_IDENTIFICADOR,
               UF,
               IBGE,
               IBGE7,
               Município,
               Sexo = q40_3,
               Escolaridade = q40_8,
               Profissão = q40_9,
               Vínculo = q40_10,
               Função = q40_11,
               Idade = d40_2,
               Faixa_idade = d40_2_categoria) %>% 
        left_join(DIA_2015_dadosgerais) %>% 
        rename(gov = q6) %>% 
        mutate(alocação = "Centro DIA",
               esfera = ifelse(NU_IDENTIFICADOR %in% c(3304553501209,5002703501139,5208703500526,5300103501130),"Estadual","Municipal"),
               proteção = "diversos")
        
# Unindo os dados --------

SUAS_2015 <- bind_rows(
        acolhimento_2015,
          POP_2015,
          cons_estad_2015,
          cons_mun_2015,
          CRAS_2015 ,
          CREAS_2015,
          conviv_2015,
          DIA_2015)


# Limpando os dados -----

SUAS_2015 <- SUAS_2015 %>% mutate(UF = as.factor(UF),
               Sexo = as.factor(Sexo),
               Escolaridade = as.factor(Escolaridade),
               Profissão = as.factor(Profissão),
               Vínculo = as.factor(Vínculo),
               Função = as.factor(Função),
               alocação = as.factor(alocação),
               Faixa_idade = as.factor(Faixa_idade))

sapply(SUAS_2015, levels)

SUAS_2015 <- SUAS_2015 %>% mutate(
        Escolaridade = toupper(Escolaridade),
        
        Sexo = ifelse(Sexo == "F","Feminino",ifelse(Sexo == "M", "Masculino",as.character(Sexo))),
        
        Profissão = case_when(Profissão %in% c("Outras formações de nível superior",
                                            "Outra formação de nível superior") ~ "Outro profissional de nivel superior",
                               Profissão == "Sociologo" ~ "Sociólogo",
                              Profissão == "Cientista Político" ~ "Cientista político" ,
                              TRUE ~ as.character(Profissão)),
        
        Vínculo = case_when(Vínculo == "Cargo público comissionado" ~ "Comissionado" ,
                            Vínculo %in% c("Empregado Público Celetista - CLT",
                                           "Empregado Público Celetista – CLT",
                                           "Emprega Público (CLT)" ) ~ "Empregado Público (CLT)",
                            Vínculo == "Sem Vínculo" ~ "Sem vínculo",
                            Vínculo %in% c("Empregado Celetista do Setor Privado",
                                           "Empregado Celetista do setor privado - CLT") ~ "Empregado (CLT)",
                            Vínculo %in% c("Servidor Estatutário",
                                           "Servidor/Estatutário")~"Servidor Público Estatutário",
                            Vínculo == "Servidor Temporário"~"Servidor Público Temporário" ,
                            Vínculo %in% c("Trabalhador de Empresa, Cooperativa ou Entidade Prestadora de Serviços" ,
                                           "Trabalhador de empresa/Cooperativa/Entidade Prestadora de Serviço",
                                           "Trabalhador de Empresa/Cooperativa/Entidade Prestadora de Serviços")~"Terceirizado" ,
                            Vínculo == "Voluntario" ~ "Voluntário",
                            TRUE ~ as.character(Vínculo)),
        
        Função = case_when(Função == "Apoio administrativo"~"Apoio Administrativo",
                           Função %in% c("Cuidador(a) Residente","Cuidador(a)") ~ "Cuidador",
                           Função == "Educador (a) Social"~ "Educador/Orientador Social" ,
                           Função %in% c("Serviços Gerais (limpeza, conservação, motoristas, etc)" ,
                                         "Serviços Gerais (limpeza, conservação, motoristas, etc.)")~ "Serviços Gerais" ,
                           Função == "Técnico(a) de nível médio" ~ "Técnico(a) de Nível Médio",
                           Função %in% c("Técnico(a) de nível superior",
                                         "Técnico de nível superior") ~ "Técnico(a) de Nível Superior",
                           TRUE ~ as.character(Função))) %>% 
        mutate(UF = as.factor(UF),
               Sexo = as.factor(Sexo),
               Escolaridade = as.factor(Escolaridade),
               Profissão = as.factor(Profissão),
               Vínculo = as.factor(Vínculo),
               Função = as.factor(Função),
               alocação = as.factor(alocação),
               Faixa_idade = as.factor(Faixa_idade))


sapply(SUAS_2015, levels)


#   Verificando tipo dos dados
sapply(SUAS_2015,class)

# Verificando e ajustando os valores de idade
library(assertive)
assert_all_are_in_closed_range(SUAS_2015$Idade[!is.na(SUAS_2015$Idade)],lower = 15,upper = 120)

idade_media <- SUAS_2015 %>% group_by(Faixa_idade) %>% 
        summarize(media_idade = floor(mean(Idade,na.rm = T)))

SUAS_2015 <- SUAS_2015 %>% 
        mutate(Idade = ifelse(Idade < 14,idade_media$media_idade[2],
                       ifelse(Idade > 113,idade_media$media_idade[1],
                        Idade)))


# Arrumando os níveis dos fatores
SUAS_2015 <- SUAS_2015 %>% mutate(Escolaridade = factor(Escolaridade, levels = c("SEM ESCOLARIDADE",
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
                                                          "Servidor Público Estatutário" )),
                     Faixa_idade = factor(Faixa_idade, levels = c("Até 29 anos",
                                                                  "De 30 a 39 anos",
                                                                  "De 40 a 49 anos",
                                                                  "50 anos ou mais")))
# Verificando valores faltantes
#Municípios
# Conselhos estaduais não está indicado, 
#em 39 conselhos de municípios não está indicado - no distrito federal
SUAS_2015 %>% filter(is.na(IBGE)) %>% summary()

#Profissão
# Conselho estadual e municipal não tem profissão, CRAs e centro dia tem alguns faltantes
SUAS_2015 %>% filter(is.na(Profissão)) %>% summary()
SUAS_2015 <- SUAS_2015 %>% mutate(Profissão = as.factor(ifelse(! alocação %in% c("Conselho estadual","Conselho município") &
                                                     is.na(Profissão),
                                                     "Sem formação profissional",
                                                     as.character(Profissão))))



#Vínculos
# Em todos as bases à dados faltantas quanto à essa informação, sendo a principal a de conselho município
SUAS_2015 %>% filter(is.na(Vínculo)) %>% summary()


#Idade
# Poucas observações não apresentam a informações da idade
SUAS_2015 %>% filter(is.na(Idade)) %>% summary()

SUAS_2015_s_conselho <- SUAS_2015 %>% filter(!str_detect(alocação,"Conselho"))

# Adicionando a coluna de ano
SUAS_2015 <- SUAS_2015 %>% mutate(ano = 2015)
#Exportando arquivos
saveRDS(SUAS_2015,"./output/SUAS-2015.rds")
saveRDS(SUAS_2015_s_conselho,"./output/SUAS-2015-s-conselho.rds")
