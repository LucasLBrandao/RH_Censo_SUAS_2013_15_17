library(tidyverse)

source("./R/1. load2013.R",encoding = "UTF-8")

# Selecionando colunas -------

acolhimento_2013_dadosgerais <- read_excel("./input/Censo SUAS 2013/Acolhimento/Censo_SUAS_2013_Acolhimento_Dados_Gerais_Divulgação.xlsx",
                                           sheet = 1) %>% select(id_acolhimento,q4)

acolhimento_2013 <- acolhimento_2013 %>% 
        select(id_acolhimento,
               UF,
               IBGE,
               IBGE7,
               Município,
               Sexo = q38_3,
               Escolaridade = q38_4,
               Profissão = q38_5,
               Vínculo = q38_6,
               Função = q38_7,
               esfera = origem) %>% 
        left_join(acolhimento_2013_dadosgerais) %>% 
        rename(gov = q4) %>% 
        mutate(alocação = "Centro de acolhimento",
               proteção = "PSE de alta complexidade")


POP_2013 <- POP_2013 %>% 
         select(UF=uf,
                IBGE=municipio,
                Sexo = q46_3,
               Escolaridade = q46_6,
               Profissão = q46_7,
               Vínculo = q46_8,
               Função = q46_9,
               Idade = D46_2,
               Faixa_idade = D46_2_categoria) %>% 
        mutate(alocação = "Centro POP",
               esfera = "Municipal",
               proteção = "PSE de média complexidade",
               gov = "Governamental") %>% 
        left_join(municipios, by = c("IBGE" = "id")) %>% 
        rename(Município = nome)


cons_estad_2013 <- cons_estad_2013 %>% 
         select(sig.UF = UF,Sexo = q67_3,
               Escolaridade = q67_4,
               Função = q67_6,
               Idade = D67_2,
               Faixa_idade = D67_2_categoria) %>% 
        mutate(alocação = "Conselho estadual",
               esfera = "Estadual",
               gov = "Governamental") %>% 
        left_join(estados, by = c("sig.UF"="id")) %>% 
        rename(UF = sigla) %>% 
        select(-sig.UF)


cons_mun_2013 <- cons_mun_2013 %>%  
        select(UF,IBGE,IBGE7,Município,Sexo = q67_3,
               Escolaridade = q67_4,
               Função = q67_6,
               Idade = D67_2,
               Faixa_idade = D67_2_categoria) %>% 
        mutate(alocação = "Conselho município",
               esfera = "Municipal",
               gov = "Governamental")


CRAS_2013 <- CRAS_2013 %>% 
        select(UF=uf,IBGE,Município,Sexo = q63_5,
               Escolaridade = q63_6,
               Profissão = q63_7,
               Vínculo = q63_8,
               Função = q63_9,
               Idade = D63_2,
               Faixa_idade = D63_2_categoria) %>% 
        mutate(alocação = "CRAS",
               esfera = "Municipal",
               proteção = "PS básica",
               gov = "Governamental")


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
               Idade = D58_2,
               Faixa_idade = D58_2_categoria,
               esfera = tipo_creas) %>% 
        mutate(alocação = "CREAS",
               proteção = "PSE de média complexidade",
               gov = "Governamental")


# Unindo os dados --------

SUAS_2013 <- bind_rows(
        acolhimento_2013,
          POP_2013,
          cons_estad_2013,
          cons_mun_2013,
          CRAS_2013,
          CREAS_2013)




        
# Limpando os dados -----

SUAS_2013 <- SUAS_2013 %>% mutate(
        Escolaridade = toupper(str_replace(Escolaridade,"Ensino ","")),
        Profissão = ifelse(Profissão == "Outras formações de nível superior",
                           "Outro profissional de nivel superior",Profissão),
        
        Vínculo = case_when(Vínculo == "Cargo público comissionado" ~ "Comissionado" ,
                            #Vínculo == "Empregado (CLT)"~"Empregado Público (CLT)" ,
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
                           TRUE ~ as.character(Função)),
        Faixa_idade = str_replace(Faixa_idade,"De 18 a 29 anos","Até 29 anos")) %>% 
        mutate(UF = as.factor(UF),
               Sexo = as.factor(Sexo),
               Escolaridade = as.factor(Escolaridade),
               Profissão = as.factor(Profissão),
               Vínculo = as.factor(Vínculo),
               Função = as.factor(Função),
               alocação = as.factor(alocação),
               Faixa_idade = as.factor(Faixa_idade))


sapply(SUAS_2013, levels)


#   Verificando tipo dos dados
sapply(SUAS_2013,class)
#   Verificando intervalo dos dados numéricos


# Verificando e ajustando os valores de idade
library(assertive)
assert_all_are_in_closed_range(SUAS_2013$Idade[!is.na(SUAS_2013$Idade)],lower = 15,upper = 120)

SUAS_2013 %>% filter(Idade <= 16 |Idade >= 110) %>% 
        select(alocação,Função,Vínculo,Idade,Faixa_idade) %>% View()

idade_media <- SUAS_2013 %>% group_by(Faixa_idade) %>% 
        summarize(media_idade = floor(mean(Idade,na.rm = T)))

SUAS_2013 <- SUAS_2013 %>% 
        mutate(Idade = ifelse(Idade < 14,idade_media$media_idade[2],
                       ifelse(Idade > 113,idade_media$media_idade[1],
                        Idade)))


# Verificando variáveis categoricas
sapply(SUAS_2013, class)

# Arrumando os níveis dos fatores
SUAS_2013 <- SUAS_2013 %>% mutate(Escolaridade = factor(Escolaridade, levels = c("SEM ESCOLARIDADE",
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
SUAS_2013 %>% filter(is.na(IBGE)) %>% summary()

SUAS_2013 %>% filter(is.na(IBGE),
                     alocação == "Conselho município")


#Profissão
# Conselho estadual e municipal não tem profissão
SUAS_2013 %>% filter(is.na(Profissão)) %>% summary()
SUAS_2013 <- SUAS_2013 %>% mutate(Profissão = as.factor(ifelse(! alocação %in% c("Conselho estadual","Conselho município") &
                                                     is.na(Profissão),
                                                     "Sem formação profissional",
                                                     as.character(Profissão))))



#Vínculos
# Conselhos municipais e estaduais não estão indicados os vínculos
SUAS_2013 %>% filter(is.na(Vínculo)) %>% summary()


#Idade
# Centro de acolhimento não tem idade e conselho município falta muitos
SUAS_2013 %>% filter(is.na(Idade)) %>% View()

SUAS_2013_s_conselho <- SUAS_2013 %>% filter(!str_detect(alocação,"Conselho"))

# Adicionando a coluna de ano
SUAS_2013 <- SUAS_2013 %>% mutate(ano = 2013)
#Exportando arquivos
saveRDS(SUAS_2013,"./output/SUAS-2013.rds")
saveRDS(SUAS_2013_s_conselho,"./output/SUAS-2013-s-conselho.rds")
