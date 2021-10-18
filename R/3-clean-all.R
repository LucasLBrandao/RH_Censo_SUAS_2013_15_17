library(tidyverse)
library(stringi)
library(readxl)

SUAS_2013 <- readRDS("./output/SUAS-2013.rds") %>% select(-id_acolhimento)
SUAS_2015 <- readRDS("./output/SUAS-2015.rds") %>% select(-id_acolhimento)
SUAS_2017 <- readRDS("./output/SUAS-2017.rds") 



munic_13 <- read_excel("./input/MUNIC/base_MUNIC_xls_2013/base.xls",sheet = "Variáveis externas")
munic_15 <- read_excel("./input/MUNIC/Base_MUNIC_2015_xls/Base_MUNIC_2015.xls",sheet = "Variáveis externas")
munic_17 <- read_excel("./input/MUNIC/Base_MUNIC_2017_xls/Base MUNIC 2017.xls",sheet = "Variáveis externas")


colnames(munic_13) <- c("IBGE","cod_uf","cod_mun","Região","MUNICIPIO","População","Porte")
SUAS_2013 <- SUAS_2013 %>% left_join(munic_13)

colnames(munic_15) <- c("IBGE7","Região","cod_uf","sig_uf","MUNICIPIO","Porte","População")
SUAS_2015 <- SUAS_2015 %>% left_join(munic_15 )

colnames(munic_17) <- c("IBGE7","Região","cod_uf","sig_uf","MUNICIPIO","População","Porte")
SUAS_2017 <- SUAS_2017 %>% left_join(munic_17 )

SUAS_13_17 <- bind_rows(SUAS_2013,SUAS_2015,SUAS_2017)

SUAS_13_17 <- SUAS_13_17 %>% 
        select(-NU_IDENTIFICADOR,-NU_identificador) %>% 
        mutate_at(c("Sexo",
                    "Escolaridade",
                    "Profissão",
                    "Vínculo",
                    "Função",
                    "esfera",
                    "proteção",
                    "gov"),
                  tolower) %>% 
        
        mutate_at(c("Escolaridade",
                    "Profissão",
                    "Vínculo",
                    "Função",
                    "esfera",
                    "proteção",
                    "gov"),
                  stri_trans_general,id = "Latin-ASCII") %>% 
        mutate_at(c("Sexo",
                    "Escolaridade",
                    "Profissão",
                    "Vínculo",
                    "Função",
                    "esfera",
                    "proteção",
                    "gov"),
                  as.factor)

SUAS_13_17 <- SUAS_13_17 %>% 
        mutate(gov = str_replace_all(gov,"-"," ") %>% 
                       str_remove_all(" estadual") %>% 
                       str_remove_all(" municipal ou do distrito federal") %>% 
                       str_remove_all("/organizacao da sociedade civil") %>% 
                       as.factor(),
               Função = str_remove_all(as.character(Função),fixed("(a)")) %>% 
                                                  str_trim() %>% 
                                                  str_replace_all("  "," ")) %>% 
        mutate(Função = case_when(Função %in% c("educador social",
                                                "orientador/educador social") ~ "educador/orientador social",
                                  Função == "diretora" ~ "diretor",
                                  TRUE ~ as.character(Função)) %>% 
                       as.factor(),
               esfera = str_remove_all(esfera,"creas ") %>% as.factor()) %>% 
        select(-IBGE7,-Faixa_idade)


SUAS_13_17 <- SUAS_13_17 %>% 
        mutate(Escolaridade = factor(Escolaridade, 
                                     levels = c("sem escolaridade",
                                                "fundamental incompleto",
                                                "fundamental completo",
                                                "medio incompleto",
                                                 "medio completo",
                                                 "superior incompleto",
                                                "superior completo",
                                                "especializacao",
                                                "mestrado",
                                                "doutorado")),
               Vínculo = factor(Vínculo,
                                levels = c("sem vinculo",
                                           "outro vinculo nao permanente",
                                           "voluntario",
                                           "terceirizado",
                                           "empregado (clt)" ,
                                           "empregado publico (clt)",
                                           "servidor publico temporario",
                                           "comissionado",
                                           "servidor publico estatutario"
                                           )))

SUAS_13_17 <- SUAS_13_17 %>% 
        mutate(Escolaridade1 = case_when(Escolaridade %in% c("sem escolaridade","fundamental incompleto") ~ "Sem instrução",
                                         Escolaridade %in% c(
                                                "fundamental completo","medio incompleto")~ "Ensino fundamental",
                                         Escolaridade %in% c(
                                                 "medio completo","superior incompleto") ~ "Ensino médio",
                                         Escolaridade %in% c("superior completo") ~ "Ensino superior",
                                         Escolaridade %in% c("especializacao",
                                                "mestrado",
                                                "doutorado") ~ "Pós-graduação",
                                         TRUE ~ as.character(Escolaridade)) %>% factor(levels = c("Sem instrução","Ensino fundamental",
                                                                                                   "Ensino médio", "Ensino superior","Pós-graduação")),
               Vínculo1 = case_when(Vínculo == "servidor publico estatutario" ~ "Estatutários",
                                    Vínculo %in% c("empregado (clt)" ,
                                           "empregado publico (clt)") ~ "CLT",
                                    Vínculo == "comissionado" ~ "Comissionados",
                                    TRUE ~ "Sem vínculo permanente") %>% factor(levels = c( "Estatutários",
                                                                                            "CLT",
                                                                                             "Comissionados",
                                                                                            "Sem vínculo permanente")))


sapply(SUAS_13_17, levels)

saveRDS(SUAS_13_17,"./output/SUAS-2013-2017.rds")
