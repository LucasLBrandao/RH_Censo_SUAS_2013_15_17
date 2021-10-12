library(tidyverse)
library(stringi)

SUAS_2013 <- readRDS("./output/SUAS-2013.rds")
SUAS_2015 <- readRDS("./output/SUAS-2015.rds")
SUAS_2017 <- readRDS("./output/SUAS-2017.rds")

SUAS_13_17 <- bind_rows(SUAS_2013,SUAS_2015,SUAS_2017)

SUAS_13_17 <- SUAS_13_17 %>% 
        mutate_at(c("Sexo",
                    "Escolaridade",
                    "Profissão",
                    "Vínculo",
                    "Função"),
                  tolower) %>% 
        
        mutate_at(c("Escolaridade",
                    "Profissão",
                    "Vínculo",
                    "Função"),
                  stri_trans_general,id = "Latin-ASCII") %>% 
        mutate_at(c("Sexo",
                    "Escolaridade",
                    "Profissão",
                    "Vínculo",
                    "Função"),
                  as.factor)

SUAS_13_17 <- SUAS_13_17 %>% 
        mutate(Função = str_remove_all(as.character(Função),fixed("(a)")) %>% 
                                                  str_trim() %>% 
                                                  str_replace_all("  "," ")) %>% 
        mutate(Função = case_when(Função %in% c("educador social",
                                                "orientador/educador social") ~ "educador/orientador social",
                                  Função == "diretora" ~ "diretor",
                                  TRUE ~ as.character(Função)) %>% 
                       as.factor()) %>% 
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

sapply(SUAS_13_17, levels)

saveRDS(SUAS_13_17,"./output/SUAS-2013-2017.rds")
