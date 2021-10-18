library(tidyverse)
library(readxl)
library(janitor)

SUAS_13_17 <- readRDS("./output/SUAS-2013-2017.rds")


SUAS_13_17 <- SUAS_13_17 %>% filter(gov == "governamental")


tabela_esc_vinc <- function(df = SUAS_13_17,Ano = 2015 ){
        
        
        df <- df %>% filter(ano == Ano ) %>% drop_na(Porte) %>% drop_na(Vínculo1) %>% drop_na(Escolaridade1)
        
       
         Vinc_totals <- df %>% group_by(Região,Porte) %>% 
                summarize(n = n()) %>% 
                mutate(Vínculo1 = "Total") %>% 
                adorn_totals(fill = "Total",name = "Brasil")
         Vinc_totals_br <- df %>% group_by(Porte) %>%
                 summarize(n = n()) %>% 
                 mutate(Região = "Brasil",
                        Vínculo1 = "Total")
                 
        Brasil_vinc <- df %>% group_by(Porte,Vínculo1) %>% 
                summarize(n = n()) %>% 
                mutate(Região = "Brasil")
        
        
        Regiões_vinc <- df %>% group_by(Região,Porte,Vínculo1) %>% 
                summarize(n = n())
        
        Brasil_regiões_vinc <- rbind(Brasil_vinc,Regiões_vinc,Vinc_totals,Vinc_totals_br)
        
        tabela_vinc <- Brasil_regiões_vinc %>% pivot_wider(names_from = "Vínculo1", values_from = "n") %>% 
                mutate(regiao_porte = paste0(Região,Porte))
        
        
        Brasil_esc <- df %>% group_by(Porte,Escolaridade1) %>% 
                summarize(n = n()) %>% 
                mutate(Região = "Brasil")
        
        Regiões_esc <- df %>% group_by(Região,Porte,Escolaridade1) %>% 
                summarize(n = n())
        
        Brasil_regiões_esc <- rbind(Brasil_esc,Regiões_esc)
        
        tabela_esc <- Brasil_regiões_esc %>% pivot_wider(names_from = "Escolaridade1", values_from = "n")%>% 
                mutate(regiao_porte = paste0(Região,Porte))
        
        tabela_esc_vinc_13 <- full_join(tabela_esc,tabela_vinc, by = "regiao_porte")
        
        tabela_esc_vinc_13 <- tabela_esc_vinc_13 %>% select(-Região.y,-Porte.y) %>% 
                select(Região.x,Porte.x,Total,Estatutários,CLT,Comissionados,`Sem vínculo permanente`,
                       `Sem instrução`,`Ensino fundamental`,`Ensino médio`,`Ensino superior`,`Pós-graduação`)
        
        return(tabela_esc_vinc_13)
        
}

tabela_esc_vinc(Ano = 2013) %>% write.csv2("./ouput/Tabela escolaridade vínculo 2013-gov.csv",row.names = F)

tabela_esc_vinc(Ano = 2015)  %>% write.csv2("./ouput/Tabela escolaridade vínculo 2015-gov.csv",row.names = F)

tabela_esc_vinc(Ano = 2017)  %>% write.csv2("./ouput/Tabela escolaridade vínculo 2017-gov.csv",row.names = F)


