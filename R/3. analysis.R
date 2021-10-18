library(tidyverse)

SUAS_13_17 <- readRDS("./output/SUAS-2013-2017.rds")

SUAS_13_17 <- SUAS_13_17 %>% filter(gov == "governamental",
                                    !str_detect(alocação,"Conselho"))

# Tema dos gráficos -----
GRAY1 <- "#231F20"
GRAY2 <- "#414040"
GRAY3 <- "#555655"
GRAY4 <- "#646369"
GRAY5 <- "#76787B"
GRAY6 <- "#828282"
GRAY7 <- "#929497"
GRAY8 <- "#A6A6A5"
GRAY9 <- "#BFBEBE"
BLUE1 <- "#174A7E"
BLUE2 <- "#4A81BF"
BLUE3 <- "#94B2D7"
BLUE4 <- "#94AFC5"
BLUE5 <- "#22435e"
BLUE6 <- "#95B3D7"
RED1 <- "#C3514E"
RED2 <- "#E6BAB7"
RED3 <- "#800000"
GREEN1 <- "#0C8040"
GREEN2 <- "#9ABB59"
GREEN3 <- "#31859C"
GREEN4 <- "#4BACC6"
GREEN5 <- "#93CDDD"
ORANGE1 <- "#F79747"
ORANGE2 <- "#FAC090"

theme_swd <- function() {
theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(size = .2, color = GRAY9),
      axis.text = element_text(color = GRAY7),
      axis.ticks.x = element_line(size = 0.5, color = GRAY9),
      axis.ticks.y = element_line(size = 0.5, color = GRAY9),
      axis.title = element_text(color = GRAY3),
      axis.title.y = element_text(hjust = 1, margin = margin(0, 6, 0, 15, "pt")),
      axis.title.x = element_text(hjust = 0, margin = margin(6, 0, 15, 0, "pt")),
      plot.subtitle = element_text(color = GRAY4, size= 8, margin = margin(0,0,20,0,"pt")),
      plot.title = element_text(color = GRAY4, size= 12, margin = margin(0,0,5,0,"pt")),
      plot.title.position = "plot", # This aligns the plot title to the very left edge
      plot.caption = element_text(hjust = 0, color = GRAY6),
      plot.caption.position = "plot",
      plot.margin = margin(.5,.5,.5,.5,"cm"),
      strip.text = element_text(color = GRAY7)) 
}

save_plot <- function(plot,title,Width = 12,Height = 8,Pointsize = 12, Res = 300, Units = "in"){
  
  png(filename = paste0("./graphics/", title),
      type = "cairo",
      units = Units, 
      width = Width, 
      height = Height, 
      pointsize = Pointsize, 
      res = Res)
  print(plot)
  dev.off()
}



# Quantidade de trabalhadores -------
#       por esfera ----

p1 <- SUAS_13_17 %>%
        #mutate(ano = as.character(ano)) %>% 
        group_by(esfera,ano) %>% 
        summarise(n = n()) %>% 
        drop_na() %>% 
        ggplot(aes(x = ano, y = n, color = reorder(esfera,-n )))+
         geom_line(size = 1)+
         geom_point()+
         geom_text(aes(label = n),
                   vjust = -1)+
         theme_swd()+
         labs(y = "Quantidade de trabalhadores da Assistência Social",
              color = "",
              title = "Número de trabalhadores do SUAS por esfera")+
        scale_y_log10(labels = scales::comma)

save_plot(p1,title = "qt de trabalhadores por esfera.png")

#       por proteção ----

p1 <- SUAS_13_17 %>%
        #mutate(ano = as.character(ano)) %>% 
        group_by(proteção,ano) %>% 
        summarise(n = n()) %>% 
        drop_na() %>% 
        ggplot(aes(x = ano, y = n, color = reorder(proteção,-n )))+
         geom_line(size = 1)+
         geom_point()+
         geom_text(aes(label = n),
                   vjust = -1)+
         theme_swd()+
         labs(y = "Quantidade de trabalhadores da Assistência Social",
              color = "",
              title = "Número de trabalhadores do SUAS por proteção")+
        scale_y_continuous(labels = scales::comma)

save_plot(p1,title = "qt de trabalhadores por proteção.png")


#       Por estado -----

p1 <- SUAS_13_17 %>%
        mutate(ano = as.character(ano)) %>%
        group_by(UF,ano) %>% 
        summarise(n = n()) %>% 
        ggplot(aes(y = reorder(UF,n), x = n/1000, color = ano))+
        geom_point()+
        geom_text(aes(label = round(n/1000,1)),
                  vjust = -1,
                  size = 2.5)+
        theme_swd()+
        theme(panel.grid.major.y = element_line(),
              legend.position = "top",
              legend.justification="left")+
        labs(y = "Estado",
             x = "Quantidade de trabalhadores da Assistência Social (em mil)",
             color = "",
              title = "Número de trabalhadores do SUAS por estadi")+
        scale_y_discrete(expand = c(0.04,0.04))+
        scale_color_manual(values = c("#6EAAE6","#2477C9","#0D2C4A"))

library('Cairo')
png(filename="./graphics/qt de trabalhadores por estado.png",
    type="cairo",
    units="in", 
    width=12, 
    height=8, 
    pointsize=12, 
    res=300)
print(p1)
dev.off()

SUAS_13_17 %>%
        mutate(ano = as.character(ano)) %>% 
        filter(!str_detect(alocação,"Conselho")) %>% 
        group_by(UF,ano) %>% 
        summarise(n = n()) %>% 
        ungroup() %>% 
        pivot_wider(names_from = "ano",values_from = "n") %>% 
        write.csv2("./output/qt de trabalhadores por estado.csv", row.names = F)

#       Por município --------

p2 <- SUAS_13_17 %>% 
        mutate(ano = as.character(ano),
               municipiouf = paste0(Município,", ",UF)) %>% 
        group_by(municipiouf,ano) %>% 
        summarise(n = n()) %>%
        pivot_wider(names_from = "ano",values_from = "n") %>% 
        arrange(desc(`2017`)) %>%
        head(20) %>% 
        pivot_longer(cols = c(`2013`,`2015`,`2017`),
                     names_to = "ano",
                     values_to = "n") %>% 
        ggplot(aes(y = reorder(municipiouf,n), x = n/1000, color = ano))+
        geom_point()+
        geom_text(aes(label = round(n/1000,1)),
                  vjust = -1,
                  size = 2.5)+
        theme_swd()+
        theme(panel.grid.major.y = element_line(),
              legend.position = "top",
              legend.justification="left")+
        labs(y = "Município, UF",
             x = "Quantidade de trabalhadores da Assistência Social (em mil)",
             color = "",
              title = "Número de trabalhadores do SUAS por município")+
        scale_y_discrete(expand = c(0.04,0.04))+
        scale_color_manual(values = c("#6EAAE6","#2477C9","#0D2C4A"))

png(filename="./graphics/qt de trabalhadores por município.png",
    type="cairo",
    units="in", 
    width=12, 
    height=8, 
    pointsize=12, 
    res=300)
print(p2)
dev.off()

 SUAS_13_17 %>% 
        mutate(ano = as.character(ano),
               municipiouf = paste0(Município,", ",UF)) %>% 
        filter(!str_detect(alocação,"Conselho")) %>% 
        group_by(municipiouf,ano) %>% 
        summarise(n = n()) %>%
        pivot_wider(names_from = "ano",values_from = "n") %>% 
        arrange(desc(`2017`)) %>% 
        write.csv2("./output/qt de trabalhadores por município.csv", row.names = F)
 
# Por alocação
#       Por alocação ------
p3 <- SUAS_13_17 %>%
        #mutate(ano = as.character(ano)) %>% 
        group_by(alocação,ano) %>% 
        summarise(n = n()) %>% 
        ggplot(aes(x = ano, y = n, color = reorder(alocação,-n )))+
         geom_line(size = 1)+
         geom_point()+
         geom_text(aes(label = n),
                   vjust = -1)+
         theme_swd()+
         labs(y = "Quantidade de trabalhadores da Assistência Social",
              color = "",
              title = "Número de trabalhadores do SUAS por alocação")
 
 png(filename="./graphics/qt de trabalhadores por alocação.png",
    type="cairo",
    units="in", 
    width=12, 
    height=8, 
    pointsize=12, 
    res=300)
print(p3)
dev.off()

SUAS_13_17 %>%
        #mutate(ano = as.character(ano)) %>% 
        filter(!str_detect(alocação,"Conselho")) %>% 
        group_by(alocação,ano) %>% 
        summarise(n = n()) %>% 
  pivot_wider(names_from = ano, values_from = n) %>% 
  write.csv2("./output/qt de trabalhadores por alocação.csv")
 
#Perfil -----

perfil_csv <- function(eixo_x,eixo_fill, Ano = c(2013,2015,2017)){
  
  eixo_x <- enquo(eixo_x)
  eixo_fill <- enquo(eixo_fill)
  
  SUAS_13_17 %>%
    filter(ano %in% Ano) %>% 
    mutate(ano = as.character(ano)) %>% 
    drop_na(!!eixo_fill) %>%
    drop_na(!!eixo_x) %>% 
    group_by(!!eixo_x,!!eixo_fill,ano) %>% 
    summarise(n = n()) %>% 
    ungroup(!!eixo_fill) %>% 
    mutate(perc = n/sum(n)) %>% 
    arrange(ano) %>% 
    ungroup()
}

perfil <- function(eixo_x, eixo_fill , Ano = 2017, ranking = 5){
  
  
  eixo_x <- enquo(eixo_x)
  eixo_fill <- enquo(eixo_fill)
  
   título <- paste("Perfil de",(eixo_fill),"* dos trabalhadores em cada",(eixo_x),"em", paste(Ano, collapse = ","))
  
  
  SUAS_13_17_perc <- SUAS_13_17 %>%
    filter(ano %in% Ano) %>% 
    mutate(ano = as.character(ano)) %>% 
    drop_na(!!eixo_fill) %>%
    drop_na(!!eixo_x) %>% 
    group_by(!!eixo_x,!!eixo_fill) %>% 
    summarise(n = n()) %>% 
    ungroup(!!eixo_fill) %>% 
    mutate(perc = n/sum(n),
           rank = rank(-perc),
           eixo_fill = !!eixo_fill) %>% 
    ungroup
  
  levels <- levels(SUAS_13_17_perc$eixo_fill)
  
  top_levels <- SUAS_13_17_perc %>% 
    filter(rank < ranking) %>% 
    mutate(eixo_fill = as.character(!!eixo_fill)) %>% 
    select(eixo_fill) %>%
    as_vector() %>% 
    unique()
  
  other_levels <- levels[!levels %in% top_levels]
  
  caption_outros <- str_wrap(paste("*",paste(other_levels,collapse = ", ")))
  
  SUAS_13_17_perc %>% 
    ungroup() %>% 
    mutate(eixo_fill = fct_collapse(!!eixo_fill, `outros*` = other_levels)) %>% 
    group_by(!!eixo_x,eixo_fill) %>% 
    summarize(perc = sum(perc)) %>% 
    ggplot(aes(x = !!eixo_x, y = perc*100, fill =eixo_fill,perc))+
    geom_col()+
    geom_text(aes(label= ifelse(perc>0.04,round(perc*100,2),"")), 
              position=position_stack(vjust = 0.5),
              color = "white")+
    labs(x = "",
         y = "Porcentagem do total (%)",
         fill = "",
         caption = caption_outros)+
    theme_swd()
  
  
}

# Perfil de escolaridade -----
#       por esfera ----
p0 <- perfil(esfera,Escolaridade,ranking = 4) +
        coord_flip()+
  scale_fill_manual(values = rev(c("#001d6c","#3c3d8a","#6560a8","#8d85c8","#b5ace8","#C2C2C2")))+
  ggtitle("Perfil de Escolaridade por esfera em 2017")+guides(fill = guide_legend(reverse=T))

save_plot(p0,Height = 6,"Perfil de escolaridade por esfera em 2017.png")

#       por proteção ----


p1 <- perfil(proteção,Escolaridade,ranking = 4) +
        coord_flip()+
  scale_fill_manual(values = rev(c("#001d6c","#3c3d8a","#6560a8","#8d85c8","#b5ace8","#C2C2C2")))+
  ggtitle("Perfil de Escolaridade por proteção em 2017")+guides(fill = guide_legend(reverse=T))

save_plot(p1,Height = 6,"Perfil de escolaridade por proteção em 2017.png")

#       por alocação ----

p4 <- perfil(alocação,Escolaridade,ranking = 4) +
        coord_flip()+
  scale_fill_manual(values = rev(c("#001d6c","#3c3d8a","#6560a8","#8d85c8","#b5ace8","#C2C2C2")))+
  ggtitle("Perfil de Escolaridade por alocação em 2017")+guides(fill = guide_legend(reverse=T))
save_plot(p4,Height = 6,"Perfil de escolaridade por alocação em 2017.png")

perfil_csv(alocação,Escolaridade) %>% write.csv2("./output/Perfil escolaridade por alocação.csv",row.names = F)

#       por ano ----

p5 <- perfil(ano,Escolaridade,Ano = c(2013,2015,2017),ranking = 6)+
  scale_fill_manual(values = rev(c("#001d6c","#3c3d8a","#6560a8","#8d85c8","#958dce","#b5ace8","#C2C2C2")))+
  ggtitle("Perfil de Escolaridade por ano")

save_plot(p5,Height = 8,Width = 8,title = "Perfil de escolaridade por ano.png")

# Perfil de formação ----
#       por esfera ----

perfil_csv(esfera,Profissão) %>% write.csv2("./output/Perfil formação por alocação.csv",row.names = F)


p6 <- perfil(esfera,Profissão,ranking = 3)+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  scale_fill_manual(values = rev(c("#3588d1", "#214d4e", "#da887c", "#29b28a", "#9e194d", "#729890","#C2C2C2")))+
  ggtitle("Perfil de formação por esfera em 2017")


save_plot(p6,title = "Perfil de formação por esfera em 2017.png")

#       por proteção ----

perfil_csv(proteção,Profissão) %>% write.csv2("./output/Perfil formação por alocação.csv",row.names = F)


p6 <- perfil(proteção,Profissão,ranking = 3)+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  scale_fill_manual(values = rev(c("#3588d1", "#214d4e", "#da887c", "#29b28a", "#9e194d", "#729890","#C2C2C2")))+
  ggtitle("Perfil de formação por protençao em 2017")


save_plot(p6,title = "Perfil de formação por proteção em 2017.png")

#       por alocação ----

perfil_csv(alocação,Profissão) %>% write.csv2("./output/Perfil formação por alocação.csv",row.names = F)


p6 <- perfil(alocação,Profissão,ranking = 3)+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  scale_fill_manual(values = rev(c("#3588d1", "#214d4e", "#da887c", "#29b28a", "#9e194d", "#729890","#C2C2C2")))+
  ggtitle("Perfil de formação por alocação em 2017")


save_plot(p6,title = "Perfil de formação por alocação em 2017.png")

#       por ano -------
p7 <- perfil(ano,Profissão,ranking = 5, Ano = c(2013,2015,2017))+
  #coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  scale_fill_manual(values = rev(c("#3588d1", "#214d4e", "#da887c", "#29b28a", "#9e194d", "#729890","#C2C2C2")))+
  ggtitle("Perfil de formação por ano")

save_plot(p7,Height = 8,Width = 7,title = "Perfil de formação por ano.png")


# Perfil de vínculo ------
#       por esfera ----

perfil_csv(esfera,Vínculo) %>% write.csv2("./output/Perfil Vínculo por alocação.csv",row.names = F)


p6 <- perfil(esfera,Vínculo,ranking = 3)+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  scale_fill_manual(values = rev(c("#001d6c","#303481","#4f4c96","#6b65ac","#8880c2","#a49cd9","#C2C2C2")))+
  ggtitle("Perfil de Vínculo por esfera em 2017")


save_plot(p6,title = "Perfil de Vínculo por esfera em 2017.png")

#       por proteção ----

perfil_csv(proteção,Vínculo) %>% write.csv2("./output/Perfil Vínculo por alocação.csv",row.names = F)


p6 <- perfil(proteção,Vínculo,ranking = 3)+
  coord_flip()+
  guides(fill = guide_legend(reverse=T))+
  scale_fill_manual(values = rev(c("#001d6c","#303481","#4f4c96","#6b65ac","#8880c2","#a49cd9","#C2C2C2")))+
  ggtitle("Perfil de Vínculo por proteção em 2017.png")


save_plot(p6,title = "Perfil de Vínculo por proteção em 2017.png")
#      por alocação ------

perfil_csv(alocação,Vínculo) %>% write.csv2("./output/Perfil Vínculo por alocação.csv",row.names = F)


p8 <- perfil(alocação,Vínculo,ranking = 3)+coord_flip()+
  scale_fill_manual(values = rev(c("#001d6c","#3c3d8a","#6560a8","#8d85c8","#b5ace8","#C2C2C2")))+
  ggtitle("Perfil de Vínculo por alocação em 2017")+guides(fill = guide_legend(reverse=T))

save_plot(p8, title = "Perfil de vínculo por alocação em 2017.png")

#     por ano -------
p9 <- perfil(ano,Vínculo, Ano = c(2013,2015,2017),ranking = 5)+
  scale_fill_manual(values = rev(c("#001d6c","#303481","#4f4c96","#6b65ac","#8880c2","#a49cd9","#C2C2C2")))+
  ggtitle("Perfil de Vínculo por ano")#guides(fill = guide_legend(reverse=T))

save_plot(p9,Height = 8,Width = 7, title = "Perfil de vínculo por ano.png")

