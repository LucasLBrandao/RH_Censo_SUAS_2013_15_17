library(tidyverse)
library(readxl)

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


# Análise -----


cras_2017 <- read_excel("./input/Censo SUAS 2017/CRAS/Censo SUAS 2017_CRAS_divulgacao.xlsx")
cras_2015 <- read_excel("./input/Censo SUAS 2015/CRAS/CensoSUAS_2015_CRAS_dadosgerais_divulgacao.xlsx")
cras_2013 <- read_excel("./input/Censo SUAS 2013/CRAS/Censo_SUAS_2013_CRAS_Dados_Gerais_Divulgação.xlsx")

cras_2013_loc <- cras_2013 %>% select(NU_IDENTIFICADOR,q1) %>% distinct(NU_IDENTIFICADOR,.keep_all = T) %>% mutate(ano = 2013)
cras_2015_loc <- cras_2015 %>% select(NU_IDENTIFICADOR,q1) %>% distinct(NU_IDENTIFICADOR,.keep_all = T) %>% mutate(ano = 2015)
cras_2017_loc <- cras_2017 %>% select(NU_IDENTIFICADOR = NºIDENTIFICADOR,q1) %>% distinct(NU_IDENTIFICADOR,.keep_all = T) %>% mutate(ano = 2017)

cras_loc <- bind_rows(cras_2013_loc,
          cras_2015_loc,
          cras_2017_loc)

cras_loc <- cras_loc %>% mutate(loc = as.factor(q1))

levels(cras_loc$loc)

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


p1 <- cras_loc %>% group_by(ano,loc) %>% 
        filter(loc != "Itinerante (Embarcação)") %>% 
        summarize(n = n()) %>% 
        ggplot(aes(x = ano, y = n, color = reorder(loc,-n)))+
        geom_line(size = 1)+
        geom_point()+
        geom_text(aes(label = n),vjust = -1)+
        ylim(0,4600)+
        labs(x = "Ano",
              y = "Número de CRAS",
             color = "")+
        theme_swd()

save_plot(p1,Height = 5,Width = 8,title = "Localização dos CRAS.png")
