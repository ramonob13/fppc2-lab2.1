
library(tidyverse)
library(here)
library(lubridate)
library(ggbeeswarm)
theme_set(theme_bw())

climas = read_csv(here::here("data/clima_cg_jp-semanal.csv"), 
                  col_types = "cTdddddd")

glimpse(climas)


## Temperatura

### Onde é mais quente?
climas %>%
    group_by(cidade)%>%
        summarize(temp_media=mean(tmedia)) %>%
            arrange(desc(temp_media))



### As temperaturas máximas semanais (o pico do calor) são tão
#diferentes quanto as médias? Mais?  

climas %>%
    group_by(cidade) %>%
        summarize(dif_temp_max_media=mean(tmax)-mean(tmedia))

### Quais os meses mais quentes e mais frios? 
climas_meses<-climas %>%
    group_by(cidade,mes) %>%
        summarize(temp_media=mean(tmedia)) %>%
            arrange(desc(temp_media))

climas_meses

ggplot(climas_meses,aes(x=mes,y=temp_media,color=cidade))+
    geom_line()


#histograma de temperaturas médias de João Pessoa
jp<-climas %>%
    filter(cidade=="João Pessoa")

hist_tmedia_joao_pessoa = jp %>%
    ggplot(aes(x=tmedia))+
        geom_histogram(binwidth=0.1,color="#F6511D",fill="white")

#histograma de temperaturas médias de Campina Grande
cg<-climas %>%
    filter(cidade=="Campina Grande")

hist_tmedia_campina_grande = cg %>%
    ggplot(aes(x=tmedia))+
    geom_histogram(binwidth=0.1,color="#F6511D",fill="white")

grid.arrange(hist_tmedia_campina_grande,hist_tmedia_joao_pessoa,ncol=2)


sao_joao_frio_cg<-climas %>%
     filter(cidade=="Campina Grande" & mes ==6) 

### Qual foi o São João  mais frio de CG que está nos nossos dados?
sao_joao_frio_cg<-climas %>%
        filter(cidade=="Campina Grande" & mes==6) %>%
            group_by(mes,ano) %>%
                summarize(temp_media=mean(tmedia)) 

sao_joao_frio_cg


## Chuva

### Quanto chove por semana em JP e CG?
jp<-climas %>%
    filter(cidade=="João Pessoa")

hist_chuva_semanal_joao_pessoa = jp %>%
    ggplot(aes(x=chuva))+
    geom_histogram(binwidth=0.1,color="#F6511D",fill="white")

cg<-climas %>%
    filter(cidade=="Campina Grande")

hist_chuva_semanal_campina_grande= cg %>%
    ggplot(aes(x=chuva))+
    geom_histogram(binwidth=0.1,color="#F6511D",fill="white")

grid.arrange(hist_chuva_semanal_joao_pessoa,hist_chuva_semanal_campina_grande,ncol=2)

#Como é a distribuição do volume de chuvas por semana em JP e CG? A chuva varia mais ou menos que a temperatura? O formato da distribuição é igual? 
    
    ### No geral, onde chove mais?
    
    ### A distribuição é diferente em meses específicos? 
    
# histograma
cg = climas %>%
    filter(cidade == "Campina Grande")

hist_chuvas = cg %>%
    ggplot(aes(x = chuva)) +
    geom_histogram(binwidth = 10, color = "#F6511D", fill = "white")

hist_temperatura = cg %>%
    ggplot(aes(x = tmedia)) +
    geom_histogram(binwidth = .5, color = "#F6511D", fill = "white")  

grid.arrange(hist_chuvas, hist_temperatura, ncol = 2)    
    
    