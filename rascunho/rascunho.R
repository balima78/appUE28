godt<- read.csv2("GODT20002017.csv")


paises28<-c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")


library(dplyr)

godt<-godt %>% filter(COUNTRY %in% paises28)

godt$DDpmh<-round(godt$dd/godt$POPULATION,1)
godt$DDTxpmh<-round(godt$DDTx/godt$POPULATION,1)
godt$LDTxpmh<-round(godt$LDTx/godt$POPULATION,1)

godt<-mutate(godt,transp =
                  ifelse(COUNTRY %in% c("Austria","Belgium","Croatia","Germany","Hungary","Luxembourg","Netherlands","Slovenia"),7,
                         ifelse(COUNTRY %in% c("Denmark","Finland","Latvia","Sweden"),6,
                                ifelse(COUNTRY %in% c("United Kingdom"),5,
                                       ifelse(COUNTRY %in% c("Ireland"),4,
                                              ifelse(COUNTRY %in% c("France"),3,
                                                     ifelse(COUNTRY %in% c("Spain"),1,2))
                                       )
                                )
                         )
                  )
)

write.csv2(godt,"GODT20002016.csv")                         

library(ggplot2)

g<-ggplot(filter(godt,year==2016),aes(LDTxpmh,DDTxpmh, label = COUNTRY)) +
  geom_point(aes(size = DDpmh, color = transp), alpha = 0.5) +
  geom_text(vjust = 0, nudge_y = 0.5, size = 3) +
  scale_x_continuous(breaks = seq(0,50, 5),
                     limits = c(-0.5,55)) +
  scale_y_continuous(breaks = seq(0,60,10),
                     limits = c(-0.5,65))
  
  

g +theme_bw() + theme(legend.position = "none")




# criar data.frame para gráfico ordens de nº Dadores pmh
dadores <- godt %>% select(pais,ano,Populacao,DCpmh) %>% group_by(ano)
dadores<-dadores %>% arrange(desc(DCpmh),.by_group = T) 
dadores<-dadores %>% group_by(ano) %>% mutate(ordem = row_number())

write.csv2(dadores,"dadores28.csv")




###### testar pacote de resumo de dados
## https://cran.r-project.org/web/packages/SmartEDA/vignettes/Report_r1.html
install.packages("SmartEDA")
library("SmartEDA")

or<-read.csv2("orgaos.csv")
ExpData(data=or,type=1,DV=NULL)
ExpData(data=or,type=2,DV=NULL)

or <- or %>% mutate(hpmh = round(Heart/pop,2))
or <- or %>%   group_by(year) %>% mutate(ordem = rank(Heart)) %>% rename(oh = ordem)


library(googleVis)
library(shiny)
runGist(4642963)


library(tidyverse)

dv18 <- godt %>% filter(year == 2017) %>% select(COUNTRY,DDpmh,DDTxpmh,LDTxpmh) 

dv18<-dv18 %>% mutate(zTxDC = round((DDTxpmh - mean(dv18$DDTxpmh,na.rm = T))/sd(dv18$DDTxpmh,na.rm = T),2))


dv18$tipo <- ifelse(dv18$zTxDC < 0, "below", "above")  # above / below avg flag
dv18 <- dv18[order(dv18$zTxDC), ]  # sort
dv18$COUNTRY <- factor(dv18$COUNTRY, levels = dv18$COUNTRY)  # convert to factor to retain sorted order in plot.

ggplot(dv18, aes(x=COUNTRY, y=zTxDC, label=zTxDC)) + 
  geom_bar(stat='identity', aes(fill=tipo), width=.5)  +
  scale_fill_manual(name="Transplantes", 
                   labels = c("Above Average", "Below Average"), 
                 values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="txs pmh (valores normalizados)", 
       title= "Diverging Bars") + 
  coord_flip()


ggplot(data = dv18, aes(x = COUNTRY, y = zTxDC, fill = zTxDC)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="red", mid="yellow", high="green") +
  coord_flip() +
  labs(y="Under/over valuation in %", x="")


dv18<-dv18 %>% mutate(zTxDV = round((LDTxpmh - mean(dv18$LDTxpmh,na.rm = T))/sd(dv18$LDTxpmh,na.rm = T),2))
dv18 <- dv18[order(dv18$zTxDV), ]  # sort
dv18$COUNTRY <- factor(dv18$COUNTRY, levels = dv18$COUNTRY)  # convert to factor to retain sorted order in plot.

cd28a<-cd28 %>% filter(ano == 2007) %>% mutate(zcd = (cdtxtaxa - mean(cdtxtaxa))/sd(cdtxtaxa))
cd28a <- cd28a[order(cd28a$zcd), ]  # sort
cd28a$pais <- factor(cd28a$pais, levels = cd28a$pais)  # convert to factor to retain sorted order in plot.

cddiv<-ggplot(data = cd28a, aes(x = pais, y = zcd, fill = zcd)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="red", mid="#e5eef0", high="#3366ff") +
  coord_flip() +
  labs(y="Under/over valuation in %", x="") +
  theme_bw() +
  theme(legend.position = "")

ld28a<-ld28 %>% filter(ano == 2017) %>% 
  mutate(zld = (ldtxtaxa - mean(ldtxtaxa))/sd(ldtxtaxa)) 

ld28a <- ld28a[order(ld28a$zld), ]  # sort
ld28a$pais <- factor(ld28a$pais, levels = ld28a$pais)  # convert to factor to retain sorted order in plot.
#ld28a<-ld28a %>%   mutate(pais = ifelse(pais == "Portugal", "PORTUGAL", levels(pais)))
levels(ld28a$pais)[match("Portugal",levels(ld28a$pais))] <- "PORTUGAL"

lddiv<-ggplot(data = ld28a, aes(x = pais, y = zld, fill = zld)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low="red", mid="#e5eef0", high="#33cc00") +
  coord_flip() +
  labs(y="Under/over valuation in %", x="") +
  theme_bw() +
  theme(legend.position = "")

grid.arrange(lddiv,cddiv, ncol=2)
