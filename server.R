
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(googleVis)
library(gridExtra)


theme_new <- theme_set(theme_classic())

# ler os dados
cd28 <- read.csv2("cdtxtaxa28.csv")
cd28[is.na(cd28)] <- 0 
ld28 <- read.csv2("ldtxtaxa28.csv")
ld28[is.na(ld28)] <- 0 
dadores <- read.csv2("dadores28.csv")
dadores[is.na(dadores)] <- 0 
dadores <- dadores %>% filter(ano > 2002)


# dados de GODT
godt<- read.csv2("GODT20002022.csv")
names(godt)<-c("X","pais","ano","Populacao","DadoresCadaver","Tx_DadorCadaver","Tx_DadorVivo", "Tx_total", "DCpmh","TxDCpmh","TxDVpmh","transparencia")

## Ordem dadores cadaver #########
# guardar o nome no principio e no fim das linhas
dadores_1 <- dadores %>%
  filter(ano == max(ano)) %>%
  mutate(ano = as.numeric(ano) + 0.25)
dadores_2 <- dadores %>%
  filter(ano == min(ano)) %>%
  mutate(ano = as.numeric(ano) - 0.25)
# atribuir cores a alguns paises
coresdad <- c("Spain" = "#a67c00"
             , "Slovenia" = "#bf9b30"
             , "Belgium" = "#ffbf00"
             , "Italy" = "#ffcf40"
             , "Portugal" = "red")
outrospaises <- dadores %>% distinct(pais) %>% 
  filter(!pais %in% names(coresdad)) %>% .$pais
coresdad <- c(coresdad, setNames(rep("gray", length(outrospaises)), outrospaises))

# agora o gráfico
pdad <- ggplot(mapping = aes(ano, y = ordem, group = pais, color = pais)) +
  geom_line(size = 1.7, alpha = 0.25, data = dadores) +
  geom_line(size = 2.5, data = dadores %>% filter(pais %in% names(coresdad)[coresdad != "gray"])) +
  geom_point(size = 4, alpha = 0.25, data = dadores) +
  geom_point(size = 4, data = dadores %>% filter(pais %in% names(coresdad)[coresdad != "gray"])) +
  geom_point(size = 1.75, color = "white", data = dadores) +
  geom_text(data = dadores_1, aes(label = pais), hjust = -0, size = 4) +
  geom_text(data = dadores_2, aes(label = pais), hjust = 1, size = 4) +
  scale_color_manual(values = coresdad) +
  ggtitle("Nº de Dadores Cadáver (pmh)") +
  xlab("Ano") +
  scale_x_continuous(breaks = seq(min(dadores$ano),
                                  max(dadores$ano)),
                     limits = c(min(dadores$ano) - 2.0,
                                max(dadores$ano) + 2.0))
pdad<-pdad +  
  scale_y_continuous(trans = "reverse", breaks = seq(1,28)) + theme(legend.position="none") 

# criar lista para graficos Ordem
grafs<-list(Dadores = NULL, Vivo = NULL, Cadaver = NULL) 
# guardar grafico
grafs$Dadores <- pdad

## Ordem Txs com Dador Vivo #####################
# guardar o nome no principio e no fim das linhas
ld28_1 <- ld28 %>%
  filter(ano == max(ano)) %>%
  mutate(ano = as.numeric(ano) + 0.25)
ld28_2 <- ld28 %>%
  filter(ano == min(ano)) %>%
  mutate(ano = as.numeric(ano) - 0.25)

# atribuir cores a alguns paises
coresld <- c("Holanda" = "#003300"
           , "Franca" = "#00cc00"
           , "Alemanha" = "#006600"
           , "Reino Unido" = "#009900"
           , "Portugal" = "red")
outrospaises <- ld28 %>% distinct(pais) %>% filter(!pais %in% names(coresld)) %>% .$pais
coresld <- c(coresld, setNames(rep("gray", length(outrospaises)), outrospaises))

# agora o gráfico
pld <- ggplot(mapping = aes(ano, y = ordem, group = pais, color = pais)) +
  geom_line(size = 1.7, alpha = 0.25, data = ld28) +
  geom_line(size = 2.5, data = ld28 %>% filter(pais %in% names(coresld)[coresld != "gray"])) +
  geom_point(size = 4, alpha = 0.25, data = ld28) +
  geom_point(size = 4, data = ld28 %>% filter(pais %in% names(coresld)[coresld != "gray"])) +
  geom_point(size = 1.75, color = "white", data = ld28) +
  geom_text(data = ld28_1, aes(label = pais), hjust = -0, size = 4) +
  geom_text(data = ld28_2, aes(label = pais), hjust = 1, size = 4) +
  scale_color_manual(values = coresld) +
  ggtitle("Transplante de rim de dador vivo (pmh)") +
  xlab("Ano") +
  scale_x_continuous(breaks = seq(min(ld28$ano),
                                  max(ld28$ano)),
                     limits = c(min(ld28$ano) - 2.0,
                                max(ld28$ano) + 2.0))
pld<-pld +  
  scale_y_continuous(trans = "reverse", breaks = seq(1,28)) + theme(legend.position="none") 

# guardar na lista
grafs$Vivo<-pld

## Ordem Txs com Dador Cadaver ##################
# guardar o nome no principio e no fim das linhas
cd28_1 <- cd28 %>%
  filter(ano == max(ano)) %>%
  mutate(ano = as.numeric(ano) + 0.25)
cd28_2 <- cd28 %>%
  filter(ano == min(ano)) %>%
  mutate(ano = as.numeric(ano) - 0.25)

# atribuir cores a alguns paises
cores <- c("Espanha" = "#011f4b"
           , "Belgica" = "#03396c"
           , "Eslovenia" = "#6497b1"
           , "Italia" = "#b3cde0"
           , "Portugal" = "red")
outrospaises <- cd28 %>% distinct(pais) %>% filter(!pais %in% names(cores)) %>% .$pais
cores <- c(cores, setNames(rep("gray", length(outrospaises)), outrospaises))

# agora o gráfico
pcd <- ggplot(mapping = aes(ano, y = ordem, group = pais, color = pais)) +
  geom_line(size = 1.7, alpha = 0.25, data = cd28) +
  geom_line(size = 2.5, data = cd28 %>% filter(pais %in% names(cores)[cores != "gray"])) +
  geom_point(size = 4, alpha = 0.25, data = cd28) +
  geom_point(size = 4, data = cd28 %>% filter(pais %in% names(cores)[cores != "gray"])) +
  geom_point(size = 1.75, color = "white", data = cd28) +
  geom_text(data = cd28_1, aes(label = pais), hjust = -0, size = 4) +
  geom_text(data = cd28_2, aes(label = pais), hjust = 1, size = 4) +
  scale_color_manual(values = cores) +
  ggtitle("Transplante de rim de dador cadáver (pmh)") +
  xlab("Ano") +
  scale_x_continuous(breaks = seq(min(cd28$ano),
                                  max(cd28$ano)),
                     limits = c(min(cd28$ano) - 2.0,
                                max(cd28$ano) + 2.0))

pcd<-pcd +  scale_y_continuous(trans = "reverse", breaks = seq(1,28)) + 
  theme(legend.position="none")
# guardar na lista
grafs$Cadaver<-pcd

## ler dados para mapa de transparencia
ue28<-read.csv2("ue28.csv")

shinyServer(function(input, output) {
  
  
  dados<-reactive({godt %>% filter(ano == input$num)})
  


  output$distPlot <- renderPlot({

    # juntar ficheiros de LD e CD
    todos<-inner_join(cd28,ld28,by=c("ano","pais"))
    todos00<-todos %>% mutate(total=ldtxtaxa+ cdtxtaxa) %>% filter(ano == input$num)
    
    # cálculo das médias por país
    medias00<-todos00 %>% group_by(pais) %>% 
      summarise(avgcd=mean(cdtxtaxa, na.rm = T), avgld=mean(ldtxtaxa, na.rm = T), avgtotal = mean(total, na.rm = T))
    medias00[2:4]<- round(medias00[2:4],2)
    medias00<-medias00 %>% mutate(total=avgcd+avgld)
    
    # transformar os dadops de modo a ter uma coluna com tipo de tx e outra com os nrs de Tx
    tmedias00<-medias00 %>% 
      gather(tipo, avg, 2:3)
    
    # mudar para PORTUGAL
    levels(tmedias00$pais)[match("Portugal",levels(tmedias00$pais))] <- "PORTUGAL"
    
    # gráfico pretendido
    graf00<-ggplot(tmedias00, aes(x=reorder(pais,total), y=avg, fill = tipo)) + 
      geom_bar(stat='identity') + 
      coord_flip() + 
      labs(x='',y='número de transplantes por milhão de habitantes') +
      scale_fill_manual(values = c("#3366ff","#33cc00"),
                        name="Tipo de transplante",
                        labels=c("Dador Cadáver", "Dador Vivo")) +
      theme_classic()  
    graf00
  })
  
  output$divPlot <- renderPlot({
    
    cd28a<-cd28 %>% filter(ano == input$num) %>% mutate(zcd = (cdtxtaxa - mean(cdtxtaxa))/sd(cdtxtaxa))
    cd28a <- cd28a[order(cd28a$zcd), ]  # sort
    cd28a$pais <- factor(cd28a$pais, levels = cd28a$pais)  # convert to factor to retain sorted order in plot.
    levels(cd28a$pais)[match("Portugal",levels(cd28a$pais))] <- "PORTUGAL"
    
    
    cddiv<-ggplot(data = cd28a, aes(x = pais, y = zcd, fill = zcd)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(low="red", mid="#e5eef0", high="#3366ff") +
      coord_flip() +
      labs(y="Under/over valuation in %", x="") +
      theme_bw() +
      theme(legend.position = "")
    
    
    ld28a<-ld28 %>% filter(ano == input$num) %>% mutate(zld = (ldtxtaxa - mean(ldtxtaxa))/sd(ldtxtaxa))
    ld28a <- ld28a[order(ld28a$zld), ]  # sort
    ld28a$pais <- factor(ld28a$pais, levels = ld28a$pais)  # convert to factor to retain sorted order in plot.
    levels(ld28a$pais)[match("Portugal",levels(ld28a$pais))] <- "PORTUGAL"
    
    
    lddiv<-ggplot(data = ld28a, aes(x = pais, y = zld, fill = zld)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(low="red", mid="#e5eef0", high="#33cc00") +
      coord_flip() +
      labs(y="Under/over valuation in %", x="") +
      theme_bw() +
      theme(legend.position = "")
    
    grid.arrange(lddiv,cddiv, ncol=2)
    
  })
  
  output$corrPlot <- renderPlotly({
    g<-ggplot(dados(),aes(TxDVpmh,TxDCpmh, label = pais)) +
      geom_point(aes( size = DCpmh, color = transparencia), alpha = 0.5) +
      geom_text(vjust = 0, nudge_y = 0.5, size = 3) +
      scale_x_continuous(breaks = seq(0,50, 5),
                         limits = c(-0.5,55)) +
      scale_y_continuous(breaks = seq(0,60,10),
                         limits = c(-0.5,65)) +
      labs(x="Transplantes com Dador Vivo (pmh)", y="Transplantes com Dador Cadáver (pmh)")
    
    g<-g + theme_minimal() + theme(legend.position = "none")
    ggplotly(g)
  })
  
  output$text1<-renderText({
    "Escolha o ano:"})
  
  output$tipoPlot <- renderPlot({
    
    grafs[input$tipo]

  })
  
  output$mapa <- renderGvis({
    gvisGeoChart(ue28, locationvar="country", 
                 colorvar="value",
                 options=list(projection="kavrayskiy-vii", 
                              region = 150,
                              colorAxis="{colors:['#3b444b', '#faf0e6']}",
                              datalessRegionColor	= "#ffffba",
                              backgroundColor = "#b3cde0"))
  })
  
 # output$text2<-renderText({
 #  "Escolha o tipo de dador:"})
    
})


#grafs[["Cadaver"]]
