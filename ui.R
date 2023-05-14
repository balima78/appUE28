
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(plotly)

shinyUI(fluidPage(
  # nome no browser
  headerPanel(title ="",
              windowTitle = "TxOR | Oficina de BioEstatistica"), 
  
  # Título da página com imagem OB
  titlePanel(a(href="http://bioestatisticas.wixsite.com/bioestatisticas", target = "_blank",
               img(src='ob.jpg', align = "right",height=60,width=150))),
  a(href="https://txor.netlify.app/", target = "_blank",
    h1("Transplants' Open Registry (TxOR)")),

  tags$head(includeScript("gtagUE28.js")),
  
  hr(),
  
  tagList(
    navbarPage(#theme = shinytheme("celurean"),
               title=div(img(src="openDoor1.jpg"), "TxOR"),
               tabPanel("Tx Renal", icon = icon("heartbeat"),
                        h3("Transplante Renal"),
                        p("Os resultados aqui apresentados foram publicados em:"),
                        a(href="http://repositorio.insa.pt/bitstream/10400.18/4864/1/Boletim_Epidemiologico_Observacoes_N19_2017_artigo3.pdf", 
                          target = "_blank",
                          h6("Bruno A Lima, Helena Alves. Atividade do transplante renal de 2003 a 2016: Portugal na União Europeia a 28. Observações - Boletim Epidemiológico, 6(19), 2017: 16-19")
                          ),
                        # Sidebar with a slider input for year
                        sidebarLayout(
                          sidebarPanel(
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            wellPanel(
                              
                              textOutput("text1"),
                              
                              sliderInput("num", "",
                                          min = 2003, max = 2021,
                                          value = 2009, step = 1, sep = "",
                                          animate = animationOptions(interval = 2500, loop = TRUE))),
    
      
      hr(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      hr(),
                            wellPanel(
                              # textOutput("text2"),
                              selectInput("tipo", "Escolha indicador:",
                                          choices = c("Nº Dadores Cadáver" = "Dadores",
                                                      "Tx com Dador Vivo" = "Vivo", 
                                                      "Tx com Dador Cadáver" = "Cadaver"))
                              )), 
      # Show a plot of the countries distribution
      mainPanel(
        h5("Taxa anual por milhão de habitantes"),
        plotOutput("distPlot"),
        br(),
        
        h5("Divergencia em relação à média anual (valores normalizados)"),
        plotOutput("divPlot"),
        br(),
        
        h5("Correlação entre os transplantes com DV e com DC"),
        plotlyOutput("corrPlot"),
        br(),
        
        h5("Evolução da ordem de cada país"),
        plotOutput("tipoPlot")
        )),
      hr(),
      print("Those informations are based on the Global Observatory on Donation and Transplantation (GODT) data, produced by the WHO-ONT collaboration.
            ")
      ), 
      tabPanel("Open data", icon = icon("database"),
               p("Dados – valores, registos, observações ou ocorrências em estado bruto. Quando apresentados numa folha de cálculo (com colunas e linhas identificadas) são designados de dados estruturados. Exemplos: a idade de um dador cadáver; o tempo de diálise de um candidato a transplante renal; o grupo sanguíneo de um doente transplantado."),
               p("Informação – mensagem gerada a partir de um conjunto de dados processados. Exemplos: média de idades dum grupo de dadores cadáver; mediana do tempo de diálise até ao transplante de um grupo de candidatos em lista de espera; gráfico de barras com as frequências relativas dos grupos sanguíneos dum conjunto de doentes transplantados."),
               p("Conhecimento – Disseminação da experiência obtida com a capacidade de processar um conjunto de informação. Exemplo: um artigo publicado numa revista científica."),
               p("Dados abertos – dados que possam ser acedidos, processados, reutilizados, modificados e partilhados por qualquer pessoa e para qualquer propósito sem qualquer tipo de restrições."),
               br(),
               h6("Bruno A Lima. A call for open data of renal transplantation in Portugal. Port J Nephrol Hypert 31(3), 2017: 155-157")),
      tabPanel("Transparency", icon = icon("map"),
               h5("Classificação dos 28 países da União Europeia de acordo com 
                  o nível de transparência de informação relativa à actividade 
                  de transplantação renal."),
               htmlOutput("mapa"),
               h6("Os resultados aqui apresentados foram publicados em:"),
               a(href="http://repositorio.insa.pt/handle/10400.18/5683", 
                 target = "_blank",
                 h6("Bruno A Lima, Helena Alves. Informação disponível sobre transplantação renal: Portugal na União Europeia a 28. Observações - Boletim Epidemiológico, 7(23), 2018: 67-71")
               )
      )
      )
    ),
  
  hr(),
  fluidRow(
    column(8, icon("copyright"), 
           a("2018, 'Oficina de BioEstatística' - All Rights Reserved |"),
           icon("envelope-o"), a(href="http://bioestatisticas.wixsite.com/bioestatisticas/contact","bioestatisticas@gmail.com")
    )),
  hr()  
  
))
