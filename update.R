install.packages("shinythemes")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("plotly")



install.packages('rsconnect')

rsconnect::setAccountInfo(name='bioestatisticas',
                          token='7ABE1E14AF786A7227F6F95837E44861',
                          secret='4DGi2kzm/a5i6ww9XiruPwzkNk0FVjTWNaPfV2Jm')

library(rsconnect)
rsconnect::deployApp("D:/CHN/OpenData/appUE28")

y

library(tidyverse)
