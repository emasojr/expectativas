library(rbcb)
library(dplyr)
library(ggplot2)
library(xts)
library(vars)
library(cowplot)

#PARÂMETROS
V1 = 2 #Anos de análise
W1 = 365*V1
di=Sys.Date()-W1
din=as.Date("01/01/2021",format="%d/%m/%Y")

#Coletando dados de Expectaticas
PIB=get_annual_market_expectations("PIB Total", start_date = di, as="data.frame")
PIB$date<-as.Date(PIB$date)
ANO=paste0("20",format(PIB$date[1], format="%y"))
DATE<-PIB$date[PIB$reference_year==ANO & PIB$date>=din]
PIB<-PIB$median[PIB$reference_year==ANO & PIB$date>=din]
PIB<-data.frame(date=DATE,PIB=PIB)

IPCAE=get_annual_market_expectations("IPCA", start_date = di, as="data.frame")
IPCAE$date<-as.Date(IPCAE$date)
DATE<-IPCAE$date[IPCAE$reference_year==ANO & IPCAE$date>=din & IPCAE$base==0]
IPCA<-IPCAE$median[IPCAE$reference_year==ANO & IPCAE$date>=din & IPCAE$base==0]
IPCA<-data.frame(date=DATE,IPCA=IPCA)


SELICE=get_annual_market_expectations("Meta para taxa over-selic", start_date = di, as="data.frame")
SELICE$date<-as.Date(SELICE$date)
DATE<-SELICE$date[SELICE$reference_year==ANO & SELICE$indic_detail=="Fim do ano" & SELICE$date>=din]
SELICE<-SELICE$median[SELICE$reference_year==ANO & SELICE$indic_detail=="Fim do ano" & SELICE$date>=din]
SELIC<-data.frame(date=DATE,SELIC=SELICE)

CAMBEX=get_annual_market_expectations("Taxa de câmbio", start_date = di, as="data.frame")
CAMBEX$date<-as.Date(CAMBEX$date)
DATE=CAMBEX$date[CAMBEX$reference_year==ANO & CAMBEX$indic_detail=="Fim do ano" & CAMBEX$date>=din]
CAMBEX=CAMBEX$median[CAMBEX$reference_year==ANO & CAMBEX$indic_detail=="Fim do ano" & CAMBEX$date>=din]
CAMBEX<-data.frame(date=DATE,CAMBEX=CAMBEX)

expectations=merge(merge(merge(PIB,CAMBEX),IPCA),SELIC)

#Gráficos
pib=ggplot(expectations) +
  aes(x = date, y = PIB) +
  geom_line(size = 1.6, colour = "#638F8C") +
  labs(x = "Data", y = "Taxa de crescimento", title = "Expectativa do Crescimento do PIB", subtitle = "Dados do Relatório Focus - BACEN", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

camb=ggplot(expectations) +
  aes(x = date, y = CAMBEX) +
  geom_line(size = 1.6, colour = "#8FCFCA") +
  labs(x = "Data", y = "Taxa de Câmbio", title = "Expectativa da Taxa de Câmbio", subtitle = "Dados do Relatório Focus - BACEN", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

ipca=ggplot(expectations) +
  aes(x = date, y = IPCA) +
  geom_line(size = 1.6, colour = "#37504E") +
  labs(x = "Data", y = "Inflação", title = "Expectativa da Inflação", subtitle = "Dados do Relatório Focus - BACEN", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

selic=ggplot(expectations) +
  aes(x = date, y = SELIC) +
  geom_line(size = 1.6, colour = "#7DB5B1") +
  labs(x = "Data", y = "Taxa Selic", title = "Expectativa da Taxa Selic", subtitle = "Dados do Relatório Focus - BACEN", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

plot_grid(selic,ipca,camb,pib)