library(WDI)
library(rCharts)
library(plyr)

countries <- c("AL", "AT", "BE", "BA", "BG", "HR", "CZ", "DK", "FI", "FR", "DE", "GR", 
               "HU", "IS", "IE", "IT", "NL", "NO", "PL", "PT", "RO", "RS", "SK", "SI", 
               "ES", "SE", "CH", "GB")

tfr <- WDI(country = countries, indicator = "SP.DYN.TFRT.IN", start = 1960, end = 2011)

#Clean up the data a bit
tfr <- rename(tfr, replace = c("SP.DYN.TFRT.IN" = "TFR"))

tfr$TFR <- round(tfr$TFR, 2)

# Create the chart
tfrPlot <- nPlot(
  TFR ~ year, 
  data = tfr, 
  group = "country",
  type = "lineChart")

# Add axis labels and format the tooltip
tfrPlot$yAxis(axisLabel = "Total fertility rate", width = 62)

tfrPlot$xAxis(axisLabel = "Year")

tfrPlot$chart(tooltipContent = "#! function(key, x, y){
        return '<h3>' + key + '</h3>' + 
        '<p>' + y + ' in ' + x + '</p>'
        } !#")


tfrPlot

candidatos = eleicoes[grep("Dilma Rousseff|Aécio Neves|Aecio Neves|Eduardo Campos|Marina Silva", eleicoes$text, ignore.case=TRUE),]
dilma = eleicoes[grep("Dilma Rousseff", eleicoes$text, ignore.case=TRUE),]
aecio = eleicoes[grep("Aécio Neves|Aecio Neves", eleicoes$text, ignore.case=TRUE),]
eduardo = eleicoes[grep("Eduardo Campos", eleicoes$text, ignore.case=TRUE),]
marina = eleicoes[grep("Marina Silva", eleicoes$text, ignore.case=TRUE),]

datas = as.Date(candidatos$created_at)
strptime('Tue May 20 04:23:38 +0000 2014',format='%a %b %j %H:%M:%S %z %Y')
as.POSIXlt('Tue May 20 04:23:38 +0000 2014')

datas = strptime(candidatos$created_at, "%a %b %d %H:%M:%S %z %Y")
print(data, format='%a %b %j %H:%M:%S %z %Y')

# Create the chart
n <- nPlot(
  TFR ~ year, 
  data = tfr, 
  group = "country",
  type = "lineChart")
