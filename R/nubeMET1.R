library("tm")
library("SnowballC")
library("wordcloud2")
library("RColorBrewer")
# nube1 <- read.csv("data/nube1.csv")

palabras=c("ESTADÍSTICA",
  "DESCRIPTIVA",
  "INERENCIA",
  "VALIDEZ",
  "UNIDAD ANÁLISIS",
  "VARIABLE",
  "DISCRETA",
  "CONTINUA",
  "CUANTITATIVA",
  "CUALITATIVA",
  "NOMINAL",
  "ORDINAL",
  "DE RAZÓN",
  "DE INTERVALO",
  "POBLACIÓN",
  "CENSO",
  "PARÁMETRO",
  "MUESTRA",
  "MUESTREO",
  "ESTIMADOR",
  "MUESTREO POR CONVENIENCIA",
  "MUESTEO POR JUICIO",
  "MUESTREO BOLA DE NIEVE",
  "MUESTREO ALEATORIO SIMPLE",
  "MUESTREO ALEATORIO ESTRATIFICADO",
  "MUESTREO ALEATORIO POR CONGLOMERADO",
  "MUESTEO SISTEMÁTICO",
  "VARIABLE DEPENDIENTE",
  "VARIABLE INDEPENDIENTE")  #29

replicas=c(20, 8, 8, 8, 8, 8, 8, 8, 8, 4, 
           4,  4, 4, 4, 4, 4, 4, 4, 4, 6, 
           6, 6,  5, 5, 5, 4, 5, 6, 5 )

nube1=rep(palabras, replicas)

t=data.frame(table(nube1))

dword=t[,1]
dfreq=t[,2]
set.seed(1234)
wordcloud::wordcloud(words = dword, freq = dfreq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"))

c("#F27F0C", "#F7AD19", "#053F5C", "#429EBD", "#034A94")
library(wordcloud2)
wordcloud2(data = t, size = 0.5, color=rep_len( c("#F27F0C", "#F7AD19", "#053F5C", "#429EBD", "#034A94"), nrow(demoFreq) ) )

           