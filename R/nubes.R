library("tm")
library("SnowballC")
library("wordcloud2")
library("RColorBrewer")
# nube1 <- read.csv("data/nube1.csv")

palabras=c("ESTADÍSTICA","DESCRIPTIVA",
  "INERENCIA","VALIDEZ", "UNIDAD ANÁLISIS", "VARIABLE","VARIABLE DISCRETA",
  "VARIABLE CONTINUA","VARIABLE CUANTITATIVA",
  "VARIABLE CUALITATIVA","ESCALA NOMINAL",
  "ESCALA ORDINAL","ESCALA DE RAZÓN",
  "ESCALA DE INTERVALO","POBLACIÓN",
  "CENSO","PARÁMETRO",
  "MUESTRA","MUESTREO",
  "ESTIMADOR",  "MUESTREO POR CONVENIENCIA",
  "MUESTEO POR JUICIO",  "MUESTREO BOLA DE NIEVE",
  "MUESTREO ALEATORIO SIMPLE",  "MUESTREO ALEATORIO ESTRATIFICADO",
  "MUESTREO ALEATORIO POR CONGLOMERADO",
  "MUESTEO SISTEMÁTICO","VARIABLE DEPENDIENTE",
  "VARIABLE INDEPENDIENTE", "MEDIA", "MEDIANA","MODA","MEDIA TRUNCADA","MEDIA GEOMÉTRICA",
  "RANGO MEDIO", "RANGO", "DESVIACIÓN ESTANDAR", "VARIANZA",
  "COEFICIENTE DE VARIACIÓN", "COEFICIENTE DE ASIMETRIA", "SESGO",
  "CURTOSIS", "PERCENTIL", "DECIL", "CUARTIL", "QUINTIL", "MÁXIMO", 
  "MÍNIMO", "TABLAS DE FRECUENCIA", "FRECUENCIA ABSOLUTA", "FRECUENCIA RELATIVA",
  "FRECUENCIA ACUMULADA", "DIAGRAMA DE TALLOS Y HOJAS", "HISTOGRAMA", 
  "DIAGRAMA DE BARRAS", "DIAGRAMA DE TORTA", "DIAGRAMA DE DISPERSIÓN",
  "DIAGRAMA DE LÍNEAS", "R", "RStudio")
 #29

replicas=c(18, sample(5:15,59, replace = TRUE))# rep(5,59))
 
nube1=rep(palabras, replicas)

t=data.frame(table(nube1))

dword=t[,1]
dfreq=t[,2]
set.seed(1234)
# wordcloud(words = dword, freq = dfreq, min.freq = 1,
#            max.words=200, random.order=FALSE, rot.per=0.35, 
#            colors=brewer.pal(8, "Dark2"))

mycolor<-c("#F27F0C", "#F7AD19", "#053F5C", "#429EBD", "#034A94")
library(wordcloud2)
wordcloud2(data = t, size = 0.5, color=rep_len( c("#F27F0C", "#F7AD19", "#053F5C", "#429EBD", "#034A94"), nrow(demoFreq)),
           minRotation = 0, 
           maxRotation = 0, 
           shuffle = F) 



