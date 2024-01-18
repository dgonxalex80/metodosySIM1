library(ggplot2)
# Tamaños de letras graficos con ggplot2
Theme1 = theme(
  axis.title.x = element_text(size = 26),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 26),
  axis.text.y = element_blank(),
  axis.text = element_text( size = 20),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20),  
  strip.text = element_text(size=20),
  title =element_text(size=20, face='bold')
)# tamaño letra por grupos



Theme2 = theme(
  axis.title.x = element_text(size = 26),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 26),
  # axis.text.y = element_blank(),
  axis.text = element_text( size = 20),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20),  
  strip.text = element_text(size=20),
  title =element_text(size=20, face='bold')
)# tamaño letra por grupos




library(readr)
library(tidyverse)
library(lubridate)
library(paqueteMETODOS)
data("CarreraLuz22")
apply(is.na(CarreraLuz22), 2, sum)   


RioCali10 <- RioCali10[!is.na(RioCali10$edad),]


RioCali10$min= RioCali10$timerun/60

# Categorias -------------------------------------------------------------------
RioCali10$categoria=NA
RioCali10$categoria[RioCali10$edad<17.9]="1. Juvenil"
RioCali10$categoria[RioCali10$edad >= 18 & RioCali10$edad < 39.9]="2. Abierta"
RioCali10$categoria[RioCali10$edad >= 40 & RioCali10$edad < 49.9]="3. Veteranos A"
RioCali10$categoria[RioCali10$edad >= 50 & RioCali10$edad < 59.9]="4. Veteranos B"
RioCali10$categoria[RioCali10$edad >= 60]="5. Veteranos C"

RioCali10$sex[RioCali10$sex == "M"]="Hombre"
RioCali10$sex[RioCali10$sex == "F"]="Mujer"
#-------------------------------------------------------------------------------
RioCali10M = subset(RioCali10, RioCali10$sex=="Hombre")
RioCali10F = subset(RioCali10, RioCali10$sex=="Mujer")

#-------------------------------------------------------------------------------
RioCali10_c1M = subset(RioCali10M, RioCali10M$categoria=="1. Juvenil")
RioCali10_c2M = subset(RioCali10M, RioCali10M$categoria=="2. Abierta")
RioCali10_c3M = subset(RioCali10M, RioCali10M$categoria=="3. Veteranos A")
RioCali10_c4M = subset(RioCali10M, RioCali10M$categoria=="4. Veteranos B")
RioCali10_c5M = subset(RioCali10M, RioCali10M$categoria=="5. Veteranos C")


RioCali10_c1F = subset(RioCali10F, RioCali10F$categoria=="1. Juvenil")
RioCali10_c2F = subset(RioCali10F, RioCali10F$categoria=="2. Abierta")
RioCali10_c3F = subset(RioCali10F, RioCali10F$categoria=="3. Veteranos A")
RioCali10_c4F = subset(RioCali10F, RioCali10F$categoria=="4. Veteranos B")
RioCali10_c5F = subset(RioCali10F, RioCali10F$categoria=="5. Veteranos C")

RioCali10_c2 = subset(RioCali10, RioCali10$categoria=="2. Abierta")

saveRDS(object=RioCali10, file="RioCali.RSD")

#---------------------------------------------------------------------------
n=200
#------------------------------------------------------------------------------
paleta6=c("#447270", "#6B9493", "#F6E271", "#F6B916", "#F69312", "#BC6C25")
p1=ggplot(RioCali10F, aes(y=timerun/60, x=categoria))+
  geom_jitter(color="#034A94", size=3, alpha=0.9) +
  aes(color=paleta6)+
  labs(title = "Mujeres",
       y= "tiempo - min",
       x= "categorías")+ 
  #facet_wrap(~ sex)
  #facet_grid(vars(sex), scale="free")+
  ylim(0,170)+
  Theme1

p1

#------------------------------------------------------------------------------
paleta6=c("#447270", "#6B9493", "#F6E271", "#F6B916", "#F69312", "#BC6C25")
p2=ggplot(RioCali10M, aes(y=timerun/60, x=categoria))+
  geom_jitter(color="#034A94", size=3, alpha=0.9) +
  aes(color=paleta6)+
  labs(title = "Hombres",
       y= "tiempo - min",
       x= "categorías")+ 
  #facet_wrap(~ sex)
  #facet_grid(vars(sex), scale="free")+
  ylim(0,170)+
  Theme1

p2


#-----------------------------------------------------------------------------
data1=data.frame(carlos=sample(RioCali10_c4M$timerun, 100), 
                 maria=sample(RioCali10_c2F$timerun, 100))

p2=ggplot(data1, aes(x=carlos/60, y=" "))+
  geom_jitter(color="#034A94", size=2, alpha=0.9) 
p2= p2 + geom_text(data = data1, x = 50.5, y = 0, label = "x",col="#034A94")
p2

#------------------------------------------------------------------------------
data2 <- data.frame(
  corredores=c(rep("Carlos",n), rep("Maria",n)),
  tiempo=c(data1$carlos/60,data1$maria/60)
)

p3=ggplot(data2, aes(x=tiempo, y=corredores, color = corredores)) +
  geom_jitter(size=5, alpha=1) +
  scale_color_manual(values=c("#FF7F00", "#034A94")) + Theme1
p3      
#-------------------------------------------------------------------------------
quantile(data1$carlos, 0.40)/60
quantile(data1$maria, 0.30)/60
#-------------------------------------------------------------------------------
deciles1=data.frame(
prob=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
valor=quantile(RioCali10_c2M$timerun, deciles, na.rm = TRUE)/60 )

p4=ggplot(RioCali10_c2M, aes(x=timerun/60, y=" ")) +
  geom_jitter(color="#034A94", size=2, alpha=0.9)+ 
  scale_color_manual(values="#034A94") +
  geom_vline(xintercept = deciles1$valor, colour= "#686868")+
  geom_point(data=deciles1, aes(x=valor), color="#FF7F00",size=3)+
   labs(title = " ",
       y= "",
       x= "minutos ")+ 
  Theme1
p4  
#-------------------------------------------------------------------------------


cuartiles1=data.frame(
  prob=c(0.25,0.5,0.75),
  valor=quantile(RioCali10_c2M$timerun,  c(0.25,0.5,0.75),na.rm = TRUE)/60 )
q=cuartiles1$valor
p4=ggplot(RioCali10_c2M, aes(x=timerun/60, y=" ")) +
  geom_jitter(color="#034A94", size=2, alpha=0.9)+ 
  scale_color_manual(values="#034A94") +
  #geom_vline(xintercept = cuartiles1$valor, colour= "#686868")+
  geom_point(data=cuartiles1, aes(x=valor), color="#FF7F00",size=3)+
  geom_boxplot(alpha = 0.4)+
  labs(title = " ",
       y= "",
       x= "minutos ")+ 
  geom_text(size=6,data = NULL, x = q[1], y = 0.5, label = "Q1",col="#FF7F00")+
  geom_text(size=6,data = NULL, x = q[2], y = 0.5, label = "Q2",col="#FF7F00") +
  geom_text(size=6,data = NULL, x = q[3], y = 0.5, label = "Q3",col="#FF7F00") +
  geom_text(size=6,data = NULL, x = 96, y = 0.5, label = "atípicos",col="#FF7F00") +  
  Theme1
p4  

#------------------------------------------------------------

aggregate(RioCali10F$timerun/60, list(RioCali10F$categoria), FUN=mean)
aggregate(RioCali10M$timerun/60, list(RioCali10M$categoria), FUN=mean)


etiquetas = c("Mujer", "Hombre")  # Change levels of group

ggplot(RioCali10, aes(x = categoria, y = timerun )) +
  geom_boxplot(aes(fill = categoria)) + 
  facet_wrap(~sex, ncol=1, 
             labeller = labeller(dose = etiquetas))+
  labs(title = "",
       y= "tiempo - min",
       x= "categorías")+
  scale_fill_manual(values = c("#00070D", "#012447", "#034A94", "#0570E1", "#3998FB","#37B6FC"))


#-----------------------------------------------------------------------------

ggplot(RioCali10F, aes(x = categoria, y = timerun/60 )) +
  geom_boxplot(aes(fill = categoria)) + 
  # facet_wrap(~sex, ncol=1, 
  #           labeller = labeller(dose = etiquetas))+
  labs(title = "",
       y= "tiempo - min",
       x= "categorías")+ Theme2
  #scale_fill_manual(values = c("#00070D", "#012447", "#034A94", "#0570E1", "#3998FB","#37B6FC"))

table(RioCali10$categoria)

#-----------------------------------------------------------------------------
ggplot(RioCali10_c1M, aes(y = " ", x = timerun/60 )) +
  geom_boxplot(fill = 4,           # Color caja
               alpha = 0.5,        # Transparencia
               color = 1,          # Color del borde
               outlier.colour = 2) +# Color atípicos) + 
    labs(title = "",
       x= "tiempo - min",
       y= " ")+ Theme1

#-------------------------------------------------------------------------------
n=200
#------------------------------------------------------------------------------

G1=c(27, 27, 28, 28, 34, 28, 26, 33, 24, 28, 25, 25, 33, 27, 34, 38, 24, 26, 22, 23, 33, 23, 26, 26, 32, 33, 29, 30, 25, 23 )
G2=c(35, 25, 19, 17, 24, 17, 55, 25, 31, 35, 43, 28, 32, 19, 20, 17, 25, 18, 21, 22, 17, 35, 29, 20, 54, 46, 24, 29, 40, 18 )

data5=data.frame(Edad=c(G1,G2),
                 Grupo = rep(c("Grupo 1", "Grupo 2"), each=30) )

p5=ggplot(data5, aes(x=Edad, y=Grupo, color=Grupo))+
  geom_jitter(size=4, alpha=0.9) +
  scale_fill_manual(values =c("Grupo 1"= "#E69F00", "Grupo 2"= "#56B4E9")) + Theme1
p5

#------------------------------------------------------------------------------

# grafico de curtosis
# Distribucion normal
library(ggfortify)
p=ggdistribution(dnorm, seq(-5, 5, 0.01), mean = 0, sd = 1,colour = 'blue')
p=ggdistribution(dnorm, seq(-5, 5, 0.01), mean = 0, sd = .7,colour = 'red', p=p)
p=ggdistribution(dnorm, seq(-5, 5, 0.01),   mean = 0, sd = 1.5,colour = 'orange', p=p)
p


# grafico de asimetria
p=ggdistribution(dnorm, seq(-15, 15, 0.01), mean = 0, sd = 2.05,colour = 'blue')
p=ggdistribution(dgamma, seq(0, 15, 0.1),shape=5,rate=1,colour = 'orange', p=p)
p


x = seq(-10,10,.01)
y1 = dnorm(x,mean = 0, sd=1)
#y2=dweibull(x, shape=2,scale=1)
y2 = dgamma(x,shape = 2, rate=1)
y3 = rev(y2)
y2[y2=0]=NA
data6=data.frame(x,y1,y2,y3)

ggplot(data6, aes(x=x)) +
       geom_line(aes(y=y2), color = "blue", size = 1.5)
       labs(x=" ", y=" ")

ggplot(data6, aes(x=x)) +
      geom_line(aes(y=y3), color = "red", size = 1.5) +
      labs(x=" ", y=" ")

ggplot(data6, aes(x=x)) +  
      geom_line(aes(y=y1), color = "orange", size = 1.5) +
      labs(x=" ", y=" ")


# -----------------------------------------------------------------------------
# tabla de frecuencia variable cuantitativa

library(agricolae)
h2=with(RioCali10_c2M,graph.freq(timerun/60,plot=FALSE, breaks = c(30,45,60,75,90,105,120)))
t2=table.freq(h2)
colnames(t2) = c("  LI  ", "  LS  ", "marca clase'", "Frec.Abs","Frec.Rel", "Frec.Abs.Ac","Frec.Rel.Ac")
t2

#-----------------------------------------------------------------------------
# grafica de torta
a=rep("Cultutal", 360145)
b=rep("Pastoral", 61496)
c=rep("Bienestar",49912)
d=rep("Deportivo",11777)
data=c(a,b,c,d)
t=table(data)

data10=data.frame(valor=c(360145,  61496, 49912, 11777),
                  porc=c(0.745, 0.127, 0.103,  0.025),
                  Medio_Universitario=c("Cultutal", "Pastoral", "Bienestar", "Deportivo"))

library(ggplot2)

ggplot(data10, aes(x = "", y = porc, fill =  Medio_Universitario)) +
  geom_col() +
  coord_polar(theta = "y")+
  geom_col(color = "black")+
  theme_void()+
  scale_fill_brewer() +
  #geom_text(aes(label = paste0(porc, "%")), position = position_stack(vjust=0.5)) 
  geom_text(aes(x = 1.6, label = scales::percent(porc, accuracy = .1)), position = position_stack(vjust = .5), size = 5) 

#-----------------------------------------------------------------------------
# grafica de barras

Evaluacion = data.frame( calificacion =c("1. Muy regular", "2. Regular", "3. Bueno", "4. Muy bueno","5. Excelente"),
                         frecuencia=c(12.4, 21.2, 21.5, 23.5, 21.5) )

ggplot(Evaluacion, aes(x=calificacion, y=frecuencia, fill=calificacion)) + 
  scale_fill_manual(values=c("#ff5b00","#dcf600","#7dbd00","#659cef","#2956b2") )+
  geom_bar(stat="identity")+
  geom_text(aes(label=frecuencia), vjust=-0.3, size=6) + 
  labs(title = "", y= "Frecuencia (%)", x= "Calificación") +
  Theme1
  
#----------------------------------------------------------------------------
# grafica de barras

# library
library(ggplot2)

ggplot(RioCali10, aes(x = categoria, fill = sex)) +
  geom_bar() +
  scale_fill_manual(values = c("#EFC000FF","#0073C2FF"))+
labs(title = "", y= "Frecuencia", x= "Categorías     Sexo") +
  Theme1

#------------------------------------------------------------------------------
# histograma
ggplot(RioCali10_c2, aes(x=timerun/60)) + 
  geom_histogram(bins=20, color="white", fill="blue", binwidth=5) +
  facet_grid(sex~.)+
  labs(title = "", y= "Frecuencia", x= "Tiempo (min)") +
  Theme2
#------------------------------------------------------------------------------
# diagrama de densidad
ggplot(RioCali10, aes(x=timerun/60)) + 
  geom_density(color="blue", size = 2)+
  labs(title = "", y= "Frecuencia", x= "Tiempo (min)") +
  scale_x_continuous(name = " ",
                     breaks = seq(30, 170, 10),
                     limits=c(30, 170))+
  Theme2
#-------------------------------------------------------------------------------  
# Diagrama de cajas



#------------------------------------------------------------------------------
# Diagrama de dispersion
ggplot(RioCali10, aes(x = timerun/60, y = edad, color = categoria)) +
  geom_point()+
  labs(title = "", y= "Edad", x= " ") +
  scale_x_continuous(name = "Tiempo (min)    Categorías",
                     breaks = seq(30, 170, 10),
                     limits=c(30, 170))+
  Theme2

#---------------------------------------------------------------------------
# Graph the data
plot(AirPassengers, main="Numero de pasajeros por mes", col="#034A94", lwd = 4, las=1, 
     ylab="número de pasajeros", xlab="año")

ggplot(AirPassengers) +
  geom_line(aes(x = AirPassengers, y = circumference), color = 'red')


data(AirPassengers)
View(AirPassengers)
#----------------------------------------------------------------------
library(tidyverse)
library(readr)
library(readr)
inflacion <- read_csv("~/Documentos/Javeriana/Ms Ciencia de Datos/recursos/data/inflacion202305.csv")
inflacion$t<- seq(as.Date("1993-02-01"),length=365,by="months")-1
inflacion$inflacion=ts(inflacion$inflacion)

ggplot(inflacion) +
        geom_line(aes(x = t, y = inflacion), color = 'red', size = 1)+ 
        scale_x_date(date_labels = "%m-%Y")+
        labs(title = "", y= "Variación anual (%)", x= "meses ") +  
        Theme2 

