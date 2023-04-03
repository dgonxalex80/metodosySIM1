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

data("CarreraLuz22")

CarreraLuz22$min= CarreraLuz22$timerun/60

# Categorias -------------------------------------------------------------------
CarreraLuz22$categoria=NA
CarreraLuz22$categoria[CarreraLuz22$edad<18]="1. Juvenil"
CarreraLuz22$categoria[CarreraLuz22$edad >= 18 & CarreraLuz22$edad < 40]="2. Abierta"
CarreraLuz22$categoria[CarreraLuz22$edad >= 40 & CarreraLuz22$edad < 49]="3. Veteranos A"
CarreraLuz22$categoria[CarreraLuz22$edad >= 50 & CarreraLuz22$edad < 59]="4. Veteranos B"
CarreraLuz22$categoria[CarreraLuz22$edad >= 60]="5. Veteranos C"


CarreraLuz22$sex[CarreraLuz22$sex == "M"]="Hombre"
CarreraLuz22$sex[CarreraLuz22$sex == "F"]="Mujer"
#-------------------------------------------------------------------------------
CarreraLuz22M = subset(CarreraLuz22, CarreraLuz22$sex=="Hombre")
CarreraLuz22F = subset(CarreraLuz22, CarreraLuz22$sex=="Mujer")

#-------------------------------------------------------------------------------
CarreraLuz22_c1M = subset(CarreraLuz22M, CarreraLuz22M$categoria=="1. Juvenil")
CarreraLuz22_c2M = subset(CarreraLuz22M, CarreraLuz22M$categoria=="2. Abierta")
CarreraLuz22_c3M = subset(CarreraLuz22M, CarreraLuz22M$categoria=="3. Veteranos A")
CarreraLuz22_c4M = subset(CarreraLuz22M, CarreraLuz22M$categoria=="4. Veteranos B")
CarreraLuz22_c5M = subset(CarreraLuz22M, CarreraLuz22M$categoria=="5. Veteranos C")


CarreraLuz22_c1F = subset(CarreraLuz22F, CarreraLuz22F$categoria=="1. Juvenil")
CarreraLuz22_c2F = subset(CarreraLuz22F, CarreraLuz22F$categoria=="2. Abierta")
CarreraLuz22_c3F = subset(CarreraLuz22F, CarreraLuz22F$categoria=="3. Veteranos A")
CarreraLuz22_c4F = subset(CarreraLuz22F, CarreraLuz22F$categoria=="4. Veteranos B")
CarreraLuz22_c5F = subset(CarreraLuz22F, CarreraLuz22F$categoria=="5. Veteranos C")



saveRDS(object=CarreraLuz22, file="RioCali.RSD")

#---------------------------------------------------------------------------
n=200
#------------------------------------------------------------------------------
paleta6=c("#447270", "#6B9493", "#F6E271", "#F6B916", "#F69312", "#BC6C25")
p1=ggplot(CarreraLuz22, aes(y=timerun/60, x=categoria))+
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
p2=ggplot(CarreraLuz22M, aes(y=timerun/60, x=categoria))+
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
data1=data.frame(carlos=sample(CarreraLuz22_c4M$timerun, 100), 
                 maria=sample(CarreraLuz22_c2F$timerun, 100))

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
valor=quantile(CarreraLuz22_c2M$timerun, deciles, na.rm = TRUE)/60 )

p4=ggplot(CarreraLuz22_c2M, aes(x=timerun/60, y=" ")) +
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
  valor=quantile(CarreraLuz22_c2M$timerun,  c(0.25,0.5,0.75),na.rm = TRUE)/60 )
q=cuartiles1$valor
p4=ggplot(CarreraLuz22_c2M, aes(x=timerun/60, y=" ")) +
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

aggregate(CarreraLuz22F$timerun/60, list(CarreraLuz22F$categoria), FUN=mean)
aggregate(CarreraLuz22M$timerun/60, list(CarreraLuz22M$categoria), FUN=mean)


etiquetas = c("Mujer", "Hombre")  # Change levels of group

ggplot(CarreraLuz22, aes(x = categoria, y = timerun )) +
  geom_boxplot(aes(fill = categoria)) + 
  facet_wrap(~sex, ncol=1, 
             labeller = labeller(dose = etiquetas))+
  labs(title = "",
       y= "tiempo - min",
       x= "categorías")+
  scale_fill_manual(values = c("#00070D", "#012447", "#034A94", "#0570E1", "#3998FB","#37B6FC"))


#-----------------------------------------------------------------------------

ggplot(CarreraLuz22F, aes(x = categoria, y = timerun/60 )) +
  geom_boxplot(aes(fill = categoria)) + 
  # facet_wrap(~sex, ncol=1, 
  #           labeller = labeller(dose = etiquetas))+
  labs(title = "",
       y= "tiempo - min",
       x= "categorías")+ Theme2
  #scale_fill_manual(values = c("#00070D", "#012447", "#034A94", "#0570E1", "#3998FB","#37B6FC"))

table(CarreraLuz22$categoria)

#-----------------------------------------------------------------------------
ggplot(CarreraLuz22_c1M, aes(y = " ", x = timerun/60 )) +
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

#-------------------------------------------------------------------------
library(ggplot2)
x=0:20
fx=dbinom(x,20,0.30)
dat=data.frame(x,fx)

ggplot(dat) + geom_point(aes(x, fx),colour = "orange", size = 4) +
  scale_x_continuous(limits = c(0, 20),
                     breaks = 0:20, 
                     labels = c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14', '15','16','17','18','19','20')


                     
w=sample(CarreraLuz22F3$timerun/60, 100) # simulación de los datos
shapiro.test(w)  # verificación de normalidad




xM1=sample(CarreraLuz22M$timerun/60, 100); shapiro.test(xM1)
xF1=sample(CarreraLuz22F$timerun/60, 100); shapiro.test(xF1)
var.test(xM1,xF1)
t.test(xM1, xF1)





library(paqueteMOD)
data(arboles)









