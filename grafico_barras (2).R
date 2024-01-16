#Abrimos la base de datos
df=read.csv("base.tecoma.csv", dec=",", sep=";", header=T)
str(df)

#Llamamos las librerías que vamos a necesitar
#no se olviden previamente de instalarlas

library(ggplot2)
library(Rmisc)
library(tidyr)

#Primero vamos a reestructurar la tabla
#vamos a convertirla de un formato "ancho" a uno "largo"

#nos aseguramos de que la columna repetición sea un factor
df$repeticion <- factor(df$repeticion)

#ahora tenemos que generar una columna
# para el tratamiento "edad de bosque"
#lo hacemos con la función gather del paquete tidyr
df1<- gather(df, bosque, diametro, 
                    joven:maduro, factor_key=TRUE)
str(df1)  
#En esta función lo que hacemos es indicar que queremos 
#crear dos nuevas columnas: bosque (tratamiento) y diámetro(variable)
#y joven:maduro, indicamos que son todas esas columnas las que uniremos 
#a bosque y a diámetro


#Ahora estamos listes para hacer el Gráfico de barras
  Error_bosque <- summarySE(df1, measurevar="diametro", groupvars="bosque")
  tgc4 <- df1
  tgc4$bosque <- factor(tgc4$bosque)
  
  #Gráfico sin particionar por tamaño de bolsa
  g1=ggplot(Error_bosque, aes(x=bosque, y=diametro, fill=bosque))+
    theme_classic() +geom_bar(stat="identity", width = 0.2, position=position_dodge())+
    ylab("Diámetro") +
    theme(legend.position="none",axis.line.y = element_line(color="black"),
          axis.title.y = element_text(margin = margin(r=8), face="bold"),
          axis.title.x = element_text(margin = margin(r=25), face="bold"))+ 
    scale_fill_manual(values=c('darkgray','red','blue' ))+
    geom_errorbar(aes(ymin=diametro-se, ymax=diametro+se),width=0.1)
  g1
 