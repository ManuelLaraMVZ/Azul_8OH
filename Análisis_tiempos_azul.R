library("pacman")
p_load("vroom", #llama datos
       "ggplot2",#gráfica
       "dplyr", #magriter
       "agricolae", #para poder agrupar el Tukey
       "ggpubr") #gráficas simplificadas


###########################
#datos

crudos8OH <- vroom(file ="https://raw.githubusercontent.com/ManuelLaraMVZ/Azul_8OH/main/Azul%20por%20tiempos.csv")
head(crudos8OH)

names(crudos8OH)

crudos2 <- crudos8OH
crudos2$Hora <- factor(crudos2$Hora, 
                       levels = c("3h", "6h", "12h", "24h"))
crudos2$Tratamientos <- factor(crudos2$Tratamientos, 
                               levels = c("Control","DMSO", "62.5 µM", "125.0 µM"))
head(crudos2)
#obtenemos el resumen de la media y e.e.m.

#sacamos los datos reales

crudos3 <- crudos2 %>% 
  select(Hora, Tratamientos, No_cel)

crudos3

resumen_azul <- crudos3 %>% 
  group_by(Hora, Tratamientos) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd") %>% #obtenemos la tabla
  select(Hora, Tratamientos, n, mean, sd)
resumen_azul


#guardamos para la gráfica

write.csv(x=resumen_azul,
          file = "Resumen_para_gráfica.csv",
          row.names = F)

#####################################

#####################################
#Prueba ANOVA todos los datos

#visualizamos la distribución
caja1 <- crudos3 %>% 
  ggboxplot(x="Tratamientos",
            y="No_cel",
            color="Hora",
            palette = "jco")
caja1

#supuestos

supuesto1 <- crudos3 %>% 
  group_by(Hora, Tratamientos) %>% 
  identify_outliers(No_cel)  #buscamos outliers

supuesto1

modelo <- lm(No_cel~Hora*Tratamientos, data = crudos3)
ggqqplot(residuals(modelo))

#prueba de normalidad de Shapiro

shapiro.test(residuals(modelo))

#No pasa la prueba de normalidad con p<0.05

######################################################
#Se decide hacer análisis de los tiempos por separados

#primero hay que extraer los datos por separados 

######################################################

######################################################
# para 3h

crudo3h <- crudos3 %>% 
  filter(Hora=="3h") %>% 
  select(Tratamientos,No_cel)

crudo3h

resumen3h <- crudo3h %>% 
  group_by(Tratamientos) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumen3h

#gráfica
grafica3h <- resumen3h %>% 
  ggplot(mapping = aes(x=Tratamientos,
                       y=mean))+
  geom_col()

grafica3h

supuesto3h <- crudo3h %>% 
  group_by(Tratamientos) %>% 
  identify_outliers(No_cel)  #buscamos outliers

supuesto3h

modelo <- lm(No_cel~Tratamientos, data = crudo3h)

ggqqplot(residuals(modelo)) #graficamos para saber si se ajusta a un modelo normal

#realizamos prueba de normalidad si p>0.05, entonces la pasa

shapiro.test(crudo3h$No_cel)
ks.test(crudo3h$No_cel, pnorm)

# p=0.1269 (similar a SigmaPlot) Pasó la prueba de normalidad para Shapiro
# prueva de igualdad de varianzas
#Levene's Test for Homogeneity of Variance


levene_test(No_cel~Tratamientos, data = crudo3h)
fligner.test(No_cel~Tratamientos, data = crudo3h)
bartlett.test(No_cel~Tratamientos, data = crudo3h)

#Pasa prueba de normalidad para levene y fligner

#Podemos proceer con el ANOVa

#################################################
#ANOVA

ANOVA3h <- aov( No_cel~Tratamientos,data = crudo3h) #Ponemos de donde se sacan los datos y luego la relación

#Visualizamos
ANOVA3h
#obtenemos la tabla e anova
summary.aov(ANOVA3h)

#Existe diferencia entre los tratamientos

#Prueba estadística
posthoc3h <- (ANOVA3h)

posthoc3h

#obtenemos las similitudes

agrupados3h <- HSD.test(ANOVA3h,"Tratamientos", group=T, console=T,
                        main = "células con tratamiento por 3h con 8-OH")

agrupados3h

#Hay que tomar los datos y copiarlos en un txt para poder realizar el análisis

similitud <- c("a","a","b","c") #se construye con los resultados de las interacciones

resumen3h_ANOVA <- resumen3h %>% 
  select(Tratamientos, n, mean, sd) %>% 
  mutate(similitud)

resumen3h_ANOVA

write.csv(resumen3h_ANOVA,
          file = "ANOVA_3h.csv",
          row.names = F)

################################################################################
################################################################################
# para 6h

crudo6h <- crudos2 %>% 
  filter(Hora=="6h") %>% 
  select(Tratamientos,No_cel)

crudo6h

resumen6h <- crudo6h %>% 
  group_by(Tratamientos) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumen6h

#gráfica
grafica6h <- resumen6h %>% 
  ggplot(mapping = aes(x=Tratamientos,
                       y=mean))+
  geom_point()

grafica6h

supuesto6h <- crudo6h %>% 
  group_by(Tratamientos) %>% 
  identify_outliers(No_cel)  #buscamos outliers

supuesto6h

modelo <- lm(No_cel~Tratamientos, data = crudo6h)

ggqqplot(residuals(modelo)) #graficamos para saber si se ajusta a un modelo normal

#realizamos prueba de normalidad si p>0.05, entonces la pasa

shapiro.test(crudo6h$No_cel)
ks.test(crudo6h$No_cel, pnorm)

# p=0.1269 (similar a SigmaPlot) Pasó la prueba de normalidad para Shapiro
# prueva de igualdad de varianzas
#Levene's Test for Homogeneity of Variance


levene_test(No_cel~Tratamientos, data = crudo6h)
fligner.test(No_cel~Tratamientos, data = crudo6h)
bartlett.test(No_cel~Tratamientos, data = crudo6h)

#Pasa prueba de normalidad para levene y fligner

#Podemos proceer con el ANOVa

#################################################
#ANOVA

ANOVA6h <- aov( No_cel~Tratamientos,data = crudo6h) #Ponemos de donde se sacan los datos y luego la relación

#Visualizamos
ANOVA6h
#obtenemos la tabla e anova
summary.aov(ANOVA6h)

#Existe diferencia entre los tratamientos

#Prueba estadística
posthoc6h <- TukeyHSD(ANOVA6h)

posthoc6h

#obtenemos las similitudes

agrupados6h <- HSD.test(ANOVA6h,"Tratamientos", group=T, console=T,
                        main = "células con tratamiento por 6h con 8-OH")

agrupados6h

#Hay que tomar los datos y copiarlos en un txt para poder realizar el análisis

similitud <- c("a","a","b","b") #se construye con los resultados de las interacciones

resumen6h_ANOVA <- resumen6h %>% 
  select(Tratamientos, n, mean, sd) %>% 
  mutate(similitud)

resumen6h_ANOVA

write.csv(resumen6h_ANOVA,
          file = "ANOVA_6h.csv",
          row.names = F)
################################################################################
################################################################################
# para 12h

crudo12h <- crudos2 %>% 
  filter(Hora=="12h") %>% 
  select(Tratamientos,No_cel)

crudo12h

resumen12h <- crudo12h %>% 
  group_by(Tratamientos) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumen12h

#gráfica
grafica12h <- resumen12h %>% 
  ggplot(mapping = aes(x=Tratamientos,
                       y=mean))+
  geom_point()

grafica12h

supuesto12h <- crudo12h %>% 
  group_by(Tratamientos) %>% 
  identify_outliers(No_cel)  #buscamos outliers

supuesto12h

modelo <- lm(No_cel~Tratamientos, data = crudo12h)

ggqqplot(residuals(modelo)) #graficamos para saber si se ajusta a un modelo normal

#realizamos prueba de normalidad si p>0.05, entonces la pasa

shapiro.test(crudo12h$No_cel~crudo12h$Tratamientos)
ks.test(crudo12h$No_cel, pnorm)

# p=0.1269 (similar a SigmaPlot) Pasó la prueba de normalidad para Shapiro
# prueva de igualdad de varianzas
#Levene's Test for Homogeneity of Variance


levene_test(No_cel~Tratamientos, data = crudo12h)
fligner.test(No_cel~Tratamientos, data = crudo12h)
bartlett.test(No_cel~Tratamientos, data = crudo12h)

#Pasa prueba de normalidad para levene y fligner

#Podemos proceer con el ANOVa

#################################################
#ANOVA

ANOVA12h <- aov( No_cel~Tratamientos,data = crudo12h) #Ponemos de donde se sacan los datos y luego la relación

#Visualizamos
ANOVA12h
#obtenemos la tabla e anova
summary.aov(ANOVA12h)

#Existe diferencia entre los tratamientos

#Prueba estadística
posthoc12h <- TukeyHSD(ANOVA12h)

posthoc12h

#obtenemos las similitudes

agrupados12h <- HSD.test(ANOVA12h,"Tratamientos", group=T, console=T,
                         main = "células con tratamiento por 12h con 8-OH")

agrupados12h

#Hay que tomar los datos y copiarlos en un txt para poder realizar el análisis

similitud <- c("a","a","b","b") #se construye con los resultados de las interacciones

resumen12h_ANOVA <- resumen12h %>% 
  select(Tratamientos, n, mean, sd) %>% 
  mutate(similitud)

resumen12h_ANOVA

write.csv(resumen12h_ANOVA,
          file = "ANOVA_12h.csv",
          row.names = F)

##########################################

################################################################################
################################################################################
# para 24h

crudo24h <- crudos2 %>% 
  filter(Hora=="24h") %>% 
  select(Tratamientos,No_cel)

crudo24h

resumen24h <- crudo24h %>% 
  group_by(Tratamientos) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumen24h

#gráfica
grafica24h <- resumen24h %>% 
  ggplot(mapping = aes(x=Tratamientos,
                       y=mean))+
  geom_point()

grafica24h

supuesto24h <- crudo24h %>% 
  group_by(Tratamientos) %>% 
  identify_outliers(No_cel)  #buscamos outliers

supuesto24h

modelo <- lm(No_cel~Tratamientos, data = crudo24h)

ggqqplot(residuals(modelo)) #graficamos para saber si se ajusta a un modelo normal

#realizamos prueba de normalidad si p>0.05, entonces la pasa

shapiro.test(crudo24h$No_cel)
ks.test(crudo24h$No_cel, pnorm)

# p=0.1269 (similar a SigmaPlot) Pasó la prueba de normalidad para Shapiro
# prueva de igualdad de varianzas
#Levene's Test for Homogeneity of Variance


levene_test(No_cel~Tratamientos, data = crudo24h)
fligner.test(No_cel~Tratamientos, data = crudo24h)
bartlett.test(No_cel~Tratamientos, data = crudo24h)

#Pasa prueba de normalidad para levene y fligner

#Podemos proceer con el ANOVa

#################################################
#ANOVA

ANOVA24h <- aov( No_cel~Tratamientos,data = crudo24h) #Ponemos de donde se sacan los datos y luego la relación

#Visualizamos
ANOVA24h
#obtenemos la tabla e anova
summary.aov(ANOVA24h)

#Existe diferencia entre los tratamientos

#Prueba estadística
posthoc24h <- TukeyHSD(ANOVA24h)

posthoc24h

#obtenemos las similitudes

agrupados24h <- HSD.test(ANOVA24h,"Tratamientos", group=T, console=T,
                         main = "células con tratamiento por 24h con 8-OH")

agrupados24h

#Hay que tomar los datos y copiarlos en un txt para poder realizar el análisis

similitud <- c("a","b","c","d") #se construye con los resultados de las interacciones

resumen24h_ANOVA <- resumen24h %>% 
  select(Tratamientos, n, mean, sd) %>% 
  mutate(similitud)

resumen24h_ANOVA

write.csv(resumen24h_ANOVA,
          file = "ANOVA_24h.csv",
          row.names = F)
