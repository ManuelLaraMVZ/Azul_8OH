library("pacman")

p_load("dplyr",
       "vroom",
       "ggplot2",
       "tidyverse",
       "ggsci",
       "tidyverse",
       "scales")#Para comma

tiempos_azul <- vroom(file ="https://raw.githubusercontent.com/ManuelLaraMVZ/Azul_8OH/main/Resumen_para_grafica")
head(tiempos)

#orden de las facetas

tiempos2_azul <- tiempos_azul
tiempos2_azul$Hora <- factor(tiempos2_azul$Hora, 
                         levels = c("12h","3h", "24h","6h")) #Convertimos el caracter en factor par apoder ordenar
tiempos2_azul$Tratamientos<- factor(tiempos2_azul$Tratamientos, 
                             levels = c("Control", "DMSO", "62.5 然", "125.0 然"))
head(tiempos2_azul)



###################################################################
write.csv(x=tiempos2_azul,file = "TIEMPOS2.csv", row.names = F)
#Esa tabla construida se le incorporan los resultados de los ANOVAS y se sube a GitHUB

#Cargamos los nuevos valores

tiempos3 <-vroom(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/Azul_8OH/main/Resumen_datos_anova")

head (tiempos3)

tiempos3$Hora <- factor(tiempos3$Hora, 
                         levels = c("3h","12h", "6h","24h")) #Convertimos el caracter en factor par apoder ordenar
tiempos3$Tratamientos<- factor(tiempos3$Tratamientos, 
                             levels = c("Control", "DMSO",  "62.5 然", "125.0 然"))

head(tiempos3)


###################################################################
graft <- tiempos3%>%
  ggplot(mapping = aes(x=Tratamientos,
                       y=mean,
                       fill=Tratamientos))+
  geom_bar(stat = "identity", colour="black", size=.8)+
  theme_classic()+
  facet_wrap(~Hora, dir = "v")+
  scale_fill_d3()+
  theme_bw()

graft

miny=0
maxy=90000

marcasy <- seq(from=miny,
               to=maxy,
               by=20000)


graft2 <- graft+
  scale_y_continuous(limits=c(miny,maxy), #colocamos los l璥ites del eje y
                     breaks=marcasy,
                     expand=c(0,0),
                     labels = comma)+
  theme(legend.position="right")+ #empezamos a meter lo que le pusimos
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())

graft2

graft3 <- graft2+
  geom_errorbar( aes(ymin=mean-sd, 
                     ymax=mean+sd), 
                 width=0.2, colour="black", alpha=1, size=.8)+
  xlab("8-HQ Concentration")+
  ylab("Viability by Trypan blue \n (Number of cells)")

graft3

graft4 <- graft3+
  theme(axis.line = element_line(size = 0.8))+
  theme(axis.ticks.x = element_line(size = 0.8))+
  theme(axis.ticks.y = element_line(size = 0.8))+
  theme(legend.title= element_blank())
graft4

graft5 <- graft4+
  theme(strip.text.x = element_text(size = 16, color = "black", face = "bold"))+  #https://www.datanovia.com/en/blog/how-to-change-ggplot-facet-labels/
  theme(strip.background = element_rect(color="black", fill="gainsboro", size=1.2, linetype="solid"))+
  theme(legend.text = element_text(size = 16, face = "bold"))+
  theme(axis.title.x = element_text(size=16, face="bold"))+
  theme(axis.title.y = element_text(size=16, face="bold"))+
  theme(axis.text.y = element_text(size=16, face = "bold"))+
  theme(axis.ticks.x.bottom = element_blank())

graft5

#se agrega estad疄tica

graft6 <- graft5+
  geom_text(aes(label=ANOVA),
            nudge_x=0.25,     #REspecto al eje x que tanto cambia
            nudge_y = 5000,      #respecto al eje y que tanto cambia
            size=6,
            face="bold")+
  scale_fill_simpsons()+
  
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())
graft6

ggsave(filename = "Concentraciones_Azulcolor.png",
       plot = graft6,
       dpi = 600,
       height = 7,
       width = 7)

#escala grises
graft7 <- graft6+
  scale_fill_grey()
graft7

ggsave(filename = "Concentraciones_Azulgris.png",
       plot = graft7,
       dpi = 600,
       height = 7,
       width = 7)
