# --------------------------------------------------- #
# ITESO  - Ingeniería Financiera                      #
# Código - Gráfica tipo Histograma                    #
# --------------------------------------------------- #

library  (ggplot2)
library  (gridExtra)
library  (plyr)
library  (moments)

color1 <- "white"
color2 <- "light gray"
color3 <- "#0066cc"
color4 <- "#333333"
color5 <- "steel blue"
color6 <- "turquoise"
color7 <- "#cc0000"
color8 <- "dark grey"
color9 <- "dark blue"
colorA <- "steel blue"
colorB <- "steel blue"

titulo  <- "Normalidad de Errores"
dfDatos <- data.frame(rnorm(1000,0,1))
mediaDatos  <- signif(mean(dfDatos[,1]),2)
dsDatos     <- signif(sd(dfDatos[,1]),2)
mediaDatos  <- signif(mean(dfDatos[,1]),2)
sesDatos    <- signif(skewness(dfDatos[,1]),2)
curtDatos   <- signif(kurtosis(dfDatos[,1]),2)

  
titulo   <- "Título de gráfica"
ejex     <- "Eje x"
ejey     <- "Eje y"
linea    <- "longdash"
tamEjex  <- 12
tamEjey  <- 12
tamEtiq  <- 5
anchoClase <- 0.25
tamTitulo  <- 14
tamTituloX <- 14
tamTituloY <- 14
anchoLinea <- .75
  
ggHistograma  <- ggplot(dfDatos,aes(dfDatos[,1])) + 
                 geom_histogram(aes(fill = ..count..,y = ..density..),
                 binwidth=0.25) + labs(title = titulo,y = ejey,x = ejex)
ggHistograma1 <- ggHistograma +  theme(panel.background = element_rect(fill = color1),
                 panel.grid.minor.y = element_line(size = .2, color = color2),
                 panel.grid.major.y = element_line(size = .2, color = color2),
                 panel.grid.minor.x = element_line(size = .2, color = color2),
                 panel.grid.major.x = element_line(size = .2, color = color2),
                 axis.text.x  = element_text(colour = color4,size = tamEjex, hjust =.5,vjust = 0),
                 axis.text.y  = element_text(colour = color4,size = tamEjey, hjust =.5,vjust = 0),
                 axis.title.x = element_text(colour = color4,size = tamTituloX,hjust =.5,vjust = 0),
                 axis.title.y = element_text(colour = color4,size = tamTituloY,hjust =.5,vjust = 1),
                 title=element_text(colour = colorB, size = tamTitulo, hjust = 1, vjust = 0.8),
                 panel.border = element_rect(linetype = 1, colour = color8, fill = NA))

ggHistograma2 <- ggHistograma1  + stat_function(fun=dnorm,args=list(mediaDatos, dsDatos),
                  geom = "area", alpha = 0.35, color = "grey", fill = "black")
    
ggInfoHist  <- ggplot_build(ggHistograma)$data[[1]]
yMax        <- (max(ggInfoHist$density))*1.1
  
ggHistograma3 <- ggHistograma2 +
geom_segment(aes(x=mediaDatos,xend=mediaDatos,yend=yMax,y=0),colour=color8,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=-6*dsDatos,xend=-6*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=+6*dsDatos,xend=+6*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=-5*dsDatos,xend=-5*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=+5*dsDatos,xend=+5*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=-4*dsDatos,xend=-4*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=+4*dsDatos,xend=+4*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=-3*dsDatos,xend=-3*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=+3*dsDatos,xend=+3*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=-2*dsDatos,xend=-2*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) +
geom_segment(aes(x=+2*dsDatos,xend=+2*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) + 
geom_segment(aes(x=-1*dsDatos,xend=-1*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea) + 
geom_segment(aes(x=+1*dsDatos,xend=+1*dsDatos,yend=yMax,y=0),colour=colorB,linetype=linea,size=anchoLinea)

ggHistograma4 <- ggHistograma3 +
annotate("text",x=mediaDatos,y=-.015,parse=TRUE,label="-mu",     colour=colorA,size=tamEtiq) +
annotate("text",x=-1*dsDatos,y=-.015,parse=TRUE,label="-sigma",  colour=colorA,size=tamEtiq) +
annotate("text",x=+1*dsDatos,y=-.015,parse=TRUE,label="sigma",   colour=colorA,size=tamEtiq) +
annotate("text",x=-2*dsDatos,y=-.015,parse=TRUE,label="-2*sigma",colour=colorA,size=tamEtiq) +
annotate("text",x=+2*dsDatos,y=-.015,parse=TRUE,label="2*sigma", colour=colorA,size=tamEtiq) +
annotate("text",x=-3*dsDatos,y=-.015,parse=TRUE,label="-3*sigma",colour=colorA,size=tamEtiq) +
annotate("text",x=+3*dsDatos,y=-.015,parse=TRUE,label="3*sigma", colour=colorA,size=tamEtiq) +
annotate("text",x=-4*dsDatos,y=-.015,parse=TRUE,label="-4*sigma",colour=colorA,size=tamEtiq) +
annotate("text",x=+4*dsDatos,y=-.015,parse=TRUE,label="4*sigma", colour=colorA,size=tamEtiq) +
annotate("text",x=-5*dsDatos,y=-.015,parse=TRUE,label="-5*sigma",colour=colorA,size=tamEtiq) +
annotate("text",x=+5*dsDatos,y=-.015,parse=TRUE,label="5*sigma", colour=colorA,size=tamEtiq) +
annotate("text",x=-6*dsDatos,y=-.015,parse=TRUE,label="-6*sigma",colour=colorA,size=tamEtiq) +
annotate("text",x=+6*dsDatos,y=-.015,parse=TRUE,label="6*sigma", colour=colorA,size=tamEtiq) +

scale_y_continuous(breaks=seq(0,yMax,round(yMax/11,2)))                +  
scale_x_continuous(breaks=round(seq(-6*dsDatos,6*dsDatos,dsDatos),2))  +
scale_fill_continuous(guide="none")

sesDatos  <- signif(skewness(dfDatos[,1]),2)
curtDatos <- signif(kurtosis(dfDatos[,1]),2)

Medidas  <- c("Media","DesvEst","Sesgo","Kurtosis")
Valor    <- c(mediaDatos, dsDatos, sesDatos, curtDatos)
df.est   <- data.frame(Medidas,Valor)

cajaMaxY <- yMax
cajaMinY <- signif((cajaMaxY*.60),2)
cajaMinX <- signif(3*dsDatos,2)
cajaMaxX <- Inf

ggHistograma5 <- ggHistograma4 + annotation_custom (grob = tableGrob((df.est[,1:2]),
show.rownames = FALSE), xmin = cajaMinX, xmax = cajaMaxX,ymin = cajaMinY, ymax = cajaMaxY)

ggHistograma5
