#
# Elias Barba Moral
# 03/2017
# Partes de este trabajo han sido inspiradas por ejemplos de clase de la asignatura
#
###################################################################################
####                Tarea Final: Algoritmos Bioinspirados                      ####
###################################################################################
#                                                                                 #
#   Esta tarea se propone resolver el problema de la pisifactoria. En una         #
#   pisifactoria se quiere dividir a un tipo de pez en tres grupos diferentes.    #
#   Interesa que dichos grupos sean lo mas heterogéneos posible geneticamente     #
#   hablando, ya que los peces resultantes presentaran menos problemas derivados  #
#   de la falta de cruce genetico.                                                #
#   Tenemos los datos de 216 peces y las relaciones entre ellos. Hay que crear    #
#   tres lotes de 30 peces donde primeramente el maximo de relacion entre dos     #
#   peces cualesquiera del lote sea minimo, y como segundo objetivo que la media  #
#   de los lotes sea minima.                                                      #
#                                                                                 #
###################################################################################
##
## Este proyecto no utiliza ninguna funcion especial que requiera la instalacion
## de ninguna libreria en R.
## El modo de obtencion de los resultados es:
##  1º: Seleccion de todo el codigo y cargalo al entorno de R (Excepcion en los datos!)
##  2º: Uso de la funcion selecrepro()
##

## Carga de los datos: 
## (Sustituir el directorio presente por el directorio en el que se encuentren los datos en su ordenador)

relatedness <- read.delim2("C:/Users/Elias/Desktop/Zaragoza/ABI/CC/Tarea/relatedness.csv", header=FALSE)

## Creacion inicial de tres lotes aleatorios para dar comienzo al algoritmo

# Seleccion de 90 individuos aleatorios
creacionloteinicial<-sample(2:length(relatedness),90)

# Numero de lotes en los que vamos a dividir los peces 
nLotes<-3 

# Inicializacion de los tres lotes
lote<-0
lote[1]<-list(creacionloteinicial[1:30])
lote[2]<-list(creacionloteinicial[31:60])
lote[3]<-list(creacionloteinicial[61:90])

# Los peces que no han sido incluidos en ningun lote formaran una "pool" de peces sin lote
pooluntakenfish<-c(2:length(relatedness))
pooluntakenfish<-setdiff(pooluntakenfish,lote[[1]])
pooluntakenfish<-setdiff(pooluntakenfish,lote[[2]])
pooluntakenfish<-setdiff(pooluntakenfish,lote[[3]])

## Constantes e inicializacion de variables
# Numero de iteraciones de la funcion principal
nIter=1000

# Inicializacion de las variables que van a guardar la media y el máximo minimos de los 
# diferentes lotes creados
mediaminimal<-0
maximminimal<-1
mediaminimal

# Inicializacion de la configuracion que minimiza el máximo de los tres lotes
lotemaximinimo<-list()
bestlote<-list()

###################################################################################
####                             Funcion principal                             ####
###################################################################################

selecrepro<-function(){
  ## contenedores
  maxim1<-maxim2<-maxim3<-maximminima<-mediaminima<-media1 <- media2 <- media3 <-mediatotal<- c()
  for (j in 1:nIter){
    
    ##Seleccion de padres entre los dos peores lotes
    ##Cada una de las funciones comentadas son funciones previas descritas en su definicion.
    
    #lote<-reproduccion(lote)
    lote<-reproduccionmejor(lote)
    
    ##Mutacion de los lotes, quitando los que mas aportan al lote. 
    
    #mutlote<-mutacionsimple(lote,j,pooluntakenfish)
    #mutlote<-mutacionsimplemejor(lote,j,pooluntakenfish)
    #mutlote<-mutacionsimplelotemedia(lote,j,pooluntakenfish)
    mutlote<-mutacionsimplelotemax(lote,j,pooluntakenfish)
    
    ##Actualizacion de los lotes de peces y de los peces que quedan fuera
    lote<-mutlote[[1]]
    pooluntakenfish<-mutlote[[2]]
    
    ##cat(j, "Máximo lote 1: ",evaluacionmaximolote(lote[[1]])[[1]],"  Máximo lote 2: ",evaluacionmaximolote(lote[[2]])[[1]],"  Máximo lote 3: ",evaluacionmaximolote(lote[[3]])[[1]],"\n")
    ##cat("   Media lote 1: ",evaluacionmedialote(lote[[1]]),"  Media lote 2: ",evaluacionmedialote(lote[[2]]),"  Media lote 3: ",evaluacionmedialote(lote[[3]]),"\n")
    
    ## Dibujo
    media1  <- c (media1,  evaluacionmedialote(lote[[1]]))
    media2  <- c (media2,  evaluacionmedialote(lote[[2]]))
    media3  <- c (media3,  evaluacionmedialote(lote[[3]]))
    maxim1 <-c (maxim1,  evaluacionmaximolote(lote[[1]])[[1]])
    maxim2 <-c (maxim2,  evaluacionmaximolote(lote[[2]])[[1]])
    maxim3 <-c (maxim3,  evaluacionmaximolote(lote[[3]])[[1]])
    
    minmin <- min (media1,media2,media3)
    maxmax <- max (maxim1,maxim2,maxim3)
    
    plot  (media1,  col = 2,lty="longdash", type="l",ylim = c (minmin, maxmax))
    lines(media2,lty="dashed",type="l",col=2)
    lines(media3,type="l",col=2)
    lines(maxim1,type = "l",col = 3,lty="longdash")
    lines(maxim2,lty="dashed",type="l",col=3)
    lines(maxim3,type="l",col=3)
    
    ##Calculo de la minima relacion maxima entre dos peces para dibujar
    vmaxim<-c(evaluacionmaximolote(lote[[1]])[[1]],evaluacionmaximolote(lote[[2]])[[1]],evaluacionmaximolote(lote[[3]])[[1]])
    if(mean(vmaxim)<maximminimal){
      lotemaximinimo<-lote
      maximminimal<-mean(vmaxim)
    }
    maximminima<-c(maximminima,maximminimal)
    lines(maximminima,col=4,type = "l",lty="solid")
    
    ##Calculo de la media del lote y dibujo del mismo
    vmedia<-c(evaluacionmedialote(lote[[1]]),evaluacionmedialote(lote[[2]]),evaluacionmedialote(lote[[3]]))
    #if(mean(vmedia)<mediaminimal){
      mediaminimal<-mean(vmedia)
    #}
    mediaminima<-c(mediaminima,mediaminimal)
    lines(mediaminima,col=1,type = "l",lty="solid")
    
    ##Guardado del lote que con el mismo máximo de relacion tiene una media menor
    if(j>2){
      if(maximminima[j]==maximminima[j-1]){
        if(mediaminima[j]>mediaminima[j-1]){
          bestlote<-bestlote
        }else{
          bestlote<-lote
        }
      }else{
        bestlote<-lote
      }
    }
  }
  
  ##Devolucion de los resultados despues de las iteraciones
  cat("Resultados finales:\n")
  cat("\n")
  for (i in 1:nLotes){
    cat("Maximo del lote ",i," : ",evaluacionmaximolote(bestlote[[i]])[[1]],"\n")
  }
  mediamaximolote<-c(evaluacionmaximolote(bestlote[[1]])[[1]],evaluacionmaximolote(bestlote[[2]])[[1]],evaluacionmaximolote(bestlote[[3]])[[1]])
  cat("Media del maximo de los tres lotes: ",mean(mediamaximolote),"\n")
  cat("\n")
  mediamedialote<-c(evaluacionmedialote(bestlote[[1]]),evaluacionmedialote(bestlote[[2]]),evaluacionmedialote(bestlote[[3]]))
  cat("Media de la media de los tres lotes: ",mean(mediamedialote),"\n")
  cat("\n")
  cat("Peces que forman los lotes:")
  for (i in 1:nLotes){
    cat("\n  El lote",i, "lo componen:\n")
    for (j in 1:length(lote[[i]])){
      cat(" ",lote[[i]][j])
    }
  }
}

###################################################################################
####                             Parte genetica                                ####
###################################################################################

#### Mutacion simple
#La mutacion simple busca la maxima relacion entre dos peces dentro del lote y 
#sustituye ambos peces por otros que no esten presentes en ningun otro lote.

mutacionsimple<-function(lote,j,fpooluntakenfish){
  #cat(j, "Pool of untaken fish: ")
  #for (i in 1:length(fpooluntakenfish))
   #cat(fpooluntakenfish[i]," ")
  #cat("\n")
  
  newfish<-sample(fpooluntakenfish,6)
  
  #cat("Newfish: ")
  #for (i in 1:length(newfish))
   #cat(newfish[i]," ")
  #cat("\n")
  b<-0
  for(i in 1:nLotes){
    max1<-evaluacionmaximolote(lote[[i]])[[2]]
    max2<-evaluacionmaximolote(lote[[i]])[[3]]
    #cat("Miembros del lote ",i," a ser añadidos a la pool: ",lote[[i]][max1]," ",lote[[i]][max2],"\n")
    fpooluntakenfish<-c(fpooluntakenfish,lote[[i]][max1],lote[[i]][max2])
    lote[[i]][max1]<-newfish[i+b]
    lote[[i]][max2]<-newfish[i+b+1]
    fpooluntakenfish<-setdiff(fpooluntakenfish,newfish[i+b])
    fpooluntakenfish<-setdiff(fpooluntakenfish,newfish[i+b+1])
    b=b+1
  }
  mutreturn<-list(lote,fpooluntakenfish)
  return(mutreturn)
}

#### Mutacion simple mejorada
#Similar al anterior, pero solo muta si la mutacion produce un maximo menor al
#que estamos tratando de sustituir

mutacionsimplemejor<-function(lote,j,fpooluntakenfish){
  #cat(j, "Pool of untaken fish: ")
  #for (i in 1:length(fpooluntakenfish))
   # cat(fpooluntakenfish[i]," ")
  #cat("\n")
  
  newfish<-sample(fpooluntakenfish,6)
  
  #cat("Newfish: ")
  #for (i in 1:length(newfish))
   # cat(newfish[i]," ")
  #cat("\n")
  b<-0
  for(i in 1:nLotes){
    max1<-evaluacionmaximolote(lote[[i]])[[2]]
    max2<-evaluacionmaximolote(lote[[i]])[[3]]
    #cat("Miembros del lote ",i," a ser añadidos a la pool: ",lote[[i]][max1]," ",lote[[i]][max2],"\n")
    dummielote<-lote[[i]]
    dummielote[max1]<-newfish[i+b]
    dummielote[max2]<-newfish[i+b+1]
    #cat("Relatedness no mutados: ",relatedness[lote[[i]][max1],lote[[i]][max2]]," Relatedness mutados: ",relatedness[dummielote[max1],dummielote[max2]],"\n")
    if(relatedness[dummielote[max1],dummielote[max2]]<relatedness[lote[[i]][max1],lote[[i]][max2]]){
      #cat("Mutacion en el lote ",i,"\n")
      fpooluntakenfish<-c(fpooluntakenfish,lote[[i]][max1],lote[[i]][max2])
      lote[[i]][max1]<-newfish[i+b]
      lote[[i]][max2]<-newfish[i+b+1]
      fpooluntakenfish<-setdiff(fpooluntakenfish,newfish[i+b])
      fpooluntakenfish<-setdiff(fpooluntakenfish,newfish[i+b+1]) 
    }
    b=b+1
  }
  mutreturn<-list(lote,fpooluntakenfish)
  return(mutreturn)
}

#### Mutacion simple mejorando la media del lote
#Similar al anterior, pero solo muta si la mutacion produce que la media del lote
#sea mas pequeño (no solo la pareja que sustituimos)

mutacionsimplelotemedia<-function(lote,j,fpooluntakenfish){
  #cat(j, "Pool of untaken fish: ")
  #for (i in 1:length(fpooluntakenfish))
  # cat(fpooluntakenfish[i]," ")
  #cat("\n")
  
  newfish<-sample(fpooluntakenfish,6)
  
  #cat("Newfish: ")
  #for (i in 1:length(newfish))
  # cat(newfish[i]," ")
  #cat("\n")
  b<-0
  for(i in 1:nLotes){
    max1<-evaluacionmaximolote(lote[[i]])[[2]]
    max2<-evaluacionmaximolote(lote[[i]])[[3]]
    #cat("Miembros del lote ",i," a ser añadidos a la pool: ",lote[[i]][max1]," ",lote[[i]][max2],"\n")
    dummielote<-lote[[i]]
    dummielote[max1]<-newfish[i+b]
    dummielote[max2]<-newfish[i+b+1]
    #cat("Relatedness no mutados: ",relatedness[lote[[i]][max1],lote[[i]][max2]]," Relatedness mutados: ",relatedness[dummielote[max1],dummielote[max2]],"\n")
    if(evaluacionmedialote(dummielote)<evaluacionmedialote(lote[[i]])){
      #cat("Mutacion en el lote ",i,"\n")
      fpooluntakenfish<-c(fpooluntakenfish,lote[[i]][max1],lote[[i]][max2])
      lote[[i]][max1]<-newfish[i+b]
      lote[[i]][max2]<-newfish[i+b+1]
      fpooluntakenfish<-setdiff(fpooluntakenfish,newfish[i+b])
      fpooluntakenfish<-setdiff(fpooluntakenfish,newfish[i+b+1]) 
    }
    b=b+1
  }
  mutreturn<-list(lote,fpooluntakenfish)
  return(mutreturn)
}

#### Mutacion simple mejorando el maximo del lote
#Similar al anterior, pero solo muta si la mutacion produce que el maximo del lote
#sea mas pequeño (no solo la pareja que sustituimos)

mutacionsimplelotemax<-function(lote,j,fpooluntakenfish){
  #cat(j, "Pool of untaken fish: ")
  #for (i in 1:length(fpooluntakenfish))
  # cat(fpooluntakenfish[i]," ")
  #cat("\n")
  
  newfish<-sample(fpooluntakenfish,6)
  
  #cat("Newfish: ")
  #for (i in 1:length(newfish))
  # cat(newfish[i]," ")
  #cat("\n")
  b<-0
  for(i in 1:nLotes){
    max1<-evaluacionmaximolote(lote[[i]])[[2]]
    max2<-evaluacionmaximolote(lote[[i]])[[3]]
    #cat("Miembros del lote ",i," a ser añadidos a la pool: ",lote[[i]][max1]," ",lote[[i]][max2],"\n")
    dummielote<-lote[[i]]
    dummielote[max1]<-newfish[i+b]
    dummielote[max2]<-newfish[i+b+1]
    #cat("Relatedness no mutados: ",relatedness[lote[[i]][max1],lote[[i]][max2]]," Relatedness mutados: ",relatedness[dummielote[max1],dummielote[max2]],"\n")
    if(evaluacionmaximolote(dummielote)[[1]]<evaluacionmaximolote(lote[[i]])[[1]]){
      #cat("Mutacion en el lote ",i,"\n")
      fpooluntakenfish<-c(fpooluntakenfish,lote[[i]][max1],lote[[i]][max2])
      lote[[i]][max1]<-newfish[i+b]
      lote[[i]][max2]<-newfish[i+b+1]
      fpooluntakenfish<-setdiff(fpooluntakenfish,newfish[i+b])
      fpooluntakenfish<-setdiff(fpooluntakenfish,newfish[i+b+1]) 
    }
    b=b+1
  }
  mutreturn<-list(lote,fpooluntakenfish)
  return(mutreturn)
}

#### Reproduccion/Cruce de padres
#La reproduccion/Cruce de padres elije los dos lotes con peor media, y eligiendo 
#un punto dentro de un lote cambia a los peces de lote partir de dicho punto.

reproduccion<-function(lote){
  medialotes<-rep(0,3)
  padres<-c(1:3)
  loteminimo<-1
  for (i in 1:nLotes){
    ##cat("Lote: ",i,"\n")
    medialotes[i]<-evaluacionmedialote(lote[[i]])
    ##cat("Para seleccion de padres: \nMedia lote ",i,": ",evaluacionmedialote(lote[[i]]),"\n")
    if(medialotes[i]==min(medialotes)){
      loteminimo<-i
      ##cat(loteminimo,"\n")
    }
  }
  ##cat("Lote con la mínima media: ",loteminimo,"\n")
  padres<-padres[-loteminimo]
  ##cat("Lotes padres: ",padres[1]," ",padres[2],"\n")
  fishtochange<-sample(1:30,10)
  ##cat("Peces a cambiar: \n")
  ##for (i in 1:length(fishtochange))
  ##cat(fishtochange[i]," ")
  ##cat("\n")
  newlote<-0
  newlote[1]<-list(0)
  newlote[2]<-list(0)
  for (i in 1:length(padres)){
    newlote[[padres[i]]]<-lote[[padres[i]]][-fishtochange]
  }
  newlote[[padres[1]]]<-c(newlote[[padres[1]]],lote[[padres[2]]][fishtochange])
  newlote[[padres[2]]]<-c(newlote[[padres[2]]],lote[[padres[1]]][fishtochange])
  
  lote[[padres[1]]]<-newlote[[padres[1]]]
  lote[[padres[2]]]<-newlote[[padres[2]]]
  return(lote)
}

#### Reproduccion/Cruce de padres mejorado
#Similar al anterior, pero solo se reproduce si la reproduccion genera dos lotes
#con maximos mas pequeños

reproduccionmejor<-function(lote){
  medialotes<-rep(0,3)
  padres<-c(1:3)
  loteminimo<-1
  for (i in 1:nLotes){
    ##cat("Lote: ",i,"\n")
    medialotes[i]<-evaluacionmedialote(lote[[i]])
    ##cat("Para seleccion de padres: \nMedia lote ",i,": ",evaluacionmedialote(lote[[i]]),"\n")
    if(medialotes[i]==min(medialotes)){
      loteminimo<-i
      ##cat(loteminimo,"\n")
    }
  }
  ##cat("Lote con la mínima media: ",loteminimo,"\n")
  padres<-padres[-loteminimo]
  ##cat("Lotes padres: ",padres[1]," ",padres[2],"\n")
  fishtochange<-sample(1:30,10)
  ##cat("Peces a cambiar: \n")
  ##for (i in 1:length(fishtochange))
  ##cat(fishtochange[i]," ")
  ##cat("\n")
  newlote<-0
  newlote[1]<-list(0)
  newlote[2]<-list(0)
  for (i in 1:length(padres)){
    newlote[[padres[i]]]<-lote[[padres[i]]][-fishtochange]
  }
  newlote[[padres[1]]]<-c(newlote[[padres[1]]],lote[[padres[2]]][fishtochange])
  newlote[[padres[2]]]<-c(newlote[[padres[2]]],lote[[padres[1]]][fishtochange])
  rmediamax<-c(evaluacionmaximolote(newlote[[padres[1]]])[[1]],evaluacionmaximolote(newlote[[padres[2]]])[[1]])
  mediamax<-c(evaluacionmaximolote(lote[[padres[1]]])[[1]],evaluacionmaximolote(lote[[padres[2]]])[[1]])
  #cat("Media maximo padres: ",mean(mediamax),"Media maximo hijos: ",mean(rmediamax),"\n")
  if(mean(rmediamax)<mean(mediamax)){
    #cat("Hijos creados \n")
    lote[[padres[1]]]<-newlote[[padres[1]]]
    lote[[padres[2]]]<-newlote[[padres[2]]]
  }
  return(lote)
}

###################################################################################
####                            Media y maximo del lote                        ####
###################################################################################

#### Busqueda del maximo del lote

evaluacionmaximolote<-function(lote){
  mejorrelatedness<-0
  columna<-0
  fila<-0
  for (i in 1:length(lote)){
    for(j in i:(length(lote))){
      if(i!=j){
#        cat("fila: ",lote[i],"columna: ",lote[j],"relatedness: ", relatedness[[lote[i],lote[j]]],"\n")
        if(relatedness[[lote[i],lote[j]]]>mejorrelatedness){
          mejorrelatedness<-relatedness[[lote[i],lote[j]]]
          fila<-i
          columna<-j
        }
      }
    }
  }
  result<-list(mejorrelatedness,fila,columna)
  return(result)
}


#### Evaluacion de la relacion media del lote

evaluacionmedialote<-function(lote){
  related<-NA
  for (i in 1:length(lote)){
    for(j in i:(length(lote))){
      if(i!=j){
#        cat("Fila: ",i," Columa: ",j,"\n")
        related<-append(related,relatedness[[lote[i],lote[j]]])
      }
    }
  }
  return(mean(related,na.rm=TRUE))
}


