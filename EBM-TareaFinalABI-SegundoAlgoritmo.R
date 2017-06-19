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
#   Interesa que dichos grupos sean lo mas heterogéneos posible geneticamente    #
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
##  1?: Seleccion de todo el codigo y cargalo al entorno de R (Excepcion en los datos!)
##  2?: Uso de la funcion genselecrepro()
##
###################################################################################
####                    Segundo Algoritmo Genetico                             ####
####                                                                           ####         
#### Este algoritmo no estudia un solo individuo, sino que intenta explorar    ####
#### varias soluciones a la misma vez.                                         ####
####                                                                           ####      
###################################################################################


## Carga de los datos: 
## (Sustituir el directorio presente por el directorio en el que se encuentren los datos en su ordenador)
relatedness <- read.delim2("C:/Users/Elias/Desktop/Zaragoza/ABI/CC/Tarea/relatedness.csv", header=FALSE)

## Numero de lotes en los que vamos a dividir los peces 
nLotes<-3 

## Numero de lotes a mutar
nummut<-3

## Los dos genes que generan el maximo relatedness por lote, como son tres lotes->2*3
numgenmut<-6

## Numero de padres que competiran en torneo
numpadres<-5

##Numero de individuos de la poblacion
numPobl<-20

## Creacion inicial de tres lotes aleatorios para dar comienzo al algoritmo. Creamos una poblacion
## de tamanio numPobl.
Poblote<-list()

##Funcion para crear nuevos individuos aleatorios
crearnuevolote<-function(){
  individuo<-list()
  pooluntakenfish<-c(2:length(relatedness))
  creacionloteinicial<-c(sample(2:length(relatedness),90))
  lote1<-c(creacionloteinicial[1:30])
  lote2<-c(creacionloteinicial[31:60])
  lote3<-c(creacionloteinicial[61:90])
  pooluntakenfish<-c(setdiff(pooluntakenfish,creacionloteinicial))
  individuo<-list(lote1,lote2,lote3,pooluntakenfish)
  return(list(individuo))
}

for (i in 1:numPobl){
  Poblote[i]<-crearnuevolote()
}

##Numero de iteraciones de la funcion de mejora
nIter=50

##Numero de iteraciones de la funcion principal
nItergen=2000

# Inicializacion de las variables que van a guardar la media y el maximo minimos de los 
# diferentes lotes creados
mediaminimal<-0
maximinimal<-1

## Variables de dibujo
maximumaxplot<-1
meanmaxplot<-1

## Variables para la sustitucion de lotes estancados
loteamejorarprevio<-0
maximoloteprevio<-1
contadormaximolote<-0
limitecontador<-5
bestlote<-list()

# Inicializacion de la configuracion que minimiza el maximo de los tres lotes
lotemaximinimo<-list()
bestlote<-list()
minimummaxplot<-1

###################################################################################
####                             Funcion principal                             ####
###################################################################################

genselecrepro<-function(){
  #cat("Calculo maximos lotes\n")
  ptm <- proc.time()
  adapmaxPobla<-lapply(Poblote, maximorelatednessindividuoini)
  minimummax<-which.min(adapmaxPobla)
  for (i in 1:nItergen){
    ##adaptacion del numero de iteraciones de la funcion de mejora conforme
    if (i>100){
      nIter=i
    }
    ##adaptaciones del contador 
    if(i>200){
      limitecontador<-10
    }
    if(i>500){
      limitecontador<-20
    }
    cat("Iteracion numero: ",i,"\n")
    
    ##Seleccion del peor lote
    loteamejorar<-which.max(adapmaxPobla)
    
    ##Evaluacion si el lote esta estancado
    if(loteamejorar==loteamejorarprevio){
      if (maximoloteprevio==maximorelatednessindividuoini(Poblote[[loteamejorar]])){
        contadormaximolote<-contadormaximolote+1
        cat("Contador activado: ",contadormaximolote,"\n")
      }else{
        contadormaximolote<-0
      }
    }else{
      contadormaximolote<-0
    }
    
    ##Creacion del nuevo lote en caso que se estanque el algoritmo
    if(contadormaximolote>limitecontador){
      Poblote[loteamejorar]<-crearnuevolote()
      cat("Nuevo lote creado debido a solucion estancada.\n")
      contadormaximolote<-0
    }
    
    ##Mejora del individuo seleccionado
    maximoloteprevio<-adapmaxPobla[[loteamejorar]]
    Poblote[loteamejorar]<-selecrepro(Poblote[loteamejorar])
    adapmaxPobla[[loteamejorar]]<-maximorelatednessindividuoini(Poblote[[loteamejorar]])
    
    ##Guardado del mejor lote
    #cat("Guardado resultados\n")
    if(adapmaxPobla[[loteamejorar]]<minimummax){
      minimummax<-adapmaxPobla[[loteamejorar]]
      bestlote<-Poblote[loteamejorar]
    }
    
    ##Dibujo de los resultados
    #cat("Dibujo\n")
    meanmaxplot<-c(meanmaxplot,mean(unlist(adapmaxPobla)))
    maximumaxplot<-c(maximumaxplot,adapmaxPobla[[which.max(adapmaxPobla)]])
    minimummaxplot<-c(minimummaxplot,minimummax)
    minmin <- min (minimummaxplot)
    maxmax <- max (maximumaxplot)
    plot  (minimummaxplot,  col = 4, type="l",ylim = c (minmin, maxmax))
    lines(maximumaxplot,type="l",col=1)
    lines(meanmaxplot,type="l",col=2)
    loteamejorarprevio<-loteamejorar
    
  }
  totaltime<-(proc.time() - ptm)
  
  ##Print del mejor lote
  cat("\n Resultados finales:\n")
  cat("\n")
  cat("Tiempo total para ",nIter," iteraciones: ",totaltime,"\n")
  for (i in 1:nLotes){
    cat("Maximo del lote ",i," : ",evaluacionmaximolote(bestlote[[1]][[i]])[[1]],"\n")
  }
  mediamedialote<-c(evaluacionmedialote(bestlote[[1]][[1]]),evaluacionmedialote(bestlote[[1]][[2]]),evaluacionmedialote(bestlote[[1]][[3]]))
  cat("Media de la media de los tres lotes: ",mean(mediamedialote),"\n")
  cat("\n")
  cat("Peces que forman los lotes:")
  for (i in 1:nLotes){
    cat("\n  El lote",i, "lo componen:\n")
    for (j in 1:length(bestlote[[1]][[i]])){
      cat(" ",bestlote[[1]][[i]][j])
    }
  }
}

###################################################################################
####                             Otras Funciones                               ####
###################################################################################

##Funcion de mejora de una soluci?n
selecrepro<-function(individuo){
  lote<-individuo
  ## contenedores
  maxim1<-maxim2<-maxim3<-maximminima<-mediaminima<-media1 <- media2 <- media3 <-mediatotal<- c()
  for (j in 1:nIter){
    
    ##Seleccion de padres entre los dos peores lotes
    ##Cada una de las funciones comentadas son funciones previas descritas en su definicion.
    #cat("Reproduccion\n")
    #lote<-reproduccion(lote)
    lote<-reproduccionmejorlista(lote)
    
    ##Mutacion de los lotes, quitando los que mas aportan al lote. 
    #cat("Mutacion\n")
    #mutlote<-mutacionsimple(lote,j,pooluntakenfish)
    #mutlote<-mutacionsimplemejor(lote,j,pooluntakenfish)
    #mutlote<-mutacionsimplelotemedia(lote,j,pooluntakenfish)
    lote<-mutacionsimplelotemax(lote)
  }
  #cat("Salida de la mejora\n")
  return(lote)
}

##Funcion de cruce de elementos dentro de los lotes
reproduccionmejorlista<-function(individuo){
  lote<-list()
  lote[[1]]<-list(individuo[[1]][[1]],individuo[[1]][[2]],individuo[[1]][[3]])
  medialotes<-rep(0,3)
  padres<-c(1:3)
  loteminimo<-1
  maxlotes<-lapply(lote[[1]], maximorelatednessindividuo)
  loteminimo<-which.min(unlist(maxlotes))
  ##cat("Lote con la minima media: ",loteminimo,"\n")
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
  for (k in 1:length(padres)){
    newlote[[padres[k]]]<-lote[[1]][[padres[k]]][-fishtochange]
  }
  newlote[[padres[1]]]<-c(newlote[[padres[1]]],lote[[1]][[padres[2]]][fishtochange])
  newlote[[padres[2]]]<-c(newlote[[padres[2]]],lote[[1]][[padres[1]]][fishtochange])
  rmediamax<-c(evaluacionmaximolote(newlote[[padres[1]]])[[1]],evaluacionmaximolote(newlote[[padres[2]]])[[1]])
  mediamax<-c(evaluacionmaximolote(lote[[1]][[padres[1]]])[[1]],evaluacionmaximolote(lote[[1]][[padres[2]]])[[1]])
  #cat("Media maximo padres: ",mean(mediamax),"Media maximo hijos: ",mean(rmediamax),"\n")
  if(max(rmediamax)<max(mediamax)){
    #cat("Hijos creados \n")
    lote[[1]][[padres[1]]]<-newlote[[padres[1]]]
    lote[[1]][[padres[2]]]<-newlote[[padres[2]]]
  }
  lote[[1]][4]<-individuo[[1]][4]
  return(lote)
}

##Funcion de mutacion de elementos
mutacionsimplelotemax<-function(individuo){
  #cat(j, "Pool of untaken fish: ")
  #for (i in 1:length(fpooluntakenfish))
  #cat(fpooluntakenfish[i]," ")
  #cat("\n")
  lote<-list()
  lote[[1]]<-list(individuo[[1]][[1]],individuo[[1]][[2]],individuo[[1]][[3]])
  fpooluntakenfish<-individuo[[1]][[4]]
  newfish<-sample(fpooluntakenfish,numgenmut)
  
  #cat("Newfish: ")
  #for (i in 1:length(newfish))
  # cat(newfish[i]," ")
  #cat("\n")
  b<-0
  for(i in 1:nLotes){
    max1<-evaluacionmaximolote(lote[[1]][[i]])[[2]]
    max2<-evaluacionmaximolote(lote[[1]][[i]])[[3]]
    # cat("Miembros del lote ",i," a ser añadidos a la pool: ",lote[[1]][[i]][max1]," ",lote[[1]][[i]][max2],"\n")
    dummielote<-lote[[1]][[i]]
    #cat("Lote original:\n")
    #for(y in 1:length(dummielote)){
    # cat(" ",dummielote[y])
    #}
    #cat("\n")
    dummielote[max1]<-newfish[i+b]
    dummielote[max2]<-newfish[i+b+1]
    #cat("Relatedness no mutados: ",relatedness[lote[[1]][[i]][max1],lote[[1]][[i]][max2]]," Relatedness mutados: ",relatedness[dummielote[max1],dummielote[max2]],"\n")
    if(evaluacionmaximolote(dummielote)[[1]]<evaluacionmaximolote(lote[[1]][[i]])[[1]]){
      # cat("Mutacion en el lote ",i,"\n")
      fpooluntakenfish<-c(fpooluntakenfish,lote[[1]][[i]][max1],lote[[1]][[i]][max2])
      lote[[1]][[i]][max1]<-newfish[i+b]
      lote[[1]][[i]][max2]<-newfish[i+b+1]
      fpooluntakenfish<-setdiff(fpooluntakenfish,newfish[i+b])
      fpooluntakenfish<-setdiff(fpooluntakenfish,newfish[i+b+1]) 
    }
    b=b+1
  }
  lote[[1]][[4]]<-fpooluntakenfish
  return(lote)
}

##Funcion que devuelve el maximo relatedness de un individuo(lista de listas)
maximorelatednessindividuoini<-function(individuo){
  mejorrelatedness<-0
  for(i in 1:nLotes){
    lote<-individuo[[i]]
    for (i in 1:length(lote)){
      for(j in i:(length(lote))){
        if(i!=j){
          #cat("fila: ",lote[i],"columna: ",lote[j],"relatedness: ", relatedness[[lote[i],lote[j]]],"\n")
          if(relatedness[[lote[i],lote[j]]]>mejorrelatedness){
            mejorrelatedness<-relatedness[[lote[i],lote[j]]]
          }
        }
      }
    }
  }
  return(mejorrelatedness)
}

##Funcion que devuelve el maximo relatedness de un individuo(vector)
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

## Evaluacion de la relacion media del lote
evaluacionmedialote<-function(lote){
  related<-NA
  for (k in 1:length(lote)){
    for(p in k:(length(lote))){
      if(k!=p & p!=0){
        #cat("Fila: ",k," Columa: ",p,"\n")
        related<-append(related,relatedness[[lote[k],lote[p]]])
      }
    }
  }
  return(mean(related,na.rm=TRUE))
}

maximorelatednessindividuo<-function(individuo){
  mejorrelatedness<-0
  for(i in 1:nLotes){
    lote<-individuo[[i]]
    for (i in 1:length(lote)){
      for(j in i:(length(lote))){
        if(i!=j){
          #cat("fila: ",lote[i],"columna: ",lote[j],"relatedness: ", relatedness[[lote[i],lote[j]]],"\n")
          if(relatedness[[lote[i],lote[j]]]>mejorrelatedness){
            mejorrelatedness<-relatedness[[lote[i],lote[j]]]
          }
        }
      }
    }
  }
  return(mejorrelatedness)
}
