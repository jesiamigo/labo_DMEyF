#Se utiliza el algoritmo Random Forest, creado por Leo Breiman en el año 2001
#Una libreria que implementa Rando Forest se llama  ranger
#La libreria esta implementada en lenguaje C y corre en paralelo, utiliza TODOS los nucleos del procesador
#Leo Breiman provenia de la estadistica y tenia "horror a los nulos", con lo cual el algoritmo necesita imputar nulos antes


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("ranger")
require("randomForest")  #solo se usa para imputar nulos

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/jesia/Desktop/4_DMEyF/")  #Establezco el Working Directory

#cargo los datos donde entreno
dataset  <- fread("./datasets/competencia2_2022.csv.gz", stringsAsFactors= TRUE)

#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dataset  <- na.roughfix( dataset[ foto_mes %in% c( 202103, 202105 ) ] )

#Feature engineering
#dataset[ , ctrx_quarter_bool :=  ifelse( ctrx_quarter>14, 1, 0 ) ]
dataset[ , mcuenta_corriente := (mcuenta_corriente_adicional + mcuenta_corriente)]
dataset[ , cprestamos := (cprestamos_personales + cprestamos_prendarios + cprestamos_hipotecarios)]
dataset[ , mprestamos := (mprestamos_personales + mprestamos_prendarios + mprestamos_hipotecarios)]
dataset[ , ccomisiones := (ccomisiones_mantenimiento + ccomisiones_otras)]
dataset[ , mcomisiones := (mcomisiones_mantenimiento + mcomisiones_otras)]
dataset[ , ctarjetas_transacciones := (ctarjeta_visa_transacciones + ctarjeta_master_transacciones)]

#dataset[,crtx_quarter:=NULL]
dataset[,mcuenta_corriente_adicional:=NULL]
dataset[,cprestamos_personales:=NULL]
dataset[,cprestamos_prendarios:=NULL]
dataset[,cprestamos_hipotecarios:=NULL]
dataset[,mprestamos_personales:=NULL]
dataset[,mprestamos_prendarios:=NULL]
dataset[,mprestamos_hipotecarios:=NULL]
dataset[,ccomisiones_mantenimiento:=NULL]
dataset[,ccomisiones_otras:=NULL]
dataset[,mcomisiones_mantenimiento:=NULL]
dataset[,mcomisiones_otras:=NULL]
dataset[,ctarjeta_visa_transacciones:=NULL]
dataset[,ctarjeta_master_transacciones:=NULL]


dtrain  <- dataset[ foto_mes == 202103 ]
dapply  <- dataset[ foto_mes == 202105 ]


#genero el modelo de Random Forest con la libreria ranger
#notar como la suma de muchos arboles contrarresta el efecto de min.node.size=1
param  <- list( "num.trees"=       2028,  #cantidad de arboles
                "mtry"=             5,  #cantidad de variables que evalua para hacer un split  sqrt(ncol(dtrain))
                "min.node.size"=  91,  #tamaño minimo de las hojas
                "max.depth"=        19   # 0 significa profundidad infinita
              )

set.seed(124739) #Establezco la semilla aleatoria

setorder( dtrain, clase_ternaria )  #primero quedan los BAJA+1, BAJA+2, CONTINUA


#genero el modelo de Random Forest llamando a ranger()
modelo  <- ranger( formula= "clase_ternaria ~ .",
                   data=  dtrain, 
                   probability=   TRUE,  #para que devuelva las probabilidades
                   num.trees=     param$num.trees,
                   mtry=          param$mtry,
                   min.node.size= param$min.node.size,
                   max.depth=     param$max.depth
                   #,class.weights= c( 1,40, 1)  #siguiendo con la idea de Maite San Martin
                 )

#aplico el modelo recien creado a los datos del futuro
prediccion  <- predict( modelo, dapply )

#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion$predictions[ ,"BAJA+2" ] > 1/40) ) ) #genero la salida

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/KA6310/", showWarnings = FALSE )
archivo_salida  <- "./exp/KA6310/KA6310_001_conFeatEngBasico.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep="," )
