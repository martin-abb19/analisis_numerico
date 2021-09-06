##Inputs ----
cupon = 0.05 #El bono de valor nominal 100 paga cupones del 5% semestrales. 

Amortizaciones = matrix(c(1,2,3,4, 25,25,25,25), ncol = 2, dimnames = list(NULL, c("t", "amort"))) #Armamos una matriz con el año y la amortización

m = 2 #Numero de pagos de intereses por anio

View(Amortizaciones)

n = max(Amortizaciones[,1])*m #numero de cashflows es el maximo de la primer columna (t) por la cantidad de pagos anuales
n
#Creo la marcha progresiva con columnas: t, saldo, Amort, Intereses, Flujo (5 COLUMNAS)

valor_actual = matrix( rep(NA, (n+1)*5), ncol = 5) #NA porque no lo he definido aún. Repetido (n+1)*5: todos los periodos donde hay CF (n) y uno mas (donde el CF=0)

colnames(valor_actual)=c("t", "saldo", "amort", "int", "C.F.")
valor_actual

##Ahora llenemos esos NA

#Columna t

valor_actual[,"t"] <- seq(from=0, to=n/m, by=1/m) #En este caso vas de 0 a 4 años de a dos pagos/año. llenas de tal año a tal otro y cada m pagos
valor_actual

#amort y saldo

k=1
valor_actual[1,"saldo"] <- sum(Amortizaciones[,"amort"]) #en t=0 no amortizó nada el bono ni paga intereses: el saldo es la suma cuatro años de amortizaciones
valor_actual
for(i in 1:(n+1)){
  if(valor_actual[i, "t"] == Amortizaciones[k, "t"]){ #fechas coincidentes->asigno amortizacion
      valor_actual[i, "amort"] = Amortizaciones[k, "amort"] #ej. 0 y 0.5 no estan en la matriz Amortizaciones. Pero 1 si, entonces a valor_actual le asigna lo que tiene en fila 1 columna amort
      #empiezo en i>1 porque no existe i=0
      if(i>1){valor_actual[i, "saldo"] = valor_actual[i-1, "saldo"] - Amortizaciones[k, "amort"]}
      k= k+1
   }else{ #si no coincide el tiempo no hay amortizacion. Es 0 en ese t y el saldo es el mismo en i e i-1
       valor_actual[i, "amort"] = 0
       if(i>1){valor_actual[i, "saldo"] = valor_actual[i-1, "saldo"]}
  }
}

#Interes y Cash flow
valor_actual[1, "int"] = 0 #t=0: no hay pago de intereses
for(i in 2:(n+1)){ 
  #la tasa del cupon usualmente es T.N.A. por ende se divide en los periodos para hacerla una tasa efectiva
  valor_actual[i, "int"] = valor_actual[i-1, "saldo"]*cupon/m
}
valor_actual[, "C.F."]<- valor_actual[,"amort"] + valor_actual[, "int"] #suma componente a componente
View(valor_actual)

##Funcion de precio ----
#debo pasarle: tasa, plazo t, cashflow en cada t
precio <- function(tasa, t, CF){
  n = length(CF)
  P = 0 
  for(i in 1:n){
    P = P + CF[i]/(1+tasa)^t[i]
  }
 return(P)  
}
precio(0.05, valor_actual[, "t"], valor_actual[, "C.F."]) #Este ejemplo es erróneo a propósito. Es T.N.A. de capitalizacion SEMESTRAL
tasa1 = (1+0.05/2)^2 -1 #T.E.A. es la efectiva semenstral capitalizada dos veces
precio(tasa1, valor_actual[,"t"], valor_actual[,"C.F."]) #si la tasa cupon es igual a la efectiva anual el bono cotizará a la par

#veamos que ocurre cuando la tasa varia 
tasas = seq(0, 1, by = 0.01)
P = precio(tasas,valor_actual[,"t"], valor_actual[,"C.F."])

plot(tasas, P, type = "b") #grafica linea y circulos con type="b"
 
#Ahora tengo todo lo que necesito para calcular la TIR: precio, tasa, CF, t.
# Buscamos el 0 de la funcion precio-VP(C.F.)
f <- function(r){ return( -80 + precio(r,valor_actual[,"t"], valor_actual[,"C.F."]) )  }
#A modo de ej la anterior funcion presupone p=80. podemos cambiarlo
plot(tasas, f(tasas), ylab = "VPN", type="l", col="red")
lines(0:1,0:1, col="blue") #Aproximamos gráficamente el valor para faciilitarle la tarea al algoritmo
source("analisis_numerico_semana_2.R") #importamos las funciones de biseccion, punto fijo, etc. Debe estar en el wd sino source no la encuentra 
biseccion(0.1, 600, 10^-10, 1000) #0.158015 es la tasa que hace 0 al VPN y se puede ver aproximada en el gráfico















