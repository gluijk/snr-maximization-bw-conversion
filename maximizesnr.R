# Combinación óptima de señales para minimizar ruido con R (I). Teoría
# www.overfitting.net
# https://www.overfitting.net/2017/04/combinacion-optima-de-senales-para.html

# Tamaño de muestras
LADOIMG=1000
LARGO=LADOIMG^2
RESOL=61L # Número de valores de alpha

# Diferencia de ruido
SD1=0.14 # El ruido es la desviación típica
SD2=SD1/(5/3) # Establecemos que la señal 2 tenga menos ruido

# Señal 1
S1=array(0.5, dim=LARGO)
N1=rnorm(LARGO, mean=0, sd=SD1)
X1=S1+N1

# Señal 2
S2=S1
N2=rnorm(LARGO, mean=0, sd=SD2)
X2=S2+N2

# Combinación óptima y mejoras teóricas de relación S/N
k=var(X1)^0.5/var(X2)^0.5 # Estimación de N2/N1=SD2/SD1=SNR2/SNR1
alphaopt=1/(k^2+1) # alpha que maximiza la S/N de la combinación
mejoraN1=(k^2+1)^0.5
mejoraN2=(1/k^2+1)^0.5

# Combinaciones lineales para alpha 0..1
alpha=as.array(seq(from=0, to=1, len=RESOL))
y=array(0, dim=c(RESOL,LARGO))
for (i in 1:RESOL) y[i,]=alpha[i]*X1+(1-alpha[i])*X2
Yopt=alphaopt*X1+(1-alphaopt)*X2

varianza=alpha
for (i in 1:RESOL) varianza[i]=var(y[i,])

plot(alpha, varianza, ylim=c(0,max(var(X1),var(X2))), type='l',
     main='Ruido en función de alpha')
abline(v=alphaopt, col='red', lty = 'dotted')
abline(h=var(Yopt), col='red', lty = 'dotted')
abline(v=0.5, col='blue', lty = 'dotted') # Y=(X1+X2)/2
abline(h=var(y[1,]), col='blue', lty = 'dotted') # Y=X1
abline(h=var(y[RESOL,]), col='blue', lty = 'dotted') # Y=X2

# Checks
SNR1=mean(X1)/var(X1)^0.5
SNR2=mean(X2)/var(X2)^0.5
SNRopt=mean(Yopt)/var(Yopt)^0.5

print(paste0("SNR2/SNR1=", round(SNR2/SNR1, digits=4),
             " vs k=", round(k,4)))
print(paste0("SNRopt/SNR1=", round(SNRopt/SNR1, digits=4),
             " vs mejoraN1=", round(mejoraN1,4)))
print(paste0("SNRopt/SNR2=", round(SNRopt/SNR2, digits=4),
             " vs mejoraN2=", round(mejoraN2,4)))

# Dibujamos un trozo de las señales y la combinación óptima
xaxis=seq(from=0, to=1, len=1000)
plot(xaxis, X1[1:1000], ylim=c(0,1), type='l', col='blue', ylab='X1')
plot(xaxis, X2[1:1000], ylim=c(0,1), type='l', col='blue', ylab='X2')
plot(xaxis, Yopt[1:1000], ylim=c(0,1), type='l', col='blue', ylab='Yopt')

# Histogramas para comparar el ruido (varianza)
hist(X1, breaks=200, xlim=0:1, ylim=c(0,30000), col='blue', border='blue')
hist(X2, breaks=200, xlim=0:1, ylim=c(0,30000), col='lightblue', border='lightblue')
hist(Yopt, breaks=200, xlim=0:1, ylim=c(0,30000), col='red', border='red')

# Truncamos posibles outliers para construir imágenes
X1[X1 < 0] <- 0
X2[X2 < 0] <- 0
Yopt[Yopt < 0] <- 0

X1[X1 > 1] <- 1
X2[X2 > 1] <- 1
Yopt[Yopt > 1] <- 1

# Convertimos vectores en imágenes (genial R!)
dim(X1) <- c(LADOIMG, LADOIMG)
dim(X2) <- c(LADOIMG, LADOIMG)
dim(Yopt) <- c(LADOIMG, LADOIMG)

# Guardamos como imágenes
library(tiff)
writeTIFF(X1,   "img1.tif",   bits.per.sample=16, compression="LZW")
writeTIFF(X2,   "img2.tif",   bits.per.sample=16, compression="LZW")
writeTIFF(Yopt, "imgout.tif", bits.per.sample=16, compression="LZW")

