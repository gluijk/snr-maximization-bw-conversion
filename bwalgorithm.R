# Combinación óptima de señales para minimizar ruido con R (II). Aplicación
# www.overfitting.net

# Extracción RAW con DCRAW (combinación óptima): dcraw -v -r 1 1 1 1 -o 0 -4 -T *.DNG
# Revelado lineal con DCRAW (BN en Photoshop): dcraw -v -w -o 2 -4 -T *.DNG

# Librería imágenes en 16 bits
library(tiff)
Gamma=2.2 # Curva Gamma para deslinealizar imágenes de salida

# Leemos extracción RAW y versión con desenfoque gaussiano
origen=readTIFF("iso12800.tiff", native=F, convert=F)
blurred=readTIFF("iso12800blurred.tif", native=F, convert=F)

# Estimación de diferencias de relación S/N
# (k y kp son matrices pues se calculan para cada píxel)
k= blurred[,,2]/blurred[,,1] # k =SNR2/SNR1=SNR_G/SNR_R
kp=blurred[,,3]/blurred[,,1] # kp=SNR3/SNR1=SNR_B/SNR_R

# Mapa para la combinación óptima
# mapa[x,y,1]+mapa[x,y,2]+mapa[x,y,3]=1 para todo (x,y)
mapa=array(0,dim(origen))
mapa[,,1]=1/(1+k^2+kp^2) # Mapa de pesos óptimos para R
mapa[,,2]=mapa[,,1]*k^2  # Mapa de pesos óptimos para G
mapa[,,3]=mapa[,,1]*kp^2 # Mapa de pesos óptimos para B
writeTIFF(mapa^(1/Gamma), "mapa.tif", bits.per.sample=16, compression="LZW")

# Calculamos matricialmente combinación óptima
Yopt=mapa*origen
writeTIFF((Yopt[,,1]+Yopt[,,2]+Yopt[,,3])^(1/Gamma), "yopt.tif",
    bits.per.sample=16, compression="LZW")

