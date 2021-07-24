require(geoR)
IDHM = read.table("edu1991.txt", header=T)#importa dados do arquivo txt para o R
names(IDHM)#verifica se a leitura dos dados está correta
attach(IDHM)
semiedu1991<-as.geodata(IDHM, coords.col=1:2, data.col=3)#cria geodados
semiedu1991#mostra as coordenadas e dados
plot(semiedu1991)#gráfico com a distribuição dos pontos na malha
summary(semiedu1991)
varioedu1991<-variog(semiedu1991, max.dist=2.5)#máxima distância para cálculo das semivariâncias deve estar entre 50% e 75% da distância máxima 
varioedu1991#mostra informações como distâncias($u), semivariâncias($v), número de pares($n) e desvio padrão($sd)
plot(varioedu1991,main="semivariograma médio - Educação 1991")
m1=eyefit(varioedu1991)
m2=eyefit(varioedu1991) 
m3=eyefit(varioedu1991)
m4=eyefit(varioedu1991)
lines(m1,col="blue")
lines(m2,col="green")
lines(m3,col="red")
lines(m4,col="black")
