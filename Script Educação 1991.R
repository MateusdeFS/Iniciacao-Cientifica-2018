require(geoR)
IDHM = read.table("edu1991.txt", header=T)#importa dados do arquivo txt para o R
names(IDHM)#verifica se a leitura dos dados est� correta
attach(IDHM)
semiedu1991<-as.geodata(IDHM, coords.col=1:2, data.col=3)#cria geodados
semiedu1991#mostra as coordenadas e dados
plot(semiedu1991)#gr�fico com a distribui��o dos pontos na malha
summary(semiedu1991)
varioedu1991<-variog(semiedu1991, max.dist=2.5)#m�xima dist�ncia para c�lculo das semivari�ncias deve estar entre 50% e 75% da dist�ncia m�xima 
varioedu1991#mostra informa��es como dist�ncias($u), semivari�ncias($v), n�mero de pares($n) e desvio padr�o($sd)
plot(varioedu1991,main="semivariograma m�dio - Educa��o 1991")
m1=eyefit(varioedu1991)
m2=eyefit(varioedu1991) 
m3=eyefit(varioedu1991)
m4=eyefit(varioedu1991)
lines(m1,col="blue")
lines(m2,col="green")
lines(m3,col="red")
lines(m4,col="black")
