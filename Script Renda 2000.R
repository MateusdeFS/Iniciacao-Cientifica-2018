require(geoR)
IDHM = read.table("renda2000.txt", header=T)#importa dados do arquivo txt para o R
names(IDHM)#verifica se a leitura dos dados est� correta
attach(IDHM)
semirenda2000<-as.geodata(IDHM, coords.col=1:2, data.col=3)#cria geodados
semirenda2000#mostra as coordenadas e dados
plot(semirenda2000)#gr�fico com a distribui��o dos pontos na malha
summary(semirenda2000)
variorenda2000<-variog(semirenda2000, max.dist=2.5)#m�xima dist�ncia para c�lculo das semivari�ncias deve estar entre 50% e 75% da dist�ncia m�xima 
variorenda2000#mostra informa��es como dist�ncias($u), semivari�ncias($v), n�mero de pares($n) e desvio padr�o($sd)
plot(variorenda2000,main="semivariograma m�dio - Renda 2000")
m1=eyefit(variorenda2000)
m2=eyefit(variorenda2000) 
m3=eyefit(variorenda2000)
m4=eyefit(variorenda2000)
lines(m1,col="blue")
lines(m2,col="green")
lines(m3,col="red")
lines(m4,col="black")



