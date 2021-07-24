require(geoR)
IDHM = read.table("idhm1991.txt", header=T)#importa dados do arquivo txt para o R
names(IDHM)#verifica se a leitura dos dados está correta
attach(IDHM)
semiidhm1991<-as.geodata(IDHM, coords.col=1:2, data.col=3)#cria geodados
semiidhm1991#mostra as coordenadas e dados
plot(semiidhm1991)#gráfico com a distribuição dos pontos na malha
summary(semiidhm1991)
varioidhm1991<-variog(semiidhm1991, max.dist=2.5)#máxima distância para cálculo das semivariâncias deve estar entre 50% e 75% da distância máxima 
varioidhm1991#mostra informações como distâncias($u), semivariâncias($v), número de pares($n) e desvio padrão($sd)
plot(varioidhm1991,main="semivariograma médio - IDHM 1991")
m1=eyefit(varioidhm1991)
m2=eyefit(varioidhm1991)
m3=eyefit(varioidhm1991) 
lines(m1,col="blue")
lines(m2,col="green")
lines(m3,col="red")
valm1<-xvalid(semiidhm1991,model = m1)
Regm1<-lm(valm1$data~valm1$predicted) 
summary(Regm1) 
plot(valm1$data,valm1$predicted, main="Validação cruzada - IDHM 1991")
malha <- expand.grid(seq(-50.68777,-45.96389,0.02), seq(-20.26777,-18.43083,0.02))
malha
limites = matrix(c(-50.687778,-19.695000,-49.942778,-18.691944,-49.186944,-18.436944,-48.705000,-18.592778,-47.876944,-18.575833,-47.608889,-18.430833,-46.517778,-18.578889,-45.965833,-19.530000,-46.377778,-19.750833,-47.541944,-19.936944,-47.804167,-19.976944,-49.198889,-20.267778),byrow= T,ncol=2)
koest <- krige.conv(semiidhm1991, loc=malha, krige=krige.control(type.krige = "ok", obj.model = m1),borders=limites)
koest
contour(koest, filled=TRUE, levels=seq(0.34,0.58, by=0.02))
contour(koest, values=koest$predict, filled=TRUE, nlevels=4)
title(main="Mapa do IDH-M (1991)")
contour(koest, filled=TRUE, levels=seq(4,6, by=0.5))
contour(koest, val=koest$krige.var, filled=TRUE, nlevels=4) 
title(main="Mapa da variância (IDH-M 1991)")

