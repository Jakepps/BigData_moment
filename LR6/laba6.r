
#   Пример классификации с помошью кластерного анализа

city.01 <- read.csv("C:/Users/nagal/OneDrive/GitHub/BigData_moment/LR6/zzz.csv")
city.02 <- read.csv("C:/Users/nagal/OneDrive/GitHub/BigData_moment/LR6/CPS1985.csv")
city.01
city.01 <- city.01[,-8]

#   Шаг 2.  Удаление пропущенных значений

city.01$dohod[city.01$dohod==-9999] <- NA
city.01 <- na.omit(city.01)

df3<-city.01
Countries<-city.01$strana

#   Шаг 3.  Стандартизация переменных.
#   В данной задаче переменные существенно различны.
#   Стандартизировать надо.

city.02 <- scale(city.01[,2:7], center = TRUE, scale = TRUE)

#   Исключим колонку "Страна"
city.02<-city.01[,-1]
maxs <- apply(city.02, 2, max)
mins <- apply(city.02, 2, min)

city.02 <- scale(city.02, center = mins, scale = maxs - mins)
city.02
#   Вернем колонку "Страна"

city.02<-data.frame(Countries,city.02)

#   Создаем матрицу попарных расстояний (по умолчанию - Евклидово расстояние)

dist.city <- dist(city.02 [,2:7])
dist.city
#   Проводим кластерный анализ, 
#   результаты записываем в список clust.protein
#   hclust ожидает матрицу расстояния, а не исходные данные.

clust.city <- hclust(dist.city, "ward.D")

#  Смотрим краткую сводку результатов анализа
clust.city$height

#  Шаг 4.  Построение дендрограммы
clust.city

plot(clust.city, labels = city.01$strana,main="????????????",ylab="C???????",xlab="fdd")

k = 5  
rect.hclust(clust.city, k = 5, border="red")
abline(h = 1.5, col = "blue", lwd='3') # h - horizontal line, col - color

plot(1:90, clust.city$height, type='b',xlab="????? ??????????",ylab = "??????????? ????????") # ???????? ?????

#  Разделим Страны на 4 кластера
#  Вектор groups содержит номер кластера, в который попал классифицируемый объект 
groups <- cutree(clust.city, k) 
# Разрезает дерево, например, полученное в результате hclust, на несколько групп 
# путем указания желаемого количества групп или высоты среза.
groups

dend <- as.dendrogram(clust.city)
if(!require(dendextend)) install.packages("dendextend"); 
library(dendextend)
dend <- color_branches(dend, k) 
plot(dend)
#dev.off() 


# Выведем страны соответсвенно сформированным кластерам 
city.01[groups==1, 1]
city.01[groups==2, 1]
city.01[groups==3, 1]
city.01[groups==4, 1]
city.01[groups==5, 1]



#  Для каждого столбц определяем, 
#  какая доля стран в среднем кластере приобретала этот столбец

#   в 1-ом кластере
g1<-colMeans(city.01[groups==1, 2:7])
g1
#   в 2-ом кластере
g2<-colMeans(city.01[groups==2, 2:7])
#   в 3-ом кластере
g3<-colMeans(city.01[groups==3, 2:7])
#   в 4-ом кластере
g4<-colMeans(city.01[groups==4, 2:7])
##   в 5-ом кластере
g5<-colMeans(city.01[groups==5, 2:7])

g11<-colMeans(city.02[groups==1, 2:7])
g11
#   во 2-ом кластере
g12<-colMeans(city.02[groups==2, 2:7])
g12
#   во 3-ом кластере
g13<-colMeans(city.02[groups==3, 2:7])
#   во 4-ом кластере
g14<-colMeans(city.02[groups==4, 2:7])
#   во 5-ом кластере
g15<-colMeans(city.02[groups==5, 2:7])





#   делаем дата фрейм из векторов групп кластеров


df2<-data.frame(g11,g12,g13,g14,g15);
df<-data.frame(g1,g2,g3,g4,g5); df
df1<-t(df2); df1

barplot(as.matrix(df2), col=c("magenta","red","yellow","blue","green","orange")) 
legend("topleft",cex=0.6, rownames(df2),fill=c("magenta","red","yellow","blue","green","orange") )
barplot(df1[,1], ylim=range(pretty(c(0,max(df1[,1])))), 
        main="Рождаемость", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))

barplot(df1[,2], ylim=range(pretty(c(0,max(df1[,2])))), 
        main="Cмертность", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))

barplot(df1[,3], ylim=range(pretty(c(0,max(df1[,3])))), 
        main="Детская смертность", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))
barplot(df1[,4], ylim=range(pretty(c(0,max(df1[,4])))), 
        main="Длительность жизни у мужчин", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))
barplot(df1[,5], ylim=range(pretty(c(0,max(df1[,5])))), 
        main="Длительность жизни у женщин", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))
barplot(df1[,6], ylim=range(pretty(c(0,max(df1[,6])))), 
        main="Доход", 
        col=c("magenta","red","yellow","blue","green"),legend=rownames(df1))
library (lattice)
library("scatterplot3d")

df3
df3["Group"]<-groups
# удаляем значения
#выведем график рассеяния с минимальным количеством параметров с выделением имени
xyplot(rozhdaem ~ smertnost,group = Group, data = df3,auto.key = TRUE,pch = 20,cex = 1.5)
#Боксплот, отражающий характеристики классов типов 
boxplot(smertnost~Group , data =df3, ylab = "Смерность", frame = FALSE, col = rainbow(3))
# График, классифицирующий типы согласно их полей
xyplot(smertnost~dlit_muzh+dlit_zhen|Group,data=df3, grid = T, auto.key=TRUE,pch = 20,cex = 1.5)
#Построим трехмерный график наших классов
cloud(rozhdaem~smertnost*detsk_smertm, group = Group, data = df3, auto.key = TRUE,pch = 20,cex = 1.5) 
packages <- c('ggplot2', 'dplyr', 'tidyr', 'tibble')
install.packages(packages)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
df3 %>%
  ggplot(aes(rozhdaem, smertnost, color=Group))+geom_point()
