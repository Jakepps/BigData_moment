library (lattice)
library("scatterplot3d")
install.packages(packages)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
install.packages("vctrs")
install.packages('tibble', repos = 'http://cran.rstudio.com/', type = 'source')
library(klaR)
library(party)
library(randomForest) 

#ЧАСТЬ 1
inf1 <- read.csv("C:/Users/nagal/OneDrive/GitHub/BigData_moment/LR6/CPS1985.csv")
#inf1 <- read.csv("C:/Users/jakep/GitHub/BigData_moment/LR6/CPS1985.csv")
inf1 <- inf1[,-8]

# Шаг 2. Удаление пропущенных значений
inf1$dohod[inf1$dohod==-9999] <- NA
inf1 <- na.omit(inf1)

inf123<-inf1
Countries<-inf1$strana

# Шаг 3. Стандартизация переменных.
# В данной задаче переменные существенно различны. Станадртизируем их

inf2 <- scale(inf1[,2:7], center = TRUE, scale = TRUE)

# Исключим колонку "Страна"
inf2<-inf1[,-1]
maxs <- apply(inf2, 2, max)
mins <- apply(inf2, 2, min)

inf2 <- scale(inf2, center = mins, scale = maxs - mins)
# Вернем колонку "Страна"

inf2<-data.frame(Countries,inf2)

# Создаем матрицу попарных расстояний (по умолчанию - Евклидово расстояние)

dist.city <- dist(inf2 [,2:7])

# Проводим кластерный анализ,результаты записываем в список clust.protein

clust.city <- hclust(dist.city, "ward.D")

#  Шаг 4.  Построение дендрограммы

plot(clust.city, labels = inf1$strana,main="Дендограмма",ylab="Сходство",xlab="Страны")

k=5
rect.hclust(clust.city, k, border="red")
abline(h = 1.5, col = "blue", lwd='3') 

plot(1:90, clust.city$height, type='b',xlab="Номер компоненты",ylab = "Собственное значение")

#  Разделим Страны на 4 кластера
#  Вектор groups содержит номер кластера, в который попал классифицируемый объект 
groups <- cutree(clust.city, k) 
# Разрезает дерево, например, полученное в результате hclust, на несколько групп 
# путем указания желаемого количества групп или высоты среза.

dend <- as.dendrogram(clust.city)
if(!require(dendextend)) install.packages("dendextend"); 
library(dendextend)
dend <- color_branches(dend, k) 
plot(dend)


# Выведем страны соответсвенно сформированным кластерам 
inf1[groups==1, 1]
inf1[groups==2, 1]
inf1[groups==3, 1]
inf1[groups==4, 1]
inf1[groups==5, 1]

#  Для каждого столбца определяем, 
#  какая доля стран в среднем кластере приобретала этот столбец

#   в 1-ом кластере
g1<-colMeans(inf1[groups==1, 2:7])
#   в 2-ом кластере
g2<-colMeans(inf1[groups==2, 2:7])
#   в 3-ом кластере
g3<-colMeans(inf1[groups==3, 2:7])
#   в 4-ом кластере
g4<-colMeans(inf1[groups==4, 2:7])
#  в 5-ом кластере
g5<-colMeans(inf1[groups==5, 2:7])

g11<-colMeans(inf2[groups==1, 2:7])
#   во 2-ом кластере
g12<-colMeans(inf2[groups==2, 2:7])
#   во 3-ом кластере
g13<-colMeans(inf2[groups==3, 2:7])
#   во 4-ом кластере
g14<-colMeans(inf2[groups==4, 2:7])
#   во 5-ом кластере
g15<-colMeans(inf2[groups==5, 2:7])

#   делаем дата фрейм из векторов групп кластеров

df2<-data.frame(g11,g12,g13,g14,g15)
df<-data.frame(g1,g2,g3,g4,g5)
df1<-t(df2)

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

inf123["Group"]<-groups

#выведем график рассеяния с минимальным количеством параметров с выделением имени
xyplot(rozhdaem ~ smertnost,group = Group, data = inf123,auto.key = TRUE,pch = 20,cex = 1.5)

#усатый ящик, отражающий характеристики классов типов 
boxplot(smertnost~Group , data =inf123, ylab = "Смерность", frame = FALSE, col = rainbow(3))

# График, классифицирующий типы согласно их полей
xyplot(smertnost~dlit_muzh+dlit_zhen|Group,data=inf123, grid = T, auto.key=TRUE,pch = 20,cex = 1.5)

#Построим трехмерный график наших классов
cloud(rozhdaem~smertnost*detsk_smertm, group = Group, data = inf123, auto.key = TRUE,pch = 20,cex = 1.5) 
packages <- c('ggplot2', 'dplyr', 'tidyr', 'tibble')

inf123 %>%
  ggplot(aes(rozhdaem, smertnost, color=Group))+geom_point()

#ЧАСТЬ 2 

city.01 <- read.csv("C:/Users/nagal/OneDrive/GitHub/BigData_moment/LR6/CPS1985.csv")
city.01
city.01 <- city.01[,-8]

#   Шаг 2.  Удаление пропущенных значений

city.01$dohod[city.01$dohod==-9999] <- NA
city.01 <- na.omit(city.01)

my_data<-city.01[,-8]
my_data
groups
my_data$Group<- c(as.factor(groups))

naive_df <- NaiveBayes(my_data$Group ~ ., data = my_data) 
naive_df$tables 
naive_df$tables$Work
naive_df

#делаем графики по байсу
opar=par() 
opar
layout(matrix(c(1,2,3,4), 2, 2)) 
plot(naive_df, lwd = 2, legendplot = FALSE)
legend("topleft",lty=1:3, cex=0.5)
#восстановление
par=opar

# Классификация Decision Tree

set.seed(1234)
ind <- sample(2, nrow(my_data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- my_data[ind==1,]
testData <- my_data[ind==2,] 
nrow(trainData)
nrow(testData)
nrow(my_data)

my_data
myFormula <- Group ~ rozhdaem + smertnost + detsk_smertm + dlit_muzh + dlit_zhen + dohod
df_ctree <- ctree(myFormula, data=trainData)
df_ctree
table(predict(df_ctree), trainData$Group) 
predict(df_ctree)
plot(df_ctree)

#Алгоритм Random Forest 

rf <- randomForest(Group ~ .,data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$Group)
print(rf)

