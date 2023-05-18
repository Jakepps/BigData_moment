#install.packages(pkgs=c("ellipse"))
#+install.packages("car")
library(car)
library(ellipse)

# импорт данных
df1 <- read.csv("C:/Users/nagal/OneDrive/GitHub/BigData_moment/LR8/LR8_dataset.csv", sep = "," , header = TRUE)
df1 <- read.csv("C:/Users/jakep/GitHub/BigData_moment/LR8/LR8_dataset.csv", sep = "," , header = TRUE)
years=1989:2018

# выборка данных по данной стране
data_set <- df1[df1$Country.Name == "France", ]

# замена отсутствующих данных на NA
for (i in 1:23){
  for (j in 5:34){
    if (data_set[i,j]!=".."){
      data_set[i,j]= as.numeric(data_set[i,j])
    }
    else{
      data_set[i,j]= NA
    }
    
  }
}

# прирост ВВП
VVP<-rep(0,30)

for (i in 5:34) {
  VVP[i-4]=data_set[2,i]
  
}

# построение графика прироста ВВП
plot(years, VVP, xlab='Год', ylab='Прирост, %', main='Прирост ВВП во Франции',type='b', lty=1, pch=1,  lwd=2)


#####################################################################
# a. Рост ВВП и прирост населения

# прирост населения
Population_growth<-rep(0,30)

for (i in 5:34) {
  Population_growth[i-4]=as.numeric(data_set[14,i])
}

# прирост ВВП
VVP_growth<-rep(0,30)

for (i in 5:34) {
  VVP_growth[i-4]=as.numeric(data_set[1,i])
}

VVP_growth <- VVP_growth/1000000

# построение графика прироста ВВП
plot(
  years,
  VVP_growth,
  xlab='Год',
  ylab='Прирост ВВП, USD, в миллионах',
  main='Прирост ВВП',
  col='blue',
  type='b',
  lty=1,
  pch=1, 
  lwd=2
)

# поиск среднего и медианы для каждой выборки
mean(VVP_growth,na.rm=TRUE)
median(VVP_growth,na.rm=TRUE)

mean(Population_growth,na.rm=TRUE)
median(Population_growth,na.rm=TRUE)

# проверка на нормальность распределения
shapiro.test(Population_growth)
shapiro.test(VVP_growth)

# замена NA медианными значениями 
for (i in 1:30){
  if (is.na(VVP_growth[i])){
    #VVP_growth[i]=0
    VVP_growth[i]= median(VVP_growth,na.rm=TRUE)
  }
  if (is.na(Population_growth[i])){
    #Population_growth[i]=0
    Population_growth[i]= median(Population_growth,na.rm=TRUE)
  }
  
}

# корреляции

# по методу Спирмена
cor(VVP_growth,Population_growth , method= "spearman")
cor.test(VVP_growth,Population_growth )
# по методу Пирсона
cor(VVP_growth,Population_growth , method= "pearson")

# корреляционная матрицы 
df1=data.frame(VVP_growth,Population_growth)

# представление в виде эллипсов
plotcorr(cor(df1))

##########################################################################
# с.Изменения расходов на медицину и увеличения продолжительности жизни и смертность.

# расходы на медицину
Changes_medical_cost<-rep(0,30)

for (i in 5:34) {
  Changes_medical_cost[i-4]=as.numeric(data_set[12,i])
}

# продолжительность жизни
Average_life<-rep(0,30)

for (i in 5:34) {
  Average_life[i-4]=as.numeric(data_set[13,i])
}

# смертность
Death_rate<-rep(0,30)

for (i in 5:34) {
  Death_rate[i-4]=as.numeric(data_set[18,i])
}

# поиск среднего и медианы для каждой выборки
mean(Changes_medical_cost,na.rm=TRUE)
median(Changes_medical_cost,na.rm=TRUE)

mean(Average_life,na.rm=TRUE)
median(Average_life,na.rm=TRUE)

mean(Death_rate,na.rm=TRUE)
median(Death_rate,na.rm=TRUE)

# проверка на нормальность распределения
shapiro.test(Changes_medical_cost)
shapiro.test(Death_rate)
shapiro.test(Average_life)

# замена NA медианными значениями 
for (i in 1:30){
  if (is.na(Changes_medical_cost[i])){
    #Changes_medical_cost[i]=0
    Changes_medical_cost[i]= median(Changes_medical_cost,na.rm=TRUE)
  }
  if (is.na(Average_life[i])){
    #Average_life[i]=0
    Average_life[i]= median(Average_life,na.rm=TRUE)
  }
  if (is.na(Death_rate[i])){
    #Death_rate[i]=0
    Death_rate[i]= median(Death_rate,na.rm=TRUE)
  }
}

# корреляции

# метод Спирмана
cor(Changes_medical_cost,Average_life, method= "spearman")
cor.test(Changes_medical_cost,Average_life )

# метод Пирсона
cor(Changes_medical_cost,Average_life, method= "pearson")

# метод Спирмана
cor(Changes_medical_cost,Death_rate, method= "spearman")
cor.test(Changes_medical_cost,Death_rate )

# метод Пирсона
cor(Changes_medical_cost,Death_rate, method= "pearson")

# корреляционная матрицы 
df3=data.frame(Average_life,Death_rate,Changes_medical_cost)

# представление в виде эллипсов
plotcorr(cor(df3))

##########################################################################
# d.Прирост людей с высшим образованием на рост экспорта товаров и на прирост высокотехнологичного производства

# люди в высшим образованием
Higher_education_growth<-rep(0,30)

for (i in 5:34) {
  Higher_education_growth[i-4]=as.numeric(data_set[19,i])
}

# экспорт товаров
Growth_exports_goods<-rep(0,30)

for (i in 5:34) {
  Growth_exports_goods[i-4]=as.numeric(data_set[17,i])
}

# высокотехнологическое производство
Growth_technical_production<-rep(0,30)

for (i in 5:34) {
  Growth_technical_production[i-4]=as.numeric(data_set[21,i])
}

# поиск среднего и медианы для каждой выборки
mean(Higher_education_growth,na.rm=TRUE)
median(Higher_education_growth,na.rm=TRUE)

mean(Growth_exports_goods,na.rm=TRUE)
median(Growth_exports_goods,na.rm=TRUE)

mean(Growth_technical_production,na.rm=TRUE)
median(Growth_technical_production,na.rm=TRUE)

# проверка на нормальность распределения
shapiro.test(Higher_education_growth)
shapiro.test(Growth_technical_production)
shapiro.test(Growth_exports_goods)

# замена NA медианными значениями 
for (i in 1:30){
  if (is.na(Higher_education_growth[i])){
    #Higher_education_growth[i]=0
    Higher_education_growth[i]= median(Higher_education_growth,na.rm=TRUE)
  }
  if (is.na(Growth_exports_goods[i])){
    #Growth_exports_goods[i]=0
    Growth_exports_goods[i]= median(Growth_exports_goods,na.rm=TRUE)
  }
  if (is.na(Growth_technical_production[i])){
    #Growth_technical_production[i]=0
    Growth_technical_production[i]= median(Growth_technical_production,na.rm=TRUE)
  }
}

#Higher_education_growth - 1 значение в таблице, нет смысла

# корреляции

# метод Спирмана
cor(Higher_education_growth,Growth_exports_goods, method= "spearman")
cor.test(Higher_education_growth,Growth_exports_goods )

# метод Пирсона
cor(Higher_education_growth,Growth_exports_goods, method= "pearson")

# метод Спирмана
cor(Higher_education_growth,Growth_technical_production, method= "spearman")
cor.test(Higher_education_growth,Growth_technical_production )

# метод Пирсона
cor(Higher_education_growth,Growth_technical_production, method= "pearson")

# корреляционная матрицы 
df4=data.frame(Growth_exports_goods,Growth_technical_production,Higher_education_growth)

# представление в виде эллипсов
plotcorr(cor(df4))

##########################################################################
# e.Расходов на образование на – кумулятивный прирост бакалавров среди женщин

# расходы на образование
Edication_cost<-rep(0,30)

for (i in 5:34) {
  Edication_cost[i-4]=as.numeric(data_set[15,i])
}

# бакалавры женщины
Female_bachelors<-rep(0,30)

for (i in 5:34) {
  Female_bachelors[i-4]=as.numeric(data_set[20,i])
}

# поиск среднего и медианы для каждой выборки
mean(Edication_cost,na.rm=TRUE)
median(Edication_cost,na.rm=TRUE)

mean(Female_bachelors,na.rm=TRUE)
median(Female_bachelors,na.rm=TRUE)

# проверка на нормальность распределения
shapiro.test(Edication_cost)
shapiro.test(Female_bachelors)

# замена NA медианными значениями 
#Female_bachelors - 1 значение, нет смысла
for (i in 1:30){
  if (is.na(Edication_cost[i])){
    #Edication_cost[i]=0
    Edication_cost[i]= median(Edication_cost,na.rm=TRUE)
  }
  if (is.na(Female_bachelors[i])){
    #Female_bachelors[i]=0
    Female_bachelors[i]= median(Female_bachelors,na.rm=TRUE)
  }
}
# корреляции

# метод Спирмана
cor(Edication_cost,Female_bachelors, method= "spearman")
cor.test(Edication_cost,Female_bachelors )

# метод Пирсона
cor(Edication_cost,Female_bachelors, method= "pearson")

# корреляционная матрицы 
df5=data.frame(Edication_cost,Female_bachelors)

# представление в виде эллипосв
plotcorr(cor(df5))

##########################################################################
# f.Прирост людей с высшим образованием на развитие высоких технологий (прирост статей в научных журналах)

# научные статьи
Scientific_Articles<-rep(0,30)

for (i in 5:34) {
  Scientific_Articles[i-4]=as.numeric(data_set[23,i])
}

# поиск среднего и медианы для каждой выборки
mean(Higher_education_growth,na.rm=TRUE)
median(Higher_education_growth,na.rm=TRUE)

mean(Scientific_Articles,na.rm=TRUE)
median(Scientific_Articles,na.rm=TRUE)

# проверка на нормальность распределения
shapiro.test(Higher_education_growth)
shapiro.test(Scientific_Articles)

# замена NA медианными значениями 
#Higher_education_growth - 1 значение в таблице, нет смысла
for (i in 1:30){
  
  if (is.na(Scientific_Articles[i])){
    #Article[i]=0
    Scientific_Articles[i]= median(Scientific_Articles,na.rm=TRUE)
  }
}

# корреляции

# метод Спирмана
cor(Higher_education_growth,Scientific_Articles, method= "spearman")
cor.test(Higher_education_growth,Scientific_Articles)

# метод Пирсона
cor(Higher_education_growth,Scientific_Articles, method= "pearson")

# корреляционная матрицы 
df6=data.frame(Higher_education_growth,Scientific_Articles)

# представление в виде эллипсов
plotcorr(cor(df6))

##########################################################################
# все выборки
df2=data.frame(VVP_growth,Population_growth,Growth_exports_goods,Scientific_Articles,Growth_technical_production,Female_bachelors,Changes_medical_cost,Death_rate,Average_life)

# представление в виде эллипсов
plotcorr(cor(df2))

# хранение всех выборок в виде матрицы
states <- as.data.frame(df2[,c('VVP_growth','Population_growth',
                               'Growth_exports_goods','Scientific_Articles',
                               'Growth_technical_production',
                               'Changes_medical_cost',
                               'Death_rate','Average_life')])
cor(states)

# независимая переменная - расходы на медицину,зависимая – рейтинг смертности.
fit <- lm(Death_rate ~ Changes_medical_cost, data=states)
fit

pred<-predict(fit, states)
pred

par(mfrow=c(1, 2))
plot(states$`Changes_medical_cost`, states$`Death_rate`, xaxt="n", xlab="Расходы на медицину", ylab="Смертность", col="blue", main="График реальных значений продолжительности жизни")
abline(fit, col="red")
plot(pred, xaxt="n", xlab="Расходы на медицину", ylab="Смертность", col="blue", main="График прогнозируемых значений продолжительности жизни")
