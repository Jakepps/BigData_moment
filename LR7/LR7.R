# Задание 1.

# импорт данных (все спортсмены по всем видам спорта)
df <- read.table("C:/Users/nagal/OneDrive/GitHub/BigData_moment/LR7/athlete_events.csv", sep=",", header=TRUE)

wrestling<-df[which(df[, "Sport"]=="Wrestling"), c("Name", "Sex", "Weight", "Sport")]

# удаление строчек с пустым значением поля "Вес"
wrestling<-wrestling[-which(is.na(wrestling[, "Weight"])),]

# удаление повторяющихся строчек
wrestling<-unique(wrestling)

# значения столбца "Вес" становятся вектором
x<-wrestling[1:nrow(wrestling), "Weight"]

# Одномерные статистические тесты.

# проверка на нормальность распределения
# Тест Шапиро-Уилкса (Shapiro-Wilk test).
shapiro.test(x)

# Графический способ.

# гистограмма с линией плотности
x2<-seq(min(x), max(x), length=length(x))
fun<-dnorm(x2, mean=mean(x), sd=sd(x))
hist(x, freq=FALSE, col="gray", main="Гистограмма по весу", xlab="Значения веса", ylab="Частота")
lines(x2, fun, col=2, lwd=2)

# квантильно-квантильный график
qqnorm(x)
qqline(x, col=4, lwd=2)
#title(main="Квантильно-квантильный график", xlab="Выборочные квантили", ylab="Теоретические квантили")

# Тест Стьюдента.
t.test(x, mu=50, conf.int=TRUE)

# Тест Уилкоксона.
wilcox.test(x, mu=mean(x), conf.int=TRUE)


#Задание 2.

wrest_gymnast<-df[which(df[, "Sport"]%in%c("Gymnastics", "Wrestling")), c("Name", "Sex", "Weight", "Sport")]

wrest_gymnast<-wrest_gymnast[-which(is.na(wrest_gymnast[, "Weight"])),]

wrest_gymnast<-wrest_gymnast[which(wrest_gymnast[, "Sex"]=="F"),]

wrest_gymnast$Sport <- factor(wrest_gymnast$Sport)
wrest_gymnast$Sport <- droplevels(wrest_gymnast$Sport)
wrest_gymnast <- unique(wrest_gymnast)

x4<-wrest_gymnast[1:nrow(wrest_gymnast), "Weight"]

# проверка на нормальность распределения
# Тест Шапиро-Уилкса (Shapiro-Wilk test).
shapiro.test(x4)

# квантильно-квантильный график
qqnorm(x4)
qqline(x4, col=4, lwd=2)

# проверка равенство дисперсий
# Тест Флингера-Киллина.
fligner.test(wrest_gymnast$Weight~wrest_gymnast$Sport, wrest_gymnast)

# проверка на отсутствие разницы в среднестатистическом значении 
t.test(wrest_gymnast$Weight~wrest_gymnast$Sport)
t.test(wrest_gymnast$Weight~wrest_gymnast$Sport, paired=FALSE, var.equal=TRUE)



