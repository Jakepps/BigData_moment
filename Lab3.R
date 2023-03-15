#Выполнить учебный импорт любых таблиц данных из csv-файла и xls-таблицы.
library(readxl)

data1 <- read.csv("Лучшая любовница Герльта из Ривии.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

data2 <- read_excel("test.xls", sheet = "test")