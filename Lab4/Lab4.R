
df_m <- read.csv("lab4_parus_m.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)
df_f<-read.csv("lab4_parus_f.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

#посчитаем суммув всех медалей
stat_places_m <- sapply(df_m[,-1], sum)
stat_places_f <- sapply(df_f[,-1], sum)

par(mfrow=c(1,2))
barplot(stat_places_m, names=c(1:8), col="coral", xlab="Место", ylab="Количество", main="Мужчины (за все время)")
barplot(stat_places_f, names=c(1:8), col="coral", xlab="Место", ylab="Количество", main="Женщины (за все время)")

#сделаем фреймы только 1 мест
first_m <- df_m[,c(1:2)][df_m$X1 > 0, ]
first_f <- df_f[,c(1:2)][df_f$X1 > 0, ]

pie(first_m$X1, labels=first_m$X1, col=rainbow(length(first_m$X1)), main = "Количество золотых медалей (мужчины)\nза все время")
legend(-1.1, 1.1, first_m$Год, cex = 0.7, fill=rainbow(length(first_m$Год)))

pie(first_f$X1, labels=first_f$X1, col=rainbow(length(first_f$X1)), main = "Количество золотых медалей (женщины)\nза все время")
legend(-1, 1, first_f$Год, cex = 0.7, fill=rainbow(length(first_f$Год)))

#выделим призовые места
prize_m <- data.frame(Год=df_m$Год, Призовых=rowSums(df_m[, 2:4]))
prize_f <- data.frame(Год=df_f$Год, Призовых=rowSums(df_f[, 2:4]))

par(mfrow=c(1,1))
plot(prize_m, type="b", pch=19, col="navyblue", xaxt="n", ylim=c(0,7), main="Призовые места России по вольной борьбе за 30 лет")
lines(prize_f, type="o", pch=19, col="hotpink")
legend(min(df_m$Год), 7, c("Мужчины", "Женщины"), fill=c("navyblue", "hotpink"))
axis(side=1, at=prize_m$Год)



events_gold <- read.csv("gold.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

plot(events_gold$Год, events_gold$США, type="b", pch=19, col="#3be8b0", xaxt="n", ylim=c(0,50), xlab="Год", ylab="Золотых медалей", main="Золотые медали за 6 последних олимпиад")
lines(events_gold$Год, events_gold$Китай, type="o", pch=19, col="#1aafd0")
lines(events_gold$Год, events_gold$Япония, type="o", pch=19, col="#6a67ce")
lines(events_gold$Год, events_gold$Великобритания, type="o", pch=19, col="#ffb900")
lines(events_gold$Год, events_gold$Россия, type="o", pch=19, col="gray70")
lines(events_gold$Год, events_gold$Австралия, type="o", pch=19, col="#2e3c54")
lines(events_gold$Год, events_gold$Нидерланды, type="o", pch=19, col="brown")
axis(side=1, at=events_gold$Год)
legend(max(events_gold$Год) - 1.5, 53, c("США", "Китай", "Япония", "Великобритания", "Россия", "Австралия", "Нидерланды"), fill=c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "gray70", "#2e3c54", "brown"))


events_prizes <- read.csv("priz.csv", fileEncoding = "Windows-1251", header = TRUE, sep = ";", check.names = F)

plot(events_prizes$Год, events_prizes$США, type="b", pch=19, col="#3be8b0", xaxt="n", ylim=c(0,130), xlab="Год", ylab="Медалей", main="Призовые медали за 6 последних олимпиад")
lines(events_prizes$Год, events_prizes$КИТАЙ, type="o", pch=19, col="#1aafd0")
lines(events_prizes$Год, events_prizes$Япония, type="o", pch=19, col="#6a67ce")
lines(events_prizes$Год, events_prizes$Великобритания, type="o", pch=19, col="#ffb900")
lines(events_prizes$Год, events_prizes$Россия, type="o", pch=19, col="gray70")
lines(events_prizes$Год, events_prizes$Австралия, type="o", pch=19, col="#2e3c54")
lines(events_prizes$Год, events_prizes$Нидерланды, type="o", pch=19, col="brown")
axis(side=1, at=events_prizes$Год)
legend(max(events_prizes$Год) - 1, 137, c("США", "Китай", "Япония", "Великобритания", "Россия", "Австралия", "Нидерланды"), fill=c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "gray70", "#2e3c54", "brown"))

#выделим призовые места за последние 6 лет
prize_6_m <- tail(prize_m, 6)
prize_6_f <- tail(prize_f, 6)

par(mfrow=c(1,3))
plot(prize_6_m, type="b", pch=19, col="navyblue", xaxt="n", ylim=c(0,7), main="Призовые места России по вольной борьбе\nза последние 6 ОИ")
lines(prize_6_f, type="o", pch=11, col="hotpink")
legend(min(prize_6_f$Год), 7.2, cex=0.7 ,c("Мужчины", "Женщины"), fill=c("navyblue", "hotpink"))
axis(side=1, at=prize_m$Год)

prize_grouped = data.frame(Призовых_М=prize_6_m$Призовых, Призовых_Ж=prize_6_f$Призовых)
barplot(height=t(as.matrix(prize_grouped)), beside=TRUE, xlab="Год", ylab="Количество", names.arg=prize_6_f$Год, col=c("navyblue", "hotpink"), main="Количество призовых мест России\nпо вольной борьбе за последние 6 ОИ")

prize_6_sum <- sapply(prize_grouped, sum)
pie(prize_6_sum, labels=c(prize_6_sum["Призовых_М"], prize_6_sum["Призовых_Ж"]), col=c("navyblue", "hotpink"), main="Всего призовых мест у М и Ж из России\nпо вольной борьбе за последние 6 ОИ")

