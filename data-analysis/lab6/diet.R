# Дисперсионный анализ. 
# Загрузим данные
data = read.csv("data/diet.csv",row.names=1)
summary(data)
# Ознакомимся со структурой и переименуем колонки, как нам удобно
colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])
# Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight
# Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

# Проверим сбалансированность данных
table(data$diet.type)

# График групповых средних
library(gplots) # при необходимости нужно установить библиотеку с помощью install.packages(gplots)
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)

# Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
# Тест на межгрупповые различия
fit <- aov(weight.loss ~ diet.type, data=data)
summary(fit)

# Отобразим попарные различия между средними значениями для всех групп
TukeyHSD(fit)

# Проведем тест Тьюки на существенные различия
library(multcomp) # при необходимости нужно установить библиотеку с помощью install.packages("multcomp")
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
# Видим, что диеты A и B не отличаются друг от друга
plot(cld(tuk, level=.05), col="lightgrey")

# Задание
# Найдем выбросы и избавимся от них:
data_new<-data[data$weight.loss<=8,]
# Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data_new,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

# Проверим сбалансированность данных
table(data_new$diet.type)

# График групповых средних
plotmeans(weight.loss ~ diet.type, data=data_new)
aggregate(data_new$weight.loss, by = list(data_new$diet.type), FUN=sd)

# Тест на межгрупповые различия
fit <- aov(weight.loss ~ diet.type, data=data_new)
summary(fit)

# Отобразим попарные различия между средними значениями для всех групп
TukeyHSD(fit)

# Проведем тест Тьюки на существенные различия
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
# Видим, что диеты A и B все также не отличаются друг от друга
plot(cld(tuk, level=.05), col="lightgrey")

# Вывод: Cущественных различий в результатах удаление выбросов не дает. Диета С осталась наиболее эффективной

# Проанализируем, зависит ли похудение от пола
# Для этого необходимо удалить выбросы
data <- na.omit(data) 
boxplot(weight.loss ~ gender, data = data, col="light gray",
        ylab = "Weight loss (kg)", xlab = "Gender")
aggregate(data$weight.loss, by = list(data$gender), FUN=sd)

# Тест на межгрупповые различия
fit <- aov(weight.loss ~ gender, data = data)
summary(fit)
# Для пола Pr(>F) = 0.835, т.е. пол не влияет на похудение

# Проанализируем зависимость похудения от диеты, пола, роста, возраста
fit <- aov(weight.loss ~ diet.type + height + age, data = data)
summary(fit)
fit <- aov(weight.loss ~ diet.type + gender + height, data = data)
summary(fit)
fit <- aov(weight.loss ~ diet.type + gender + height + age, data = data)
summary(fit)

# Поскольку все значения Pr(>F) достаточно большие, то можно сделать вывод, что на похудение влияет только тип диеты
