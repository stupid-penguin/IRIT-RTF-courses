# Проанализируем данные о возрасте и физических характеристиках молюсков
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=TRUE, sep=",")
summary(data)
colnames(data)
colnames(data) <- c("sex", "length", "diameter", "height", 
                    "whole_weight", "shucked_weight",
                    "viscera_weight", "shell_weight", "rings")

colnames(data)
data$sex <- as.factor(data$sex)
par(mfrow=c(1,3)) 
hist(data$diameter, main = "Диаметр, мм")
hist(data$height, main = "Высота, мм")
hist(data$whole_weight, main = "Полный вес, гр")

# Видим ассиметрию и выбросы
boxplot(data$diameter, main = "Диаметр, мм")
boxplot(data$height, main = "Высота, мм")
boxplot(data$whole_weight, main = "Полный вес, гр")

# Найдём строки с выбросами
# Выбросы хранятся тут: boxplot.stats(y)$out
outliers_weight <- which(data$whole_weight %in% boxplot.stats(data$whole_weight)$out)
outliers_height <- which(data$height %in% boxplot.stats(data$height)$out)
outliers_diameter <- which(data$diameter %in% boxplot.stats(data$diameter)$out)
# Найдём все уникальные строки выбросов
outliers <- unique(c(outliers_weight, outliers_height, outliers_diameter))
# Удалим эти выбросы
results_data = data[-c(outliers),]

# Проверим, удалились ли выбросы
par(mfrow=c(1,3)) 
boxplot(results_data$diameter, main = "Диаметр, мм")
boxplot(results_data$height, main = "Высота, мм")
boxplot(results_data$whole_weight, main = "Полный вес, гр")

# Поскольку в колонке веса все ещё сохранились выбросы, проделаем ту же операцию, пока не удалим все выбросы
outliers_weight <- which(results_data$whole_weight %in% boxplot.stats(results_data$whole_weight)$out)
results_data = results_data[-c(outliers_weight),]
outliers_weight <- which(results_data$whole_weight %in% boxplot.stats(results_data$whole_weight)$out)
results_data = results_data[-c(outliers_weight),]

# В последний раз проверим, удалились ли выбросы
par(mfrow=c(1,3)) 
boxplot(results_data$diameter, main = "Диаметр, мм")
boxplot(results_data$height, main = "Высота, мм")
boxplot(results_data$whole_weight, main = "Полный вес, гр")
# Теперь наши данные чистые, и мы можем приступать к анализу
# Будем работать с данными results_data, всего осталось 4076 объектов

# Визулизируем возможные зависимости
par(mfrow=c(1,3)) 
plot(results_data$diameter, results_data$whole_weight,'p',main = "Зависимость веса от диаметра")
plot(results_data$height, results_data$whole_weight,'p',main = "Зависимость веса от высоты")
plot(results_data$height, results_data$diameter,'p',main = "Зависимость высоты от диаметра")

# Хорошо видна зависимость, займёмся её исследованием 
# Построим линейные модели при помощи функции lm, посмотрим их характеристики
par(mfrow=c(1,4))
# Зависимость диаметра от веса
lm_diameter_weight <- lm(diameter~whole_weight, data = results_data)
plot(lm_diameter_weight)
summary(lm_diameter_weight)
# Зависимость диаметра от высоты
lm_diameter_height <- lm(diameter~height, data = results_data)
plot(lm_diameter_height)
summary(lm_diameter_height)
# Зависимость веса от высоты
lm_weight_height <- lm(whole_weight~height, data = results_data)
plot(lm_weight_height)
summary(lm_weight_height)

#Деление массива на две части и прогнозирование значений
results_data_random <- results_data
odds <- seq(1, nrow(results_data_random), by=2)
train <- results_data_random[odds,]
test <- results_data_random[-odds,]

par(mfrow=c(1,4))
linear_model_half <- lm (diameter ~ ., data=train)
plot(linear_model_half)
summary(linear_model_half)

# Анализ тренировочных и тестовых данных
data_predict <- predict(linear_model_half)
cor(train$diameter, data_predict)

data_predict_out <- predict(linear_model_half, test)
cor(test$diameter, data_predict_out)

par(mfrow=c(1,2))
plot(train$diameter, data_predict, main = "Тренировочные данные")
plot(test$diameter, data_predict_out, main = "Тестовые данные")
