setwd("/home/joao/Documents/BIG/v2/")
#install.packages("keras")
library(plyr)
library(keras)

model <- keras_model_sequential() %>%
  layer_dense(units=64, activation = "relu", input_shape = 2) %>%
  layer_dense(units=32, activation = "relu") %>%
  layer_dense(units=1, activation = "linear")

model %>% compile(
  loss = "mse",
  optimizer = "adam",
  metrics = list("mean_absolute_error")
)

model %>% summary()

data <- read.csv("data/food-choices/clean_data.csv")[,-1]
colnames(data) <- gsub(".", ' ',colnames(data), fixed = TRUE)

metrics <- read.csv("data/weight-height.csv")
metrics$Height <- 2.54*metrics$Height
metrics$Weight <- 0.453592*metrics$Weight

sex <- as.numeric(revalue(metrics$Gender, c("Female" = 1, "Male" = 0)))

sd(metrics$Height)

model %>% fit(as.matrix(data.frame(sex, metrics$Weight)), metrics$Height, epochs = 100, verbose = 0)
scores = model %>% evaluate(as.matrix(data.frame(sex, metrics$Weight)), metrics$Height, verbose = 0)
print(scores)
height_predict <- model %>% predict(as.matrix(data.frame(sex, metrics$Weight)))

ggplot(data=data.frame(metrics$Weight, metrics$Height, height_predict), aes(x=metrics.Weight)) + 
  geom_point(aes(y=metrics.Height), color="blue") + 
  geom_point(aes(y=height_predict), color="red")

x_peso <- data$Peso
x_sexo <- as.numeric(revalue(data$Sexo, c("Masculino" = 0, "Feminino" = 1)))
altura_predict <- model %>% predict(as.matrix(data.frame(x_sexo, x_peso)))
head(altura_predict)
data$altura <- round((altura_predict + runif(125, 0, sd(metrics$Weight)))/100, 2)
data$altura

ggplot(data, aes(x=Peso, y=altura)) + 
  geom_point(aes(color = Sexo))

ggplot(data, aes(x=altura)) + 
  geom_histogram(breaks = seq(min(data$altura), max(data$altura), by=0.05), aes(y=..density..))

altura <- as.data.frame(data$altura, col.names = c("Altura"))
head(altura)
colnames(altura) <- c("Altura")
write.csv(altura, file = "data/altura.csv")
