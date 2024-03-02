library(readxl)
library(dplyr)
library(e1071)
library(rpart)
library(randomForest)
library(weights)


setwd("/Users/yagdrassyl/Documents/Code/University/MBIA/DataMining/exam")
data <- read_excel("Datos_membresia.xlsx")

# Check the data
glimpse(data)
data$Acepta_membresia <- ifelse(data$Acepta_membresia == "si", 1, 0)

# Desarrolle un modelo que logre clasificar la respuesta de los clientes
# ante una invitación para suscribirse a una membresía en un club campestre...
# (Utilizando los datos proporcionados en el archivo "Datos_membresia.xlsx".

# Classify models: Logistic Regression, Decision Tree, Random Forest, SVM

# naive Bayes

set.seed(666)

# Split the data into training and testing sets
smp_size <- floor(0.70 * nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

naive_bayes_model <- naiveBayes(train$Acepta_membresia ~ ., data = train)
predNB_Train <- predict(naive_bayes_model, newdata = train, type = "raw")
predNB_Test <- predict(naive_bayes_model, newdata = test, type = "raw")


# Decision Tree
tree_model <- rpart(Acepta_membresia ~ ., data = train, method = "class")


# Random Forest
random_forest_model <- randomForest(Acepta_membresia ~ ., data = train, importance = F, type = "class")

# Logistic regression
logistic_regression_model <- glm(Acepta_membresia ~ ., data = train, family = "binomial")
