# 1. Cargar los datos a R y las librerías que utilizarán.
library(readxl)
library(dplyr)
library(tidyr)
file_name <- "Bank_Personal_Loan_Modelling.xlsx"

data <- read_excel(file_name, sheet = "Data")
dimensions <- read_excel(file_name, sheet = "Description")


# 2. Realizar exploración de datos.
glimpse(data)
glimpse(dimensions)


# 3. Construir un dataframe train y un dataset test
train <- data %>% sample_frac(0.7)
test <- data %>% sample_frac(0.3)

# 4. Construir al menos 2 modelos con Naive Bayes.
# 5. Calcular el Acurracy de ambos modelos
# 6. Calcular el Recall
# 7. Calcular el Precision
# 8. Como interpretaría el Acurracy, Recall y Precision
# 9. ¿Con cuál de los modelos que construyó se quedaría y por qué?


# Read the data
