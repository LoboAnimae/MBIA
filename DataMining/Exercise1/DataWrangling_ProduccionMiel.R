# This script demonstrates the steps taken to clean and

# prepare the honeyproduction dataset.



# Load Libraries ------------------------------------------------------

library(dplyr)

library(tidyr)

library(readr)

library(stringr)

library(data.table)

library(formattable)

library(readxl)


# Load Data -----------------------------------------------------------

# Tenemos 3 datasets con el mismo tipo de información de producción de miel
# y precios, la diferencia es que están divididos por grupos de años
# (1998-2002, 2003-2007 y 2008-2012).
#
# El archivo está un poco sucio, pero la buena noticia es que con data wrangling
# podemos limpiarlo facilmente.
#
# Ejercicio: aplicar DataWrangling y generar un arhivo tal cual el que se llama "Muestra limpia.xlsx" y responder a las preguntas:
# 1) ¿La tendencia de producción de miel va en aumento o en disminución?
# 2) ¿Qué estado presenta la mayor tendencia de crecimiento en producción de miel en los últimos 12 años?
# 3) ¿Cuál es el es estado con más ventas en 1998 y cual ha sido su tendencia desde entonces?

# Entregable: script con comentarios y word con respuestas.
# El proceso de data wrangling debe ser en R pero una vez que tenga los datos limpios
# puede utilizar R con ggplot o cualquier otro programa para presentar gráficas para respaldar sus respuestas.

# Bonus: El dataset1 ya lleva su proceso de limpieza listo

#--------------------------------------------------------------
#              Limpieza archivo de 1998 al 2002
#--------------------------------------------------------------

honeyraw9802 <- read_excel("honeyraw_1998to2002.xlsx", skip = 3)
honeyraw9802 <- data.frame(honeyraw9802[-1:-2])
str(honeyraw9802)


titulos <- honeyraw9802[c(1:3), ]
for (i in seq_len(nrow(titulos))) {
  nuevos <- titulos[i, ]
  if (i == 1) {
    titulos2 <- nuevos
  } else {
    titulos2 <- paste(titulos2, nuevos, sep = "-")
  }
}

titulos2 <- str_replace_all(titulos2, "-NA-", "")
titulos2 <- str_replace_all(titulos2, "NA-", "")
titulos2 <- str_replace_all(titulos2, "-NA", "")

dimensional <- honeyraw9802[5, ]
dimensional <- str_replace_all(dimensional, "1,000 Pounds", "1000")
dimensional <- str_replace_all(dimensional, "1,000 Dollars", "1000")
dimensional <- str_replace_all(dimensional, "Cents", "0.01")
dimensional <- str_replace_all(dimensional, "Pounds", "1")
dimensional <- as.numeric(dimensional)


honeyraw9802_2 <- honeyraw9802[-c(1:5), ]
colnames(honeyraw9802_2) <- titulos2

honeyraw9802_2[, 2:7] <- sapply(honeyraw9802_2[, 2:7], as.numeric)

for (i in 2:ncol(honeyraw9802_2)) {
  honeyraw9802_2[, i] <- honeyraw9802_2[, i] * dimensional[i]
}

honeyraw9802_2$nchar_estado <- nchar(honeyraw9802_2$State)

# los estados solo tienen 2 caracteres por lo que me quedo solo con los elementos que tengan 2 digitos
honeyraw9802_2 <- subset(honeyraw9802_2, nchar_estado == 2)

estados <- as.data.frame(unique(honeyraw9802_2$State))
names(estados) <- "Estados"

# Cada año empieza cuando aparece el estado AL y termina cuando llegamos a WY, y sabemos que el sistema de registro los genera en orden, por lo que voy a colocar un ciclo que le agregue el año dependiendo del estado
for (i in seq_len(nrow(estados))) {
  nueva_data <- subset(honeyraw9802_2, State == estados$Estados[i])
  nueva_data$contador <- 1
  nueva_data$contador <- cumsum(nueva_data$contador)
  nueva_data$contador <- nueva_data$contador - 1
  nueva_data$anio <- 1998 + nueva_data$contador
  if (i == 1) {
    honeyraw9802_3 <- nueva_data
  } else {
    honeyraw9802_3 <- rbind(honeyraw9802_3, nueva_data)
  }
}

honeyraw9802_3 <- honeyraw9802_3 %>%
  arrange(anio) %>%
  rename(
    numcol = `Honey-Producing-Colonies`,
    yieldpercol = `Yield-per-Colony`,
    totalprod = Production,
    stocks = `Stocks-Dec 15 2/`,
    priceperlb = `Average-Price per-Pound`,
    prodvalue = `Value-of-Production`,
    year = anio
  ) %>%
  select(State, numcol, yieldpercol, totalprod, stocks, priceperlb, prodvalue, year)

all_data <- honeyraw9802_3
# Libero RAM
rm(estados)
rm(honeyraw9802)
rm(honeyraw9802_2)
rm(honeyraw9802_3)
rm(nueva_data)
rm(nuevos)
rm(titulos)
rm(dimensional)
rm(i)
rm(titulos2)

#--------------------------------------------------------------
#              Limpieza archivo de 2003 al 2007
#--------------------------------------------------------------

# Erase the first two columns
dataset0307 <- read_excel("honeyraw_2003to2007.xlsx", skip = 19)
dataset0307 <- data.frame(dataset0307[, -c(1:2)])

titles <- dataset0307[c(1:3), ]



for (i in seq_len(nrow(titles))) {
  new_title <- titles[i, ]
  if (i == 1) {
    new_titles <- new_title
  } else {
    new_titles <- paste(new_titles, new_title, sep = "-")
  }
}

reg <- "\\-NA\\-|\\-NA|NA\\-"

new_titles <- str_replace_all(new_titles, reg, "")
colnames(dataset0307) <- new_titles
dataset0307 <- dataset0307[-c(1:4), ]

dimensions <- dataset0307[1, ]

dimensions <- str_replace_all(dimensions, "1,000 Pounds", "1000")
dimensions <- str_replace_all(dimensions, "1,000 Dollars", "1000")
dimensions <- str_replace_all(dimensions, "Cents", "0.01")
dimensions <- str_replace_all(dimensions, "Pounds", "1")
dimensions <- as.numeric(dimensions)

dataset0307 <- dataset0307[-1, ]
dataset0307[, 2:7] <- sapply(dataset0307[, 2:7], as.numeric)


# Only grab the states (The one that have 2 characters)
dataset0307$nchar_State <- nchar(dataset0307$State)
# Grab the ones where nchar_State is 2
dataset0307 <- subset(dataset0307, dataset0307$nchar_State == 2)

# Convert the data to the actual dimensions
for (i in 2:ncol(dataset0307)) {
  dataset0307[, i] <- dataset0307[, i] * dimensions[i]
}

# For each state, figure out their year. Figure out the states first.
states <- unique(dataset0307$State)
states <- as.data.frame(states)
dataset0307$nchar_State <- NULL

for (i in seq_len(nrow(states))) {
  # Grab only the tuples with the specific state
  state <- states[i, ]
  # Grab the rows from this same state
  state_rows <- subset(dataset0307, dataset0307$State == state)
  state_rows$counter <- 1
  state_rows$counter <- cumsum(state_rows$counter)
  state_rows$counter <- state_rows$counter - 1
  state_rows$year <- state_rows$counter + 2003

  if (i == 1) {
    new_data <- state_rows
  } else {
    new_data <- rbind(new_data, state_rows)
  }
}

new_data$counter <- NULL

new_data <- new_data %>%
  arrange(year) %>%
  rename(
    numcol = `Honey-Producing-Colonies 2/`,
    yieldpercol = `Yield-per-Colony`,
    totalprod = Production,
    stocks = `Stocks-Dec 15 3/`,
    priceperlb = `Average-Price per-Pound 4/`,
    prodvalue = `Value-of-Production 5/`
  ) %>%
  select(State, numcol, yieldpercol, totalprod, stocks, priceperlb, prodvalue, year)


all_data <- rbind(all_data, new_data)

rm(dataset0307)
rm(new_data)
rm(new_title)
rm(state_rows)
rm(states)
rm(titles)
rm(dimensions)
rm(i)
rm(new_titles)
rm(reg)
rm(state)

#--------------------------------------------------------------
#              Limpieza archivo de 2008 al 2012
#--------------------------------------------------------------

file_data <- read_excel("honeyraw 2008 - 2012.xlsx")

file_data[, 3:7] <- sapply(file_data[, 3:7], as.numeric)

file_data <- gather(file_data, year, ventas, -state, -Valores)
file_data <- spread(file_data, Valores, ventas)


glimpse(file_data)
glimpse(all_data)

colnames(file_data)
colnames(all_data)[1] <- "state"

all_data <- rbind(all_data, file_data)

all_data %>% filter(state == "MD")
data <- all_data %>% fill(numcol:prodvalue, .direction = "down")
data %>% filter(state == "MD")
data$year <- as.factor(all_data$year)
# data$year <- as.numeric(all_data$year)
save(data, file = "clean.Rdata")
