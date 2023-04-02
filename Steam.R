################################################################################
###################     Game Recommendations on Steam     ######################
################################################################################
# https://www.kaggle.com/datasets/antonkozyriev/game-recommendations-on-steam

#==============================================#
# 0. Importar datos de CSV      
#==============================================#

# games <- read.csv("data/games.csv", header = TRUE)  # Importar
# View(games)                                         # Visualizar (46068 × 13)

#==============================================#
# A) ESTRUCTURA DE DATOS      
#==============================================#

# 1. Crear vectores con los títulos de la dataset
games_names <- names(games)                         # Encabezados de columnas

# 2. Crear vector numérico con el precio final de los videojuegos
precio_final <- games$price_final                   # nombre_del_objeto$nombre_de_la_columna

# 3. Crear una condición lógica para precios bajos (menores a 10.99)
cond_precios_bajos <- precio_final < 10.99
precios_bajos <- precio_final[cond_precios_bajos]

# 4.Sumar 5 al vector de precios bajos
precios_bajos_mas_5 <- precios_bajos + 5

# 5. Dividir la puntuación entre 2 (Adopto ranking como puntuación)
# Convierto los valores categoricos a numericos

# Definir los niveles del factor en el orden deseado
levels <- c("Overwhelmingly Negative", "Very Negative", "Negative", "Mostly Negative", "Mixed", "Mostly Positive", "Positive", "Very Positive", "Overwhelmingly Positive")

# Crear un factor con los niveles y los valores categóricos de la columna 'rating'
factor_rating <- factor(games$rating, levels=levels)

# Crear una nueva columna 'rating_numerico' con los valores numéricos correspondientes a los niveles del factor 'factor_rating'
games$rating_numerico <- as.integer(factor_rating)

# Creo la columna games$score = rating_numerico/2
games$score <- games$rating_numerico/2

# 6. Calcular la media, moda, max, min de los datos de tipo numérico (Verficar con la función Class)
sapply(games, class)
# Seleccionar solo las columnas numéricas
games_numeric <- games[, sapply(games, is.numeric)]
games_numeric
# Calcular la media, moda, max y min de cada columna numérica
# Crear una lista con los resultados para cada columna numérica
resultados <- lapply(games_numeric, function(x) {
  media <- mean(x)
  # La función mode no está disponible en R base, por lo que podemos crear una función personalizada
  mode_func <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  moda <- mode_func(x)
  maximo <- max(x)
  minimo <- min(x)
  c(media, moda, maximo, minimo)
})

resultados
resultados_names <- names(resultados)
# Convertir la lista en un dataframe
summary_games <- as.data.frame(resultados)

# Nombrar las filas y las columnas del dataframe
colnames(summary_games) <- c("app_id","positive_ratio","user_reviews","price_final","price_original","discount","rating_numerico","score")
rownames(summary_games) <- c("media", "moda", "maximo", "minimo")

# Imprimir el dataframe
summary_games

# 7. Crear un dataFrame de 13 col con la base de datos y guardar en una nueva variable
# Crear un nuevo dataframe llamado games2 que excluya las columnas "rating"y "rating_numerico"
games2 <- games[, !(names(games) %in% c("rating", "rating_numerico"))]

# 8. Agregar filas y columnas a la matriz 
# 9. Agregar columna de "1"
games2$columna_de_uno <- 1
# 10. Agregar fila con los datos de un video juego de tu preferencia
# Crear una nueva fila con datos
new_row <- c(max(games2$app_id) + 1, "Left4Dead3", format(Sys.Date()), "true", "true", "true", 100, 1, 0, 0, 0, "true", 5, 1)
# Agregar la nueva fila al final del dataframe
games2 <- rbind(games2, new_row)

# 11. Eliminar filas y columnas de la matriz
# Eliminar última fila de games2
games2 <- games2[-nrow(games2),]
# Eliminar última columna de games2
games2 <- games2[,-ncol(games2)]

# 12. Seleccionar los elementos de la matriz
# Seleccionar la primera fila de games2
primer_fila <- head(games2, n=1)
# Seleccionar la primera columna de games2
primer_columna <- games2[,1]
# Seleccionar el primer elemento de la última columna de games2
primer_elem_ult_col <- games2[nrow(games2), ncol(games2)][1]
# Seleccionar el último elemento de la primera columna de games2
ultimo_elem_prim_col <- games2[nrow(games2), 1]

# 13. Convertir la matriz en data.frame y asignar nombres a las columnas
# Convertir el dataframe en una matriz
matriz <- as.matrix(games2)
games3 <- as.data.frame(matriz)
# Asignar nombres a las columnas
colnames(games3) <- c("id",'title','date_release','win','mac','linux','positive_ratio','reviews','price_final','price_original','discount','steam_deck','score')
# 14. Acceder a los datos del dataframe
games3
# 15. Cambiar nombre de dataframe
# Cambiar el nombre de games3 a games_review
games_review <- games3

# 16. Seleccionar un elemento del dataframe
ultimo_elem_prim_col_games3 <- games2[nrow(games3), 1]

#==============================================#
# B) IMPORTAR DATOS      
#==============================================#
# 1. Importar Datos desde Excel y Ordenar los datos con la función order(), de preferencia para la variable Price_final
# games <- read.csv("games.csv", header = TRUE)
games_ordenados <- games[order(games$price_final, decreasing = TRUE),]

# 2. Mostrar el dataframe ordenado de manera ascendente y descendente
games_ascendente <- games[order(games$price_final),]

# 3. Calcular el resumen estadístico de los datos con la función que corresponde
summary(games)

#4. Realizar las graficas
# Convierto los datos date_realease a formato fecha
date_release <- games$date_release
date_release <- as.Date(date_release)
class(date_release)

# Grafico el histograma de lanzamientos
hist(date_release, breaks = "year", freq = TRUE, 
     main = "Histograma de fechas de lanzamiento de juegos", 
     xlab = "Fecha de lanzamiento", ylab = "Frecuencia", xlim = range(date_release))

# Grafico de torta de win
win <- games$win
# win <- as.logical(win == "true")
# Obtener la tabla de frecuencias de la columna 'win'
freq_table <- table(win)
# Calcular los porcentajes
percentages <- round(freq_table / sum(freq_table) * 100, 2)
# Etiquetas
labels <- paste(names(freq_table), "\n", freq_table, "\n", percentages, "%")
# Graficar el gráfico de torta con etiquetas y porcentajes
pie(freq_table, main = "Gráfico de torta de la columna 'win'", labels = labels, col = c("red", "blue"), clockwise = TRUE)
legend("bottomright", c("no", "yes"), cex = 0.8, fill = c("red", "blue"))

# Grafico de torta de mac
mac <- games$mac
# mac <- as.logical(mac == "true")
# Obtener la tabla de frecuencias de la columna 'mac'
freq_table <- table(mac)
# Calcular los porcentajes
percentages <- round(freq_table / sum(freq_table) * 100, 2)
# Etiquetas
labels <- paste(names(freq_table), "\n", freq_table, "\n", percentages, "%")
# Graficar el gráfico de torta con etiquetas y porcentajes
pie(freq_table, main = "Gráfico de torta de la columna 'mac'", labels = labels, col = c("red", "blue"), clockwise = TRUE)
legend("bottomright", c("no", "yes"), cex = 0.8, fill = c("red", "blue"))

# Grafico de torta de linux
linux <- games$linux
# linux <- as.logical(linux == "true")
# Obtener la tabla de frecuencias de la columna 'linux'
freq_table <- table(linux)
# Calcular los porcentajes
percentages <- round(freq_table / sum(freq_table) * 100, 2)
# Etiquetas
labels <- paste(names(freq_table), "\n", freq_table, "\n", percentages, "%")
# Graficar el gráfico de torta con etiquetas y porcentajes
pie(freq_table, main = "Gráfico de torta de la columna 'linux'", labels = labels, col = c("red", "blue"), clockwise = TRUE)
legend("bottomright", c("no", "yes"), cex = 0.8, fill = c("red", "blue"))

# Distribucion de Score
score = games2$score
score <- as.numeric(score)
# Grafico el histograma de puntuaciones
hist(score, freq = TRUE, breaks = 10, main = "Histograma de puntuaciones")

# Histograma de positive_ratio
positive_ratio = games$positive_ratio
hist(positive_ratio, freq = TRUE, main = "Histograma de postitive_ratio")

# Boxplot de user_reviews
boxplot(games$user_reviews, main = "Boxplot de user_reviews")
# Eliminar el outlier maximo de user_reviews
# Identificar el valor máximo de games$user_reviews
max_value <- max(games$user_reviews, na.rm = TRUE)
# Crear un nuevo vector sin el valor máximo
new_vector <- games$user_reviews[games$user_reviews < max_value]
# Crear un boxplot con el nuevo vector
boxplot(new_vector)

# boxplot de Precio final
boxplot(games$price_final, main = "Boxplot de precio final")

#==============================================#
# C) PROGRAMACION      
#==============================================#

# 1. Implementar una función para la multiplicación de dos vectores(xy) y probar con valores
multiply <- function(x, y) {return(x * y)}
# Creamos dos vectores
x <- c(1, 2, 3)
y <- c(4, 5, 6)
# Llamamos a la función multiply
z <- multiply(x, y)
# Imprimimos el resultado
print(z)

# 2. Implementar una función que muestre el resultado de la ecuación de Bhaskara y probar con valores.
bhaskara <- function(a, b, c) {discriminante <- b^2 - 4*a*c
if (discriminante < 0) {
  cat("No existen soluciones reales\n")} 
else {x1 <- (-b + sqrt(discriminante))/(2*a)
x2 <- (-b - sqrt(discriminante))/(2*a)
cat("Las soluciones son x1 =", x1, " y x2 =", x2, "\n")}
}
bhaskara(1, -4, 3)

# 3. Se quiere conocer la media muestral de n observaciones obtenidas independientemente de una distribución normal con media = 0 y varianza =1.
# 3.1. Realizar una simulación, luego calcular las estadísticas descriptivas aplicando la función que corresponde y graficar.
# Para realizar la simulación, se puede utilizar la función rnorm() de R, 
# que genera valores aleatorios de una distribución normal con media y desviación estándar especificadas. 
# En este caso, se quiere simular n observaciones de una distribución normal con media 0 y varianza 1, lo que se puede hacer de la siguiente manera:
n <- 1000  # número de observaciones
simulacion <- rnorm(n, mean = 0, sd = 1)
# Luego, se pueden calcular las estadísticas descriptivas utilizando las funciones mean(), sd(), min(), max(), median(), entre otras. Por ejemplo:
media <- mean(simulacion)
desviacion <- sd(simulacion)
minimo <- min(simulacion)
maximo <- max(simulacion)
mediana <- median(simulacion)
# Finalmente, se puede graficar la distribución de los valores simulados utilizando un histograma:
hist(simulacion, main = "Distribución de n observaciones de una N(0,1)", xlab = "Valor", ylab = "Frecuencia")