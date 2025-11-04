library(clickR)

#Carga de datos
datos_raw <- read.csv2("messy_IMDB_dataset.csv", encoding="latin1")

#Después de cargar los datos podemos hacer un descriptivo inicial para ver los
#problemas más evidentes
descriptive(datos_raw)

#Vemos que es un auténtico desastre...

#Lo primero es arreglar los nombres de las variables, ya que son poco manejables
names(datos_raw)

#La función nice_names() estandariza los nombres de variables (quita caracteres como 
#acentos, apóstrofes, espacios en blanco, puntos...)
datos <- nice_names(datos_raw)
names(datos)

#Revisamos el registro de cambios mediante la función track_changes()
track_changes(datos)

#2º Problema: Hay valores faltantes que no se han reconocido como tal por R (por ejemplo,
#pueden ser espacios en blanco, ND, ?, -, NULL, etc.). Podemos arreglarlo con fix_NA()
datos <- fix_NA(datos)

#track_changes incluye ahora las modificaciones realizadas por fix_NA()
track_changes(datos)

#Podemos filtrar el listado de track_changes() con expresiones lógicas
track_changes(datos, fun == "fix_NA")
track_changes(datos, variable == "duration")
track_changes(datos, variable == "duration" & fun == "fix_NA")

#Podemos obtener un "mapa" de valores faltantes con la función mine.plot()
mine.plot(datos)

#Esta función nos permite guardar sus resultados en un listado
missing_data <- mine.plot(datos)
missing_data$list
missing_data$summary

#La función mine.plot() en realidad no es exclusiva para missing data. Puede utilizarse
#cualquier otra función cuya evaluación sea un resultado lógico TRUE o FALSE
mine.plot(iris, function(x) x > 7, main="Valor > 7")
mine.plot(iris, function(x) x == "virginica", main="Valor = virginica")
mine.plot(iris[,-5], function(x) x > mean(x, na.rm=TRUE) + 2*sd(x, na.rm=TRUE), main="Outliers")
mine.plot(iris, function(x) tryCatch(x > mean(x, na.rm=TRUE) + 2*sd(x, na.rm=TRUE), error=function(e) rep(NA, length(x))), main="Outliers")

#En el mine.plot de los datos vemos que hay una columna en blanco 
#(100% de missings), así como una fila completamente en blanco. POdemos eliminarlas
#con la función remove_empty()
datos <- remove_empty(datos)

#Podemos ver los cambios realizados por la función
track_changes(datos, fun=="remove_empty")

#Si volvemos a nuestro descriptivo:
descriptive(datos)

#La variable release_year parece una fecha, pero no ha sido reconocida como tal. La función
#fix_dates() detecta todas las columnas con posibles fechas y las arregla
datos$release_year
datos <- fix_dates(datos)

#Revisamos los cambios
track_changes(datos, fun == "fix_dates")

#Vemos que hay dos fechas no convertidas, porque son fechas imposibles. También vemos un posible
#error, ya que la observación 6 la detecta como 4 de febrero de 2022 y es posible que sea
#22 de febrero de 2004. Habría que comprobarlo.


#Volvamos al descriptivo
descriptive(datos)

#Seguimos detectando problemas, esta vez con algunas variables que deberían ser
#numéricas como 'duration', 'income', 'votes' o 'score'. Podemos
#arreglar estos problemas en variables numéricas con la función fix_numerics()
datos <- fix_numerics(datos)
track_changes(datos, fun == "fix_numerics")
track_changes(datos, fun == "fix_numerics" & variable == "duration")
track_changes(datos, fun == "fix_numerics" & variable == "income")
track_changes(datos, fun == "fix_numerics" & variable == "votes")
track_changes(datos, fun == "fix_numerics" & variable == "score")
track_changes(datos, fun == "fix_numerics" & variable == "imbd_title_id")

#En este caso observamos dos problemas. Primero, la variable imbd_title_id ha sido
#convertida en numérica, cuando en realidad es un identificador. Podemos deshacer 
#esta conversión con la función restore_changes()
datos <- restore_changes(track_changes(datos, fun == "fix_numerics" & variable == "imbd_title_id"))
datos$imbd_title_id
track_changes(datos, fun == "fix_numerics" & variable == "imbd_title_id")

#El otro problema son algunos de los valores de score, que no han sido convertidos
#por fix_numerics(). Podemos arreglarlos manualmente con la función manual_fix().
#La conveniencia de manual_fix() es que registra los cambios realizados en el listado
#de track_changes()

datos <- manual_fix(datos, "score", rownames(datos) %in% c("11", "15", "16"), c(8.8, 8.7, 8.7))
track_changes(datos, fun == "manual_fix")
track_changes(datos, observation %in% c("11", "15", "16") & variable == "score")

descriptive(datos)

#Después de arreglar los problemas con las variables numéricas y las fechas, nos quedaría
#revisar si hay algún problema con los factores. Las variables 'content_rating', 'country' y
#'genre' deberían ser consideradas factores. Sin embargo, si las revisamos, aparecen como 'character'
datos$country
datos$content_rating
datos$genre
unique(datos$country)
datos <- fix_factors(datos, k=18)
track_changes(datos, fun == "fix_factors")

datos$country
datos$content_rating


#Ahora ya tenemos las variables country y content ratin como factores, sin embargo, si revisamos bien, vemos que
#la variable 'country' sigue con problemas. Hay paises mal escritos, lo que hace que haya
#niveles de más en el factor.
levels(datos$country)

#Podemos arreglar esto con la función fix_levels(). A diferencia de las otras funciones fix
#vistas hasta ahora, fix_levels() funciona sobre una única columna y no sobre el data.frame
fix_levels(datos, "country", k=10, plot=TRUE)

#Vemos que 10 niveles se queda corto, vamos a probar con 13
fix_levels(datos, "country", k=13, plot=TRUE)

#Así está perfecto, por lo que podemos asignar
datos <- fix_levels(datos, "country", k=13, levels="auto")
track_changes(datos, fun == "fix_levels")

descriptive(datos)


#Vamos a revisar el otro factor 'content_rating'
levels(datos$content_rating)

#Los valores '#n/a", "not rated" y "unrated" podrían tal vez agruparse en una sola categoría
#o ser marcados como NA
fix_levels(datos, "content_rating", k = 7, plot= TRUE, method = "jaccard")

datos <- fix_levels(datos, "content_rating", k = 7, method = "jaccard", levels=c("approved", "pg", "pg-13", "r", "g"))
track_changes(datos, fun == "fix_levels")

#Por último, nos queda un último problema con la variable genre
datos$genre

#Podemos crear una serie de variables dummy para cada uno de los géneros de manera que
#el género sea un parámetro analizable con la función fix_concat(). Al igual que 
#en el caso de fix_levels(), fix_concat() se aplica sobre una columna del data.frame.
datos <- fix_concat(datos, "genre")
track_changes(datos, fun == "fix_concat")

#Ahora que ya hemos arreglado los datos, podemos exportarlos en csv para compartir
write.csv2(datos, "IMDB_clean_data.csv")

#Junto con un registro de todos los cambios realizados sobre los datos originales
write.csv2(track_changes(datos), "changes_to_IMDB_messy_data.csv", row.names = FALSE)

#Otras funciones interesantes del paquete

#Operadores lógicos con NAs
datos[datos$duration <= 90,]
datos[datos$duration <= 90 & !is.na(datos$duration), ]
datos[datos$duration %<=NA% 90, ]

#existen %<=NA%, %<NA%, %>=NA% y %>NA% (%==NA% no es necesario porque se utiliza %in%)


#good2go() sirve para cargar todos los paquetes que se llaman en algún script de un
#proyecto determinado de Rstudio
clickR::good2go()

#outliers() es una función muy sencilla para detectar outliers según el método del boxplot
outliers(datos$votes)
datos$votes[outliers(datos$votes)]

#ipboxplot() es un boxplot que representa también las observaciones individuales
ipboxplot(votes ~ content_rating, data=datos, outline=FALSE)

#bivariate_outliers es un poco más compleja, sirve para detectar outliers bivariantes
bivariate_outliers(airquality)
ggplot(airquality, aes(x=Ozone, y=Temp, color=Ozone %in% 168 & Temp %in% 81)) + geom_point() + guides(color=FALSE)

#Una función para detectar asociaciones entre distintos tipos de variables
cluster_var(iris)

#Asociación entre dos variables categóricas (Goodman & Kruskal's Tau)
GK_assoc()

#Función para buscar en distintos scripts de R
search_scripts("cluster_var(iris)")

#Exploración rápida del workspace
workspace()
workspace_sapply()

#versión mejorada de head()
peek(datos, n=3)

#check_quality (aun en desarrollo), sirve para una comprobación de la calidad de los datos 
#con un exploratorio sencillo. 
check_quality(datos$votes)
check_quality(datos$release_year)
check_quality(datos$country)
check_quality(datos$income)
check_quality(datos_raw$Votes)
