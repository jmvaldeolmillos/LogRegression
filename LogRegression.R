# leemos y cada blanco, lo cambiamos por un NA.
train.data.raw <- read.csv("data/train.csv", header=T, na.strings = c(""))

# miramos cuantos na hay. Interesante para controlar si los datos están correctos.
sapply(train.data.raw, function(x) sum(is.na(x)))
# unique nos devuelve los valores únicos. Para ver los tipos de variables diferentes.
sapply(train.data.raw, function(x) length(unique(x)))

# Paquete Amelia para plotear los missing values.
library(Amelia)
missmap(train.data.raw, main="Missing values vs observed")
# Cabin y age son los que tienen más datos perdidos. Como sacamos anteriormente.

# Seleccionamos las columnas con mejores datos (quitando las no necesarias y las que tienen muchos missing)
data <- subset(train.data.raw, select = c(2,3,5,6,7,8,10,12))

# Rellenamos los missing de edad con su media (una forma de trabajar los missing values)
data$Age[is.na(data$Age)]<- mean(data$Age, na.rm=T)
missmap(data, main="Missing values vs observed") # todas NA rellenadas con la media de la columna.

# Miramos que esté todo ok. Las variables numéricas y categóricas están OK.
str(data)
contrasts(data$Sex)
contrasts(data$Embarked)

# Eliminamos las missing de Embarked y eliminamos los rownames (numeros)
data <- data[!is.na(data$Embarked),] # No parecía haber, pero hay dos que no muestra el gráfico.
rownames(data) <- NULL

# Generamos test y training para aplicar el modelo
filas.entrenamiento <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[filas.entrenamiento,]
test  <- data[-filas.entrenamiento,]

# Aplicamos el modelo. family = binomial. Supervivencia en función del resto.
model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = train)

# Vemos el resultado:
summary(model)

# De SibSp a Embarked no son estadísticamente significativas.
# El sexo es el de menor p. Fuerte asociación del sexo con la supervivencia. Hombre menor posibilidad de supervivencia.

# Uso de ANOVA para analizar la tabla de desviación.
anova(model, test = "Chisq")

# 

# Aunque no es exactamente igual a R2, el uso de McFaddden R2 se puede usar para evaluar el ajuste del modelo.
library(pscl)
pR2(model)

# Vamos a aplicar y evaluar el modelo para predecir con los datos nuevos.
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

# Miramos el nivel de precisión
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

# Da un valor de 0.84 que es bueno. Sería conveniente aplicar validación cruzada. Además la elección de datos no fue con sample.

# Trazamos la curva ROC y calculamos AUC (área bajo la curva)
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# un modelo con buena capacidad de predicción debe tener una AUC más cercano a 1 (1 es ideal) de hasta 0,5