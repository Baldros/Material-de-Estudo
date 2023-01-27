# Crie o vetor de dados
data <- c(2, 2, 3, 10, 13, 14, 15, 15, 16, 16,
          18, 18, 20, 21, 22, 22, 23, 24, 25, 25,
          26, 27, 29, 29, 30, 32, 36, 42, 44, 45,
          45, 46, 48, 52, 58, 59, 61, 61, 61, 65,
          66, 66, 68, 75, 78, 80, 89, 90, 92, 97)

length(data)

# Defina os bins como intervalos de tamanho 20
breaks <- seq(0, 100, by = 20)

# Crie o histograma com bins de tamanho 20
hist(data, breaks = breaks)

#Media:
mean(data)

#Mediada:
median(data)

#Variancia:
var(data)

#Desvio Padrão:
sd(data)

#Versão do R:
R.version.string

summary(data)
