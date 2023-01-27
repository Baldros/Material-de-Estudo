#Testes:
x <- c(3,5)

mean(x)
    var(x)
sd(x)
soma = 0
for (i in x){
  valor = ((i-mean(x))**2)/length(x)
  soma = soma + valor
  print(soma)
}


v = c(54,50,48,32,30,30,29,27,24,24,24,23,21,21,16)
e = c(5,2,2,1,4,4,3,3,1,3,4,2,3,1,2)

soma = 0
for (i in e){
  valor = i**2
  soma = soma + valor
}

cor.test(e,v,method='spearman')
cov(e,v)
