#===============================================================================
# PROJETO DE EXTENSÃO: PROCESSAMENTO E ANÁLISE DE DADOS AMBIENTAIS COM R
# CURSO: ESTATÍSTICA BÁSICA NO R
# E-MAIL: pexcca.lamet@uenf.br
# ATUALIZAÇÃO (SCRIPT): 15/12/2022
# https://linktr.ee/pexcca.lamet
#===============================================================================

# Caso o script abra com problemas na codificação (acentos não interpretados),
# clique em File > Reopen with Encoding > Escolher ISO-8859-1 ou UTF-8 > Ok

#-------------------------------------------------------------------------------
### Definindo o diretório de trabalho

# Diretório de trabalho é uma pasta no seu computador que interage com o R/RStudio.

getwd()	# Informa o diretório de trabalho atual.

# setwd() 	# Seta um local como novo diretório.
# Exemplo: setwd("C:/Users/Desktop/Minicurso R")

# No RStudio, também é possível definir o diretório:
# 1. Utilizando o atalho Ctrl + Shift + H.
# 2. Clicando em Session > Set Working Directory > Choose Directory.

dir()     # Lista os arquivos dentro do diretório de trabalho.

#-------------------------------------------------------------------------------
### Importação de dados
# Vamos utilizar os dados mensais de Macaé-RJ disponíveis no link abaixo: 
# https://drive.google.com/drive/u/3/folders/1ngqp7SVYtPiuwLwj8FRPCV3tF0FGIyN0

# Importação dos dados:
MAC <- read.csv(file = "Macaé.csv",  # Nome e extensão do arquivo que será importado.
                header = TRUE,       # Use header=TRUE para ler a primeira linha como cabeçalho das colunas.
                na.strings = "NA",   # na.strings: caracterer que deve ser interpretado como valor NA.
                sep = ";",           # Separador de colunas.
                dec = ",",           # Separador de decimais.
                fileEncoding = "ISO-8859-1")  # Especifica a codificação a ser usada.

head(MAC)

#-------------------------------------------------------------------------------
### Medidas de tendência central: média e mediana 

# Média aritmética: soma de todos os elementos dividida pela quantidade deles.

mean(MAC$Prec, na.rm = TRUE)

## Mediana: valor que separa os dados em duas partes iguais.
# 50% dos dados estão abaixo da mediana e 50% acima.

median(MAC$Prec, na.rm = TRUE)

# Função by e tapply: agrupa os dados de acordo uma variável a partir de uma função.

# Calcular a média por mês:
by(MAC$Prec, MAC$Mês, mean, na.rm = TRUE)  
tapply(MAC$Prec, MAC$Mês, mean, na.rm = TRUE)

# Calcular a médiana por mês:
by(MAC$Prec, MAC$Mês, median, na.rm = TRUE)  
tapply(MAC$Prec, MAC$Mês, median, na.rm = TRUE)

#-------------------------------------------------------------------------------
### Medidas de Posição (Quantis ou Separatrizes)

# Assim como a mediana, existem outras medidas que separam os dados em partes iguais, 
# denominados genericamente de quantis.

# Os quantis mais usados são:
# Quartis: dividem os dados em 4 partes (cada parte tem 25% dos dados).
quantile(MAC$Prec, na.rm = TRUE)

# Decis: dividem os dados em 10 partes iguais (cada parte tem 10% dos dados).
quantile(MAC$Prec, probs = seq(0, 1, by = 0.1), na.rm = TRUE)

# Percentis: dividem os dados em 100 partes iguais (cada parte tem 1% dos dados).
quantile(MAC$Prec, probs = seq(0, 1, by = 0.01), na.rm = TRUE)

#-------------------------------------------------------------------------------
### Medidas de dispersão: variância, desvio-padrão coeficiente de variação.

# Variância: é a soma dos desvios quadrados da média dividida pelo tamanho da amostra (menos um).
# Como utilizamos a soma dos desvios quadráticos, esta terá unidade de medida quadrática, 
# o que pode ser de difícil interpretação.

var(MAC$Prec, na.rm = TRUE)

# Desvio padrão: é a raiz quadrada da variância.
sd(MAC$Prec, na.rm = TRUE)   # Quanto mais próximo de 0 ele estiver, temos o desvio padrão mais homogêneo.

# Coeficiente de variação: fornece a variação dos dados obtidos em relação à média.
#  Quanto menor for o seu valor, mais homogêneos serão os dados.

(cv <- sd(MAC$Prec, na.rm = TRUE)/ mean(MAC$Prec, na.rm = TRUE) * 100)  

# O coeficiente de variação é dado em %, por isso a fórmula é multiplicada por 100.
# O fato do coeficiente de variação ser dado em termos relativos nos permite comparar
# séries de valores que apresentam unidades de medida distintas.

#-------------------------------------------------------------------------------
# Boxplot (diagrama de caixa): é um gráfico utilizado para avaliar a distribuição dos dados.

boxplot(MAC$Prec)
boxplot(MAC$Prec ~ MAC$Ano)
boxplot(MAC$Prec ~ MAC$Mês)

boxplot(MAC$Prec ~ MAC$Mês, 
        ylim = c(0,400), ylab = "Preciptiação (mm)", xlab = "Mês",
        col = "pink", outline = TRUE, range = 1.5)

# outline: se TRUE, plota os outliers. FALSE não plota os outliers.
# Usando range = 1.5, os outliers serão os valores fora de LI = Q1 - 1.5*(Q3 - Q1) e LS = Q3 + 1.5*(Q3 -Q1).
# Q1 = 1º quartil; Q3 = 3º quartil.

# O R usa como limites do boxplot:
# Limite Inferior (LI): o próximo valor existente da variável a partir do limite inferior calculado (Q1 - 1.5*(Q3 - Q1)).
# Limite Superior (LS): O valor anterior existente da variável a partir do limite superior calculado (Q3 + 1.5*(Q3 -Q1)).

#-------------------------------------------------------------------------------
### Teste de Hipóteses

# Uma hipótese estatística é uma suposição ou afirmação que pode ou não ser verdadeira, 
# relativa a uma ou mais populações. Com base em uma amostra aleatória da população de interesse 
# é estabelecido se a hipótese é provavelmente verdadeira ou provavelmente falsa.
# A decisão de que a hipótese é provavelmente verdadeira ou falsa é tomada com base nas distribuições amostrais.

# Testes bilaterais e unilaterais
# Teste bilateral: há interesse em identificar diferença para qualquer direção.
# Teste unilateral: apenas tem sentido diferença em uma direção.

#-------------------------------------------------------------------------------
# Passos para realizar um teste de hipótese:

# Passo 1 - Formular as hipóteses nula e alternativa.
# Hipótese Nula (H0): é um valor suposto para um parâmetro.
# Hipótese Alternativa (H1): é uma hipótese que contraria a hipótese nula, complementar de H0.

# Passo 2 - Escolher a distribuição amostral adequada (teste Z ou t). 
# O uso do teste Z ou T depende da quantidade das amostras que será usada na tomada de decisão.

# Teste Z: Usada quando temos amostras grandes (n ≥ 30) e desvio-padrão populacional conhecido.

# Teste t (Distribuição de t de Student): usada quando temos amostras pequenas (n < 30) 
# e desvio-padrão populacional desconhecido.

# Passo 3 -  Determinar a região crítica. 
# A região crítica é a região onde H0 é rejeitada. 
# A área da região crítica é igual ao nível de significância, que estabelece a 
# probabilidade de rejeitar H0 quando ela é verdadeira.

#-------------------------------------------------------------------------------
# Testes para média

# Vamos usar a função z.test() do pacote "BSDA.
# Essa função é baseada na distribuição normal padrão e cria intervalos de confiança e 
# testa hipóteses para problemas de uma e duas amostras.

# install.packages("BSDA")   # Se o pacote não estiver instalado no seu computador, remova o #.
library(BSDA)             

args(z.test)
# alternative: "two.sided", "greater" ou "less".
# mu: valor da média ou diferença nas médias especificadas pela hipótese nula.
# sigma.x: desvio padrão da população para x.
# sigma.y: desvio padrão da população para y.
# conf.level: nível de confiança para o intervalo de confiança retornado.

# Formulação das hipóteses:
# H0: as médias são iguais.
# H1: as médias são diferentes.

# Para testes de médias, a estatística do teste é a variável padronizada Z.
z.test(MAC$Vel.max, MAC$Vel.med, 
       sigma.x = sd(MAC$Vel.max, na.rm = TRUE),     # Exemplo utilizando o desvio padrão da amostra.
       sigma.y = sd(MAC$Vel.med, na.rm = TRUE))

# Se não conhecemos o valor da variância e desvio padrão populacional e a
# amostra é pequena (geralmente com n < 30), devemos usar o teste t-Student.

# Teste t (Distribuição de t de Student):
t.test(MAC$Vel.max, MAC$Vel.med, 
       alternative = "two.sided")

t.test(MAC$Vel.max, mu = 8, 
       alternative = "less") # teste unilateral esquerdo.

t.test(MAC$Vel.max, mu = 8, 
       alternative = "greater") # teste unilateral direito.

#-------------------------------------------------------------------------------
### Correlação: é uma medida que permite mensurar a relação entre duas variáveis.

# Diagrama de dispersão: ferramenta gráfica que permite visualizarmos a relação entre duas variáveis.
plot(MAC$Vel.max, MAC$Vel.med)

# A função cor() retorna a correlação entre dois vetores ou uma matriz.
cor(MAC$Vel.max, MAC$Vel.med, 
    method = "pearson",   # method="pearson": coeficiente de correlação de Pearson.
    use = "complete.obs") # use="complete.obs": removendo todos os casos com valores omissos (dados ausentes).

# A função cor.test() também calcula o coeficiente de correlação, mas ela traz mais detalhes.
cor.test(MAC$Vel.max, MAC$Vel.med, 
         method = "pearson", conf.level = 0.95)

# Matriz de correlação:
(cor.dados <- cor(MAC[,4:8], 
                  method = "pearson", use = "complete.obs"))

#-------------------------------------------------------------------------------
# Exibição gráfica de uma matriz de correlação usando o pacote "corrplot".

# install.packages("corrplot") # Se o pacote não estiver instalado no seu computador, remova o #.
library(corrplot)   

corrplot(cor.dados, method = "number")
corrplot.mixed(cor.dados, lower = "number", upper = "pie")

#-------------------------------------------------------------------------------
### Visualização de dados com ggplot2

# Uma das principais bibliotecas para visualização de dados no R é o ggplot2.

# install.packages("ggplot2")  # Se o pacote não estiver instalado no seu computador, remova o #.
library(ggplot2)

# O ggplot2 faz parte da coleção de pacotes "Tidyverse" e foi construído por uma equipe
# liderada por Hadley Wickham. Para mais informações, acesse a página de ajuda do R.

# No ggplot2, os gráficos são construídos camada por camada.
# A camada base é dada pela função ggplot(), que recebe o conjunto de dados.

ggplot(MAC)

# Para definir o tipo de gráfico (barras, linha etc), usamos a função geom_<FUNCTIONS>().
# Os mapeamentos estéticos são especificados por aes().

# O ggplot2 possui mais de 30 funções geom_<FUNCTIONS>(), veja a lista no link:
# https://ggplot2.tidyverse.org/reference

#-------------------------------------------------------------------------------
# Histograma: geom_histogram().

# Histograma sumariza uma variável a dividindo em intervalos e contando quantas
# observações estão presentes em cada intervalo.

ggplot(MAC) +
  geom_histogram(aes(x = Tmed))

# Para adicionar mais elementos ao gráfico, basta utilizar o sinal "+" e as funções que desejar.

#-------------------------------------------------------------------------------
# Boxplot (diagrama de caixa): geom_boxplot()

# O boxplot é um gráfico utilizado para avaliar a distribuição dos dados.

ggplot(MAC) +
  geom_boxplot(aes(x = Tmed))

MAC$Mês <- as.factor(MAC$Mês) # Transformar a coluna "Mês" em um fator.

ggplot(MAC) +
  geom_boxplot(aes(y = Tmed, x = Mês))

#-------------------------------------------------------------------------------
# Mapeamento Estético:
# x e y: observações que serão mapeadas;
# color: define a cor de pontos e retas;
# fill: define a cor dos preenchimentos das formas com área;
# size: altera o tamanho das formas;
# alpha: altera a transparência das formas;
# shape: altera o estilo das formas;
# labels: altera o nome das observações.

# Os aspectos que podem ou devem ser mapeados dependem do tipo de gráfico que você está construindo.

#-------------------------------------------------------------------------------
# Histograma e Boxplot.

ggplot(MAC) +
  geom_histogram(aes(x = Tmed),
                 fill = "white", 
                 color = "black") +
  geom_boxplot(aes(x = Tmed, y = -1.2)) +
  labs(title = "Temperatura (°C)",           # Título do gráfico.
       x = "Temperatura Média de Macaé-RJ",  # Modificando o rótulo do eixo x.
       y = "Contagem") +                     # Modificando o rótulo do eixo y.
  theme_minimal()   # Alterando o tema do gráfico.

#-------------------------------------------------------------------------------
# Visualização de uma matriz de correlação.

# O pacote 'ggcorrplot' pode ser usado para visualizar uma matriz de correlação usando 'ggplot2'.

install.packages("ggcorrplot")  # Se o pacote não estiver instalado no seu computador, remova o #.
library(ggcorrplot)

ggcorrplot(cor.dados)

ggcorrplot(cor.dados,
           method = "circle",   # Alterando o método de visualização da matriz de correlação.
           type = "lower")      # Exibição da parte inferior da matriz.

# Adicionando p-valores e ordenando com base em semelhança.
# A função cor_pmat() Calcula os valores p de uma matriz de correlação.

(co_p <- cor_pmat(MAC[,4:8], 
                  use = "complete.obs")) # use="complete.obs": removendo todos os casos com valores omissos (dados ausentes).

ggcorrplot(cor.dados,
           method = "circle",
           type = "lower",
           hc.order = TRUE,  # Se TRUE, a matriz de correlação será ordenada usando a função hclust.
           p.mat = co_p,     # Matriz de p-valor.
           colors = c("gold2", "white", "blue2"),  # Vetor de 3 cores para valores de correlação baixo, médio e alto.
           lab = TRUE)       # Se TRUE, adicione o coeficiente de correlação no gráfico.       

#-------------------------------------------------------------------------------      
# Boxplot com teste t 

# Vamos funções dopacote "ggpubr".

# install.packages("ggpubr")  # Se o pacote não estiver instalado no seu computador, remova o #.
library(ggpubr)

(jan.ago <- subset(MAC, Mês == 1 | Mês == 8))  # Selecionando os dados dos meses 1 (janeiro) e 8 (agosto).

ggboxplot(jan.ago,  
          x = "Mês", y = "Prec", color = "Mês",             
          palette = c("red", "blue"), 
          add = "jitter") +
  
# Vamos usar a função stat_compare_means() para adicionar o valor-p.
stat_compare_means(comparisons = list(c("1", "8")),  # Variáveis a serem comparadas.
                     method = "t.test",    # Método que será usado para fazer a comparação.
                     label.y = 430)        # posicionamento do rótulo.


par(mfrow = c(1, 2))
hist(MAC$Vel.med)
boxplot(MAC$Vel.med)
