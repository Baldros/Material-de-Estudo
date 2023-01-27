#===============================================================================
# PROJETO DE EXTENSÃO: PROCESSAMENTO E ANÁLISE DE DADOS AMBIENTAIS COM R
# CURSO: ESTATÍSTICA BÁSICA NO R
# E-MAIL: pexcca.lamet@uenf.br
# ATUALIZAÇÃO (SCRIPT): 15/12/2022
# https://linktr.ee/pexcca.lamet
#===============================================================================

# Caso o script abra com problemas na codificação (acentos não interpretados),
# seguir os seguintes passos no RStudio:
# File > Reopen with Encoding > Escolher ISO-8859-1 ou UTF-8 > Ok

#-------------------------------------------------------------------------------
### Introdução a estatística (população, amostra e variável)

# Ao se fazer um estudo estatístico tem que considerar o tipo de variável.
# Variável é a característica que é medida ou avaliada em cada elemento da amostra ou população.

# População: conjunto de elementos (finitos ou não) que tem pelo menos uma característica em comum.
# Amostra: subconjunto de elementos de uma população.

# As variáveis podem ser classificadas em quantitativas (contínuas e discretas) 
# ou qualitativas (nominais e ordinais).

### Exemplos:
# Variáveis quantitativas discretas: número de dias com chuva, número de inundações no ano;
# Variáveis quantitativas contínuas: precipitação, vazão, altura;
# Variáveis qualitativas nominais: grupo sanguíneo, cores, região;
# Variáveis qualitativas ordinais: mês de observação (janeiro,., dezembro), 
# intensidade da chuva (fraca, moderada e forte);

#-------------------------------------------------------------------------------
### Definindo o diretório de trabalho

# Diretório de trabalho: é uma pasta no seu computador que interage com o R/RStudio.

getwd()	# Informa o diretório de trabalho atual.

# setwd() 	# Seta um local como novo diretório.
# Exemplo: setwd("C:/Users/Desktop/Minicurso R")

# No RStudio, também é possível definir o diretório:
# 1. Utilizando o atalho Ctrl + Shift + H.
# 2. Clicando em Session > Set Working Directory > Choose Directory.

dir()     # Lista os arquivos dentro do diretório de trabalho.

#-------------------------------------------------------------------------------
### Importação de dados
# Vamos utilizar os dados mensais de Macaé-RJ obtidos do INMET (https://bdmep.inmet.gov.br/), 
# disponíveis no link: https://drive.google.com/drive/u/3/folders/1ngqp7SVYtPiuwLwj8FRPCV3tF0FGIyN0

# Importação dos dados:
dados <- read.csv(file = "dados_A608_M_2006-09-21_2022-11-30.csv",  # Nome e extensão do arquivo que será importado.
                  header = TRUE,           # Use header=TRUE para ler a primeira linha como cabeçalho das colunas.
                  skip = 10,               # Use skip=n para pular n linhas antes de ler o arquivo.
                  na.strings = "null",     # na.strings: caracterer que deve ser interpretado como valor NA.
                  sep = ";",               # Separador de colunas.
                  dec = ",",               # Separador de decimais.
                  check.names = FALSE)     # Se TRUE, os nomes das variáveis são verificados e, se necessário, ajustados.

head(dados)  # Mostra as primeiras linhas (por padrão 6 linhas) dos dados.
tail(dados)  # Mostra as últimas linhas (por padrão 6 linhas) dos dados.

names(dados)  # Lista os nomes das colunas.
names(dados) <- c("Data", "Nd.Prec", "Prec", "Tmed", "Vel.max", "Vel.med") # Renomear as colunas.

str(dados)  # Mostra uma síntese da estrutura da base de dados.

dados[, -7]  # Seleciona todos os dados, exceto a coluna 7.

DMAC <- dados[, -7]
head(DMAC)

DMAC$Tmed  # Utilize $ para selecionar uma coluna.

attach(dados)  # Permite acessar os elementos individualmente (sem necessidade de $).

#-------------------------------------------------------------------------------
### R-base e pacotes:
#
# No R, parte dos comandos/funções estão implementados no ambiente básico do R (R base), 
# mas muitas funções estão implementadas em pacotes de funções adicionais (packages).

# Ou seja, o R divide-se entre a instalação básica e pacotes (packages).
# Pacotes R: são bibliotecas para funções ou áreas de estudo específicas.

# Na instalação básica, você encontrará os comandos básicos necessários para efetuar suas análises.
#-------------------------------------------------------------------------------
# Vamos usar o pacote "lubridate" para trabalhar com as datas.

# install.packages("lubridate")  # Se o pacote não estiver instalado no seu computador, remova o #.
library(lubridate)

# Vamos usar a função ymd() do lubridate para converter a coluna "Data" (caractere) para o formato Date (data).
DMAC$Data <- ymd(DMAC$Data)
str(DMAC)

# Extrair componentes de uma data:
Ano = year(DMAC$Data)  # Obtem os anos da Data.
Mês = month(DMAC$Data) # Obtem os meses da Data.
Dia = day(DMAC$Data)   # Obtem os dias da Data.

# Criar um data grame com as colunas Ano, Mês, Dia e o conjunto de dados DMAC (exceto a coluna 1). 
dados.org <- data.frame(Ano, Mês, Dia, DMAC[,2:6])
head(dados.org)

summary(dados.org)  # Apresenta o resumo de algumas medidas estatísticas.

# A função by() aplica uma função a um subgrupo (fatores/categorias).
by(dados.org$Prec, dados.org$Ano, summary)
by(dados.org$Prec, dados.org$Mês, summary)
by(dados.org$Prec, dados.org$Mês, mean, na.rm = TRUE) # Use o argumento na.rm=TRUE para ignorar os valores ausentes(NA).

# Selecionar os dados para o período a partir 2020.
subset(dados.org, dados.org$Ano>=2020)

# Selecionar os dados para o período de 2020 a 2022.
(D2022 <- subset(dados.org, dados.org$Ano>=2020 & dados.org$Ano<=2022))

#-------------------------------------------------------------------------------
### Apresentação dos dados

sort(D2022$Prec, decreasing = FALSE)  # Ordenar a variável em ordem decrescente.
sort(D2022$Prec, decreasing = TRUE)  # Ordenar a variável em ordem crescente.

min(D2022$Prec, na.rm = T)
max(D2022$Prec, na.rm = T)
range(D2022$Prec, na.rm = T)  # Mostra os valores mínimo e máximo dos dados.

# Criar uma tabela de contagem (frequência) de cada combinação de nível.
table(D2022$Nd.Prec)

# Calcular a frequência por intervalos (por classe). 
# A função cut() divide dados em intervalos.

table(cut(D2022$Prec, 
          breaks = c(20, 50, 100, 200, 320)))  # Intervalos diferentes

(F<-table(cut(D2022$Prec, 
              breaks = seq(20, 320, 50),  # Intervalos iguais.
              right = FALSE))) # right = FALSE  faz com que o intervalo seja aberto na direita e fechado na esquerda.

# Gerar os percentuais da frequência.
prop.table(F) * 100     

(Fr<-prop.table(F)*100)

#-------------------------------------------------------------------------------
### Tabela de distribuição de frequências usando o pacote "fdth". 

install.packages("fdth") # Se o pacote não estiver instalado no seu computador, remova o #.
library(fdth)              

# Por padrão (default), a função fdt() usa breaks = "Sturges".
# Método de Sturges: k = 1 + 3,333 * log10(n)
## k é o número de classes.
## n é o número total de observações na amostra.
## Log é o logaritmo comum da base 10.

(tab1 <- fdt(D2022$Prec, na.rm = TRUE)) # Use o argumento na.rm=TRUE para ignorar os valores ausentes(NA).

# frequência absoluta (f), frequência relativa (rf) e frequência relativa em percentual (rf(%)).
# frequência absoluta acumulada (cf) e frequência acumulada em percentual(cf(%)). 

(tab2 <- fdt(D2022$Prec, start = 20, end = 320))

#-------------------------------------------------------------------------------
# Para exportar os dados em csv ou txt, use a função write.table().

write.table(x = dados.org,        # Nome do arquivo que será exportado.
            file = "Macaé.csv",   # Nome e extensão do arquivo que será gerado.
            dec = ",",            # Separador de decimais.
            row.names = FALSE,    # FALSE:  os nomes das linhas não devem ser considerados.
            sep = ";",            # Separador de colunas.
            na = "NA",            # Identificação dos valores ausentes.
            fileEncoding = "ISO-8859-1")  # Especifica a codificação a ser usada.

