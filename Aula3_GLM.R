# Jornada da Bioestatística
# Aula 3

#  Significado das siglas mais comuns em modelagem estatística:
    # lm   = linear model             
    # glm  = generalized linear model  
    # glmm = generalized linear mixed model 
    # Ajustar comentário tools -> Global Option... -> code -> sotf-wrap 

# -----------------------------------------------------------------
# Importar dados *
# -----------------------------------------------------------------

# Baixar pacote caso não tenha baixado 
install.packages("openxlsx")

# Carergar pacote na biblioteca
library(openxlsx)

# importat dados usando a função read.xlsx do pacote openxlsx
dados = read.xlsx("planilha.xlsx")

# Visualizar planilha 

View(dados)

summary(dados)


# -----------------------------------------------------------------
# Análise exploratória: 
# -----------------------------------------------------------------

#          Analise exploratória
# Artigo leve e eficiente
# Passo fundamental antes de qualquer modelo estatístico
# Para quem quer aprofundar, recomendo o protocolo de Zuur et al. (2010): 
# "A protocol for data exploration to avoid common statistical problems"


# Etapa 1 do protocolo: verificar possíveis outliers na variável resposta
# O resumo estatístico já ajuda a identificar valores muito altos ou muito baixos
summary(dados$N_pulgoes)

# Etapa 1 (continuação): uso do Cleveland dotplot (dotchart) para explorar a distribuição
# Esse gráfico ajuda a visualizar valores extremos com mais detalhe do que o boxplot

dotchart(dados$N_pulgoes, 
         xlab = "Número de pulgões", 
         main = "Cleveland dotplot")

# -----------------------------------------------------------------
# Ajustando o modelo GLMM:----
# -----------------------------------------------------------------


# Estamos avaliando o efeito do tratamento sobre o número de pulgões (contagem),
# considerando "Local" como efeito aleatório para controlar a variação entre locais.
# Como a variável dependente (N_pulgoes) é uma contagem, usamos a distribuição de Poisson.

# Instalar pacote

install.packages("lme4")

# Abrir na biblioteca 
library(lme4)

citation("lme4")

modelo = glmer(N_pulgoes ~ Tratamento + (1 | Local),   # ~ em função de
               data = dados, family = poisson)

# Saiba mais sobre a função glmer
?glmer


# Validando o modelo:
# Esse comando gera gráficos de diagnóstico com base nos resíduos do modelo.
# Pode ajudar a identificar problemas como overdispersion, padrões nos resíduos, etc.

plot(modelo)


# Modelo nulo com apenas o intercepto e o efeito aleatório do local

modelo_nulo = glmer(N_pulgoes ~ 1 + (1 | Local), 
                    data = dados, 
                    family = poisson)


# Comparando os dois modelos usando
# A comparação avalia se incluir o efeito fixo Tratamento melhora significativamente o ajuste do modelo em relação ao modelo nulo.

# Se o p-valor do teste for menor do que 0,05; significa que Tratamento tem efeito importante no Comprimento do número de pulgão

anova(modelo, modelo_nulo)


#Interpretando o valor de p no modelo GLMM:

# 
# Estamos avaliando se o número de pulgões (N_pulgoes) difere entre os tratamentos,
# controlando a variação entre os locais (efeito aleatório).
# 
# Se o valor de p for menor que 0.05:
#   → Assumimos que há diferença significativa entre os tratamentos.
#   → Ou seja, o tipo de tratamento influencia o número de pulgões.
# 
# Se o valor de p for maior que 0.05:
#   → Não há evidência estatística suficiente para afirmar que existe diferença.
#   → As variações observadas podem ter ocorrido por acaso.

# < 2e-16 = 0,0000000000000002
# Pergunta: com base nesse valor de p o tratamente tem um efeito no número de pulgão?

# -----------------------------------------------------------------
# Confecção de figura
# -----------------------------------------------------------------


# Se ainda não instalou o pacote sciplot

install.packages("sciplot")

library("sciplot")


# Confecção de figuras usando a função lineplot.CI do pacote sciplot

lineplot.CI(Tratamento, N_pulgoes, data = dados, type = "p", xlab = "Tratamento", 
            ylab = "Número de pulgões", bty = "l")


#########################################################################

###################### Caso da enxaqueca #################################

# Importando dados 

library(openxlsx)

dados = read.xlsx("Enxaqueca.xlsx")

View(dados)

# Etapa 1: Verificar possíveis outliers na variável de resposta
# A variável resposta aqui é "Enxaqueca", que é binária (0 ou 1)
table(dados$Enxaqueca)

# Etapa 1 (continuação): Explorar a variável explicativa principal (IMC)
summary(dados$IMC)

dotchart(dados$IMC,
         xlab = "IMC",
         main = "Visualização de possíveis valores extremos de IMC")


# -----------------------------------------------------------------
# AJUSTE DO MODELO GLMM
# -----------------------------------------------------------------

# Ajustando um modelo misto generalizado:
# Estamos avaliando se o IMC influencia a ocorrência de enxaqueca (variável binária),
# controlando a variação de fatores como sexo, sono, estresse, faixa etária e atividade física.
# Como a variável dependente é binária (0/1), usamos a distribuição binomial (modelo logístico)


modelo = glmer(Enxaqueca ~ IMC + (1 | Sexo) + (1 | Qualidade_sono) + (1 | Estresse) + (1 | Faixa_etaria) + (1 | Atividade_fisica), data = dados, family = binomial)



# -----------------------------------------------------------------
# VALIDAÇÃO E INTERPRETAÇÃO DO MODELO
# -----------------------------------------------------------------

plot(modelo)


# Modelo nulo com apenas o intercepto e os efeitos aleatórios
modelo_nulo = glmer(Enxaqueca ~ 1 + (1 | Sexo) + (1 | Qualidade_sono) + (1 | Estresse) + (1 | Faixa_etaria) + (1 | Atividade_fisica), data = dados, family = binomial)




# Comparando os dois modelos: investigando se incluir o efeito fixo IMC melhor significativamente o ajuste do modelo em relação ao modelo nulo.

# Se o p-valor do teste for menor do que 0,05; significa que IMC tem efeito importante no Comprimento do número de pulgão

anova(modelo, modelo_nulo)


# -----------------------------------------------------------------
# INTERPRETAÇÃO DO VALOR DE P
# -----------------------------------------------------------------

# Se o valor de p associado ao IMC for menor que 0.05:
#   → Consideramos que há evidência de que o IMC afeta a chance de ter enxaqueca.
#   → Quanto maior o IMC, maior a probabilidade de relatar enxaqueca.
#
# Se o valor de p for maior que 0.05:
#   → Não há evidência suficiente para afirmar que o IMC influencia a ocorrência de enxaqueca.
#   → A diferença observada pode ser atribuída ao acaso.

# -----------------------------------------------------------------
# FIGURA (OPCIONAL): Comparando média de IMC conforme enxaqueca
# -----------------------------------------------------------------

# Gráfico de médias com erro padrão (não desvio padrão, pois IMC é contínuo)

lineplot.CI(Enxaqueca, 
            IMC, 
            data = dados, 
            xlab = "Enxaqueca (0 = Não, 1 = Sim)",
            ylab = "IMC médio", bty="l", type = "p")




# -----------------------------------------------------------------
#                             Exercício 
# -----------------------------------------------------------------

# Quiz - modelo misto

# https://bioestatistica-academy.com/modelo-misto

# Analisar os dados das aves. Os dados estão no site interno: (Aula 4)

# https://cursobioestatistica.com.br/2jornadaaulas
