# Definindo o projeto de curso
#
# Pergunta: O que afeta a qualidade do ar? Como?

#install.packages("Ecdat")
library(Ecdat)


# Carregando o banco de dados
data(Airq)

# Exibe os nomes das variáveis
names(Airq)

# Descrevendo as variáveis
# airq: índice de qualidade do ar (quanto menor, melhor)
# vala: valor das empresas nas cidades (milhares de dólares)
# rain: quantidade de chuva (em polegadas)
# coas: posição costeira da cidade (sim ou não)
# dens: densidade populacional (milhas quadradas)
# medi: renda média per capita (dólares)

# Análise descritiva ou exploratória

# sumário das variáveis
summary(Airq)

# as variáveis podem ser contínuas ou categóricas
# a variável resposta é a qualidade do ar (airq)

# plotando
plot(airq ~ vala, data = Airq)

# Criando um modelo estatístico

m1 <- lm(formula = airq ~ vala, data = Airq)
summary(m1) # A variável vala não influenciou significativamente a variável airq

m2 <- lm(airq ~ coas, data = Airq)
summary(m2) # A variável coas afetou a qualidade do ar. 
plot(airq~coas, data=Airq) # As cidades costeiras apresentam melhor qualidade do ar


# A variável medi afeta a qualidade do ar?
m3 <- lm(airq ~ medi, data = Airq)
summary(m3) # Não
plot(airq ~ medi, data = Airq)

# Será que a quantidade de chuva influencia a qualidade do ar?
m4 <- lm(airq ~ rain, data= Airq)
summary(m4) # Não

# A densidade populacional afeta a qualidade do ar?
m5 <- lm(airq~dens, data=Airq)
summary(m5) #Não

# Retas de modelos não significativos são opcionais nos gráficos.
plot(airq ~ medi, data = Airq)
curve(9.936e+01 + 5.638e-04 * x, add=T)


#############################
#                           #
# Regressão linear múltipla #
#                           #
#############################

mRM1 <- lm(airq ~ vala + coas, data=Airq)
summary(mRM1) # Então existe um efeito da posição costeira o do valor das empresas na qualidade do ar

# Gráfico regressão múltipla
plot(airq ~ vala, data = Airq)
curve(1.171e+02 + 1.999e-03 * x, add=T)
curve(1.171e+02 + 1.999e-03 * x -2.968e+01, lty = 2, add=T)

# A qualidade do ar é afetada tanto pelo valor das empresas, quanto pela posição costeira. 
# As cidades não costeiras possuem uma pior qualidade do ar

mRM2 <- lm(airq ~ vala + coas + dens, data = Airq)
summary(mRM2)


# A densidade não foi significativa, mas ela exerce efeito sobre as outras variáveis explicativas.
# Será que eu posso remover ela da minha análise?

# Para decidir, eu preciso fazer um "Contraste de Modelos" que consiste basicamente em
# criar os modelos com e sem a variável e, na sequencia, utilizar a anova para compará-los
# 

modeloCompleto <- lm(airq ~ vala + coas + dens, data = Airq)
modeloIncompleto <- lm(airq ~ vala + coas, data = Airq)

# Os modelos são iguais?
# se p > 0.05, não existe diferença entre os modelos, logo eu devo ficar com o modelo mais simples
# se p <= 0.05, os modelos são estatisticamente diferentes.

anova(modeloCompleto, modeloIncompleto) # dens não é significativo, então eu posso remover
