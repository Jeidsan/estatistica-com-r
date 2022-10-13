# Carregar os dados
alunos <- data.frame(
  Sem_Alura = c(4, 3, 5, 4, 8, 6, 5, 7, 6, 7, 8, 7, 3, 7, 4, 7, 5, 7, 5, 7, 6, 4, 3, 3, 6, 3, 7, 4, 6, 6),
  Com_Alura = c(8, 6, 10, 3, 9, 5, 8, 5, 4, 10, 5, 9, 8, 5, 10, 8, 7, 9, 9, 5, 5, 7, 8, 9, 8, 5, 5, 8, 5, 9)
)

# Definir os parâmetros do teste
significancia <- 0.1
confianca <- 1 - significancia
n <- 30
probabilidade <- (0.5 + (confianca / 2))
z_alpha_2 <- qnorm(probabilidade)

# Calcular as diferenças
alunos['Diff'] <- alunos$Com_Alura - alunos$Sem_Alura

# Calcular os módulos das diferenças
alunos['|Diff|'] <- abs(alunos['Diff'])

# Ordenar conforme módulo das diferenças
alunos <- alunos[order(alunos$`|Diff|`),]

# Determinar as ordens das diferenças absolutas
alunos['Ordem das diferenças absolutas'] <- seq(1, nrow(alunos))

# Agregar os postos
postos <- aggregate(x = alunos$'Ordem das diferenças absolutas', by = list(alunos$`|Diff|`), FUN = mean)
colnames(postos) <- c('|Diff|', 'Posto Médio')

# Adicionar os postos médios com base na tabela de postos
alunos <- merge(x = alunos, y = postos, by = '|Diff|', all.x = TRUE)

# Determinar os postos positivos
alunos['Posto (+)'] <- apply(alunos[, c('Diff', 'Posto Médio')], 1, function(x) if(x[1] > 0) x[2] else 0)

# Determinar os postos negativos
alunos['Posto (-)'] <- apply(alunos[, c('Diff', 'Posto Médio')], 1, function(x) if(x[1] < 0) x[2] else 0)

# Calcular o valor de T (mínimo entre as somas dos postos positivos e negativos)
T <- min( sum( alunos['Posto (+)'] ), sum( alunos['Posto (-)'] ) )

# Calcular mu de T e sigma de T
mu_T <- ( n * ( n + 1 ) ) / 4
sigma_T <- sqrt( ( n * ( n + 1 ) * ( ( 2 * n ) + 1 ) ) / 24 )

# Calcular o valor de Z
Z <- ( T - mu_T ) / sigma_T

# Comparar com os valores Z de alfa
if (Z <= -z_alpha_2 | Z >= z_alpha_2) print('Rejeitar a hipótese nula') else print('Aceitar a hipótese nula')
