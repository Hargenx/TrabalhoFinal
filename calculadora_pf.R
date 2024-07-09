# Carregar pacote necessário
if (!require("triangle")) install.packages("triangle")
library(triangle)

# Função para calcular Pontos de Função ajustados
calcularPontosFuncao <- function(ILF, EIF, EI, EO, EQ, pesos, ajusteComplexidade) {
    # Cálculo dos Pontos de Função não ajustados
    PF <- (ILF * pesos["ILF"]) +
        (EIF * pesos["EIF"]) +
        (EI * pesos["EI"]) +
        (EO * pesos["EO"]) +
        (EQ * pesos["EQ"])

    # Cálculo dos Pontos de Função ajustados
    PF_ajustado <- PF * (0.65 + 0.01 * ajusteComplexidade)

    # Retornar o resultado
    return(PF_ajustado)
}

# Função para simular o custo utilizando Monte Carlo
calcularCustoMC <- function(num_pontos_funcao, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica, n_simulacoes) {
    # Vetor para armazenar os custos simulados
    custos <- numeric(n_simulacoes)

    # Realizar as simulações de Monte Carlo
    for (i in 1:n_simulacoes) {
        preco <- rtriangle(1, a = min_preco, b = max_preco, c = mode_preco)
        custo <- preco * num_pontos_funcao * experiencia_equipe * complexidade_tecnica
        custos[i] <- custo
    }

    # Retornar os custos simulados
    return(custos)
}

# Exemplo de uso das funções

# Definição dos pesos para cada tipo de componente
pesos <- c(ILF = 7, EIF = 5, EI = 4, EO = 5, EQ = 4)

# Dados de exemplo para um sistema fictício
ILF <- 3 # Exemplo: Arquivos Lógicos Internos
EIF <- 2 # Exemplo: Arquivos de Interface Externa
EI <- 4 # Exemplo: Entradas Externas
EO <- 3 # Exemplo: Saídas Externas
EQ <- 2 # Exemplo: Consultas Externas

# Ajuste de complexidade do sistema (variando de 0 a 5)
ajusteComplexidade <- 3

# Calcular Pontos de Função ajustados
PF_ajustado <- calcularPontosFuncao(ILF, EIF, EI, EO, EQ, pesos, ajusteComplexidade)

# Parâmetros para a simulação de Monte Carlo
min_preco <- 5000
mode_preco <- 10000
max_preco <- 20000
experiencia_equipe <- 1.1
complexidade_tecnica <- 1.2
n_simulacoes <- 10000

# Calcular os custos utilizando Monte Carlo
custos <- calcularCustoMC(PF_ajustado, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica, n_simulacoes)

# Resultados
cat("Pontos de Função ajustados:", PF_ajustado, "\n")
cat("Custo médio estimado:", mean(custos), "\n")
cat("Desvio padrão dos custos:", sd(custos), "\n")
cat("Percentil 5% dos custos:", quantile(custos, 0.05), "\n")
cat("Percentil 95% dos custos:", quantile(custos, 0.95), "\n")
