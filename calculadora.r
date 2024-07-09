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

# Exemplo de uso da função
# Definição dos pesos para cada tipo de componente
pesos <- c(ILF = 7, EIF = 5, EI = 4, EO = 5, EQ = 4)

# Dados de exemplo para um sistema fictício
ILF <- 5 # Arquivos Lógicos Internos
EIF <- 8 # Arquivos de Interface Externa
EI <- 4 # Entradas Externas
EO <- 12 # Saídas Externas
EQ <- 3 # Consultas Externas

# Ajuste de complexidade do sistema (variando de 0 a 5)
ajusteComplexidade <- 3

# Calcular Pontos de Função ajustados
PF_ajustado <- calcularPontosFuncao(ILF, EIF, EI, EO, EQ, pesos, ajusteComplexidade)

# Imprimir resultado
cat("Pontos de Função ajustados:", PF_ajustado, "\n")
