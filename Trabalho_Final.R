# Carregar pacotes necessários
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("triangle")) install.packages("triangle")
library(ggplot2)
library(triangle)

# Função para calcular Pontos de Função ajustados
calcularPontosFuncao <- function(pesos, ajusteComplexidade, ILF, EIF, EI, EO, EQ) {
    PF <- ILF * pesos["ILF"] + EIF * pesos["EIF"] + EI * pesos["EI"] + EO * pesos["EO"] + EQ * pesos["EQ"]
    PF_ajustado <- PF * (0.65 + 0.01 * ajusteComplexidade)
    return(PF_ajustado)
}

# Função para simular o custo utilizando Monte Carlo
calcularCustoMC <- function(num_pontos_funcao, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica, n_simulacoes) {
    custos <- numeric(n_simulacoes)
    for (i in 1:n_simulacoes) {
        preco <- rtriangle(1, a = min_preco, b = max_preco, c = mode_preco)
        custos[i] <- preco * num_pontos_funcao * experiencia_equipe * complexidade_tecnica
    }
    return(custos)
}

# Configuração dos parâmetros de simulação
pesos <- c(ILF = 7, EIF = 5, EI = 4, EO = 5, EQ = 4)
componentes <- list(ILF = 3, EIF = 2, EI = 4, EO = 3, EQ = 2)
ajusteComplexidade <- 3

# Calcular Pontos de Função ajustados
PF_ajustado <- calcularPontosFuncao(pesos, ajusteComplexidade, componentes$ILF, componentes$EIF, componentes$EI, componentes$EO, componentes$EQ)

# Definição dos parâmetros de preço
min_preco <- 450
mode_preco <- 1200
max_preco <- 2500
experiencia_equipe <- 1.1
complexidade_tecnica <- 1.2
n_simulacoes <- 10000

# Realizar simulação de Monte Carlo
custos <- calcularCustoMC(PF_ajustado, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica, n_simulacoes)

# Custo por ponto de função
custo_por_pf <- custos / PF_ajustado

# Criar um data frame para plotagem
dados_para_plotagem <- data.frame(CustoPorPF = custo_por_pf)

# Exibir estatísticas descritivas
cat("Pontos de Função ajustados:", PF_ajustado, "\n")
cat("Estatísticas Descritivas dos Custos Totais do Projeto:\n")
cat("- Custo médio por ponto de função: R$", format(mean(custo_por_pf), big.mark = ".", decimal.mark = ","), "\n")
cat("- Média: R$", format(mean(custos), big.mark = ".", decimal.mark = ","), "\n")
cat("- Desvio Padrão: R$", format(sd(custos), big.mark = ".", decimal.mark = ","), "\n")
cat("- Percentil 5%: R$", format(quantile(custos, 0.05), big.mark = ".", decimal.mark = ","), "\n")
cat("- Percentil 95%: R$", format(quantile(custos, 0.95), big.mark = ".", decimal.mark = ","), "\n")

# Gráficos
# Gráfico de Histograma dos Custos Simulados
hist_custos <- ggplot(data.frame(custos), aes(x = custos)) +
    geom_histogram(binwidth = 100000, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = "Histograma dos Custos Simulados", x = "Custo (R$)", y = "Frequência") +
    theme_minimal() +
    scale_x_continuous(labels = scales::comma_format(prefix = "R$"))

# Boxplot dos custos simulados
boxplot_custos <- ggplot(data.frame(custos), aes(y = custos)) +
    geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
    labs(title = "Boxplot dos Custos Simulados", y = "Custo Simulado (R$)") +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma_format(prefix = "R$"))

# Gráfico de Densidade dos Custos Simulados
densidade_custos <- ggplot(data.frame(custos), aes(x = custos)) +
    geom_density(fill = "green", alpha = 0.7) +
    labs(title = "Gráfico de Densidade dos Custos Simulados", x = "Custo Simulado (R$)", y = "Densidade") +
    theme_minimal() +
    scale_x_continuous(labels = scales::comma_format(prefix = "R$"))

# Gráfico de Histograma dos Custos Médios por Ponto de Função
histograma_custo_medio <- ggplot(dados_para_plotagem, aes(x = CustoPorPF)) +
    geom_histogram(binwidth = 100, fill = "red", color = "black", alpha = 0.7) +
    labs(
        title = "Histograma dos Custos Médios por Ponto de Função",
        x = "Custo Médio por Ponto de Função (R$)",
        y = "Frequência"
    ) +
    theme_minimal() +
    scale_x_continuous(labels = scales::comma_format(prefix = "R$"))

# Exibir gráficos
print(hist_custos)
print(boxplot_custos)
print(densidade_custos)
print(histograma_custo_medio)