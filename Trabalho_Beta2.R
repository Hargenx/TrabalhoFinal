# Instalar pacotes necessarios se nao estiverem instalados
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("triangle", quietly = TRUE)) install.packages("triangle")

library(dplyr)
library(ggplot2)
library(triangle)

# Definir variaveis de preco
min_preco <- 186887.90
max_preco <- 393745.98
mode_preco <- 290000.00

cat("Min:", min_preco, "\nMax:", max_preco, "\nMode:", mode_preco, "\n")

# Funcao para calcular o custo utilizando Monte Carlo
calcular_custo_mc <- function(num_pontos_funcao, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica, n_simulacoes) {
    custos_simulacao <- numeric(n_simulacoes)
    for (i in 1:n_simulacoes) {
        preco_ponto_funcao <- rtriangle(1, min_preco, max_preco, mode_preco)
        fator_complexidade <- experiencia_equipe * complexidade_tecnica
        custos_simulacao[i] <- num_pontos_funcao * preco_ponto_funcao * fator_complexidade
    }
    return(custos_simulacao)
}

# Funcao para simular por tipo
simular_por_tipo <- function(dados_projetos, n_simulacoes, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica) {
    resultados <- list()
    for (i in seq_len(nrow(dados_projetos))) {
        num_pontos_funcao <- dados_projetos$Quantidade[i]
        custos <- calcular_custo_mc(num_pontos_funcao, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica, n_simulacoes)
        resultados[[dados_projetos$Especificacao[i]]] <- custos
    }
    return(resultados)
}

# Funcao para calcular pontos de funcao
calcular_pontos_funcao <- function(tipo, complexidade) {
    contrib_funcional <- switch(tipo,
        "ALI" = switch(complexidade,
            "Baixa" = 7,
            "Media" = 10,
            "Alta" = 15
        ),
        "AIE" = switch(complexidade,
            "Baixa" = 5,
            "Media" = 7,
            "Alta" = 10
        ),
        "EE" = switch(complexidade,
            "Baixa" = 3,
            "Media" = 4,
            "Alta" = 6
        ),
        "CE" = switch(complexidade,
            "Baixa" = 3,
            "Media" = 4,
            "Alta" = 6
        ),
        "SE" = switch(complexidade,
            "Baixa" = 4,
            "Media" = 5,
            "Alta" = 7
        ),
        stop("Tipo ou complexidade desconhecido")
    )
    return(contrib_funcional)
}

# Definir as variaveis para simulacao
num_pontos_funcao <- 500
experiencia_equipe <- 1.1 # Exemplo de uma equipe com experiencia moderada
complexidade_tecnica <- 1.2 # Exemplo de alta complexidade tecnica
n_simulacoes <- 10000 # Numero de simulacoes

# Simulacao dos custos utilizando Monte Carlo
set.seed(123) # Para reprodutibilidade
custos_mc <- calcular_custo_mc(num_pontos_funcao, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica, n_simulacoes)

# Grafico: Histograma dos custos
hist(custos_mc, breaks = 50, main = "Distribuicao dos Custos do Projeto (Monte Carlo)", xlab = "Custo Total (R$)", col = "blue", border = "black")

# Grafico: Boxplot dos custos
boxplot(custos_mc, main = "Boxplot dos Custos do Projeto (Monte Carlo)", ylab = "Custo Total (R$)")

# Estatisticas descritivas
summary(custos_mc)

# Definir um limite superior de custo
limite_superior <- 150000000

# Calcular a probabilidade de ultrapassar o limite superior
probabilidade_ultrapassar <- mean(custos_mc > limite_superior)
cat("Probabilidade de ultrapassar", limite_superior, ": ", probabilidade_ultrapassar * 100, "%\n")

summary(custos_mc)
hist(custos_mc, breaks = 50, main = "Distribuicao dos Custos de Desenvolvimento", xlab = "Custo (R$)")
abline(v = limite_superior, col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("Limite Superior =", limite_superior), col = "red", lwd = 2, lty = 2)

# Mostrar a probabilidade
cat("A probabilidade de ultrapassar o limite superior de", limite_superior, "e de", probabilidade_ultrapassar * 100, "%\n")

# Calcular o intervalo de confianca para os custos do projeto (95%)
intervalo_confianca <- quantile(custos_mc, probs = c(0.025, 0.975))
cat("Intervalo de confianca (95%):\n")
print(intervalo_confianca)

# Sensibilidade para experiencia da equipe
experiencia_variacoes <- c(1.0, 1.1, 1.2, 1.3, 1.4)
sensibilidade_experiencia <- sapply(experiencia_variacoes, function(exp) {
    calcular_custo_mc(num_pontos_funcao, min_preco, mode_preco, max_preco, exp, complexidade_tecnica, n_simulacoes)
})
mean_custos_experiencia <- colMeans(sensibilidade_experiencia)
plot(experiencia_variacoes, mean_custos_experiencia, type = "b", xlab = "Experiencia da Equipe", ylab = "Custo Medio (R$)", main = "Analise de Sensibilidade - Experiencia da Equipe")

# Sensibilidade para complexidade tecnica
complexidade_variacoes <- c(1.0, 1.1, 1.2, 1.3, 1.4)
sensibilidade_complexidade <- sapply(complexidade_variacoes, function(comp) {
    calcular_custo_mc(num_pontos_funcao, min_preco, mode_preco, max_preco, experiencia_equipe, comp, n_simulacoes)
})
mean_custos_complexidade <- colMeans(sensibilidade_complexidade)
plot(complexidade_variacoes, mean_custos_complexidade, type = "b", xlab = "Complexidade Tecnica", ylab = "Custo Medio (R$)", main = "Analise de Sensibilidade - Complexidade Tecnica")

# Dados dos projetos
dados_projetos <- data.frame(
    Especificacao = c(
        "Desenvolvedor JAVA", "Desenvolvedor PHP", "Desenvolvedor Python", "Desenvolvedor Mobile", "Desenvolvedor Outras linguagens",
        "Desenvolvimento manutencao de sistema legado", "Desenvolvedor JAVA para correcoes e novos modulos",
        "Desenvolvedor PHP para correcoes e novos modulos", "Desenvolvedor Python para correcoes e novos modulos",
        "Desenvolvedor Mobile para correcoes e novos modulos", "Desenvolvedor Outras linguagens para correcoes e novos modulos"
    ),
    Unidade = "Ponto de Funcao",
    Quantidade = c(
        16429, 5546, 150, 2747, 58, 1796, 4933, 5541, 343, 1868, 511
    ),
    Valor_Unitario = c(
        565.15, 565.15, 565.15, 565.15, 565.15, 680.00, 527.00, 527.00, 527.00, 527.00, 527.00
    )
)

print(dados_projetos)

resultados_simulacao <- simular_por_tipo(dados_projetos, n_simulacoes, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica)

print(resultados_simulacao[["Desenvolvedor JAVA"]])

for (tipo in names(resultados_simulacao)) {
    hist(resultados_simulacao[[tipo]], breaks = 50, main = paste("Custos -", tipo), xlab = "Custo Total (R$)", col = "blue", border = "black")
}

estatisticas <- data.frame(
    Especificacao = character(),
    Media = numeric(),
    Mediana = numeric(),
    Desvio_Padrao = numeric(),
    Intervalo_Confianca = character(),
    stringsAsFactors = FALSE
)

for (tipo in names(resultados_simulacao)) {
    custos <- resultados_simulacao[[tipo]]
    media <- mean(custos)
    mediana <- median(custos)
    desvio_padrao <- sd(custos)
    intervalo_confianca <- quantile(custos, probs = c(0.025, 0.975))

    estatisticas <- rbind(estatisticas, data.frame(
        Especificacao = tipo,
        Media = media,
        Mediana = mediana,
        Desvio_Padrao = desvio_padrao,
        Intervalo_Confianca = paste0("(", round(intervalo_confianca[1], 2), ", ", round(intervalo_confianca[2], 2), ")")
    ))
}

print(estatisticas)

# Calcular pontos de funcao para um exemplo especifico
tipo_exemplo <- "ALI"
complexidade_exemplo <- "Alta"
pontos_funcao_exemplo <- calcular_pontos_funcao(tipo_exemplo, complexidade_exemplo)
cat("Pontos de Funcao para", tipo_exemplo, "com complexidade", complexidade_exemplo, ":", pontos_funcao_exemplo, "\n")

# Grafico: Estatisticas dos custos por tipo
library(reshape2)
estatisticas_long <- melt(estatisticas, id.vars = "Especificacao")
ggplot(estatisticas_long, aes(x = Especificacao, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Estatisticas dos Custos por Tipo", x = "Tipo", y = "Valor", fill = "Estatistica") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gravar estatisticas em um arquivo CSV
write.csv(estatisticas, "estatisticas_custos.csv", row.names = FALSE)

# Calcular pontos de funcao para todos os dados do projeto
dados_projetos$pontos_funcao <- mapply(calcular_pontos_funcao, tipo = rep("ALI", nrow(dados_projetos)), complexidade = rep("Media", nrow(dados_projetos)))

# Mostrar os dados dos projetos com pontos de funcao calculados
print(dados_projetos)

# Sensibilidade para variacoes de precos
precos_variacoes <- list(
    min_preco_variado = min_preco * c(0.8, 1.2),
    max_preco_variado = max_preco * c(0.8, 1.2),
    mode_preco_variado = mode_preco * c(0.8, 1.2)
)

sensibilidade_precos <- data.frame(
    Variacao = character(),
    Media = numeric(),
    Mediana = numeric(),
    Desvio_Padrao = numeric(),
    Intervalo_Confianca = character(),
    stringsAsFactors = FALSE
)

for (min_v in precos_variacoes$min_preco_variado) {
    for (max_v in precos_variacoes$max_preco_variado) {
        for (mode_v in precos_variacoes$mode_preco_variado) {
            custos_v <- calcular_custo_mc(num_pontos_funcao, min_v, mode_v, max_v, experiencia_equipe, complexidade_tecnica, n_simulacoes)
            media_v <- mean(custos_v)
            mediana_v <- median(custos_v)
            desvio_padrao_v <- sd(custos_v)
            intervalo_confianca_v <- quantile(custos_v, probs = c(0.025, 0.975))
            variacao <- paste("Min:", round(min_v, 2), "Mode:", round(mode_v, 2), "Max:", round(max_v, 2))
            sensibilidade_precos <- rbind(sensibilidade_precos, data.frame(
                Variacao = variacao,
                Media = media_v,
                Mediana = mediana_v,
                Desvio_Padrao = desvio_padrao_v,
                Intervalo_Confianca = paste0("(", round(intervalo_confianca_v[1], 2), ", ", round(intervalo_confianca_v[2], 2), ")")
            ))
        }
    }
}

print(sensibilidade_precos)

# Gravar estatisticas de sensibilidade em um arquivo CSV
write.csv(sensibilidade_precos, "sensibilidade_precos.csv", row.names = FALSE)
