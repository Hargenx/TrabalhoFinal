# https://colab.research.google.com/drive/1aHM5-eFHOLcUOkdkzxY1hiwDvxvaPAMM#scrollTo=giBwCC9rJWa8

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("triangle", quietly = TRUE)) install.packages("triangle")

library(dplyr)
library(ggplot2)
library(triangle)

min_preco <- 186887.90
max_preco <- 393745.98
mode_preco <- 290000.00

cat("Min:", min_preco, "\nMax:", max_preco, "\nMode:", mode_preco, "\n")

# FunC'C#o para calcular o custo utilizando Monte Carlo
calcular_custo_mc <- function(num_pontos_funcao, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica, n_simulacoes) {
    custos_simulacao <- numeric(n_simulacoes)
    for (i in 1:n_simulacoes) {
        preco_ponto_funcao <- rtriangle(1, min_preco, max_preco, mode_preco)
        fator_complexidade <- experiencia_equipe * complexidade_tecnica
        custos_simulacao[i] <- num_pontos_funcao * preco_ponto_funcao * fator_complexidade
    }
    return(custos_simulacao)
}

# Definindo as variC!veis para simulaC'C#o
num_pontos_funcao <- 500
experiencia_equipe <- 1.1 # Exemplo de uma equipe com experiC*ncia moderada
complexidade_tecnica <- 1.2 # Exemplo de alta complexidade tC)cnica
n_simulacoes <- 10000 # NC:mero de simulaC'C5es

# SimulaC'C#o dos custos utilizando Monte Carlo
set.seed(123) # Para reprodutibilidade
custos_mc <- calcular_custo_mc(num_pontos_funcao, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica, n_simulacoes)

# GrC!fico: Histograma dos custos
hist(custos_mc, breaks = 50, main = "DistribuiC'C#o dos Custos do Projeto (Monte Carlo)", xlab = "Custo Total (R$)", col = "blue", border = "black")

# GrC!fico: Boxplot dos custos
boxplot(custos_mc, main = "Boxplot dos Custos do Projeto (Monte Carlo)", ylab = "Custo Total (R$)")

# EstatC-sticas descritivas
summary(custos_mc)

# Definir um limite superior de custo
limite_superior <- 150000000

# Calcular a probabilidade de ultrapassar o limite superior
probabilidade_ultrapassar <- mean(custos_mc > limite_superior)
cat("Probabilidade de ultrapassar", limite_superior, ": ", probabilidade_ultrapassar * 100, "%\n")

summary(custos_mc)
hist(custos_mc, breaks = 50, main = "DistribuiC'C#o dos Custos de Desenvolvimento", xlab = "Custo (R$)")
abline(v = limite_superior, col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("Limite Superior =", limite_superior), col = "red", lwd = 2, lty = 2)

# Mostrar a probabilidade
cat("A probabilidade de ultrapassar o limite superior de", limite_superior, "C) de", probabilidade_ultrapassar * 100, "%\n")

# Calcular o intervalo de confianC'a para os custos do projeto (95%)
intervalo_confianCa <- quantile(custos_mc, probs = c(0.025, 0.975))
cat("Intervalo de confianCa (95%):\n")
print(intervalo_confianCa)


# Sensibilidade para experiC*ncia da equipe
experiencia_variacoes <- c(1.0, 1.1, 1.2, 1.3, 1.4)
sensibilidade_experiencia <- sapply(experiencia_variacoes, function(exp) {
    calcular_custo_mc(num_pontos_funcao, min_preco, mode_preco, max_preco, exp, complexidade_tecnica, n_simulacoes)
})
mean_custos_experiencia <- colMeans(sensibilidade_experiencia)
plot(experiencia_variacoes, mean_custos_experiencia, type = "b", xlab = "ExperiC*ncia da Equipe", ylab = "Custo MC)dio (R$)", main = "AnC!lise de Sensibilidade - ExperiC*ncia da Equipe")

# Sensibilidade para complexidade tC)cnica
complexidade_variacoes <- c(1.0, 1.1, 1.2, 1.3, 1.4)
sensibilidade_complexidade <- sapply(complexidade_variacoes, function(comp) {
    calcular_custo_mc(num_pontos_funcao, min_preco, mode_preco, max_preco, experiencia_equipe, comp, n_simulacoes)
})
mean_custos_complexidade <- colMeans(sensibilidade_complexidade)
plot(complexidade_variacoes, mean_custos_complexidade, type = "b", xlab = "Complexidade TC)cnica", ylab = "Custo MC)dio (R$)", main = "AnC!lise de Sensibilidade - Complexidade TC)cnica")


dados_projetos <- data.frame(
    Especificacao = c(
        "Desenvolvedor JAVA", "Desenvolvedor PHP", "Desenvolvedor Python", "Desenvolvedor Mobile", "Desenvolvedor Outras linguagens",
        "Desenvolvimento manutenC'C#o de sistema legado", "Desenvolvedor JAVA para correC'C5es e novos modulos",
        "Desenvolvedor PHP para correC'C5es e novos modulos", "Desenvolvedor Python para correC'C5es e novos modulos",
        "Desenvolvedor Mobile para correC'C5es e novos modulos", "Desenvolvedor Outras lingugens para correC'C5es e novos modulos"
    ),
    Unidade = "Ponto de FunCC#o",
    Quantidade = c(
        16429, 5546, 150, 2747, 58, 1796, 4933, 5541, 343, 1868, 511
    ),
    Valor_Unitario = c(
        565.15, 565.15, 565.15, 565.15, 565.15, 680.00, 527.00, 527.00, 527.00, 527.00, 527.00
    )
)

print(dados_projetos)

simular_por_tipo <- function(dados_projetos, n_simulacoes, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica) {
    resultados <- list()
    for (i in seq_len(nrow(dados_projetos))) {
        num_pontos_funcao <- dados_projetos$Quantidade[i]
        custos <- calcular_custo_mc(num_pontos_funcao, min_preco, mode_preco, max_preco, experiencia_equipe, complexidade_tecnica, n_simulacoes)
        resultados[[dados_projetos$Especificacao[i]]] <- custos
    }
    return(resultados)
}

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
    Intervalo_ConfianCa = character(),
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
        Intervalo_ConfianCa = paste0("(", intervalo_confianca[1], ", ", intervalo_confianca[2], ")")
    ))
}

print(estatisticas)


# Definir um limite superior de custo para cada tipo de desenvolvimento
limite_superior <- 120000

# Calcular a probabilidade de ultrapassar o limite superior para cada tipo de desenvolvimento
probabilidades <- data.frame(
    Especificacao = character(),
    Probabilidade = numeric(),
    stringsAsFactors = FALSE
)

for (tipo in names(resultados_simulacao)) {
    custos <- resultados_simulacao[[tipo]]
    probabilidade_ultrapassar <- mean(custos > limite_superior)

    probabilidades <- rbind(probabilidades, data.frame(
        Especificacao = tipo,
        Probabilidade = probabilidade_ultrapassar * 100
    ))
}

print(probabilidades)


estimativas_custos <- data.frame(
    Especificacao = c(
        "Desenvolvedor JAVA", "Desenvolvedor PHP", "Desenvolvedor Python",
        "Desenvolvedor Mobile", "Desenvolvedor Outras linguagens",
        "Desenvolvimento manutenCC#o de sistema legado",
        "Desenvolvedor JAVA para correCC5es e novos modulos",
        "Desenvolvedor PHP para correCC5es e novos modulos",
        "Desenvolvedor Python para correCC5es e novos modulos",
        "Desenvolvedor Mobile para correCC5es e novos modulos",
        "Desenvolvedor Outras lingugens para correCC5es e novos modulos"
    ),
    Custo_Minimo = c(430, 430, 430, 430, 430, 430, 430, 430, 430, 430, 430),
    Custo_Maximo = c(886.51, 886.51, 886.51, 886.51, 886.51, 886.51, 886.51, 886.51, 886.51, 886.51, 886.51),
    Custo_Modal = c(565.15, 565.15, 565.15, 565.15, 565.15, 565.15, 527, 527, 527, 527, 527)
)

# FunCC#o para simular os custos usando distribuiCC#o triangular
simular_custos <- function(min, max, mode, n) {
    rtriangle(n, min, max, mode)
}

# Aplicar a simulaCC#o de Monte Carlo para cada tipo de desenvolvimento
n_simulacoes <- 10000
resultados_simulacao <- lapply(seq_len(nrow(estimativas_custos)), function(i) {
    simular_custos(
        estimativas_custos$Custo_Minimo[i],
        estimativas_custos$Custo_Maximo[i],
        estimativas_custos$Custo_Modal[i],
        n_simulacoes
    )
})

names(resultados_simulacao) <- estimativas_custos$Especificacao

par(mfrow = c(2, 2)) # Organizar grC!ficos em uma grade de 2x2

for (tipo in names(resultados_simulacao)) {
    hist(resultados_simulacao[[tipo]], breaks = 50, main = paste("Custos -", tipo), xlab = "Custo Total (R$)", col = "blue", border = "black")
}

estatisticas <- data.frame(
    Especificacao = character(),
    Media = numeric(),
    Mediana = numeric(),
    Desvio_Padrao = numeric(),
    Intervalo_ConfianCa = character(),
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
        Intervalo_ConfianC'a = paste0("(", intervalo_confianca[1], ", ", intervalo_confianca[2], ")")
    ))
}

print(estatisticas)


# Dados das atas de registro de preC'os
atas <- data.frame(
    especificacao = c(
        "Desenvolvedor JAVA", "Desenvolvedor PHP", "Desenvolvedor Python", "Desenvolvedor Mobile",
        "Desenvolvedor Outras linguagens", "Desenvolvimento manutenC'C#o de sistema legado",
        "Desenvolvedor JAVA para correC'C5es e novos modulos", "Desenvolvedor PHP para correC'C5es e novos modulos",
        "Desenvolvedor Python para correC'C5es e novos modulos", "Desenvolvedor Mobile para correC'C5es e novos modulos",
        "Desenvolvedor Outras linguagens para correC'C5es e novos modulos",
        "Desenvolvedor JAVA", "Desenvolvedor PHP", "Desenvolvedor Python", "Desenvolvedor Mobile",
        "Desenvolvedor Outras linguagens", "Desenvolvimento manutenC'C#o de sistema legado",
        "Desenvolvedor JAVA para correC'C5es e novos modulos", "Desenvolvedor PHP para correC'C5es e novos modulos",
        "Desenvolvedor Python para correC'C5es e novos modulos", "Desenvolvedor Mobile para correC'C5es e novos modulos",
        "Desenvolvedor Outras linguagens para correC'C5es e novos modulos",
        "Desenvolvedor JAVA", "Desenvolvedor PHP", "Desenvolvedor Python", "Desenvolvedor Mobile",
        "Desenvolvedor Outras linguagens", "Desenvolvimento manutenC'C#o de sistema legado"
    ),
    unidade = "Ponto de FunC'C#o",
    quantidade = c(
        16429, 5546, 150, 2747, 58, 1796, 4933, 5541, 343, 1868, 511,
        5204, 295, 3940, 874, 2542, 4930, 8397, 2604, 901, 1000, 4804,
        7357, 6043, 200, 730, 184, 1644
    ),
    valor_unitario = c(
        565.15, 565.15, 565.15, 565.15, 565.15, 565.15, 527.00, 527.00, 527.00, 527.00, 527.00,
        600.00, 680.00, 680.00, 680.00, 680.00, 680.00, 540.00, 680.00, 680.00, 680.00, 540.00,
        550.00, 530.00, 840.00, 750.00, 800.00, 720.00
    )
)

# ParC"metros de simulaC'C#o
min_valor <- 430
max_valor <- 886.51
mode_valor <- 565.15
num_simulacoes <- 10000


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


complexidades <- data.frame(
    especificacao = c(
        "Desenvolvedor JAVA", "Desenvolvedor PHP", "Desenvolvedor Python", "Desenvolvedor Mobile",
        "Desenvolvedor Outras linguagens", "Desenvolvimento manutenC'C#o de sistema legado",
        "Desenvolvedor JAVA para correC'C5es e novos modulos", "Desenvolvedor PHP para correC'C5es e novos modulos",
        "Desenvolvedor Python para correC'C5es e novos modulos", "Desenvolvedor Mobile para correC'C5es e novos modulos",
        "Desenvolvedor Outras linguagens para correC'C5es e novos modulos"
    ),
    tipo_funcional = c(
        "ALI", "AIE", "EE", "CE", "SE", "ALI", "ALI", "AIE", "EE", "CE", "SE"
    ),
    complexidade = c(
        "Alta", "Media", "Baixa", "Media", "Alta", "Baixa", "Media", "Alta", "Baixa", "Media", "Alta"
    )
)

atas <- atas %>%
    left_join(complexidades, by = "especificacao") %>%
    rowwise() %>%
    mutate(pontos_funcao = calcular_pontos_funcao(tipo_funcional, complexidade) * quantidade)
if (any(is.na(atas$pontos_funcao))) {
    stop("Existem valores NaN nos pontos de funC'C#o calculados.")
}

print(head(atas$pontos_funcao))

custos_totais <- replicate(num_simulacoes, {
    custos_por_ponto <- rtriangle(nrow(atas), min_valor, mode_valor, max_valor)
    sum(custos_por_ponto * atas$pontos_funcao)
})

custos_totais_validos <- custos_totais[!is.na(custos_totais)]

hist(custos_totais_validos, breaks = 50, main = "DistribuiC'C#o dos Custos Totais do Projeto (Monte Carlo)", xlab = "Custo Total (R$)")

risco_custos <- quantile(custos_totais_validos, probs = c(0.05, 0.50, 0.95))
print(risco_custos)