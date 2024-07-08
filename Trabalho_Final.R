library(ggplot2)
library(dplyr)
library(purrr)

# Funções existentes para calcular pontos de função
determine_complexity <- function(type, rlr, der) {
    if (type == "ALI" || type == "AIE") {
        if (rlr <= 1) {
            if (der <= 19) {
                return("Simples")
            } else {
                return("Média")
            }
        } else if (rlr <= 5) {
            if (der <= 19) {
                return("Simples")
            } else {
                return("Média")
            }
        } else {
            if (der <= 19) {
                return("Média")
            } else {
                return("Complexa")
            }
        }
    } else if (type == "EE" || type == "SE" || type == "CE") {
        if (rlr <= 1) {
            if (der <= 4) {
                return("Simples")
            } else if (der <= 15) {
                return("Média")
            } else {
                return("Complexa")
            }
        } else if (rlr <= 3) {
            if (der <= 4) {
                return("Simples")
            } else if (der <= 15) {
                return("Média")
            } else {
                return("Complexa")
            }
        } else {
            if (der <= 4) {
                return("Média")
            } else {
                return("Complexa")
            }
        }
    } else {
        stop("Tipo de função desconhecido")
    }
}

calculate_fp <- function(type, complexity) {
    fp_matrix <- matrix(c(
        7, 10, 15, # ALI
        5,  7, 10, # AIE
        3,  4,  6, # EE
        4,  5,  7, # SE
        3,  4,  6 # CE
    ), nrow = 5, byrow = TRUE)

    rownames(fp_matrix) <- c("ALI", "AIE", "EE", "SE", "CE")
    colnames(fp_matrix) <- c("Simples", "Média", "Complexa")

    return(fp_matrix[type, complexity])
}

calculate_total_fp <- function(components) {
    total_fp <- 0
    for (component in components) {
        type <- component$type
        rlr <- component$rlr
        der <- component$der
        complexity <- determine_complexity(type, rlr, der)
        fp <- calculate_fp(type, complexity)
        total_fp <- total_fp + fp
    }
    return(total_fp)
}

calculate_project_value <- function(components, cost_per_fp) {
    total_fp <- calculate_total_fp(components)
    total_value <- total_fp * cost_per_fp
    return(list(total_fp = total_fp, total_value = total_value))
}

# Estrutura de dados dos contratos
contracts <- list(
    list(
        number = "00004/2020",
        initial_value = 3230000.00,
        final_value = 2981772.80,
        items = list(
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS", quantity = 4480, unit_price = 524.96),
            list(description = "SUSTENTAÇÃO DE SOFTWARE", quantity = 1200, unit_price = 524.96)
        )
    ),
    list(
        number = "00061/2021",
        initial_value = 3705000.00,
        final_value = 3705000.00,
        items = list(
            list(description = "DESENVOLVIMENTO DE NOVO SOFTWARE - JAVA", quantity = 6000, unit_price = 494.00),
            list(description = "MANUTENCAO DE SOFTWARE (CORRETIVA, PREVENTIVA, ADAPTATIVA)", quantity = 1500, unit_price = 494.00)
        )
    ),
    list(
        number = "6/2016",
        initial_value = 7650000.00,
        final_value = 32467808.70,
        items = list(
            list(description = "MANUTENCAO / INSTALACAO / DESENVOLVIMENTO SOFTWARE", quantity = 7650, unit_price = 7650000.00)
        )
    ),
    list(
        number = "00006/2021",
        initial_value = 12164000.00,
        final_value = 10301556.75,
        items = list(
            list(description = "SEM INFORMAÇÃO", quantity = NA, unit_price = NA)
        )
    ),
    list(
        number = "00031/2023",
        initial_value = 11335267.92,
        final_value = 11335267.92,
        items = list(
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS - SÊNIOR", quantity = 6, unit_price = 361020.82),
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS - PLENO", quantity = 6, unit_price = 275293.05),
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS - SÊNIOR", quantity = 9, unit_price = 251849.63),
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS - PLENO", quantity = 9, unit_price = 186887.90),
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS - CIENTISTA DE DADOS", quantity = 3, unit_price = 393745.98),
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS - ANALISTA DE REQUISITOS", quantity = 6, unit_price = 230015.39),
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS - ANALISTA DE SEGURANÇA", quantity = 3, unit_price = 335805.55)
        )
    ),
    list(
        number = "00030/2020",
        initial_value = 3640000.00,
        final_value = 4193585.76,
        items = list(
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS", quantity = 3600, unit_price = 599.08),
            list(description = "SUSTENTACAO DE SOFTWARE", quantity = 3400, unit_price = 599.08)
        )
    ),
    list(
        number = "00035/2020",
        initial_value = 19830000.00,
        final_value = 22809600.00,
        items = list(
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS", quantity = 5000, unit_price = 760.32),
            list(description = "MANUTENCAO EVOLUTIVA DE SOFTWARE (ACRESCIMO DE NOVAS FUNCIONALIDADES) - OUTRAS LINGUAGENS", quantity = 20000, unit_price = 760.32),
            list(description = "MANUTENCAO DE SOFTWARE (CORRETIVA, PREVENTIVA, ADAPTATIVA)", quantity = 5000, unit_price = 760.32)
        )
    ),
    list(
        number = "00032/2023",
        initial_value = 872109.00,
        final_value = 872109.00,
        items = list(
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS - ANALISTA DE BI - SÊNIOR", quantity = 3, unit_price = 290703.00)
        )
    ),
    list(
        number = "11/2018",
        initial_value = 2041999.80,
        final_value = 3095857.80,
        items = list(
            list(description = "DESENVOLVIMENTO E OU EVOLUCAO DE SOFTWARE - FRAMEWORK, .NET,BASIC, ASP, DELPHI, PASCAL E PERL", quantity = 22900, unit_price = 2041999.80)
        )
    ),
    list(
        number = "00020/2021",
        initial_value = 2292867.00,
        final_value = 3095313.72,
        items = list(
            list(description = "SUSTENTACAO DE SOFTWARE - ADMINISTRADOR DE DADOS - PLENO", quantity = 1, unit_price = 184514.88),
            list(description = "SUSTENTACAO DE SOFTWARE - ADMINISTRADOR DE DADOS - SÊNIOR", quantity = 1, unit_price = 253758.72),
            list(description = "SUSTENTACAO DE SOFTWARE - ANALISTA DE REQUISITOS - PLENO", quantity = 1, unit_price = 181068.00),
            list(description = "SUSTENTACAO DE SOFTWARE - ANALISTA DE REQUISITOS - SÊNIOR", quantity = 2, unit_price = 253035.12),
            list(description = "SUSTENTACAO DE SOFTWARE - CIENTISTA DE DADOS", quantity = 1, unit_price = 393273.96),
            list(description = "SUSTENTACAO DE SOFTWARE - ARQUITETO DE SISTEMAS - PLENO", quantity = 2, unit_price = 206016.24),
            list(description = "SUSTENTACAO DE SOFTWARE - ARQUITETO DE SISTEMAS - SÊNIOR", quantity = 2, unit_price = 279258.84),
            list(description = "SUSTENTACAO DE SOFTWARE - DBA - PLENO", quantity = 1, unit_price = 182645.40),
            list(description = "SUSTENTACAO DE SOFTWARE - DBA - SÊNIOR", quantity = 1, unit_price = 253035.12),
            list(description = "SUSTENTACAO DE SOFTWARE - DESENVOLVEDOR MOBILE", quantity = 1, unit_price = 186009.48),
            list(description = "SUSTENTACAO DE SOFTWARE - DEVOPS", quantity = 2, unit_price = 183822.24),
            list(description = "SUSTENTACAO DE SOFTWARE - ENGENHEIRO DE DADOS", quantity = 1, unit_price = 186162.12),
            list(description = "SUSTENTACAO DE SOFTWARE - FULLSTACK - PLENO", quantity = 2, unit_price = 182645.40),
            list(description = "SUSTENTACAO DE SOFTWARE - FULLSTACK - SÊNIOR", quantity = 2, unit_price = 253035.12),
            list(description = "SUSTENTACAO DE SOFTWARE - GERENTE DE PROJETO", quantity = 1, unit_price = 360330.60),
            list(description = "SUSTENTACAO DE SOFTWARE - TÉCNICO DE TESTES - PLENO", quantity = 2, unit_price = 181521.48),
            list(description = "SUSTENTACAO DE SOFTWARE - TÉCNICO DE TESTES - SÊNIOR", quantity = 2, unit_price = 253035.12)
        )
    ),
    list(
        number = "00006/2023",
        initial_value = 705689.52,
        final_value = 705689.52,
        items = list(
            list(description = "TREINAMENTO E CAPACITAÇÃO", quantity = 6, unit_price = 117614.92)
        )
    ),
    list(
        number = "00047/2022",
        initial_value = 3596362.82,
        final_value = 3487443.84,
        items = list(
            list(description = "FASE DE DESENVOLVIMENTO DE SISTEMAS - DESENVOLVEDOR DE SISTEMAS - JAVA - PLENO", quantity = 12, unit_price = 197206.44),
            list(description = "FASE DE DESENVOLVIMENTO DE SISTEMAS - DESENVOLVEDOR DE SISTEMAS - JAVA - SÊNIOR", quantity = 12, unit_price = 264463.44)
        )
    )
)

# Função para calcular custo médio por ponto de função baseado nos contratos
calculate_average_cost_per_fp <- function(contracts) {
    total_value <- sum(sapply(contracts, function(contract) {
        sum(sapply(contract$items, function(item) {
            ifelse(!is.na(item$quantity) & !is.na(item$unit_price), item$quantity * item$unit_price, 0)
        }))
    }))
    total_fp <- sum(sapply(contracts, function(contract) {
        sum(sapply(contract$items, function(item) {
            ifelse(!is.na(item$quantity), item$quantity, 0)
        }))
    }))
    return(ifelse(total_fp > 0, total_value / total_fp, NA))
}


# Excluir o contrato Número do Contrato 00020/2021
contracts <- contracts[!sapply(contracts, function(contract) contract$number == "00020/2021")]
contracts <- contracts[!sapply(contracts, function(contract) contract$number == "00031/2023")]

# Calcular o custo médio por ponto de função
average_cost_per_fp <- calculate_average_cost_per_fp(contracts)

# Comparar valores reais com valores calculados
comparisons <- data.frame(
    contract_number = character(),
    real_value = numeric(),
    calculated_value = numeric(),
    percentage_difference = numeric(),
    description = character(),
    stringsAsFactors = FALSE
)

for (contract in contracts) {
    if (!is.na(contract$items[[1]]$quantity)) {
        items <- lapply(contract$items, function(item) {
            list(type = "ALI", rlr = 1, der = 1) # Substituir pelos valores reais de rlr e der
        })
        result <- calculate_project_value(items, average_cost_per_fp)
        percentage_difference <- (result$total_value - contract$final_value) / contract$final_value * 100
        comparisons <- rbind(comparisons, data.frame(
            contract_number = contract$number,
            real_value = contract$final_value,
            calculated_value = result$total_value,
            percentage_difference = percentage_difference,
            description = contract$items[[1]]$description
        ))
    }
}

# Função para formatar a diferença percentual de forma descritiva
format_percentage_difference <- function(percentage) {
    if (percentage > 0) {
        return(paste("acima do valor feito em nossa calculadora em", round(percentage, 1), "%"))
    } else {
        return(paste("abaixo do valor feito em nossa calculadora em", round(abs(percentage), 1), "%"))
    }
}

# Aplicar a formatação descritiva na coluna percentage_difference
comparisons$description <- factor(comparisons$description, levels = unique(comparisons$description))
comparisons$percentage_description <- sapply(comparisons$percentage_difference, format_percentage_difference)

# Exibir a tabela com os resultados formatados
print(comparisons)

# Visualização das barras comparativas com valores e rótulos
ggplot(comparisons, aes(x = contract_number)) +
    geom_col(aes(y = real_value, fill = "Valor Real"), alpha = 0.7) +
    geom_col(aes(y = calculated_value, fill = "Valor Calculado"), alpha = 0.7) +
    geom_text(aes(y = real_value, label = scales::dollar(real_value, prefix = "R$")), vjust = -0.5) +
    geom_text(aes(y = calculated_value, label = scales::dollar(calculated_value, prefix = "R$")), vjust = 1.5) +
    scale_fill_manual(values = c("Valor Real" = "blue", "Valor Calculado" = "red")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
        title = "Comparação de Valores de Contratos Reais vs. Calculados",
        x = "Número do Contrato", y = "Valor (R$)"
    )

# Visualização do histograma de porcentagem de diferença
ggplot(comparisons, aes(x = percentage_difference)) +
    geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7, color = "black") +
    labs(title = "Histograma das Diferenças Percentuais", x = "Diferença Percentual (%)", y = "Frequência")

# Visualização das barras comparativas com valores e rótulos
ggplot(comparisons, aes(x = contract_number)) +
    geom_bar(aes(y = real_value, fill = "Valor Real"), stat = "identity", alpha = 0.7) +
    geom_bar(aes(y = calculated_value, fill = "Valor Calculado"), stat = "identity", alpha = 0.7) +
    geom_text(aes(y = real_value, label = paste("R$", format(real_value, big.mark = ".", decimal.mark = ","))), vjust = -0.5) +
    geom_text(aes(y = calculated_value, label = paste("R$", format(calculated_value, big.mark = ".", decimal.mark = ","))), vjust = 1.5) +
    scale_fill_manual(values = c("Valor Real" = "blue", "Valor Calculado" = "red")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
        title = "Comparação de Valores de Contratos Reais vs. Calculados",
        x = "Número do Contrato", y = "Valor (R$)"
    )

# Visualização do histograma de porcentagem de diferença com curva de densidade
ggplot(comparisons, aes(x = percentage_difference)) +
    geom_histogram(aes(y = ..density..), binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_density(color = "red", size = 1) +
    labs(
        title = "Distribuição das Diferenças Percentuais",
        x = "Diferença Percentual (%)", y = "Densidade"
    )

# Função para simular dados de quantidade e preço unitário
simulate_data <- function(n_simulations, contracts) {
    simulated_contracts <- vector("list", n_simulations)

    for (i in 1:n_simulations) {
        simulated_contract <- lapply(contracts, function(contract) {
            simulated_items <- lapply(contract$items, function(item) {
                simulated_quantity <- rnorm(1, mean = item$quantity, sd = 0.1 * item$quantity) # Exemplo de distribuição normal para quantidade
                simulated_unit_price <- rnorm(1, mean = item$unit_price, sd = 0.1 * item$unit_price) # Exemplo de distribuição normal para preço unitário
                list(quantity = simulated_quantity, unit_price = simulated_unit_price)
            })
            list(number = contract$number, items = simulated_items)
        })
        simulated_contracts[[i]] <- simulated_contract
    }

    return(simulated_contracts)
}

# Função para calcular custo médio por ponto de função baseado nos contratos simulados
calculate_average_cost_per_fp <- function(simulated_contracts) {
    total_fp <- 0
    total_value <- 0
    for (simulated_contract in simulated_contracts) {
        for (item in simulated_contract$items) {
            if (!is.na(item$quantity) && !is.na(item$unit_price)) {
                total_value <- total_value + (item$quantity * item$unit_price)
                total_fp <- total_fp + item$quantity
            }
        }
    }
    return(total_value / total_fp)
}

# Simulação de dados para 1000 iterações
n_simulations <- 1000
simulated_contracts <- simulate_data(n_simulations, contracts)

# Calcular o custo médio por ponto de função para cada simulação
average_costs_per_fp <- map_dbl(simulated_contracts, ~ calculate_average_cost_per_fp(.x))

# Visualização da distribuição dos custos médios por ponto de função
ggplot(data.frame(average_costs_per_fp), aes(x = average_costs_per_fp)) +
    geom_histogram(binwidth = 5000, fill = "blue", alpha = 0.7, color = "black") +
    labs(
        title = "Distribuição dos Custos Médios por Ponto de Função",
        x = "Custo Médio por Ponto de Função (R$)",
        y = "Frequência"
    )

# Análise de sensibilidade
summary(average_costs_per_fp)
quantile(average_costs_per_fp, c(0.05, 0.95))


#------------------------------------------------------------------------------#
# Detalhando os Resultados da Simulação
#------------------------------------------------------------------------------#

# Cálculo de estatísticas descritivas e intervalos de confiança
mean_cost_per_fp <- mean(average_costs_per_fp)
sd_cost_per_fp <- sd(average_costs_per_fp)
var_cost_per_fp <- var(average_costs_per_fp)
ci_95 <- quantile(average_costs_per_fp, c(0.025, 0.975))

# Exibição das estatísticas descritivas e intervalos de confiança
cat("Estatísticas Descritivas dos Custos Médios por Ponto de Função:\n")
cat("- Média:", format(mean_cost_per_fp, big.mark = ".", decimal.mark = ","), "R$\n")
cat("- Desvio Padrão:", format(sd_cost_per_fp, big.mark = ".", decimal.mark = ","), "R$\n")
cat("- Variância:", format(var_cost_per_fp, big.mark = ".", decimal.mark = ","), "R$\n")
cat(
    "- Intervalo de Confiança (95%):",
    paste(
        "R$", format(ci_95[1], big.mark = ".", decimal.mark = ","), "-",
        format(ci_95[2], big.mark = ".", decimal.mark = ",")
    )
)