library(ggplot2)
library(dplyr)
library(purrr)

# Função para determinar a complexidade
determine_complexity <- function(type, rlr, der) {
    if (type %in% c("ALI", "AIE")) {
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
    } else if (type %in% c("EE", "SE", "CE")) {
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

# Função para calcular pontos de função
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

# Função para calcular pontos de função ajustados
calcularPontosFuncao <- function(ILF, EIF, EI, EO, EQ, pesos, ajusteComplexidade) {
    PF <- ILF * pesos["ILF"] + EIF * pesos["EIF"] + EI * pesos["EI"] + EO * pesos["EO"] + EQ * pesos["EQ"]
    PF_ajustado <- PF * (0.65 + 0.01 * ajusteComplexidade)
    return(PF_ajustado)
}

# Função para calcular total de pontos de função
calculate_total_fp <- function(components, pesos, ajusteComplexidade) {
    total_fp <- 0
    for (component in components) {
        type <- component$type
        rlr <- component$rlr
        der <- component$der
        complexity <- determine_complexity(type, rlr, der)
        fp <- calculate_fp(type, complexity)
        total_fp <- total_fp + fp
    }
    PF_ajustado <- calcularPontosFuncao(
        ILF = sum(sapply(components, function(x) ifelse(x$type == "ALI" | x$type == "AIE", 1, 0))),
        EIF = sum(sapply(components, function(x) ifelse(x$type == "EE" | x$type == "SE" | x$type == "CE", 1, 0))),
        EI = sum(sapply(components, function(x) ifelse(x$type == "EI", 1, 0))),
        EO = sum(sapply(components, function(x) ifelse(x$type == "EO", 1, 0))),
        EQ = sum(sapply(components, function(x) ifelse(x$type == "EQ", 1, 0))),
        pesos = pesos,
        ajusteComplexidade = ajusteComplexidade
    )
    return(PF_ajustado)
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
        number = "00022/2020",
        initial_value = 2964982.00,
        final_value = 2964982.00,
        items = list(
            list(description = "NA", quantity = NA, unit_price = NA)
        )
    ),
    list(
        number = "00021/2021",
        initial_value = 30065000.00,
        final_value = 27658635.87,
        items = list(
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS", quantity = 3000, unit_price = 10021.67),
            list(description = "SUSTENTACAO DE SOFTWARE", quantity = 5000, unit_price = 10021.67)
        )
    ),
    list(
        number = "00011/2020",
        initial_value = 4184000.00,
        final_value = 3437925.00,
        items = list(
            list(description = "DESENVOLVIMENTO E/OU EVOLUCAO DE SOFTWARE - OUTRAS LINGUAGENS", quantity = 3280, unit_price = 1275.00),
            list(description = "SUSTENTACAO DE SOFTWARE", quantity = 3280, unit_price = 1275.00)
        )
    )
)

# Convertendo a estrutura de dados para um dataframe
contracts_df <- contracts %>%
    map_dfr(~ {
        contract <- .x
        items_df <- as.data.frame(contract$items)
        contract$items <- NULL
        cbind(contract, items_df)
    })

# Calculando custo total por contrato
contracts_df$total_value <- with(contracts_df, ifelse(is.na(quantity), unit_price, quantity * unit_price))

# Selecionando contratos válidos (excluindo contrato "00020/2021" devido à grande divergência)
contracts_valid <- contracts_df %>%
    filter(number != "00020/2021")

# Calculando pontos de função ajustados por contrato
components <- list(
    list(type = "ILF", rlr = 5, der = 25),
    list(type = "ILF", rlr = 3, der = 15),
    list(type = "AIE", rlr = 1, der = 10),
    list(type = "EE", rlr = 1, der = 3),
    list(type = "SE", rlr = 2, der = 7),
    list(type = "EO", rlr = 3, der = 4),
    list(type = "EQ", rlr = 4, der = 8)
)

pesos <- c("ILF" = 7, "EIF" = 5, "EI" = 3, "EO" = 4, "EQ" = 3)

adjusted_fps <- contracts_valid %>%
    group_by(number) %>%
    summarise(total_fp = calculate_total_fp(components, pesos, ajusteComplexidade = 0.65))

# Exibindo resultados
print(adjusted_fps)
