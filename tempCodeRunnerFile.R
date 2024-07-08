ggplot(data.frame(average_costs_per_fp), aes(x = average_costs_per_fp)) +
    geom_histogram(binwidth = 5000, fill = "blue", alpha = 0.7, color = "black") +
    labs(
        title = "Distribuição dos Custos Médios por Ponto de Função",
        x = "Custo Médio por Ponto de Função (R$)",
        y = "Frequência"
    )