import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns


# Definindo a função para gerar valores da distribuição triangular
def rtriang(n, min_val, mode, max_val):
    return np.random.triangular(min_val, mode, max_val, n)


# Simulando os custos para o exemplo
np.random.seed(123)
custos = rtriang(10000, min_val=450, mode=1200, max_val=2500) * 47.6 * 1.1 * 1.2

# Configurando o estilo do gráfico
sns.set(style="whitegrid")

# Criando o histograma dos custos simulados
plt.figure(figsize=(10, 6))
sns.histplot(custos, bins=50, color="blue", kde=False)

# Formatando o eixo x
plt.gca().xaxis.set_major_formatter(
    plt.FuncFormatter(lambda x, loc: "{:,}".format(int(x)))
)
plt.xlabel("Custo (R$)")
plt.ylabel("Frequência")
plt.title("Histograma dos Custos Simulados")
plt.show()
