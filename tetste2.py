import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats

# Simulated cost data per function point (randomly generated for demonstration)
np.random.seed(42)
simulated_costs = np.random.normal(loc=1828, scale=267, size=1000)

# Histogram and Q-Q plot
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

# Histogram
ax1.hist(simulated_costs, bins=30, color="skyblue", edgecolor="black", alpha=0.7)
ax1.set_title("Histogram of Simulated Costs")
ax1.set_xlabel("Cost per Function Point (R$)")
ax1.set_ylabel("Frequency")

# Q-Q plot
stats.probplot(simulated_costs, dist="norm", plot=ax2)
ax2.set_title("Q-Q Plot of Simulated Costs")

plt.tight_layout()
plt.show()

# Shapiro-Wilk Test
shapiro_test = stats.shapiro(simulated_costs)
shapiro_test
