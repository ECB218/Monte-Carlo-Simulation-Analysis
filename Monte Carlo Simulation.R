library(ggplot2)
library(dplyr)
set.seed(42)

#   Portfolio A: 100% Gold (7% return, 18% volatility)
#   Portfolio B: Diversified portfolio (8% return, 12% volatility)
#   Initial investment of $100k, 10k iterations over 10 years

initial_investment <- 100000
years <- 10
iterations <- 10000
mu_gold <- 0.07
sigma_gold <- 0.18
mu_div <- 0.08
sigma_div <- 0.12

simulate_portfolio <- function(mu, sigma, years, iterations, initial) {
  returns <- matrix(rnorm(years * iterations, mean = mu, sd = sigma), nrow = years)
  wealth_paths <- initial * apply(1 + returns, 2, cumprod)
  return(wealth_paths)
}

wealth_gold <- simulate_portfolio(mu_gold, sigma_gold, years, iterations, initial_investment)
wealth_div  <- simulate_portfolio(mu_div, sigma_div, years, iterations, initial_investment)

final_gold <- wealth_gold[years, ]
final_div  <- wealth_div[years, ]

# 1. 50 Random Trial Paths
path_indices <- sample(1:iterations, 50)

paths_data <- data.frame(
  Year = rep(0:years, length(path_indices) * 2),
  Value = c(
    as.vector(rbind(matrix(initial_investment, 1, length(path_indices)), wealth_gold[, path_indices])),
    as.vector(rbind(matrix(initial_investment, 1, length(path_indices)), wealth_div[, path_indices]))
  ),
  Portfolio = rep(c("Gold", "Diversified"), each = (years + 1) * length(path_indices)),
  Path = rep(rep(1:50, each = years + 1), 2)
)

ggplot(paths_data, aes(x = Year, y = Value, group = interaction(Portfolio, Path), color = Portfolio)) +
  geom_line(alpha = 0.3) +
  scale_color_manual(values = c("Gold" = "gold", "Diversified" = "blue")) +
  labs(title = "Trial Paths for Both Portfolios",
       x = "Years", y = "Portfolio Value ($)") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("paths_r.png", width = 12, height = 6)

# 2. Final Wealth Distribution
final_df <- data.frame(
  Wealth = c(final_gold, final_div),
  Portfolio = rep(c("Gold", "Diversified"), each = iterations)
)

ggplot(final_df, aes(x = Wealth, fill = Portfolio)) +
  geom_histogram(bins = 100, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("Gold" = "gold", "Diversified" = "blue")) +
  labs(title = "Wealth Distribution",
       x = "Final Wealth ($)", y = "Frequency") +
  theme_minimal()

ggsave("histogram_r.png", width = 12, height = 6)

# 3. Percentiles Table
percentiles <- function(x) {
  c(`10th` = quantile(x, 0.10),
    `50th (Median)` = quantile(x, 0.50),
    `90th` = quantile(x, 0.90))
}

cat("\n=== Percentiles Comparison ===\n")
print("Portfolio A (100% Gold):")
print(round(percentiles(final_gold), 0))

print("\nPortfolio B (Diversified):")
print(round(percentiles(final_div), 0))

# 4. 95% VaR
var_95_gold <- initial_investment - quantile(final_gold, 0.05)
var_95_div  <- initial_investment - quantile(final_div, 0.05)

cat("\n=== 95% Value at Risk (Potential Loss) ===\n")
cat("Gold:         $", round(var_95_gold, 0), "\n")
cat("Diversified:  $", round(max(0, var_95_div), 0), "\n")  # floor at 0

# Summary Table
cat("\n=== Summary Table ===\n")
results <- data.frame(
  Metric = c("10th Percentile", "Median", "90th Percentile"),
  Gold = round(percentiles(final_gold), 0),
  Diversified = round(percentiles(final_div), 0)
)
print(results)