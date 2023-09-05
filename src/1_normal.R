# libraries
library(cmdstanr) # Stan - our core package for Bayesian inference
library(bayesplot) # for visualizing fitting process
library(ggplot2) # visualizations
library(ggdist) # visualizing distributions
library(distributional) # visualizing distributions part 2
library(posterior) # for extracting posterior samples
library(tidyverse) # general R package for data manipulation
library(mcmcse) # for comparing posterior samples between groups


# prep -------------------------------------------------------------------------
# data
df_crop <- read.csv("./data/crop.csv")

# Bayeisan model
model <- cmdstan_model("./models/normal.stan")

# yields per fertilizer
yield_1 <- df_crop$yield[df_crop$fertilizer == 1]
yield_2 <- df_crop$yield[df_crop$fertilizer == 2]
yield_3 <- df_crop$yield[df_crop$fertilizer == 3]


# fertilizer 1 -----------------------------------------------------------------
# prep the data
stan_data <- list(
  n = length(yield_1),
  y = yield_1
)

# fit
fit_1 <- model$sample(
  data = stan_data,
  parallel_chains = 4
)

print(mcmc_trace(fit_1$draws()))
print(fit_1$summary())

# extract parameter samples
df_samples_1 <- as_draws_df(fit_1$draws())


# fertilizer 2 -----------------------------------------------------------------
# prep the data
stan_data <- list(
  n = length(yield_2),
  y = yield_2
)

# fit
fit_2 <- model$sample(
  data = stan_data,
  parallel_chains = 4
)

print(mcmc_trace(fit_2$draws()))
print(fit_2$summary())

# extract parameter samples
df_samples_2 <- as_draws_df(fit_2$draws())


# fertilizer 3 -----------------------------------------------------------------
# prep the data
stan_data <- list(
  n = length(yield_3),
  y = yield_3
)

# fit
fit_3 <- model$sample(
  data = stan_data,
  parallel_chains = 4
)

print(mcmc_trace(fit_3$draws()))
print(fit_3$summary())

# extract parameter samples
df_samples_3 <- as_draws_df(fit_3$draws())


# practical questions ----------------------------------------------------------
# 1. What is the probability that the mean yield of fertilizer 1 is greater than
#    the mean yield of fertilizer 2?
mcse(df_samples_1$mu > df_samples_2$mu)
mcse(df_samples_2$mu > df_samples_1$mu)

# 2. What is the probability that the mean yield of fertilizer 2 is greater than
#    the mean yield of fertilizer 3?
mcse(df_samples_2$mu > df_samples_3$mu)
mcse(df_samples_3$mu > df_samples_2$mu)

# 3. What is the probability that the mean yield of fertilizer 3 is greater than
#    the mean yield of fertilizer 1?
mcse(df_samples_3$mu > df_samples_1$mu)

# 4. What are the probabilites that one of the fertilizers is the best?
n <- nrow(df_samples_1)
best_1 <- vector()
best_2 <- vector()
best_3 <- vector()
for (i in 1:n) {
  mu_1 <- df_samples_1$mu[i]
  mu_2 <- df_samples_2$mu[i]
  mu_3 <- df_samples_3$mu[i]

  if (mu_1 > mu_2 && mu_1 > mu_3) {
    best_1 <- c(best_1, 1)
    best_2 <- c(best_2, 0)
    best_3 <- c(best_3, 0)
  } else if (mu_2 > mu_1 && mu_2 > mu_3) {
    best_1 <- c(best_1, 0)
    best_2 <- c(best_2, 1)
    best_3 <- c(best_3, 0)
  } else if (mu_3 > mu_1 && mu_3 > mu_2) {
    best_1 <- c(best_1, 0)
    best_2 <- c(best_2, 0)
    best_3 <- c(best_3, 1)
  } else {
    best_1 <- c(best_1, 0)
    best_2 <- c(best_2, 0)
    best_3 <- c(best_3, 0)
  }
}
mcse(best_1)
mcse(best_2)
mcse(best_3)

# 5. If we were to use fertilizer 3 next year, what is the probability that it
#    will yield more than other two?
n <- nrow(df_samples_1)
best_next_year_1 <- vector()
best_next_year_2 <- vector()
best_next_year_3 <- vector()
for (i in 1:n) {
  next_year_1 <- rnorm(1, df_samples_1$mu[i], df_samples_1$sigma[i])
  next_year_2 <- rnorm(1, df_samples_2$mu[i], df_samples_2$sigma[i])
  next_year_3 <- rnorm(1, df_samples_3$mu[i], df_samples_3$sigma[i])

  if (next_year_1 > next_year_2 && next_year_1 > next_year_3) {
    best_next_year_1 <- c(best_next_year_1, 1)
    best_next_year_2 <- c(best_next_year_2, 0)
    best_next_year_3 <- c(best_next_year_3, 0)
  } else if (next_year_2 > next_year_1 && next_year_2 > next_year_3) {
    best_next_year_1 <- c(best_next_year_1, 0)
    best_next_year_2 <- c(best_next_year_2, 1)
    best_next_year_3 <- c(best_next_year_3, 0)
  } else if (next_year_3 > next_year_1 && next_year_3 > next_year_2) {
    best_next_year_1 <- c(best_next_year_1, 0)
    best_next_year_2 <- c(best_next_year_2, 0)
    best_next_year_3 <- c(best_next_year_3, 1)
  } else {
    best_next_year_1 <- c(best_next_year_1, 0)
    best_next_year_2 <- c(best_next_year_2, 0)
    best_next_year_3 <- c(best_next_year_3, 0)
  }
}
mcse(best_next_year_1)
mcse(best_next_year_2)
mcse(best_next_year_3)


# a visualization of our results -----------------------------------------------
# means
# prepare the df
df_means <- data.frame(mu = df_samples_1$mu, label = "Fertilzer 1") %>%
  add_row(data.frame(mu = df_samples_2$mu, label = "Fertilzer 2")) %>%
  add_row(data.frame(mu = df_samples_3$mu, label = "Fertilzer 3"))

ggplot(data = df_means, aes(x = mu, y = label)) +
  stat_halfeye(fill = "skyblue", alpha = 0.75) +
  xlab("Yield") +
  ylab("") +
  xlim(173, 180)

# next year
mu_1 <- mean(df_samples_1$mu)
sigma_1 <- mean(df_samples_1$sigma)
mu_2 <- mean(df_samples_2$mu)
sigma_2 <- mean(df_samples_2$sigma)
mu_3 <- mean(df_samples_3$mu)
sigma_3 <- mean(df_samples_3$sigma)

df_next <- tibble(
  dist = c(
    dist_normal(mu_1, sigma_1),
    dist_normal(mu_2, sigma_2),
    dist_normal(mu_3, sigma_3)
  ),
  dist_name = c("Fertilzer 1", "Fertilzer 2", "Fertilzer 3")
)

ggplot(df_next, aes(y = dist_name, xdist = dist)) +
  stat_halfeye(fill = "skyblue", alpha = 0.75) +
  xlab("Yield") +
  ylab("") +
  xlim(173, 180)
