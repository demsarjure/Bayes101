# libraries --------------------------------------------------------------------
library(tidyverse) # for general data manipulation
library(cmdstanr) # core library for Bayesian statistics
library(bayesplot) # for dignostics visualization
library(posterior) # for extracting posterior samples
library(mcmcse) # for getting results and their error
library(ggplot2) # for visualizations
library(ggdist) # for visualizing distributions
library(distributional) # visualizing distributions part 2


# prep -------------------------------------------------------------------------
# data
df_crop <- read.csv("./data/crop.csv")
df_1 <- df_crop %>% filter(fertilizer == 1)
df_2 <- df_crop %>% filter(fertilizer == 2)
df_3 <- df_crop %>% filter(fertilizer == 3)

# model
model <- cmdstan_model("./models/normal.stan")


# fit the model for the first fertilizer ---------------------------------------
# prep data
stan_data <- list(
    n = length(df_1$yield),
    y = df_1$yield
)

# fit
fit_1 <- model$sample(
    data = stan_data
)

# diagnostics
mcmc_trace(fit_1$draws())
fit_1$summary()

# extract samples
df_samples_1 <- as_draws_df(fit_1$draws())


# fit the model for the second fertilizer --------------------------------------
# prep data
stan_data <- list(
    n = length(df_2$yield),
    y = df_2$yield
)

# fit
fit_2 <- model$sample(
    data = stan_data
)

# diagnostics
mcmc_trace(fit_2$draws())
fit_2$summary()

# extract samples
df_samples_2 <- as_draws_df(fit_2$draws())


# fit the model for the third fertilizer ---------------------------------------
# prep data
stan_data <- list(
    n = length(df_3$yield),
    y = df_3$yield
)

# fit
fit_3 <- model$sample(
    data = stan_data
)

# diagnostics
mcmc_trace(fit_3$draws())
fit_3$summary()

# extract samples
df_samples_3 <- as_draws_df(fit_3$draws())


# practical questions ----------------------------------------------------------
# 1. Is the mean yield of f1 better than the mean yield of f2?
mcse(df_samples_1$mu > df_samples_2$mu)
mcse(df_samples_2$mu > df_samples_1$mu)

# 2. Is the mean yield of f1 better than the mean yield of f3?
mcse(df_samples_1$mu > df_samples_3$mu)
mcse(df_samples_3$mu > df_samples_1$mu)

# 3. Is the mean yield of f3 better than the mean yield of f2?
mcse(df_samples_3$mu > df_samples_2$mu)

# 4. What is the probability that f3 is better than f1 and f2 on average?
n <- nrow(df_samples_3)
best_3 <- vector()
for (i in 1:n) {
    best_3[i] <-
        df_samples_3$mu[i] > df_samples_1$mu[i] &&
            df_samples_3$mu[i] > df_samples_2$mu[i]
}
mcse(best_3)

# 5. What is the probability that f3 will give better results next year?
best_next_year_3 <- vector()
for (i in 1:n) {
    next_1 <- rnorm(1, df_samples_1$mu[i], df_samples_1$sigma[i])
    next_2 <- rnorm(1, df_samples_2$mu[i], df_samples_2$sigma[i])
    next_3 <- rnorm(1, df_samples_3$mu[i], df_samples_3$sigma[i])
    best_next_year_3[i] <- next_3 > next_1 && next_3 > next_2
}
mcse(best_next_year_3)

# a visualization of our results -----------------------------------------------
# means
# prepare the df
df_means <- data.frame(mu = df_samples_1$mu, label = "Fertilzer 1") %>%
    add_row(data.frame(mu = df_samples_2$mu, label = "Fertilzer 2")) %>%
    add_row(data.frame(mu = df_samples_3$mu, label = "Fertilzer 3"))

# plot
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
