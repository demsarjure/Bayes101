# libraries --------------------------------------------------------------------
library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)
library(mcmcse)
library(ggplot2)
library(emg) # for exponentially modified Gaussian (normal)


# prep -------------------------------------------------------------------------
# data
data <- read.csv("./data/flanker.csv")
df_incongruent <- data %>% filter(congruency == "incongruent")
df_congruent <- data %>% filter(congruency == "congruent")

# normal model
model_normal <- cmdstan_model("./models/normal.stan")

# exponentially modified gaussian/normal (EMG) model
# normal + exponential: mean = mu + 1/lambda, var = sigma^2 + 1\lambda^2
model_exp <- cmdstan_model("./models/exp_normal.stan")


# fit incongruent with the normal model ----------------------------------------
# prep the data for Stan
n <- nrow(df_incongruent)
y <- df_incongruent$rt
stan_data <- list(n = n, y = y)

# fit
fit_normal <- model_normal$sample(
    data = stan_data
)

# diagnostics
mcmc_trace(fit_normal$draws())
fit_normal$summary()

# extract draws
df_normal <- as_draws_df(fit_normal$draws())

# use 100 samples to generate underlying distributions
n <- 100
df_normal_100 <- sample_n(df_normal, n)
x <- seq(0, 2, length.out = 1000)

# data frame for storing generated data
df_generated_normal <- data.frame(x = numeric(), y = numeric(), id = numeric())
for (i in 1:100) {
    y <- dnorm(x, mean = df_normal_100$mu[i], sd = df_normal_100$sigma[i])

    # bind
    df_generated_normal <- rbind(
        df_generated_normal,
        data.frame(x = x, y = y, id = i)
    )
}

# visual check of the fit
ggplot() +
    geom_density(
        data = df_incongruent, aes(x = rt),
        fill = "skyblue", alpha = 0.75, color = NA
    ) +
    geom_line(
        data = df_generated_normal,
        aes(x = x, y = y, group = id),
        alpha = 0.05,
        linewidth = 1
    ) +
    theme_minimal() +
    xlab("Reaction time [s]") +
    ylab("Density")


# fit incongruent with the emg model -------------------------------------------
fit_exp_i <- model_exp$sample(
    data = stan_data
)

# diagnostics
mcmc_trace(fit_exp_i$draws())
fit_exp_i$summary()

# extract draws
df_exp_i <- as_draws_df(fit_exp_i$draws())

# use 100 samples to generate underlying distributions
n <- 100
df_exp_i_100 <- sample_n(df_exp_i, n)
x <- seq(0, 2, length.out = 1000)

# data frame for storing generated data
df_generated_exp_i <- data.frame(x = numeric(), y = numeric(), id = numeric())
for (i in 1:100) {
    y <- demg(x,
        mu = df_exp_i_100$mu[i],
        sigma = df_exp_i_100$sigma[i],
        lambda = df_exp_i_100$lambda[i]
    )

    # bind
    df_generated_exp_i <- rbind(
        df_generated_exp_i,
        data.frame(x = x, y = y, id = i)
    )
}

# posterior predictive check
ggplot() +
    geom_density(
        data = df_incongruent,
        aes(x = rt),
        fill = "skyblue",
        alpha = 0.75,
        color = NA
    ) +
    geom_line(
        data = df_generated_exp_i,
        aes(x = x, y = y, group = id),
        alpha = 0.05,
        linewidth = 1
    ) +
    theme_minimal() +
    xlab("Reaction time [s]") +
    ylab("Density")


# fit congruent with the emg model ---------------------------------------------
# data prep
n <- nrow(df_congruent)
y <- df_congruent$rt
stan_data <- list(n = n, y = y)

fit_exp_c <- model_exp$sample(
    data = stan_data
)

# diagnostics
mcmc_trace(fit_exp_c$draws())
fit_exp_c$summary()

# extract draws
df_exp_c <- as_draws_df(fit_exp_c$draws())


# compare congruent vs incongruent ---------------------------------------------
# incongruent
# !!! distribution mean is now mu + 1/lambda !!!
# !!! mu is the mean of the normal component !!!
mean_i <- df_exp_i$mu + (1 / df_exp_i$lambda)
mean_c <- df_exp_c$mu + (1 / df_exp_c$lambda)
mcse(mean_c < mean_i)

# visualize
df_plot <- data.frame(mean = mean_i, label = "incongruent") %>%
    add_row(data.frame(mean = mean_c, label = "congruent"))
ggplot(data = df_plot, aes(x = mean, y = label)) +
    stat_halfeye(fill = "skyblue", alpha = 0.75) +
    xlab("Reaction time") +
    ylab("")


# region of practical equivalence (ROPE) ---------------------------------------
comparison <- data.frame(equal = 0, c = 0, i = 0)
# measurement error 0.15 ms
rope <- 0.15
for (i in seq_len(nrow(df_exp_i))) {
    mean_i <- mean(df_exp_i[i, ]$mu + 1 / df_exp_i[i, ]$lambda)
    mean_c <- mean(df_exp_c[i, ]$mu + 1 / df_exp_c[i, ]$lambda)

    diff <- abs(mean_i - mean_c)

    if (diff > rope) {
        if (mean_c < mean_i) {
            comparison$c <- comparison$c + 1
        } else {
            comparison$i <- comparison$i + 1
        }
    } else {
        comparison$equal <- comparison$equal + 1
    }
}

# to percentages
comparison$equal <- comparison$equal / nrow(df_exp_i)
comparison$c <- comparison$c / nrow(df_exp_i)
comparison$i <- comparison$i / nrow(df_exp_i)
comparison
