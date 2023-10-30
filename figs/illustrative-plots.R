library(ggplot2)
library(dplyr)
library(mvtnorm)
library(purrr)
library(distributional)
library(ggdensity)

rbinorm_tibble <- function(N, mean, sigma, ...){
  vals <- rmvnorm(N, mean = mean, sigma = sigma)
  tibble(x = vals[,1], y = vals[,2], ...)
}

posterior <- function(x, y, sim, mu0, Sigma0, Sigma){
  n <- length(x)
  xbar <- c(mean(x), mean(y))
  Sigma1 <- solve(solve(Sigma0) + n * solve(Sigma))
  mu1 <- Sigma1 %*% (solve(Sigma0, mu0) + n * solve(Sigma, xbar))
  list(mean = mu1, sigma = Sigma1, sim = unique(sim))
}

M <- 10 # replicates
N <- 10 # data size
S <- 50 # posterior sample size

# prior
mu0 <- rep(1,2)
Sigma0 <- diag(2)

# likelihood
Sigma <- diag(2)

# true values from prior
priortrue_data <- rbinorm_tibble(M, mean = mu0, sigma = Sigma0, sim = 1:M, val = "Prior") %>%
  mutate(val = ordered(val, levels = c("Prior", "Data", "Posterior")))

# data
obs_list <- priortrue_data %>% 
  pmap(function(x,y,sim,val) rbinorm_tibble(N, mean = c(x,y), sigma = Sigma, sim = sim, val = "Data") )
  
obs_data <- obs_list %>% list_rbind() %>%
  mutate(val = ordered(val, levels = c("Prior", "Data", "Posterior")))

# posterior
posterior_list <- obs_list %>% 
  map(function(df) posterior(df$x, df$y, df$sim, mu0, Sigma0, Sigma))

posterior_data <- posterior_list %>% 
  map(function(ls) rbinorm_tibble(S, mean = ls$mean, sigma = ls$sigma, sim = ls$sim, val = "Posterior")) %>%
  list_rbind()

approx_posterior_data <- posterior_data %>% 
  mutate(x = x + 0.5, y = y - 0.5, val = "Posterior") %>%
  mutate(val = ordered(val, levels = c("Prior", "Data", "Posterior")))

# background distributions

# distributions <- rbind(
#   tibble(dist = dist_multivariate_normal(mu = list(mu0), sigma = list(Sigma0)), sim = 1:M, val = "Prior"),
#   posterior_list %>% map(function(ls) tibble(dist = dist_multivariate_normal(mu = list(ls$mean), sigma = list(ls$sigma)), sim = ls$sim, val = "Posterior")) %>%
#     list_rbind
# )

distributions <- rbind(
  map(1:M, function(s) priortrue_data %>% mutate(sim = s, val = "Prior"))  %>% list_rbind(),
  posterior_data) %>% 
  mutate(val = ordered(val, levels = c("Prior", "Data", "Posterior")))


# sim_id <- 1:3
# 
# ggplot() + 
#   geom_hdr(aes(x,y), distributions %>% filter(sim %in% sim_id), method = "mvnorm", fill = "blue") + 
#   geom_point(aes(x,y), obs_data %>% filter(sim %in% sim_id)) +
#   geom_point(aes(x,y), approx_posterior_data %>% filter(sim %in% sim_id), alpha = 0.2) +
#   geom_point(aes(x,y), priortrue_data %>% filter(sim %in% sim_id), colour = "red") +
#   geom_hdr_lines(aes(x,y), approx_posterior_data %>% filter(sim %in% sim_id), method = "mvnorm", colour = "red", probs = 0.95) +
#   facet_grid(sim ~ val, scales = "fixed") +
#   theme_bw() + theme(legend.position = "none")  
