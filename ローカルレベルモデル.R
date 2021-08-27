
library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(1)
wn <- rnorm(n = 100, mean = 0, sd = 1)

cumsum(c(1,3,2))

rw <- cumsum(wn)

p_wn_1 <- autoplot(ts(wn), main = "")
p_rw_1 <- autoplot(ts(rw), main = "")


grid.arrange(p_wn_1, p_rw_1)


wn_mat <- matrix(nrow = 100, ncol = 20)
rw_mat <- matrix(nrow = 100, ncol = 20)

set.seed(1)
for(i in 1:20){
  wn <- rnorm(n = 100, mean = 0, sd = 1)
  wn_mat[,i] <- wn
  rw_mat[,i] <- cumsum(wn)
}

p_wn_2 <- autoplot(ts(wn_mat), facets = F, main = "") + 
  theme(legend.position = 'none') 

p_rw_2 <- autoplot(ts(rw_mat), facets = F, main = "") + 
  theme(legend.position = 'none') 


grid.arrange(p_wn_2, p_rw_2)


sales_df <- read.csv("5-2-1-sales-ts-1.csv")
sales_df$date <- as.POSIXct(sales_df$date)

head(sales_df, n = 3)

POSIXct_time <- as.POSIXct("1970-01-01 00:00:05", tz="UTC")
as.numeric(POSIXct_time)


data_list <- list(
  y = sales_df$sales, 
  T = nrow(sales_df)
)

local_level_stan <- stan(
  file = "5-2-1-local-level.stan",
  data = data_list,
  seed = 1
)

mcmc_rhat(rhat(local_level_stan))


print(local_level_stan,
      pars = c("s_w", "s_v","lp__"),
      probs = c(0.025, 0.5, 0.975))


mcmc_sample <- rstan::extract(local_level_stan)

state_name <- "mu"

quantile(mcmc_sample[[state_name]][, 1], 
         probs=c(0.025, 0.5, 0.975))

result_df <- data.frame(t(apply(
  X = mcmc_sample[[state_name]],
  MARGIN = 2,
  FUN = quantile,
  probs=c(0.025, 0.5, 0.975)
)))

colnames(result_df) <- c("lwr", "fit", "upr")

result_df$time <- sales_df$date

result_df$obs <- sales_df$sales


head(result_df, n = 3)

ggplot(data = result_df, aes(x = time, y = obs)) + 
  labs(title="") +
  ylab("sales") + 
  geom_point(alpha = 0.6, size = 0.9) +
  geom_line(aes(y = fit), size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + 
  scale_x_datetime(date_labels = "")



plotSSM <- function(mcmc_sample, time_vec, obs_vec = NULL,
                    state_name, graph_title, y_label,
                    date_labels = ""){

 result_df <- data.frame(t(apply(
    X = mcmc_sample[[state_name]],
    MARGIN = 2, quantile, probs = c(0.025, 0.5, 0.975)
  )))
  
 colnames(result_df) <- c("lwr", "fit", "upr")
  
 result_df$time <- time_vec
  
 if(!is.null(obs_vec)){
    result_df$obs <- obs_vec
  }
  
 p <- ggplot(data = result_df, aes(x = time)) + 
    labs(title = graph_title) +
    ylab(y_label) +
    geom_line(aes(y = fit), size = 1.2) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + 
    scale_x_datetime(date_labels = date_labels)
  
 if(!is.null(obs_vec)){
    p <- p + geom_point(alpha = 0.6, size = 0.9, 
                        data = result_df, aes(x = time, y = obs))

  return(p)
}

plotSSM(mcmc_sample = mcmc_sample, time_vec = sales_df$date, 
        obs_vec = sales_df$sales,
        state_name = "mu", graph_title = "",
        y_label = "sales") 





