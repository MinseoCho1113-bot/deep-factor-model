# R/autoencoder.R
# Conditional autoencoder for asset pricing (Gu-Kelly-Xiu 2021 style),
# implemented in torch for R.
#
#   r_{i,t} = beta(z_{i,t-1})' f_t + e_{i,t}
#
# beta_net:   z_{i,t-1} (characteristics) -> beta_i,t  (K-dim loadings)
# factor_net: r_t (cross-section of returns) -> f_t    (K-dim latent factors)
#
# Trained jointly to minimize reconstruction error of r_t.

library(torch)

#' Build the beta network: characteristics -> K factor loadings per asset.
build_beta_net <- function(n_chars, k_factors, hidden = c(32, 16)) {
  layers <- list()
  in_dim <- n_chars
  for (h in hidden) {
    layers <- c(layers, nn_linear(in_dim, h), nn_relu())
    in_dim <- h
  }
  layers <- c(layers, nn_linear(in_dim, k_factors))
  do.call(nn_sequential, layers)
}

#' Build the factor network: cross-section of returns -> K latent factors.
#' Kept linear (single dense layer, no activation) per GKX's baseline spec —
#' nonlinearity lives in the beta network, not the factor network.
build_factor_net <- function(n_assets, k_factors) {
  nn_linear(n_assets, k_factors, bias = FALSE)
}

#' Conditional autoencoder module combining beta_net and factor_net.
#' forward(z, r): z is [n_assets, n_chars] at t-1, r is [n_assets] at t.
#' Returns predicted r_hat = beta(z) %*% f(r).
conditional_autoencoder <- nn_module(
  "ConditionalAutoencoder",
  initialize = function(n_chars, n_assets, k_factors, hidden = c(32, 16)) {
    self$beta_net <- build_beta_net(n_chars, k_factors, hidden)
    self$factor_net <- build_factor_net(n_assets, k_factors)
  },
  forward = function(z, r) {
    beta <- self$beta_net(z)             # [n_assets, k_factors]
    f <- self$factor_net(r$unsqueeze(1)) # [1, k_factors]
    r_hat <- torch_matmul(beta, f$squeeze(1))  # [n_assets]
    r_hat
  }
)

#' Train the conditional autoencoder on a panel with a balanced
#' (asset x time) characteristics array and a matching return matrix.
#'
#' @param char_array  array [n_time, n_assets, n_chars]
#' @param ret_mat     matrix [n_time, n_assets]
#' @param k_factors   number of latent factors
#' @param epochs,lr   training hyperparameters
train_autoencoder <- function(char_array, ret_mat, k_factors = 5,
                               hidden = c(32, 16), epochs = 200, lr = 1e-3,
                               weight_decay = 1e-4, seed = 42) {
  torch_manual_seed(seed)
  n_time <- dim(char_array)[1]
  n_assets <- dim(char_array)[2]
  n_chars <- dim(char_array)[3]

  model <- conditional_autoencoder(n_chars, n_assets, k_factors, hidden)
  optimizer <- optim_adam(model$parameters, lr = lr, weight_decay = weight_decay)

  loss_history <- numeric(epochs)

  for (epoch in seq_len(epochs)) {
    epoch_loss <- 0
    for (t in seq_len(n_time)) {
      z_t <- torch_tensor(char_array[t, , ], dtype = torch_float())
      r_t <- torch_tensor(ret_mat[t, ], dtype = torch_float())
      valid <- !torch_isnan(r_t)

      optimizer$zero_grad()
      r_hat <- model(z_t, r_t)
      loss <- nnf_mse_loss(r_hat[valid], r_t[valid])
      loss$backward()
      optimizer$step()

      epoch_loss <- epoch_loss + loss$item()
    }
    loss_history[epoch] <- epoch_loss / n_time
    if (epoch %% 10 == 0) {
      message(sprintf("Epoch %d/%d - avg loss: %.6f", epoch, epochs, loss_history[epoch]))
    }
  }

  list(model = model, loss_history = loss_history)
}

#' Extract fitted betas and factors from a trained model for a given period.
extract_beta_factor <- function(model, z_t, r_t) {
  z_t <- torch_tensor(z_t, dtype = torch_float())
  r_t <- torch_tensor(r_t, dtype = torch_float())
  beta <- model$beta_net(z_t)
  f <- model$factor_net(r_t$unsqueeze(1))$squeeze(1)
  list(beta = as.matrix(beta), factor = as.numeric(f))
}
