# Define the sigmoid activation function
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

softmax <- function(x) {
  exp(x) / rowSums(exp(x))
}

# Initialize weights and biases
initialize_parameters <- function(n, n1, n2, num_classes = 1, const = NA) {
  n_hidden_layers <- 2
  if(n2==0){
    n_hidden_layers <- 1
    n2 <- num_classes
  }
  if(n1==0){
    n_hidden_layers <- 0
    n1 <- num_classes
  }
  if(is.na(const)){
    weight_fun <- runif
  } else{
    weight_fun <- function(n){rep(const, n)}
  }
  out <- list(
    W1 = matrix(weight_fun(n * n1), nrow = n1, ncol = n),
    b1 = matrix(weight_fun(n1), nrow = n1, ncol = 1)
  )
  if(n_hidden_layers>0){
    out$W2 <- matrix(weight_fun(n1 * n2), nrow = n2, ncol = n1)
    out$b2 <- matrix(weight_fun(n2), nrow = n2, ncol = 1)
  }
  if(n_hidden_layers>1){
    out$W3 <- matrix(weight_fun(n2*num_classes), nrow = num_classes, ncol = n2)
    out$b3 <- matrix(weight_fun(num_classes), nrow = num_classes, ncol = 1)
  }
  return(out)
}

# Forward propagation
forward_propagation <- function(X, params) {
  Z1 <- params$W1 %*% X + matrix(params$b1, nrow = length(params$b1), ncol = ncol(X))
  A1 <- sigmoid(Z1)
  out <- list(Z1 = Z1, A1 = A1)
  if(is.null(params$W2)){ # No hidden layers
    if(length(params$b1)>1){ # Change to softmax if num_classes > 1
      out$A1 <- softmax(t(Z1))
    }
    return(out)
  }
  # Now there is at least one hidden layer
  out$Z2 <- try(params$W2 %*% A1 + matrix(params$b2, nrow = length(params$b2), ncol = ncol(A1)))
  # if(inherits(out$Z2, "try-error")){
  #   browser()
  # }
  if(!is.null(params$W3)){
    out$A2 <- sigmoid(out$Z2)
    out$Z3 <- params$W3 %*% out$A2 + matrix(params$b3, nrow = length(params$b3), ncol = ncol(out$A2))
    if(length(params$b3)>1){
      out$A3 <- softmax(t(out$Z3))
    } else{
      out$A3 <- sigmoid(out$Z3)
    }
  } else{
    if(length(params$b2)>1){
      out$A2 <- softmax(t(out$Z2))
    } else{
      out$A2 <- sigmoid(out$Z2)
    }
  }
  return(out)
}

# Compute the loss (binary cross-entropy)
compute_loss <- function(Y, output, loss_function = c("cross-entropy", "squared")) {
  num_classes <- nrow(Y)
  if(loss_function == "squared"){
    if(is.null(num_classes) || num_classes==1){
      return(1/2*sum((Y - t(output))^2))
    } else{
      return(1/2*sum((Y - output)^2))
    }
  } else if(loss_function == "cross-entropy"){
    if(is.null(num_classes) || num_classes==1){
      return(mean(-Y * log(t(output)) - (1 - Y) * log(1 - t(output))))
    } else{
      return(-mean(rowSums(Y * log(output))))
    }
  } else{
    stop("Unknown loss function")
  }
}

# Backward propagation
backward_propagation <- function(X, Y, params, cache, loss_function = "cross-entropy") {
  m <- ncol(X)
  loss_grad <- function(Y, output, loss_function){
    num_classes <- nrow(Y)
    if(loss_function == "cross-entropy"){
      if(is.null(num_classes) || num_classes==1){
        return(output - Y)
      } else{
        return(t(output) - Y)
      }
    } else if(loss_function == "squared"){
      if(is.null(num_classes) || num_classes==1){
        return((output - Y) * output * (1 - output))
      } else{
        return((t(output) - Y) * t(output) * (1 - t(output)))
      }
    } else{
      stop("Unknown loss function")
    }
  }
  if(!is.null(params$W3)){ # Two hidden layers
    dZ3 <- loss_grad(Y, cache$A3, loss_function)
    dW3 <- dZ3 %*% t(cache$A2)
    db3 <- rowSums(dZ3)
    dA2 <- t(params$W3) %*% dZ3
    dZ2 <- dA2 * cache$A2 * (1 - cache$A2)
  } else if(!is.null(params$W2)){ # One hidden layers
    dZ2 <- loss_grad(Y, cache$A2, loss_function)
  }
  if(!is.null(params$W2)){ # One hidden layer
    dW2 <- dZ2 %*% t(cache$A1)
    db2 <- rowSums(dZ2)
    dA1 <- t(params$W2) %*% dZ2
    dZ1 <- dA1 * cache$A1 * (1 - cache$A1)
  } else{ # No hidden layers
    dZ1 <- loss_grad(Y, cache$A1, loss_function)
  }
  dW1 <- dZ1 %*% t(X)
  db1 <- rowSums(dZ1)
  out <- list(dW1 = dW1, db1 = db1)
  if(!is.null(params$W2)){
    out$dW2 <- dW2
    out$db2 <- db2
  }
  if(!is.null(params$W3)){
    out$dW3 <- dW3
    out$db3 <- db3
  }
  return(out)
}

# Update parameters
update_parameters <- function(params, grads, learning_rate) {
  params$W1 <- params$W1 - learning_rate * grads$dW1
  params$b1 <- params$b1 - learning_rate * grads$db1
  if(!is.null(params$W2)){
    params$W2 <- params$W2 - learning_rate * grads$dW2
    params$b2 <- params$b2 - learning_rate * grads$db2
  }
  if(!is.null(params$W3)){
    params$W3 <- params$W3 - learning_rate * grads$dW3
    params$b3 <- params$b3 - learning_rate * grads$db3
  }
  params
}

# Train the neural network
train_neural_network <- function(X, Y, n1, n2, iterations, learning_rate, params = NULL, loss_function = "cross-entropy") {
  if(n1==0 & n2>0){
    n1 <- n2
  }
  n <- nrow(X)
  if(is.null(params)){
    params <- initialize_parameters(n, n1, n2, num_classes = nrow(Y))
  }
  cache_list <- loss_list <- grads_list <- params_list <- list()
  for (i in 1:iterations) {
    cache_list[[i]] <- cache <- forward_propagation(X, params)
    output <- if(!is.null(params$W3)){cache$A3} else{ if(!is.null(params$W2)){cache$A2} else{cache$A1} }
    loss_list[[i]] <- loss <- compute_loss(Y, t(output), loss_function)
    grads_list[[i]] <- grads <- backward_propagation(X, Y, params, cache, loss_function = loss_function)
    params_list[[i]] <- params <- update_parameters(params, grads, learning_rate)
    if (i %% 1000 == 0) {
      cat("Iteration", i, "loss:", loss, "\n")
    }
  }
  list(cache = cache_list, loss = unlist(loss_list), grads = grads_list, params = params_list)
}

nn_fun <- function(formula, data, weights = NA, n_hidden = c(1,1), activation = "sigmoid", eta = 0.01, eps = 0.0001, max_it = 1000, scale = TRUE, lossfun = "squared"){
  ## Assumes response variable has values +/-1.
  ## Unchanged for "identity" and "softsign". Changed to 0/1 for "sigmoid".
  x <- model.matrix(formula, data = data)
  colnames(x)[colnames(x)=="(Intercept)"] <- "bias"
  y_name <- as.character(formula)[2]
  y <- data[[y_name]]
  
  if(is.character(y) | is.factor(y)){
    # Ensure y is a factor
    if(!is.factor(y)){
      y <- factor(y)
    }
    if(length(levels(y))==1){stop("Response variable must have at least two levels")}
    if(length(levels(y))==2){
      y <- ifelse(y==levels(y)[1], 1, 0)
    } else{
      y <- model.matrix(~y-1, data = data)
    }
  }
  y <- t(y)
  X <- t(x[,colnames(x)!="bias"])
  if(length(weights)==1){
    params <- initialize_parameters(nrow(X), n_hidden[1], n_hidden[2], num_classes = nrow(y), const = weights)
  } else{
    params <- initialize_parameters(nrow(X), n_hidden[1], n_hidden[2], num_classes = nrow(y))
  }
  train_neural_network(X, y, n1 = n_hidden[1], n2 = n_hidden[2], iterations = max_it, learning_rate = eta, params = params, loss_function = lossfun)
}
# 
# Example usage
# set.seed(123)
# n_sample <- 5
# n <- 3  # Number of input neurons
# n1 <- 2  # Number of neurons in the first hidden layer
# n2 <- 4  # Number of neurons in the second hidden layer
# X <- matrix(runif(n * n_sample), nrow = n)  # Example input data
# # Y <- matrix(sample(0:1, n_sample, replace = TRUE), nrow = 1)  # Example output data
# Y <- replicate(n_sample, sample(c(1,0,0)))
# iterations <- 1000
# learning_rate <- 0.01
# 
# trained_params <- train_neural_network(X, Y, n1, n2, iterations, learning_rate)

ir <- iris
ir[,1:4] <- scale(ir[,1:4])
# ir <- ir[c(1:3, 51:53, 101:103),]
fit_ir <- nn_fun(Species ~ ., ir, n_hidden = c(0,0), eta = 0.05, max_it = 20000, lossfun = "cross-entropy")
