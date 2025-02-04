library(visNetwork)
visualize_neural_network_interactive <- function(params, cache) {
  n <- ncol(params$W1)
  n1 <- params$W1 |> nrow()
  n2 <- params$W2 |> nrow()
  num_classes <- params$W3 |> nrow()
  nodes <- data.frame(
    # id = c(1:n, (n+1):(n+n1), (n+n1+1):(n+n1+n2), (n+n1+n2+1):(n+n1+n2+num_classes)),
    id = 1:(n+1+n1+1+n2+1+num_classes),
    label = c("X_bias", paste0("X_", 1:n), "Y_bias", paste0("Y_", 1:n1), "Z_bias", paste0("Z_", 1:n2), paste0("O_", 1:num_classes)),
    group = c(rep("Input", n+1), rep("Hidden1", n1+1), rep("Hidden2", n2+1), rep("Output", num_classes)),
    level = c(rep(1, n+1), rep(2, n1+1), rep(3, n2+1), rep(4, num_classes)),
    shape = c("box", rep("circle", n), "box", rep("circle", n1), "box", rep("circle", n2), rep("circle", num_classes))
  )
  # nodes$title <- nodes$label
  # nodes$title[(n+1):(n+n1)] <- paste0(nodes$title[(n+1):(n+n1)], "=", round(cache$A1, 2))
    
  edges <- data.frame(
    from = integer(),
    to = integer(),
    value = numeric(),
    label = character(),
    title = character()
  )
  
  # Bias 1
    for (j in 1:n1) {
      weight <- round(params$b1[j, 1], 3)
      edges <- rbind(edges, data.frame(from = 1, to = n+2 + j, value = abs(weight), label = weight, title = paste("Weight:", weight)))
    }
  
  # Layer 1
  for (i in 1:n) {
    for (j in 1:n1) {
      weight <- round(params$W1[j, i], 3)
      edges <- rbind(edges, data.frame(from = i+1, to = n+2 + j, value = abs(weight), label = weight, title = paste("Weight:", weight)))
    }
  }
  
  # Bias 2
  for (j in 1:n2) {
    weight <- round(params$b2[j, 1], 3)
    edges <- rbind(edges, data.frame(from = n+2, to = n+3+n1 + j, value = abs(weight), label = weight, title = paste("Weight:", weight)))
  }

  # Layer 2
  for (i in 1:n1) {
    for (j in 1:n2) {
      weight <- round(params$W2[j, i], 3)
      edges <- rbind(edges, data.frame(from = n+2 + i, to = n+3 + n1 + j, value = abs(weight), label = weight, title = paste("Weight:", weight)))
    }
  }
  
  # Bias 3
  for (j in 1:num_classes) {
    weight <- round(params$b3[j, 1], 3)
    edges <- rbind(edges, data.frame(from = n+3+n1, to = n+3+n1+n2 + j, value = abs(weight), label = weight, title = paste("Weight:", weight)))
  }
  
  # Layer 3
  for (i in 1:n2) {
    for (j in 1:num_classes) {
      weight <- round(params$W3[j, i], 3)
      edges <- rbind(edges, data.frame(from = n+3+n1 + i, to = n+3 + n1 + n2 + j, value = abs(weight), label = weight, title = paste("Weight:", weight)))
    }
  }
  
  visNetwork(nodes, edges) %>%
    visEdges(arrows = "to") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visInteraction(hover = TRUE) %>%
    visHierarchicalLayout(direction = "LR")
}

# Example usage
set.seed(123)
n <- 3  # Number of input neurons
n1 <- 2  # Number of neurons in the first hidden layer
n2 <- 4  # Number of neurons in the second hidden layer
X <- matrix(runif(n * 5), nrow = n)  # Example input data
params <- initialize_parameters(n, n1, n2, num_classes = 2)
cache <- forward_propagation(X, params)

visualize_neural_network_interactive(params, cache)
