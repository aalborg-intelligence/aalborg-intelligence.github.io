library(visNetwork)
visualize_neural_network_interactive <- function(params, cache) {
  n <- params$W1 |> ncol()
  n1 <- params$W1 |> nrow()
  n2 <- params$W2 |> nrow()
  nodes <- data.frame(
    id = c(1:n, (n+1):(n+n1), (n+n1+1):(n+n1+n2), n+n1+n2+1),
    label = c(paste0("X_", 1:n), paste0("Y_", 1:n1), paste0("Z_", 1:n2), "O"),
    group = c(rep("Input", n), rep("Hidden1", n1), rep("Hidden2", n2), "Output"),
    level = c(rep(1, n), rep(2, n1), rep(3, n2), 4)
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
  
  for (i in 1:n) {
    for (j in 1:n1) {
      weight <- round(params$W1[j, i], 2)
      edges <- rbind(edges, data.frame(from = i, to = n + j, value = weight, label = weight, title = paste("Weight:", weight)))
    }
  }
  
  for (i in 1:n1) {
    for (j in 1:n2) {
      weight <- round(params$W2[j, i], 2)
      edges <- rbind(edges, data.frame(from = n + i, to = n + n1 + j, value = weight, label = weight, title = paste("Weight:", weight)))
    }
  }
  
  for (i in 1:n2) {
    weight <- round(params$W3[1, i], 2)
    edges <- rbind(edges, data.frame(from = n + n1 + i, to = n + n1 + n2 + 1, value = weight, label = weight, title = paste("Weight:", weight)))
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
n1 <- 1  # Number of neurons in the first hidden layer
n2 <- 1  # Number of neurons in the second hidden layer
X <- matrix(runif(n * 10), nrow = n)  # Example input data
params <- initialize_parameters(n, n1, n2)
cache <- forward_propagation(X, params)

visualize_neural_network_interactive(params, cache)
