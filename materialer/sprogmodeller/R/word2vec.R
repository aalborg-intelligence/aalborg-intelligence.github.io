# Parametre
m <- 3 # Dimension af vektorrummet
win <- 1 # Vinduestørrelse

# Data
datadir <- here::here("materialer", "sprogmodeller", "data")
bog <- readLines(file.path(datadir, "bog.txt"))
bog <- paste(bog, collapse = " ")
bog <- gsub(".", " .", bog, fixed = TRUE)
bog <- strsplit(bog, " ")[[1]]
ord <- unique(bog) |> sort()
n_ord <- length(ord)

find_kontekst <- function(x, bog, win) {
  i <- which(bog == x)
  i_neg <- list()
  i_pos <- list()
  for(j in seq_len(win)){
    i_neg[[j]] <- i - j
    i_pos[[j]] <- i + j
  }
  ii <- c(unlist(i_neg), unlist(i_pos))
  ii <- ii[ii > 0 & ii <= length(bog)]
  return(data.frame(input = x, kontekst = bog[ii], forekomst = 1))
}

# Kontekst
kontekst <- lapply(ord, find_kontekst, bog = bog, win = win) |> Reduce(rbind, x = _)

find_nonkontekst <- function(x, kontekst) {
  x_kontekst <- kontekst[kontekst$input == x,]
  andre_ord <- setdiff(unique(kontekst$input), x)
  brugte_ord <- unique(kontekst[kontekst$input == x, "kontekst"])
  ubrugte_ord <- setdiff(andre_ord, brugte_ord)
  return(data.frame(input = x, kontekst = sample(ubrugte_ord, 3*nrow(x_kontekst), replace = TRUE), forekomst = 0))
}

# Non-kontekst
nonkontekst <- lapply(ord, find_nonkontekst, kontekst = kontekst) |> Reduce(rbind, x = _)

# Sammensætning
dat <- rbind(kontekst, nonkontekst)

# Numeriske data
dat_int <- lapply(dat, factor) |> lapply(as.integer) |> as.data.frame()

# w <- matrix(runif(n_ord*m), nrow = n_ord, ncol = m)
# w <- w/sqrt(rowSums(w^2))
# k <- matrix(runif(n_ord*m), nrow = n_ord, ncol = m)
# k <- k/sqrt(rowSums(k^2))

w <- structure(c(8.23451544209576, -0.970342196770266, 0.077357966941637, 
                 -11.7995004521059, -2.14204284710078, -0.107170352314718, -0.229449821030887, 
                 -3.62642100559122, -2.54235284910889, -0.210420604340615, -1.06860896199437, 
                 -2.90522251184898, -0.822407002917316, -0.104293553437261, -2.49372809883801, 
                 -1.58237986733117, -0.728844755609585, -0.848931410814236, 0.064785709172424, 
                 -0.235912252360065, -6.25450194915652, -1.19389112922061, -0.666711444060757, 
                 -0.531537586293111, -0.046569053944761, -2.60772557490084, -1.22458337440568, 
                 -0.364258826810501, 1.16160667883813, 20.7149228106079, -5.12905878156055, 
                 -1.52021804379217, 0.070275773183233, 8.1463561987219, -0.452746461397758, 
                 -2.93260562458418, 0.188337241958266, 21.174151010079, 3.36312313867819, 
                 -0.818394453235172, 0.064987829712636, 3.78723842848944, -0.121382259796833, 
                 0.969115910969284, 10.3380155305135, 12.2009361424247, 15.3113688021045, 
                 -0.607477709605071, 1.63610361490381, 4.21223067745812, 4.38830949045195, 
                 10.4026465643541, 1.76138405356682, 1.22682530049518, -28.7716337606735, 
                 2.36750984030068, 8.63732248224641, 17.5484154384092, 1.30310925763561, 
                 0.238433277327289, 3.85276315475692, 5.68921923254637, 3.64851301762645, 
                 0.617845206166384, 11.1340547410336, -4.39470421412035, 0.460094625138338, 
                 0.222200013763795, -1.83442841164784, 2.53988762799172, 1.75745170739692, 
                 6.06831400279842, 5.14911621478424, 6.78170362210238, 9.73978177739286, 
                 2.95317655956634, 3.07949682733913, -1.50406061783121, 4.53763897332711, 
                 -2.74382232754076, 2.77774453057953), dim = c(27L, 3L))

k <- structure(c(3.62785120307596, 22.8941812590914, 2.1033241921013, 
                 3.1912819004497, -0.419952735535914, 12.5707545996889, 0.191308888637283, 
                 1.45711513798882, -1.91215650183408, -0.301485032802798, -0.30206052415582, 
                 2.89949216581443, 6.86474751343024, 2.67922841238037, 5.39800475306114, 
                 7.71443214970419, 3.2784877137936, 11.228027244127, -22.6199220837079, 
                 -1.73468370900684, 1.76553744676608, 5.02432580836495, 10.0488589654205, 
                 23.3533537158102, -16.3076231585691, 6.03289593623322, 10.9230214701297, 
                 0.728733350094971, -9.39943561898402, -2.32990343943834, -4.87546342189849, 
                 -2.92082159373816, -2.85260946252272, 6.93088146846964, -7.7390611339152, 
                 -0.903316234143183, -0.136154567483581, -0.495878710907598, -0.508430728127541, 
                 -4.97169641706881, -0.632936750924507, -1.15398743987854, -10.8198658207588, 
                 -0.508272601322176, 3.60389770849938, 5.44978865952053, 5.74954756150533, 
                 -7.12434310911546, -0.360223474878672, 3.28901592938512, 3.58646473906036, 
                 8.11453290188665, -1.25006580920544, -3.08991494583136, 2.82495740618502, 
                 6.4898647479036, -11.0528258066404, 0.700256786897254, 0.0907568381674222, 
                 6.15077402130358, -21.3431890974653, -4.18026266881609, -0.381167742689801, 
                 -0.00963752441673107, -4.45826008738412, 1.63103213554469, 4.27173572178816, 
                 1.00119109302022, 3.1058646371924, 1.72825469084562, 0.489532233105375, 
                 -9.57418191727698, -38.4909259447509, -14.5768875930535, -3.6578918768109, 
                 0.147282476786766, -8.74123609193741, 6.83272426203373, -20.3232158883863, 
                 3.45692532991118, 3.01577788181373), dim = c(27L, 3L))

make_matrices <- function(wk_vec, n_ord, m){
  w <- matrix(wk_vec[1:(n_ord*m)], nrow = n_ord, ncol = m)
  k <- matrix(wk_vec[(n_ord*m+1):(2*n_ord*m)], nrow = n_ord, ncol = m)
  return(list(w = w, k = k))
}

llik <- function(wk_vec, dat_int, n_ord, m){
  wk_list <- make_matrices(wk_vec, n_ord, m)
  wk <- wk_list$w %*% t(wk_list$k)
  pos <- dat_int[dat_int$forekomst==2, ][,1:2] |> as.matrix()
  neg <- dat_int[dat_int$forekomst==1, ][,1:2] |> as.matrix()
  llik <- sum(log(1+exp(-wk[pos]))) + sum(log(1+exp(wk[neg]))) 
  return(llik)
}

# llik_grad <- function(wk_vec, dat_int, n_ord, m){
#   wk_list <- make_matrices(wk_vec, n_ord, m)
#   wk <- wk_list$w %*% t(wk_list$k)
# }

wk_vec <- c(w, k)

system.time(o <- optim(wk_vec, llik, dat_int = dat_int, n_ord = n_ord, m = m, control = list(maxit = 1e6)))
tmp <- o$par |> make_matrices(n_ord = n_ord, m = m)

list(w=tmp$w, k=tmp$k, ord = ord) |> saveRDS(file.path(datadir, "word2vec_estimates.rds"))

# dd <- round(w %*% t(w), 1)
