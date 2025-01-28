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

w <- structure(c(8.24762532790771, -0.847028453337616, -0.201662117721216, 
                 -10.719581568575, -2.01410607601336, -0.0631075122635756, -0.37510449239827, 
                 -3.68069090224994, -2.57656737715272, -0.235678677902664, -1.59774962528179, 
                 -4.5112524865535, -0.979633376990192, -0.105112153629626, -4.73328740543727, 
                 -2.21592261036045, -0.664393900716524, -0.753573223016137, -0.205800625114603, 
                 -0.226789684923222, -6.00151650124593, -0.959275825115737, -0.750839481230833, 
                 -1.02564245260246, -0.260768004393747, -3.55267767586223, -0.938956443791429, 
                 0.912072117867605, 1.1905127368751, 19.9722201538414, -0.231699959445894, 
                 -1.52631088620426, 0.25464162186288, 8.40314511975218, -0.115668748943332, 
                 -3.37938197779268, 0.475366866836612, 18.3890530913965, 3.37595238606999, 
                 -0.777096465174802, 0.65038963102853, 4.53323226166751, -0.0980049913967417, 
                 1.84668329992993, 10.8840672144098, 13.508239167182, 17.61264937031, 
                 -0.162072589006278, 2.37075767259052, 4.43152988470404, 3.50638735397182, 
                 13.1257865584932, 3.19763407999485, 1.35664031161233, -28.7770148299137, 
                 2.02866487498753, 7.6612017095684, 16.7337766495922, 1.10371068782766, 
                 0.254717701876612, 3.71381754258744, 5.70116716105623, 3.58686913401385, 
                 0.811231311096056, 9.73207953483115, -4.30764453425182, 0.547610488019477, 
                 0.436208049555208, -2.47052945344696, 3.53935313138341, 1.94937822214467, 
                 5.4350263885782, 5.3916900905358, 6.73989986297603, 9.25489458264949, 
                 2.73651021516166, 3.13035196351688, -0.862217829504663, 5.22934351399835, 
                 -2.65954401096451, 2.22478971737191), dim = c(27L, 3L))

k <- structure(c(3.41125865028751, 23.088148409295, 2.43265476557543, 
                 3.83414818032206, -0.179439302181947, 12.873666578092, 0.372379360211998, 
                 0.975831439484873, -2.30947637279232, -0.274703833972254, -1.06307680129529, 
                 3.13680235255299, 6.45310245114436, 2.58978090961949, 5.55400649927393, 
                 3.47409316409987, 4.38335001948142, 9.06170542806142, -9.55792013159146, 
                 8.28343582937137, 1.88419403105393, 2.53522039217398, 7.45914493212174, 
                 23.2183224874283, -15.5967242439145, 6.23690252264222, 11.5375258887921, 
                 0.690905791765742, -2.47988521133816, 0.762492748276711, -5.02800746004061, 
                 -2.60256399421648, -2.54646996397924, 8.25627359521937, -9.7113150733812, 
                 -1.62343031171574, -0.130116688638317, -0.591655495967012, -0.48626391024115, 
                 -4.69409030847632, -0.518635249980562, -1.06877719889883, -7.32025838419779, 
                 -0.628677412718298, 4.12625156981479, -10.8210283501613, 5.41983712264496, 
                 -7.63316597573107, -0.271940466219158, 3.46066146116225, -0.217890983642541, 
                 7.92721128192352, -1.17996948305309, -1.36902124318398, 2.62469405304538, 
                 6.37890789853754, -6.27417670615769, 0.818659471295215, 0.174953935096853, 
                 6.38222791907663, -22.4515044274123, -7.42864481443324, -0.509347858552832, 
                 0.00354759413881151, -5.76601006691692, 1.86887324072183, 4.33229930464257, 
                 0.947682498913962, 3.32102235857564, 0.567470555329662, 1.05605807522105, 
                 -11.1545015032165, -40.0639079713888, -15.0869550585222, -3.90958918788634, 
                 -0.291909484631859, -9.38769705179182, 6.83711018003056, -21.9518529199616, 
                 3.68213162013177, 3.08068848775589), dim = c(27L, 3L))

make_matrices <- function(wk_vec, n_ord, m){
  w <- matrix(wk_vec[1:(n_ord*m)], nrow = n_ord, ncol = m)
  k <- matrix(wk_vec[(n_ord*m+1):(2*n_ord*m)], nrow = n_ord, ncol = m)
  return(list(w = w, k = k))
}

llik <- function(wk_vec, dat_int, n_ord, m, lambda = 0){
  wk_list <- make_matrices(wk_vec, n_ord, m)
  wk <- wk_list$w %*% t(wk_list$k)
  pos <- dat_int[dat_int$forekomst==2, ][,1:2] |> as.matrix()
  neg <- dat_int[dat_int$forekomst==1, ][,1:2] |> as.matrix()
  llik <- sum(log(1+exp(-wk[pos]))) + sum(log(1+exp(wk[neg]))) + lambda*sum(w^2) + lambda*sum(k^2)
  return(llik)
}

# llik_grad <- function(wk_vec, dat_int, n_ord, m){
#   wk_list <- make_matrices(wk_vec, n_ord, m)
#   wk <- wk_list$w %*% t(wk_list$k)
# }

wk_vec <- c(w, k)

tid <- system.time(o <- optim(wk_vec, llik, dat_int = dat_int, n_ord = n_ord, m = m, lambda = 1, control = list(maxit = 5e4)))
tid
tmp <- o$par |> make_matrices(n_ord = n_ord, m = m)

rslt <- list(w=tmp$w, k=tmp$k, ord = ord)
saveRDS(rslt, file.path(datadir, "word2vec_estimates_reg.rds"))

# dd <- round(w %*% t(w), 1)
