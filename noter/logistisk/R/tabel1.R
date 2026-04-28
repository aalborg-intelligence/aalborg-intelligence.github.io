interval <- cut(x, breaks = seq(75, 300, by=25), right = TRUE)
tabel_data <- table(interval, y)
tabel <- data.frame(Blodtryk = rownames(tabel_data), Rask = tabel_data[,1], Syg = tabel_data[,2])
tabel$total <- rowSums(tabel_data)
tabel$`Andel syge` <- round(tabel$Syg/tabel$total, 3)
tabel |> subset(select = -total) |> 
  knitr::kable(row.names = FALSE, align = "rrrc")
# Tal til teksten
rask2 <- tabel$Rask[2]
syg2 <- tabel$Syg[2]
total2 <- tabel$total[2]
andel2 <- tabel$`Andel syge`[2]
