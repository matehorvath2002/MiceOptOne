MiceOptOne <- function(df, x) {
  {

  }
  {
    Fulled <- df
    Cleaned <- df[is.na(df[,x])=="FALSE",]
    Imputation <- df
  }
  {
    Mi_n <- min(Cleaned[,x])
    Ma_x <- max(Cleaned[,x])
    Delt_a <- abs(Mi_n) + abs(Ma_x)
    k <- round(log(nrow(Cleaned))/log(2), 0)
    Di_f <- Delt_a/k
    Table <- 1:k
    Table <- as.data.frame(Table)
    Table$x <- 1:k
    Table$y <- 1:k
    Szamlalo <- as.data.frame(1:k)
    Tablex <- Cleaned[,x]
    Tablex <- as.data.frame(Tablex)
    Tablex$Num <- 1
    SubTablex <- Tablex[Tablex[,1] <= Mi_n+Di_f, 2]
    Table$x[1] <- sum(SubTablex)
    SubTablex <- Tablex[Tablex[,1] > Ma_x-Di_f, 2]
    Table$x[k] <- sum(SubTablex)
  }
  {
    for (i in 2:(k-1)) {
      {
        SubTablex <- Tablex[Tablex[,1] <= Mi_n+Di_f*(i) & Tablex[,1] > Mi_n+Di_f*(i-1), 2]
        Table$x[i] <- sum(SubTablex)
      }
    }
  }
  {
    Tabley <- 1:nrow(Fulled)
    Tabley <- as.data.frame(Tabley)
    Tabley[1:nrow(Cleaned),1] <- Cleaned[,x]
    Table$Table <- NULL
  }
  {
    Collector1V <- 1:(nrow(Fulled)-nrow(Cleaned))
    Collector1 <- as.data.frame(Collector1V)
  }
  {
    for (j in 1:79) {
      Collector1$A <- Collector1V
      colnames(Collector1)[colnames(Collector1)=="A"] <- "B"
    }
  }
  {
    Collector2 <- 1:80
    Collector2 <- as.data.frame(Collector2)
    set.seed(200)
  }
  {
    for (l in 20:99) {
      {
        MiceData <- mice(Fulled, m = 1, method = "pmm", maxit = l)
      }
      {
        MiceDataV <- MiceData$imp[x]
      }
      {
        MiceDataV <- as.data.frame(MiceDataV)
      }
      {
        Tabley[(nrow(Cleaned)+1):nrow(Fulled),1] <- MiceDataV[1:(nrow(Fulled)-nrow(Cleaned)),1]
      }
      {
        Tabley$Num <- 1
      }
      {
        SubTabley <- Tabley[Tabley[,1] <= Mi_n+Di_f, 2]
        Table$y[1] <- sum(SubTabley)
      }
      {
        SubTabley <- Tabley[Tabley[,1] > Ma_x-Di_f, 2]
        Table$y[k] <- sum(SubTabley)
      }
      {
        for (i in 2:(k-1)) {
          {
            SubTabley <- Tabley[Tabley[,1] <= Mi_n+Di_f*(i) & Tabley[,1] > Mi_n+Di_f*(i-1), 2]
            Table$y[i] <- sum(SubTabley)
          }
        }
      }
      {
        ChiProb <- sum((1/(Table[,1]+Table[,2]))*((Table[,1]/sum(Table[,1])-Table[,2]/sum(Table[,2]))^2))*nrow(Cleaned)*nrow(Fulled)
      }
      {
        Collector2[l-19,1] <- ChiProb
      }
      {
        Collector1[,l-19] <- MiceDataV[1:(nrow(Fulled)-nrow(Cleaned)),1]
      }
    }
  }
  {
    Collector2 <- as.data.frame(Collector2)
  }
  {
    Collector2$Num1 <- 20:99
    Collector2$Num2 <- 1:80
  }
  {
    BestMin <- Collector2[Collector2[,1]==min(Collector2[,1]),3]
  }
  {
    BestImput <- Collector1[,BestMin]
  }
  {
    BestImput <- as.data.frame(BestImput)
  }
  {
    Imputation[is.na(Imputation[,x])=="TRUE", x] <- BestImput[,1]
  }
  {
    return(Imputation)
  }
}
