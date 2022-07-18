
contact_data <- read_csv("data/contact_data.csv")

m <- seq(0.001,3,0.001)
k <- seq(1,200,1)

mk_grid <- expand.grid(m,k)

mle <- function(m,k,x,y){

  P <- NULL
  for (i in 1:length(x)){
    P[i] <- y[i]*(log(gamma(k+x[i])/(gamma(k)*factorial(x[i])))+x[i]*log(m/k)-(k+x[i])*log((m+k)/k))
  }

  P <- sum(P)
  return(P)
}

# xx <- mle(1,1,c(0,1),c(16,1))
xx <- map2(mk_grid$Var1,mk_grid$Var2,mle,contact_data$contacts_traced,contact_data$occurance)

mk_grid <- mk_grid %>%
  mutate(mle = xx)

mk_grid[which.max(xx),]
