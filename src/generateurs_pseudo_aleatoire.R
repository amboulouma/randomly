RANDU <- function(graine)
{
  a <- 65539
  b <- 0
  m <- 2^31
  S <- (a*graine+b)%%m
  return (S)
}


StandardMinimal <- function(graine)
{
  a <- 16807
  b <- 0
  m <- 2^31 - 1
  S <- (a*graine+b)%%m
  return (S)
}


generateur_RANDU <- function(graine, n)
{
  randoms <- rep(0, n)
  random <- RANDU(graine)
  i <- 1
  for(i in 1:n)
  {
    randoms[i] <- random
    random <- RANDU(random)
    i <- i + 1
  }
  return (randoms)
}


generateur_StandardMinimal <- function(graine, n)
{
  randoms <- rep(0, n)
  random <- StandardMinimal(graine)
  i <- 1
  for(i in 1:n)
  {
    randoms[i] <- random
    random <- StandardMinimal(random)
    i <- i + 1
  }
  return (randoms)
}


hist(generateur_RANDU(234, 1000))
hist(generateur_StandardMinimal(234, 1000))

plot(generateur_RANDU(234, 1000),xlab='dimension 1', ylab='dimension 2', main='RANDU')
plot(generateur_StandardMinimal(234, 1000),xlab='dimension 1', ylab='dimension 2', main='Standard Minimal')