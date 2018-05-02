dist.res <- dist(data, method = "euclidean")
hc <- hclust(dist.res, method = "complete")
k.max <- 20

wss <- function(d) {
  sum(scale(d, scale = FALSE)^2)
}

wrapper <- sapply(1:k.max, function(k) {
  cl <- cutree(hc, k)
  spl <- split(data, cl)
  wss <- sum(sapply(spl, wss))
  wss
})

plot(1:k.max, wrapper,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K")
abline(v = 10, lty =2)

