library(arules)
library(arulesViz)

titanic = read.csv("train.csv", header = TRUE)
titanic<- data.frame(sapply(titanic,as.factor))
names(data)

rules <- apriori(titanic)
inspect(rules)

### Pruning Redundant Rules

# find redundant rules
rules <- apriori(titanic,parameter=list(minlen=2, supp=0.005, conf=0.8),appearance=list(rhs=c("Survived=0", "Survived=1"),default="lhs"),control=list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# remove redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

plot(rules.pruned)

plot(rules.pruned[1:100], method="graph", control=list(type="items"))

plot(rules.pruned[1:50], method="paracoord", control=list(reorder=TRUE))