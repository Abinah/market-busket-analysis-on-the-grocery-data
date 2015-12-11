#load libriries
library(arules)
library(arulesViz)
library(datasets)
#load data
data(Groceries)
# Create an item frequency plot for the top 15 items
itemFrequencyPlot(Groceries,topN=15,type="absolute")
# Get the rules
# mine some rules!
#always  pass the minimum required support and confidence.

#set the minimum support to 0.001
#set the minimum confidence of 0.8
#then show the top 5 rules

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 4 rules, but only 2 digits
options(digits=2)
inspect(rules[1:4])
#sort to get the most relevant rules first
rules<-sort(rules, by="confidence", decreasing=TRUE)
#to avoid geting a long rule like rule 4,u set the maxlen like below
#rule=apriori(Groceries,parameters=list(supp=0.001,conf=0.8,maxlen=3))
#now lets get the redundant ruless out
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
#what if you want to concentrate on a product,like whole milk i.e you want to know what
#will some one buy b4 buying whole milk
#to do this,we set the [rhs] to whole milk
rules=apriori(data=Groceries,parameter =list(supp=0.001,conf=0.8),appearance=list(default="lhs",rhs="whole milk"),control=list(verbose=F))
#let view this rules now
options(digits=2)
inspect(rules[1:4])
rules<-sort(rules, by="confidence", decreasing=TRUE)
#cool,now i can predict with 91% confidence that if one buys"curd and cereals" he will buy milk
#you can create a simple visualization of your rules
#Visualize
plot(rules,method="graph",interactive=TRUE)
