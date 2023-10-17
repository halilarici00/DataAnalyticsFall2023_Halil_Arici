require(rpart)
library(rpart.plot)
swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) # try some different plot options
prp(swiss_rpart)
prp(swiss_rpart,extra=1)
title("Swiss R Part Tree Plot")
text(swiss_rpart) # try some different text options
text(swiss_rpart, col="red")
text(swiss_rpart, col="blue")


require(party)

treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
# look at help info, vary parameters.
help(cforest)
cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(teststat= "quad",mtry=2, mincriterion=0))



library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)

#find "prettier" ways to plot the tree
plot(tr)
text(tr,pretty=1, col="red")
title("iris classification tree")


