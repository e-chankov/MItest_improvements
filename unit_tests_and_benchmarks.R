library(MRCV)
library(data.table)
library(Matrix)
library(checkmate)
source("mi.test_improvements.R")

#### comparing results on farmer data
( test1 <- MI.test(farmer1, 1, 5, type = "rs2") )
data <- as.data.table(farmer1)
( test2 <- multiple.chiRS2.test(data, 1, 5) )
all.equal(test1$rs2, test2[-1])

( test1 <- MI.test(farmer2, 3, 4, type = "rs2") )
data <- as.data.table(farmer2)
( test2 <- multiple.chiRS2.test(data, 3, 4) )
all.equal(test1$rs2, test2[-1])




library(plfm)
data(car)

brandData <- data.table(car$datalongformat[, c("IDrater", "rating", "objectlabel", "attributelabel")])
setnames(brandData, c(1,3,4), c("rater", "brand", "attribute"))
brandData[, rating := as.integer(rating)]
setkey(brandData, brand, attribute)


############# MMI.test examples
###
selected.cars <- sort(c("Opel Corsa", "Volvo V50"))
selected.attr <- levels(brandData$attribute)[1:13]
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]
data <- data[, 1:(length(selected.attr)+1), with = FALSE]

df <- as.data.frame(data)
MI.test(df, length(selected.attr), 1, type = "rs2")
multiple.chiRS2.test(data, length(selected.attr), 1)

### more than 900 times faster --- 9835 milliseconds vs 10.2 milliseconds
microbenchmark::microbenchmark(MI.test(df, length(selected.attr), 1, type = "rs2"), times = 10)
microbenchmark::microbenchmark(multiple.chiRS2.test(data, length(selected.attr), 1))

###
selected.cars <- sort(c("Opel Corsa", "Volvo V50"))
selected.attr <- levels(brandData$attribute)[1:14]
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]
data <- data[, 1:(length(selected.attr)+1), with = FALSE]

df <- as.data.frame(data)
### Error: cannot allocate vector of size 8.0 Gb
MI.test(df, length(selected.attr), 1, type = "rs2")
### 11.1 milliseconds
microbenchmark::microbenchmark(multiple.chiRS2.test(data, length(selected.attr),1))



###
selected.cars <- c("Audi A4", "Renault Espace")
selected.attr <- c("Agile", "Good price-quality ratio", "High trade-in value")
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]
data <- data[, length(selected.attr):(length(selected.attr)*2), with = FALSE]
# Error in G %*% tau : non-conformable arguments
MI.test(as.data.frame(data), 1, 3, type = "rs2")
# removing on the fly columns with a constant value
multiple.chiRS2.test(data, 1, length(selected.attr))


### the case when multiple variable is homogeneous
selected.cars <- c("Audi A4", "Renault Espace")
selected.attr <- c("High trade-in value")
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]
data <- data[, length(selected.attr):(length(selected.attr)*2), with = FALSE]
multiple.chiRS2.test(data, 1, length(selected.attr))



### the case when single variable takes only one unique value
selected.cars <- c("Renault Espace", "Volvo V50")
selected.attr <- c("Agile", "Good price-quality ratio", "High trade-in value")
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]
data <- data[, length(selected.attr):(length(selected.attr)*2), with = FALSE]
multiple.chiRS2.test(data, 1, length(selected.attr))



### the case of the chi-squared test
selected.cars <- c("BMW X5", "Fiat 500")
selected.attr <- c("Safe")
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]
data <- data[, length(selected.attr):(length(selected.attr)*2), with = FALSE]
multiple.chiRS2.test(data, 1, length(selected.attr))


############ SPMI.test examples


### compare perfomance of the origianl MI test and its modification
selected.cars <- c("Audi A4", "BMW X5")
selected.attr <- levels(brandData$attribute)[1:7]
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]

df <- as.data.frame(data)
MI.test(df, length(selected.attr), length(selected.attr), type = "rs2")

multiple.chiRS2.test(data, length(selected.attr), length(selected.attr))

### more than 2300 times faster --- 15801 milliseconds vs 6.7 milliseconds
microbenchmark::microbenchmark(MI.test(df, length(selected.attr), length(selected.attr), type = "rs2"), times = 10)
microbenchmark::microbenchmark(multiple.chiRS2.test(data, length(selected.attr), length(selected.attr)))


### data too big for the original MI test
selected.cars <- c("Audi A4", "BMW X5")
selected.attr <- levels(brandData$attribute)[1:8]
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]

selected.cars <- c("Ford Focus Cmax", "Volkswagen Golf")
selected.attr <- c("Agile", "Economical",  "Popular", "Practical", "Reliable", "Safe", "Sustainable", "Versatile")

df <- as.data.frame(data)
### MI.test() will return the following error message
### Error: cannot allocate vector of size 32.0 Gb
MI.test(df, length(selected.attr), length(selected.attr), type = "rs2")

multiple.chiRS2.test(data, length(selected.attr), length(selected.attr))
# calc time ~ 8 milliseconds
microbenchmark::microbenchmark(multiple.chiRS2.test(data, length(selected.attr), length(selected.attr)))



### all 27 items
selected.cars <- c("Audi A4", "BMW X5")
selected.attr <- levels(brandData$attribute)
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]

multiple.chiRS2.test(data, length(selected.attr), length(selected.attr))
# calc time ~ 1.06 sec
microbenchmark::microbenchmark(multiple.chiRS2.test(data, length(selected.attr), length(selected.attr)), times = 10)


###
selected.cars <- c("Audi A4", "Renault Espace")
selected.attr <- c("Agile", "Good price-quality ratio", "High trade-in value")
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]

df <- as.data.frame(data)
### MI.test() returns the following error message
### Error in H %*% kronecker(t(j.2r), i.2c) : non-conformable arguments
MI.test(df, length(selected.attr), length(selected.attr), type = "rs2")

multiple.chiRS2.test(data, length(selected.attr), length(selected.attr))


### reduced to the SPMI test
selected.cars <- c("Audi A4", "Renault Espace")
selected.attr <- c("Good price-quality ratio", "High trade-in value")
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]

df <- as.data.frame(data)
### MI.test() returns the following error message
### Error in H %*% kronecker(t(j.2r), i.2c) : non-conformable arguments
MI.test(df, length(selected.attr), length(selected.attr), type = "rs2")

multiple.chiRS2.test(data, 2, 2)


### reduced to Pearson's chi-squared test
selected.cars <- c("Opel Corsa", "Renault Espace")
selected.attr <- c("High trade-in value", "Powerful")
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]

multiple.chiRS2.test(data, 2, 2)


### the case when an observed value is the same as the expected value
selected.cars <- c("Audi A4", "Citroen C4 Picasso")
selected.attr <- c("Economical", "Attractive")
data <- brandData[CJ(selected.cars, selected.attr)]
data <- dcast(data, rater ~ brand + attribute, value.var="rating")
data[, rater := NULL]
multiple.chiRS2.test(data, length(selected.attr), length(selected.attr))