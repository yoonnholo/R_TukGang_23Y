library(knitr)
include_graphics("./figure/fig9.png")


library(knitr)
include_graphics("./figure/fig10.png")



include_graphics("./figure/fig22.png")



include_graphics("./figure/fig26.jpg")



include_graphics("./figure/fig27.jpg")



include_graphics("./figure/fig29.png")


include_graphics("./figure/fig31.png")


include_graphics("./figure/fig35.png")


include_graphics("./figure/fig36.png")


include_graphics("./figure/fig34.png")

## Lesson 01 ----
## 이후 대쉬 4개 (----) <- 책갈피 기능



library(modeest)

mfv(c(1,2,1,2,3,3,3,4,5,4,5))

mfv(c(1,5,7,7,9,9,10))

## nothing happen

3 + 5
-1 / 0

?mfv

13 %% 4 ## 자연수 나누기 나머지
13 %/% 4 ## 자연수 나누기 몫


x = 5; y = 6 ## ; <- enter
x + y
sin(x) + exp(y)

## cmd + shift + a -> 코드 띄어쓰기 하기

logical(length = 0)
is.logical(x)

is.numeric(50)

is.numeric('3')
is.numeric("3")

is.character(x)
is.character('5')
is.character("5")
is.infinite(-Inf)

x1 <- c(1, 3, 5, 7, 9)
x1

x2 = seq(1, 9, 2)
x2
x3 = rep(1, 10)
x3
x4 = 1:10
x4

x5 = c('1', '2', '3')
x5
x6 = c('A', 'B', 'C')
x6

x1 ^ 2 + 2 * x2 ^ 2

x1; x3
x1 + x3

myvector <- c(8, 6, 9, 10, 5)
mylist <- list(name = "Fred", wife = "Mary", myvector)
mylist

mylist[[2]]
mylist$wife
mylist[[3]]

mynames <- c("Mary", "John", "Ann", "Sinead", "Joe",
             "Mary", "Jim", "John", "Simon")
table(mynames)

mytable <- table(mynames)
mytable[[4]]
mytable[["John"]]

mymatrix <- matrix(1:12, nrow = 4, ncol = 3)
mymatrix

myarray <- array(1:8, dim = c(2, 2, 2))
myarray

include_graphics("./figure/fig37.png")


include_graphics("./figure/fig37.png")


myFamNames = c("Dad", "Mom", "Sis", "Bro", "Dog")
myFamNames
myFamAges = c(43, 42, 12, 8, 5)
myFamGenders = c("Male","Female","Female","Male","Female")
myFamWeights = c(188, 136, 83, 61, 44)

myFamily <- data.frame(myFamNames, myFamAges,
                       myFamGenders, myFamWeights)
myFamily

str(myFamily)

myFamilyStr <- data.frame(myFamNames,
                          myFamAges,
                          myFamGenders,
                          myFamWeights,
                          stringsAsFactors = TRUE)


str(myFamilyStr)

summary(myFamily)

myFamily$myFamAges

myFamily[2,]

myFamAges <- c(myFamAges, 11)
myFamAges
myFamily$myFamAges

myFamily$myFamAges <- c(myFamily$myFamAges, 11)

summary(iris)

str(iris)

head(iris)

apply(iris[, 1:4], 2, sum)

lapply(iris[, 1:4], mean)

sapply(iris[, 1:4], mean)

y = sapply(iris[, 1:4], function(x){x > 3}) 
head(y, 3)

levels(iris$Species)
tapply(iris$Sepal.Length, iris$Species, mean)

byspecies <- split(iris, iris$Species)
str(byspecies)

setosa <- subset(iris, Species == "setosa")
head(setosa)

colsel <- subset(iris, select = c(Sepal.Length, Species))
head(colsel)

colsel <- subset(iris, select = -c(Sepal.Length, Species))
head(colsel)

x <- c(20, 11, 33, 50, 47)
sort(x)
order(x)
x[order(x)]

iris.ordered <- iris[order(iris$Sepal.Length), ]
head(iris.ordered)

head(iris[order(iris$Sepal.Length , iris$Petal.Length), ])

sample(1:10, 5) # 비복원추출 (중복 불허)
sample(1:10, 5, replace = TRUE) # 복원추출 (중복 허용)
iris.resample <- iris[sample(NROW(iris), NROW(iris)), ]
head(iris.resample, 5)

aggregate(Sepal.Width  ~  Species,  iris, mean)
