scan("z1.txt")
scan("z2.txt")
scan("z3.txt")
scan("z3.txt", what = "")


scan("z4.txt", what = "")
scan("z3.txt", what = "")
scan("z3.txt", what = "", sep = "\n")

x <- 1:3
print(x^2)
print("abc")

cat("abc\n")
cat(x, "abc", "de\n")
cat(x, "abc", "de\n", sep = "")

##프린트랑 비슷한데 [1] 이런거 없음. \n 이건 엔터 치라는 뜻. sep 이건 "" 사이에 있는 게 각 캐릭터 사이에 생김.

library(knitr)
include_graphics("./figure/fig4.png")


id = c(1, 2, 3)
name = c('Mr. Foo', 'Ms. Bar', 'Mr. Baz')
score = c(95, 97, 92)
a = data.frame(id, name, score)

write.csv(a, file = 'a.csv')
write.csv(a, file = 'a2.csv', row.names = FALSE)
write.table(a, quote = FALSE, sep = ',',
            file = 'a3.csv', row.names = FALSE)
write.table(a, quote = FALSE, sep = '\t',
            file = 'a4.txt', row.names = FALSE)

x <- read.csv("a2.csv")
x
str(x)

library(knitr)
include_graphics("./figure/fig5.png")


write.table(a, quote = FALSE, sep = ',', file = 'b.csv',
            row.names = FALSE, col.names = FALSE)

y <- read.csv("b.csv", header = FALSE)
y

colnames(y)
colnames(y) <- c('id', 'name', 'score')
y

z <- read.csv("a2.csv", stringsAsFactors = TRUE)
str(z)

b = list(a = 1:3, b = TRUE, c = 'oops')
save(a, b, file = 'xy.RData')
load('xy.RData')

include_graphics("./figure/fig_uci.png")


addr <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
w = read.csv(addr, header = TRUE, sep = ";")
w[1:6, 1:3]

e = rnorm(100)
n = length(e)
s = rep(0, n)
for (i  in  1:n) s[i] = sum(e[1:i])
t = 1:n
plot(t, s)

b = c(1, 5, -8, 0, -1, 2)
counter = 0
isPositive = TRUE
while (isPositive) {
  counter = counter + 1
  isPositive = (b[counter] >= 0)
}
cat("A negative number is detected at the",
    counter, "th place.\n")


include_graphics("./figure/fig2.jpg")


(x <- sample(1:5, 5))
counter = 0
for (val in x) {
  counter = counter + 1
  if (val == 4){
    cat("x is 4 at the", counter, "th place.\n")
    break
  }
}

include_graphics("./figure/fig3.jpg")


(x <- sample(1:5, 5))
counter = 0
for (val in x) {
  counter = counter + 1
  if (val <= 3){
    cat("x is less than 3 at the", counter, "th place.\n")
    next
  }
}

x = 3
if (x > 1) {
  y = 1
} else {
  y = -1
}
y

# c.f., ifelse (test, yes, no)
(y = ifelse(x > 1, 1, -1))

x = runif(100)
n = length(x)
argm = 1
m = x[1]
for (i in (2:n)) {
  if (m < x[i]) {
    m = x[i]
    argm = i
  }
}
c(argm, m)

myvector = c(1, 1, 1, 2, 2, 2, 2)
mean(myvector)

myfunction <- function(x) {  20 + x * x } 
myfunction(10)
myfunction(25)

fish(myvector)

x1 <- c(1, 2, 1, 5, 2, 3, 3, 4, 5, 4, 2)
(count_x1 <- tabulate(x1))
which.max(count_x1)

x2 <- c(3, 4, 7, 2, 3, 3, 7, 3, 2, 6, 6)
count_x2 <- tabulate(x2)
which.max(count_x2)

MyMode <- function(xval){
  count_xval <- tabulate(xval)
  res <- which.max(count_xval)
  return(res)
}

MyMode(x1) # x1 <- c(1, 2, 1, 2, 2, 3, 3, 4, 5, 4, 5)
MyMode(x2) # x2 <- c(3, 4, 7, 2, 3, 3, 7, 3, 2, 6, 6)

mdata = c(1, 2, 1, 3, 4, 9, 5)
which.max(mdata)
mdata = c(1, 2, 1, 3, 4, 9, 5, 9)
which.max(mdata)
##-------------------------------------

SSS <- function(xval){which.max(xval)}

##-------------------------------------
mywhich.max = function(x, val = FALSE, all = FALSE) { 
  n = length(x); ind = 1; m = x[1]
  for (i in 2:n) { if (m < x[i]) { ind = i; m = x[i] } }
  all.ind = (1:n)[x == m]

  if (val == TRUE) { 
    if (all == TRUE) { 
      return(list(max.ind = all.ind, max.val = m))
    } else {
      return(list(max.ind = ind, max.val = m))
    }
  } else {  
    if (all == TRUE) { 
      return(list(max.ind = all.ind))
    } else { 
      return(list(max.ind = ind))
    }
  }
}

mywhich.max(mdata)
mywhich.max(mdata, all = TRUE)


mywhich.max(mdata, val = TRUE)
mywhich.max(mdata, val = TRUE, all = TRUE)

(mdata2 = rbind(mdata, mdata))
mywhich.max(mdata2)
mywhich.max(mdata2, all = TRUE)

mywhich.max = function(x, val = FALSE, all = FALSE) {
  if (!is.vector(x)) stop('input is not a vector!')
  n = length(x);  ind = 1;  m = x[1]
  for (i in 2:n) {  if (m < x[i]) { m = x[i]; ind = i }  }
  all.ind = (1:n)[x == m]
  if (val == TRUE) {
    if (all == TRUE) {
      return(list(max.ind = all.ind, max.val = m))
    } else {
      return(list(max.ind = ind, max.val = m))
    }
  } else {
    if (all == TRUE) {
      return(list(max.ind = all.ind))
    } else {
      return(list(max.ind = ind))
    }  }  }

mywhich.max(mdata2)

f1 = function(a, b)
  return(a + b)
f2 = function(a, b)
  return(a - b)
g = function(h, a, b)
  h(a, b)  # h 는 임의의 함수
g(f1, 3, 2)
g(f2, 3, 2)

g = function(y) {
  h = function(x) {return(x ^ 2 + y)}
  return(h)
}

##-----------

A = function(y) {
  h = function(x){return(x+y+100)}
  return(h)
}

A(1)
AAA <- A(1)
AAA(3)


##-----------

test.ft = g(1) # test.ft가 g()의 output인 h 함수 역할을 함
test.ft(2) # 2^2 + 1

## source("djdjdjdj.R") <- 어떤 R 파일에서 환경 불러오기



x <- y <- runif(1000000)
head(x)

# looped
z1 <- c()
system.time(for (i in 1:1000000) {z1[i] <- x[i] + y[i]})

# vectorized
z2 <- c()
system.time(z2 <- x+y)

# looped
oddcount1 <- function(x) {
  nodd <- 0
  for (i in seq_along(x)) {
    if (x[i] %% 2 == 1)
      nodd <- nodd + 1
  }
  return(nodd)
}

# vectorized 
oddcount2 <- function(x) { return(sum(x %% 2 == 1)) }

xseq <- sample(1:1000000, 100000, replace = T)

system.time(oddcount1(xseq))
system.time(oddcount2(xseq))

library(foreach)
library(doParallel)

core_use = detectCores() - 1 # Set the number of cores to use
registerDoParallel(cores = core_use) # Register a number of cores to use.

results = foreach(l = 1:5, .combine = rbind) %dopar% {
  sample = rnorm(1000); mean(sample)
}
results

mean.parallel <- function(n) {
  x <- runif(1000000)
  mx = mean(x)
  fname = paste('meanx', n, '.RD', sep = '') ##paste : 글자 붙이기 함수
  save(mx, file = fname)
  return(n)
}

core_use = detectCores() - 1
registerDoParallel(cores = core_use)

results = foreach(l = 1:20, .combine = rbind) %dopar% {
  mean.parallel(l)
}

xvec = rep(0, 20)
for (i in 1:20) {
  fname = paste('meanx', i, '.RD', sep = '')
  load(fname)
  xvec[i] = mx
}
xvec
