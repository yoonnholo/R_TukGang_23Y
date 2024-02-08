xtabs(data = MIdata, ~ SBPgt140 + MI) -> test

matrix(c(1244, 711, 27, 29), nrow = 2) -> test2
dimnames(test2) <- list(SBPgt140=c("No","Yes"), MI=c("No","Yes"))

datata <- tribble(
  ~SBPgt140, ~MI, ~Freq,
  "No", "No", 1244,
  "No", "Yes", 27,
  "Yes", "No", 711,
  "Yes", "Yes", 29,
)
test3 <- xtabs(Freq ~ SBPgt140 + MI, data = datata)


Exercise2dat <- read_csv("Exercise2dat.csv")
View(Exercise2dat)
str(Exercise2dat)
Exercise2dat <- as.data.frame(Exercise2dat)
table(Exercise2dat$smoke)
table(Exercise2dat$Health)
xtabs(data = Exercise2dat, ~ smoke + Health) -> testtest

testtest <- testtest [ ,c(4, 2, 3, 5, 1)]

chisq.test(testtest)

smoke_yes <- testtest["Yes", ]
smoke_total <- margin.table(testtest, 2)
prop.trend.test(smoke_yes, smoke_total)

mosaicplot(t(testtest))



MA <- matrix(c(59, 29, 77, 310), nrow = 2)
dimnames(MA) <- list(IVDU=c("Yes", "No"), HIV=c("+", "-"))

chisq.test(MA)$p.value
RelRisk(MA)
OddsRatio(MA, conf.level = 0.95)

Fish <- matrix(c(3, 1, 1, 3), nrow = 2)
dimnames(Fish) <- list(Real = c("Milk", "Tea"), Guess = c("Milk", "Tea"))
fisher.test(Fish, alternative = "greater")


MI2 <- read_csv("MI2.csv")
View(MI2)

slow <- matrix(c(22, 31, 184, 273), nrow = 2)
dimnames(slow) <- list(Belt = c("not", "worm"), Driver = c("dead", "alive"))

fast <- matrix(c(185, 73, 129, 137), nrow = 2)
dimnames(fast) <- list(Belt = c("not", "worm"), Driver = c("dead", "alive"))


