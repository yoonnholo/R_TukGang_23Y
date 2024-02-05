# install.packages("tidyverse")
library(tidyverse)


#######################
###   mpg dataset   ###
#######################

# ----- 데이터 살펴보기 ----- #
str(mpg)


# ----- 산점도 그리기 ----- #
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) # 간결한 표현


# ----- Aesthetic ----- #
# color, shape, size, alpha 등 매핑 가능
# size/alpha -> numeric 변수 매핑

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy, color = class))

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy), color = 'blue') # 비교


# ----- Geom ----- #
# point, smooth, line, bar, histogram, density 등
# 데이터의 형태에 따라 적절한 geom 선택

ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) # point geom

ggplot(mpg) +
  geom_smooth(aes(x = displ, y = hwy)) # smooth geom

# 여러개의 geom 동시 사용 가능
ggplot(mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  geom_smooth(aes(x = displ, y = hwy))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() + geom_smooth() # 간결한 표현


# ----- Label ----- #
# title, subtitle, x, y, caption 등 매핑 가능

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class), size = 4) + geom_smooth() +
  labs(title = "망할 그래프",
       x = "Engine size", y = "Fuel efficiency") +
  theme(plot.title = element_text(size = 15, hjust = 0.5))


# ----- 연속형 자료의 요약 ----- #
# 히스토그램
ggplot(mpg, aes(x = hwy)) +
  geom_histogram(bins = 20) +
  labs(title = "Fuel efficiency(hwy) Histogram")

# 상자그림
ggplot(mpg, aes(x = drv, y = hwy)) +
  geom_boxplot() +
  labs(title = "hwy(fuel efficiency) vs. drv(type of drive train)")


# ----- 이산형 자료의 요약 ----- #
# 도수분포표
mpg$hwy_new = ifelse(mpg$hwy <= 25, "bad", "good")
table(mpg$drv, mpg$hwy_new)

# 막대그래프
ggplot(mpg) +
  geom_bar(aes(x = drv, y = hwy), stat = 'summary', fun = 'mean') +
  labs(title = 'Mean of hwy')

ggplot(mpg) +
  geom_bar(aes(x = drv, fill = hwy_new))+
  labs(title = 'Count plot of hwy_new vs. drv')



########################
###   Iris dataset   ###
########################

# ----- 데이터 살펴보기 ----- #
data(iris)
str(iris)


# ----- 데이터 시각화 ----- #
# 꽃잎의 크기와 너비 산점도
ggplot(iris) +
  geom_point(aes(x = Petal.Length, y = Petal.Width)) +
  labs(title = "Scatter plot of Petal Width vs. Petal Length",
       x = "petal length", y = "petal width") +
  theme(plot.title = element_text(size = 18, hjust = 0.5))

# 품종에 따른 산포를 살펴보기 위해 color aes 지정
ggplot(iris) +
  geom_point(aes(x = Petal.Length, y = Petal.Width,
                 color = Species, shape = Species), size = 2) +
  labs(title = "Scatter plot of Petal Width vs. Petal Length",
       x = "petal length", y = "petal width") +
  theme(plot.title = element_text(size = 18, hjust = 0.5))

# 꽃받침의 크기와 너비 산점도 (같은 방식으로)
ggplot(iris) +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width,
                 color = Species, shape = Species), size = 4) +
  labs(title = "Scatter plot of Sepal Width vs. Sepal Length",
       x = "sepal length", y = "sepal width") +
  theme(plot.title = element_text(size = 22, hjust = 0.5))

# 상자그림을 이용한 탐색
# install.packages("gridExtra")
library(gridExtra)

p1 = ggplot(iris) +
  geom_boxplot(aes(x = Species, y = Sepal.Length)) +
  labs(title = "Sepal Length") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p2 = ggplot(iris) +
  geom_boxplot(aes(x = Species, y = Sepal.Width)) +
  labs(title = "Sepal Width") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p3 = ggplot(iris) +
  geom_boxplot(aes(x = Species, y = Petal.Length)) +
  labs(title = "Petal Length") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

p4 = ggplot(iris) +
  geom_boxplot(aes(x = Species, y = Petal.Width)) +
  labs(title = "Petal Width") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

grid.arrange(p1,p2,p3,p4, nrow = 2) # 그래프 여러개 동시에 그리고 싶을 때


# ----- 수치 요약을 통한 탐색 ----- #
tapply(iris$Sepal.Width, iris$Species, summary)


# ----- 가설 검정 ----- #
# 등분산 검정
vs = iris$Sepal.Width[iris$Species == 'versicolor']
vg = iris$Sepal.Width[iris$Species == 'virginica']
var.test(vs, vg) # 기각 x -> 등분산 가정

# 등분산 독립 이표본 t검정
t.test(vs, vg, var.equal = TRUE)
