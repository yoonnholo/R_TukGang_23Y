#  Exercise 1
#  심근경색 자료:  
MI <- read.csv("MIdata.csv")
View(MI)
MI.tab1 <-xtabs(~SBPgt140+MI, data=MI)
MI.tab1

MI.tab2 <-matrix(c(1244,711,27,29), nrow=2)
dimnames(MI.tab2)<-list(SBPgt140 = c("저혈압" , "고혈압"),
                        MI =c("건강" , "심장마비" ) )
MI.tab2

Freq_dat <- tribble(
  ~SBPgt140, ~MI, ~Freq,
  "저혈압", "건강", 1244,
  "저혈압", "심장마비", 27,
  "고혈압", "건강", 711,
  "고혈압", "심장마비", 29)
MI.tab3 <- xtabs(Freq ~ SBPgt140 + MI, data = Freq_dat)
MI.tab3


# Exercise 2 

dat <- read.csv("Exercise2dat.csv")
report.tab <- xtabs(~ smoke + Health, data = dat)
report.tab <- report.tab[,c(4,2,3,5,1)]

#report.tab <-matrix(c(11,7,27,15,42,16,53,13,11,1), nrow=2)
#dimnames(report.tab) =list(smoke=c("No", "Yes"), 
#                           Health=c("Poor","Fair","Good","V. Good", "Exc"))

chisq.test(report.tab) 

report.smoke <-report.tab["Yes",]
report.total <-margin.table(report.tab,2)
prop.trend.test(report.smoke, report.total)

mosaicplot(t(report.tab))



# Exercise 3:  마약정맥주사와 HIV 감염여부

## 1. Chi-square test of independence 
chisq.test(prison.tab)$p.value

## 2. Relative risk estimation 
RelRisk(prison.tab , conf.level = 0.95)  

## 3. Relative risk estimation 
OddsRatio(prison.tab , conf.level = 0.95)  

 


# Exercise 4: Lady tasting tea
 
TeaTasting <-matrix(c(3, 1, 1, 3),nrow = 2)
dimnames(TeaTasting) <- list(Guess = c("Milk", "Tea"),
                             Truth = c("Milk", "Tea"))
fisher.test(TeaTasting, alternative = "greater")
 

# Exercise 5: 

accident <-as.table(array(c(31,22,273,184,73,185,137,129),
                          dim = c(2, 2, 2)))
dimnames(accident) <-list(Seat_belt = c("worn","not"),
                          Drive = c("dead","alive"),
                          Impact_Speed = c("<40mph", ">=40mph"))
accident <- Rev(accident, 1)
# 0. Visualize
mosaicplot(~ Seat_belt + Drive, data = accident)
mosaicplot(~ Impact_Speed + Seat_belt + Drive, data = accident)
ggplot( data = Untable(accident), aes(x = Seat_belt, fill = Drive)) +
  geom_bar(position = "fill") + facet_wrap(~Impact_Speed)

# 1. 속도별 오즈비 추정
apply(accident, 3,
      function(x) list(rbind(
        "Case-control (odds ratio)" = OddsRatio(x, conf.level = 0.95),
        "Cohort (col1 risk)" = RelRisk(x, conf.level = 0.95),
        "Cohort (col2 risk)" = RelRisk(Rev(x, 1), conf.level = 0.95))))

# 2. 오즈비가 층별로 다른지 검정
BreslowDayTest(accident)

# 3. 통합 오즈비 추정 및 검정
mantelhaen.test(accident)

 