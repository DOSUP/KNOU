home <- read.csv("homework.csv")
View(home)
library(dplyr)

# 1번 원지수, 계절조정 그리기
home_ts <-home %>% select(원지수)
home_ts_s <- home %>% select(계절조정)

par(mfrow = c(2, 1))

home_ts_plot <- ts(home_ts, start = 1982, frequency = 12)
plot(home_ts_plot, main = "원지수 시계열 그래프")

home_ts_s_plot <- ts(home_ts_s, start = 1982, frequency = 12)
plot(home_ts_s_plot, main = "계절조정 시계열 그래프")

# 2번 스펙트럼 구하기 
ts_spec <- spectrum(home_ts, spans = c(3, 3), main = "원지수 스펙트럼")
ts_s_spec <- spectrum(home_ts_s, spans = c(3,3), main = "계절조정 스펙트럼", col = "red")

# 3번 차분하기
home_diff <- diff(home_ts_s_plot)
plot(home_diff, main = "차분")
plot(home_ts_s_plot, main = "계절조정")
adf.test(home_ts_s_plot)
adf.test(home_diff)

par(mfrow = c(2,2))
acf(home_ts_s_plot, main = "계절조정 상관도표")
pacf(home_ts_s_plot, main = "계절조정 자기상관도표")
acf(home_diff, main = "차분 상관도표")
pacf(home_diff, main = "차분 자기상관도표")
