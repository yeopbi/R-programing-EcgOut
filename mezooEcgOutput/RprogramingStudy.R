###### Homework1 ######
txtFilePath = "C:/R-programing-EcgOut/ecg60HzData.txt"
readEcgData = scan(txtFilePath)

time = seq(from = 0, to = 100, length.out = length(readEcgData))
timeAxis = seq(from = 0, to = 10, length.out = length(readEcgData))

par(mfrow = c(2, 1))
plot(time, readEcgData, type = "l", xlab = "t", ylab = "ecg")
plot(timeAxis, readEcgData, type = "l", xlab = "t", ylab = "ecg")


###### Homework2 ######
install.packages("zoo")
library(zoo)

# Moving Average Filter FIR 설계
moving_average <- function(x, n = 10) {
  stats::filter(x, rep(1/n, n), sides = 2)
}

# Moving Average 필터 적용
readEcgData = scan(txtFilePath)
filtered_ecg <- moving_average(x = readEcgData, n = 5)

# 시간 범위 설정
time <- seq(from = 0, to = 10, length.out = length(readEcgData))

# 그래프 그리기
par(mfrow = c(2, 1))
plot(time, readEcgData, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data")
plot(time, filtered_ecg, type = "l", xlab = "Index", ylab = "ECG", main = "Moving Average Filtered ECG Data")

###### Homework3 ######
install.packages("signal")
library(signal)

fs = 480    # 샘플링 주파수
bw <- 0.05  # 대역폭

# 60Hz Notch Filter FIR 설계
f_notch_cutoff <- 60 / (fs / 2)
notch_fir <- fir1(200, c(f_notch_cutoff - bw, f_notch_cutoff + bw), type = "stop")
notch_filtered_data <- filter(notch_fir, readEcgData)

# 0.5Hz High Pass Filter FIR 설계
f_hp_cutoff <- 0.5 / (fs / 2) 
hp_fir <- fir1(200, f_hp_cutoff, type = "high")
hp_filtered_data <- filter(hp_fir, readEcgData)

# 40Hz Low Pass Filter FIR 설계
f_lp_cutoff <- 40 / (fs / 2)
lp_fir <- fir1(200, f_lp_cutoff, type = "low")
lp_filtered_data <- filter(lp_fir, readEcgData)

# ALL Filtered 설계
notch_filtered_data1 <- filter(notch_fir, readEcgData)  # 노치 필터 적용
hp_filtered_data2 <- filter(hp_fir, notch_filtered_data1)  # 하이패스 필터 적용
final_filtered_data <- filter(lp_fir, hp_filtered_data2)  # 로우패스 필터 적용

# 그래프 그리기
par(mfrow = c(5, 1))
plot(time, readEcgData, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data")
plot(time, notch_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Notch Filtered ECG Data")
plot(time, hp_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> High Pass Filtered ECG Data")
plot(time, lp_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Low Pass Filtered ECG Data")
plot(time, final_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Notch + High Pass + Low Pass Filtered ECG Data")
