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

###### Homework3 - 1######
install.packages("signal")
library(signal)

# FIR Filter
fs = 480    # 샘플링 주파수
bw <- 0.05  # 대역폭

# 0.5Hz High Pass Filter FIR 설계
f_hp_cutoff <- 0.5 / (fs / 2) 
hp_fir <- fir1(500, f_hp_cutoff, type = "high")
hp_filtered_data <- filter(hp_fir, readEcgData)

# 60Hz Notch Filter FIR 설계
f_notch_cutoff <- 60 / (fs / 2)
notch_fir <- fir1(30, c(f_notch_cutoff - bw, f_notch_cutoff + bw), type = "stop")
notch_filtered_data <- filter(notch_fir, readEcgData)

# 40Hz Low Pass Filter FIR 설계
f_lp_cutoff <- 40 / (fs / 2)
lp_fir <- fir1(30, f_lp_cutoff, type = "low")
lp_filtered_data <- filter(lp_fir, readEcgData)

# ALL Filtered 설계 1
notch_filtered_data1 <- filter(notch_fir, readEcgData)  # 노치 필터 적용
hp_filtered_data2 <- filter(hp_fir, notch_filtered_data1)  # 하이패스 필터 적용
final_filtered_data1 <- filter(lp_fir, hp_filtered_data2)  # 로우패스 필터 적용

# ALL Filtered 설계 2
stepfir1 <- filter(hp_fir, readEcgData)  # 하이패스 필터 적용
stepfir2 <- filter(notch_fir, stepfir1)  # 노치 필터 적용
final_filtered_data2 <- filter(lp_fir, stepfir2)  # 로우패스 필터 적용

# 그래프 시각화
par(mfrow = c(5, 1))
plot(time, readEcgData, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data")
plot(time, hp_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> High Pass Filtered ECG Data")
plot(time, notch_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Notch Filtered ECG Data")
plot(time, lp_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Low Pass Filtered ECG Data")
plot(time, final_filtered_data2, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Notch + High Pass + Low Pass Filtered ECG Data")

###### Homework3 - 2######
install.packages("signal")
library(signal)

# IIR Filter
fs <- 480  # 샘플링 주파수

# 2차 Butterworth # 40Hz Low Pass Filter IIR 설계
cutoff_lpf <- 40 / (fs / 2) 
lpf_filter <- butter(2, cutoff_lpf, type = "low")
lpf_filtered_data <- filter(lpf_filter, readEcgData)
print(lpf_filter)

# 2차 Butterworth 0.5Hz High Pass Filter IIR 설계
cutoff_hpf <- 0.5 / (fs / 2)
hpf_filter <- butter(1, cutoff_hpf, type = "high")
hpf_filtered_data <- filter(hpf_filter, readEcgData)
print(hpf_filter)

# 60Hz Notch Filter IIR 설계
notch_freq <- 60 / (fs / 2) 
bw <- 0.05  
notch_filter <- butter(1, c(notch_freq - bw, notch_freq + bw), type = "stop")
notch_filtered_data <- filter(notch_filter, readEcgData)
print(notch_filter)


# ALL Filtered 설계
step1 <- filter(hpf_filter, readEcgData)  # 60Hz 노치 필터 적용
step2 <- filter(notch_filter, step1)  # 0.5Hz 고주파 통과 필터 적용
final_filtered_data <- filter(lpf_filter, step2)  # 40Hz 저주파 통과 필터 적용

# 그래프 그리기
# 시간 축 설정 (데이터 길이에 따라 조정)
time <- seq(from = 0, to = 10, length.out = length(readEcgData))

# 그래프 시각화
par(mfrow = c(5, 1))
plot(time, readEcgData, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data")
plot(time, hpf_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> High Pass Filtered ECG Data")
plot(time, notch_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Notch Filtered ECG Data")
plot(time, lpf_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Low Pass Filtered ECG Data")
plot(time, final_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> High Pass + Notch + Low Pass Filtered ECG Data")

###### Homework4 ######
install.packages("signal")
library(signal)

# 샘플링 주파수 및 대역폭 설정
fs <- 125    # 샘플링 주파수
bw <- 0.02   # 대역폭

# 0.5Hz High-pass Filter FIR 설계 및 계수 출력
f_hp_cutoff <- 0.5 / (fs / 2)  # 정규화된 차단 주파수
hp_fir <- fir1(30, f_hp_cutoff, type = "high")  # 30차 HPF 설계
print(hp_fir)  # 계수 출력

fs <- 125    # 샘플링 주파수
f_hp_cutoff <- 0.5 / (fs / 2)  # 정규화된 차단 주파수
hp_fir <- fir1(30, f_hp_cutoff, type = "high")

# 60Hz Notch Filter FIR 설계 및 계수 출력
f_notch_cutoff <- 60 / (fs / 2)  # 정규화된 노치 주파수
notch_fir <- fir1(30, c(f_notch_cutoff - bw, f_notch_cutoff + bw), type = "stop")
print(notch_fir)  # 계수 출력

# 40Hz Low-pass Filter FIR 설계 및 계수 출력
f_lp_cutoff <- 40 / (fs / 2)  # 정규화된 차단 주파수
lp_fir <- fir1(30, f_lp_cutoff, type = "low")  # 30차 LPF 설계
print(lp_fir)  # 계수 출력

# IIR Filter
fs <- 125  # 샘플링 주파수

# 2차 Butterworth # 40Hz Low Pass Filter IIR 설계
cutoff_lpf <- 40 / (fs / 2) 
lpf_filter <- butter(5, cutoff_lpf, type = "low")
lpf_filtered_data <- filter(lpf_filter, readEcgData)
print(lpf_filter)

# 2차 Butterworth 0.5Hz High Pass Filter IIR 설계
cutoff_hpf <- 0.5 / (fs / 2)
hpf_filter <- butter(2, cutoff_hpf, type = "high")
hpf_filtered_data <- filter(hpf_filter, readEcgData)
print(hpf_filter)

# 60Hz Notch Filter IIR 설계
notch_freq <- 60 / (fs / 2) 
bw <- 0.03  
notch_filter <- butter(2, c(notch_freq - bw, notch_freq + bw), type = "stop")
notch_filtered_data <- filter(notch_filter, readEcgData)
print(notch_filter)

# 그래프 시각화
par(mfrow = c(5, 1))
plot(time, readEcgData, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data")
plot(time, hpf_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> High Pass Filtered ECG Data")
plot(time, notch_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Notch Filtered ECG Data")
plot(time, lpf_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Low Pass Filtered ECG Data")
plot(time, final_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> High Pass + Notch + Low Pass Filtered ECG Data")

###### Homework5 ######
##### PanTompkins #####

library(signal)
rawdata125 = "C:/Users/KANGJONGYEOP/Desktop/새 폴더/FW/Labview Test/ECG Data/rawdata.txt"
ad7171rawdata = scan(rawdata125)

# IIR Filter
fs <- 125  # 샘플링 주파수

# 2차 Butterworth # 30Hz Low Pass Filter IIR 설계
cutoff_lpf <- 30 / (fs / 2) 
lpf_filter <- butter(3, cutoff_lpf, type = "low")
lpf_filtered_data <- filter(lpf_filter, ad7171rawdata)
print(lpf_filter)

# 2차 Butterworth 0.5Hz High Pass Filter IIR 설계
cutoff_hpf <- 5 / (fs / 2)
hpf_filter <- butter(1, cutoff_hpf, type = "high")
hpf_filtered_data <- filter(hpf_filter, ad7171rawdata)
print(hpf_filter)

# 60Hz Notch Filter IIR 설계
notch_freq <- 60 / (fs / 2) 
bw <- 0.03
notch_filter <- butter(2, c(notch_freq - bw, notch_freq + bw), type = "stop")
notch_filtered_data <- filter(notch_filter, ad7171rawdata)
print(notch_filter)

# 그래프 시각화
par(mfrow = c(4, 1))
plot(ad7171rawdata, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data")
plot(hpf_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> High Pass Filtered ECG Data")
plot(lpf_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Low Pass Filtered ECG Data")
plot(notch_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Notch Filtered ECG Data")

# 주파수 응답 곡선
par(mfrow = c(3, 1))
freqz(lpf_filter, Fs = fs, main = "Low Pass Filter Frequency Response")
freqz(hpf_filter, Fs = fs, main = "High Pass Filter Frequency Response")
freqz(notch_filter, Fs = fs, main = "Notch Filter Frequency Response")

dev.new()  # 새로운 그래프 창 열기
freqz(lpf_filter, Fs = fs, main = "Low Pass Filter Frequency Response")
dev.new()  # 새로운 그래프 창 열기
freqz(hpf_filter, Fs = fs, main = "High Pass Filter Frequency Response")
dev.new()  # 새로운 그래프 창 열기
freqz(notch_filter, Fs = fs, main = "Notch Filter Frequency Response")

###### test ######

library(signal)
rawdataJoey = "//mezooNAS/HW_Team/21. HW_Development/1. HiCardi/LabVIEW/HiCardi_Sleep_test_종엽/test.txt"
data = scan(rawdataJoey)

# IIR Filter
fs <- 500  # 샘플링 주파수

# 2차 Butterworth # 30Hz Low Pass Filter IIR 설계
cutoff_lpf <- 30 / (fs / 2) 
lpf_filter <- butter(3, cutoff_lpf, type = "low")
lpf_filtered_data <- filter(lpf_filter, data)
print(lpf_filter)

# 2차 Butterworth 0.5Hz High Pass Filter IIR 설계
cutoff_hpf <- 5 / (fs / 2)
hpf_filter <- butter(1, cutoff_hpf, type = "high")
hpf_filtered_data <- filter(hpf_filter, data)
print(hpf_filter)

# 60Hz Notch Filter IIR 설계
notch_freq <- 60 / (fs / 2) 
bw <- 0.03
notch_filter <- butter(2, c(notch_freq - bw, notch_freq + bw), type = "stop")
notch_filtered_data <- filter(notch_filter, data)
print(notch_filter)

# ALL Filtered
step1 <- filter(hpf_filter, data)  # 60Hz 노치 필터 적용
step2 <- filter(notch_filter, step1)  # 0.5Hz 고주파 통과 필터 적용
final_filtered_data <- filter(lpf_filter, step2)  # 40Hz 저주파 통과 필터 적용

# 그래프 시각화
par(mfrow = c(5, 1))
plot(data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data")
plot(hpf_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> High Pass Filtered ECG Data")
plot(lpf_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Low Pass Filtered ECG Data")
plot(notch_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> Notch Filtered ECG Data")
plot(final_filtered_data, type = "l", xlab = "Index", ylab = "ECG", main = "Raw ECG Data -> High Pass + Notch + Low Pass Filtered ECG Data")


# 주파수 응답 곡선
par(mfrow = c(3, 1))
freqz(lpf_filter, Fs = fs, main = "Low Pass Filter Frequency Response")
freqz(hpf_filter, Fs = fs, main = "High Pass Filter Frequency Response")
freqz(notch_filter, Fs = fs, main = "Notch Filter Frequency Response")

dev.new()  # 새로운 그래프 창 열기
freqz(lpf_filter, Fs = fs, main = "Low Pass Filter Frequency Response")
dev.new()  # 새로운 그래프 창 열기
freqz(hpf_filter, Fs = fs, main = "High Pass Filter Frequency Response")
dev.new()  # 새로운 그래프 창 열기
freqz(notch_filter, Fs = fs, main = "Notch Filter Frequency Response")
