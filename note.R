# 가정: 'data1' 데이터 프레임 내에 'RiskTaking'과 변환된 두 'EgoStrength' 변수(예를 들어 'logEgoStrength', 'sqrtEgoStrength'라고 가정)가 있다.

# 첫 번째 변환된 변수에 대한 회귀 모델 적합
logM
# 두 번째 변환된 변수에 대한 회귀 모델 적합
SqM

# 원본 데이터에 대한 산점도
plot(data1$RiskTaking ~ data1$EgoStrength, main = "Scatterplot with Fitted Regression Lines", xlab = "Ego Strength", ylab = "Risk Taking")

# 첫 번째 회귀선 추가
abline(logM, col = "blue")

# 두 번째 회귀선 추가
abline(SqM, col = "red")
