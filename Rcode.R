### 12/08 ###

# Step 1 環境設定與資料載入
rm(list = ls()) # 清空環境
setwd("C:/SDAC/Final_project/") # 設定工作目錄

# 載入資料
load("Complete_48.RData")
load("Complete_72.RData")

# ------------------------------------------------------------------------------
# Step 2：資料合併
# 使用 release_id 合併兩波資料
Full_Data <- merge(Complete_48, Complete_72, 
                   by = "release_id", 
                   suffixes = c("_48", "_72"))

cat("合併後資料維度:", dim(Full_Data), "\n")

# ------------------------------------------------------------------------------
# Step 3：資料清理
# 選取分析欄位
Raw_Data <- Full_Data[, c("release_id", 
                          # 48月齡
                          "med0801", "med0802", "med0803",
                          "health06_48", "emo_eap_48", 
                          # 72月齡
                          "med1202", "med1203", "med1204",
                          "health06_72", "emo_eap_72",
                          # 控制變數
                          "pfb12_48", "pfa0201_48", "pfa0202_48", "baby_sex_48")]

##建立Allergy_48
temp_asthma_48   <- Raw_Data$med0801
temp_rhinitis_48 <- Raw_Data$med0802
temp_skin_48     <- Raw_Data$med0803

# 跳答(9996)視為0
temp_asthma_48[temp_asthma_48 == 9996]     <- 0
temp_rhinitis_48[temp_rhinitis_48 == 9996] <- 0
temp_skin_48[temp_skin_48 == 9996]         <- 0

# 三者其一即為過敏
Raw_Data$Allergy_48 <- ifelse(temp_asthma_48 == 1 | temp_rhinitis_48 == 1 | temp_skin_48 == 1, 1, 0)


##建立Allergy_72
temp_asthma_72   <- Raw_Data$med1202
temp_rhinitis_72 <- Raw_Data$med1203
temp_skin_72     <- Raw_Data$med1204

temp_asthma_72[temp_asthma_72 == 9996]     <- 0
temp_rhinitis_72[temp_rhinitis_72 == 9996] <- 0
temp_skin_72[temp_skin_72 == 9996]         <- 0

Raw_Data$Allergy_72 <- ifelse(temp_asthma_72 == 1 | temp_rhinitis_72 == 1 | temp_skin_72 == 1, 1, 0)

# 複製一份作為最終分析資料
Clean_Data <- Raw_Data

## 處理異常值
# 整體健康
Clean_Data$health06_48[Clean_Data$health06_48 > 4] <- NA
Clean_Data$health06_72[Clean_Data$health06_72 > 4] <- NA

# 收入&教育
Clean_Data$pfb12_48[Clean_Data$pfb12_48 > 8000]     <- NA
Clean_Data$pfa0201_48[Clean_Data$pfa0201_48 > 9000] <- NA
Clean_Data$pfa0202_48[Clean_Data$pfa0202_48 > 9000] <- NA

# 重新命名欄位
colnames(Clean_Data)[match(c("release_id", "emo_eap_48", "emo_eap_72", 
                             "pfb12_48", "pfa0201_48", "pfa0202_48"), 
                           colnames(Clean_Data))] <- 
  c("ID", "Emo_48", "Emo_72", "Income_48", "Edu_F_48", "Edu_M_48")

# 因子轉換
Clean_Data$Allergy_48 <- factor(Clean_Data$Allergy_48, levels = c(0, 1), labels = c("No", "Yes"))
Clean_Data$Allergy_72 <- factor(Clean_Data$Allergy_72, levels = c(0, 1), labels = c("No", "Yes"))
Clean_Data$Sex <- factor(Clean_Data$baby_sex_48, levels = c(1, 2), labels = c("Boy", "Girl"))
Clean_Data$Health_48 <- as.factor(Clean_Data$health06_48)
Clean_Data$Health_72 <- as.factor(Clean_Data$health06_72)

cat("\n--- 資料清理完成，最終變數檢查 ---\n")
str(Clean_Data[, c("Emo_48", "Emo_72", "Allergy_48", "Allergy_72")])

# ------------------------------------------------------------------------------
# Step 4：敘述性統計與繪圖
par(mfrow = c(2, 2))
hist(Clean_Data$Emo_48, main = "Emotion Score (48m)", xlab = "Score", col = "lightblue")
hist(Clean_Data$Emo_72, main = "Emotion Score (72m)", xlab = "Score", col = "lightgreen")
barplot(table(Clean_Data$Allergy_48), main = "Allergy (48m)", col = c("gray", "salmon"))
barplot(table(Clean_Data$Allergy_72), main = "Allergy (72m)", col = c("gray", "salmon"))
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# Step 5：二樣本檢定 (Two-Sample T-test)

# --- 1. [橫斷面] 48月過敏 vs 48月情緒 ---
cat("\n====== [橫斷面] 48月過敏 vs 48月情緒 ======\n")
var_test_48 <- var.test(Emo_48 ~ Allergy_48, data = Clean_Data)
print(var_test_48)
t_test_48 <- t.test(Emo_48 ~ Allergy_48, 
                    data = Clean_Data, 
                    var.equal = var_test_48$p.value > 0.05,
                    alternative = "greater") 
print(t_test_48)

# --- 2. [橫斷面] 72月過敏 vs 72月情緒 ---
cat("\n====== [橫斷面] 72月過敏 vs 72月情緒 ======\n")
var_test_72 <- var.test(Emo_72 ~ Allergy_72, data = Clean_Data)
print(var_test_72)
t_test_72 <- t.test(Emo_72 ~ Allergy_72, 
                    data = Clean_Data, 
                    var.equal = var_test_72$p.value > 0.05,
                    alternative = "greater") 
print(t_test_72)

# --- 3. [縱貫面] 48月過敏 vs 72月情緒 ---
cat("\n====== [縱貫面] 48月過敏 vs 72月情緒 ======\n")
var_test_long <- var.test(Emo_72 ~ Allergy_48, data = Clean_Data)
print(var_test_long)

t_test_long <- t.test(Emo_72 ~ Allergy_48, 
                      data = Clean_Data, 
                      var.equal = var_test_long$p.value > 0.05,
                      alternative = "greater") 
print(t_test_long)

# 48月過敏對72月情緒的影響
boxplot(Emo_72 ~ Allergy_48, data = Clean_Data,
        main = "Allergy(48m) -> Emotion(72m)",
        col = c("lightgray", "salmon"),
        ylab = "Emotion Score (72m)",
        xlab = "Allergy Status at 48m")

# ------------------------------------------------------------------------------
# Step 6：多樣本檢定 (ANOVA) 與 假設檢查

# 檢查各健康狀況分組人數
cat("\n--- [48月齡] 健康狀況分組人數統計 ---\n")
print(table(Clean_Data$Health_48))

cat("\n--- [72月齡] 健康狀況分組人數統計 ---\n")
print(table(Clean_Data$Health_72))

# 執行 ANOVA 與 事後比較
# --- [48月齡] ---
cat("\n================ [48月齡] ANOVA: 健康狀況 vs 情緒 ================\n")
anova_48 <- aov(Emo_48 ~ Health_48, data = Clean_Data)
print(summary(anova_48))
if(summary(anova_48)[[1]][["Pr(>F)"]][1] < 0.05) {
  print(TukeyHSD(anova_48))
}

# --- [72月齡] ---
cat("\n================ [72月齡] ANOVA: 健康狀況 vs 情緒 ================\n")
anova_72 <- aov(Emo_72 ~ Health_72, data = Clean_Data)
print(summary(anova_72))
if(summary(anova_72)[[1]][["Pr(>F)"]][1] < 0.05) {
  print(TukeyHSD(anova_72))
}

# 3. ANOVA 假設檢查圖形 (Box-plot & QQ Plot)
par(mfrow = c(2, 2))

# [48月齡]
boxplot(residuals(anova_48) ~ anova_48$model$Health_48,
        main = "Boxplot of Residuals (48m)", 
        xlab = "Health Group", 
        ylab = "Residuals", 
        col = "lightblue")

qqnorm(residuals(anova_48), main = "Q-Q Plot (48m)")
qqline(residuals(anova_48), col = "red", lwd = 2)

var(residuals(anova_48))

# [72月齡]
boxplot(residuals(anova_72) ~ anova_72$model$Health_72,
        main = "Boxplot of Residuals (72m)", 
        xlab = "Health Group", 
        ylab = "Residuals", 
        col = "lightgreen")

qqnorm(residuals(anova_72), main = "Q-Q Plot (72m)")
qqline(residuals(anova_72), col = "red", lwd = 2)


# ------------------------------------------------------------------------------
# Step 7：統計模型 - Regression

#lm_model <- lm(Emo_72 ~ Allergy_48 + Health_72 + Emo_48  + 
#                 Income_48 + Edu_F_48 + Edu_M_48 + Sex, 
#               data = Clean_Data) 

lm_model <- lm(Emo_72 ~ Allergy_48 + Health_48 + Emo_48 +
              Income_48 + Edu_F_48 + Edu_M_48 + Sex, 
              data = Clean_Data)
# 補充
lm_model <- lm(Emo_72 ~ Allergy_48 + Health_48 +
                 Income_48 + Edu_F_48 + Edu_M_48 + Sex, 
               data = Clean_Data)

summary(lm_model)

# 模型診斷圖
plot(lm_model)


# =============================================================================
cat_vars <- c("med0801", "med0802", "med0803", 
              "med1202", "med1203", "med1204",
              "health06_48", "health06_72",
              "pfb12_48",               
              "pfa0201_48", "pfa0202_48",
              "baby_sex_48")

for (var in cat_vars) {
  if (var %in% names(Raw_Data)) {
    cat(paste0("\n[變數名稱]: ", var, "\n"))
    print(table(Raw_Data[[var]], useNA = "always"))
  } else {
    cat(paste0("\n[警告]: 找不到變數 ", var, "\n"))
  }
}




# ==============================================================================
# Step 7.1：回歸模型假設檢查
# ==============================================================================

# 1. 提取模型資訊
res <- residuals(lm_model)  # 殘差
fit <- fitted(lm_model)     # 預測值 (Fitted values)

# ------------------------------------------------------------------------------
# 檢查一：常態性假設 (Normality)
# 方法：Shapiro-Wilk Normality Test
# H0: 殘差服從常態分佈
# ------------------------------------------------------------------------------
cat("\n--- [檢查 1] 常態性檢定 (Shapiro-Wilk) ---\n")
shapiro_res <- shapiro.test(res)
print(shapiro_res)

# 判斷結果輸出
if(shapiro_res$p.value > 0.05){
  cat("結果：P-value > 0.05，無法拒絕 H0，殘差符合常態分佈假設。\n")
} else {
  cat("結果：P-value < 0.05，拒絕 H0，殘差可能違反常態假設 (樣本數大時常態檢定較敏感，請搭配 Q-Q 圖判斷)。\n")
}

# ------------------------------------------------------------------------------
# 檢查二：變異數同質性假設 (Homoscedasticity)
# 方法：Split-Sample Variance Test (依照老師 case0702 作法)
# 邏輯：將樣本依「預測值」由小到大排序，切成「低預測值組」與「高預測值組」，
#       檢定兩組殘差變異數是否相等。
# H0: 兩組變異數相等 (變異數同質)
# ------------------------------------------------------------------------------
cat("\n--- [檢查 2] 變異數同質性檢定 (Split-Sample F-test) ---\n")

# 建立暫存資料框 (包含預測值與殘差)
diag_data <- data.frame(Fitted = fit, Residuals = res)

# 依照預測值 (Fitted) 由小到大排序
diag_data <- diag_data[order(diag_data$Fitted), ]

# 切分資料 (前 50% vs 後 50%)
n_obs <- nrow(diag_data)
cut_point <- floor(n_obs / 2)

res_low  <- diag_data$Residuals[1:cut_point]          # 低預測值組
res_high <- diag_data$Residuals[(cut_point + 1):n_obs] # 高預測值組

# 執行 F-test (var.test)
var_test_homo <- var.test(res_low, res_high)
print(var_test_homo)

# 判斷結果輸出
if(var_test_homo$p.value > 0.05){
  cat("結果：P-value > 0.05，無法拒絕 H0，兩組變異數無顯著差異 (滿足同質性假設)。\n")
} else {
  cat("結果：P-value < 0.05，拒絕 H0，變異數可能存在異質性 (Heteroscedasticity)。\n")
}

# ------------------------------------------------------------------------------
# 檢查三：診斷圖形 (視覺化輔助)
# ------------------------------------------------------------------------------
# 開啟新繪圖視窗 (若在 RStudio 可忽略 x11)
# x11() 

par(mfrow = c(2, 2))

# 1. 殘差直方圖 (Histogram)
hist(res, breaks = 20, col = "lightblue", 
     main = "", xlab = "Residuals")

# 2. 常態機率圖 (Q-Q Plot)
qqnorm(res, main = "")
qqline(res, col = "red", lwd = 2)

# 3. 殘差 vs 預測值 (整合檢查版：同時看線性與同質性)
plot(diag_data$Fitted, diag_data$Residuals, pch = 20, col = "gray50",
     main = "Sorted Residuals (Linearity & Homogeneity)", 
     xlab = "Sorted Fitted Values", ylab = "Residuals")
# A. 畫零位基準線 (紅色虛線)
abline(h = 0, col = "red", lty = 2)
# B. 畫垂直切分線 (藍色虛線) -> 檢查同質性 (左右寬度)
abline(v = diag_data$Fitted[cut_point], col = "darkblue", lwd = 2, lty = 3) 
# C. 畫平滑趨勢線 -> 檢查線性
lines(lowess(diag_data$Fitted, diag_data$Residuals), col = "blue", lwd = 2)

par(mfrow = c(1, 1)) # 復原設定

