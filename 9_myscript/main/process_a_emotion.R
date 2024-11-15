# 0. Info ----

# Project: annotated data process
# Author: LiuZiyu
# Created date: 2024/11
# Last edited date: 2024/11/12

# This script is for
#   1) import annotated valence and arousal data,
#   2) fill in v and a data,
#   3) calculate rater consistency

# Loads the Excel data.
# Processes the data (selects columns, replaces NAs, and calculates time).
# Merges the data based on imgIdx.
# Computes the Kappa value.
# Saves the processed data and the plot.

# Data source:
# annotated valence and arousal data exported from boris

# 0 program set up ----
rm(list = ls()) # clear work space

root_dir <- "D:/# Library/0 Academic/2_Programs/2_fNIRS/process_a_emotion"
config_dir <- file.path(root_dir, "9_myscript", "config")
source(file.path(config_dir, "config.R"))

# 1 import data1 ----
# Example of usage:
file1 <- file.path(raw_dir, "0908-第3组-S1S2-前测-讨论-A_lzy.xlsx")  # Replace with actual path
file2 <- file.path(raw_dir, "0908-第3组-S1S2-前测-讨论-A_wny.xlsx")  # Replace with actual path

# Call the function
output_files <- process_and_save_data(file1, file2, my_dir)























input_file <- file.path(raw_dir, "0908-第3组-S1S2-前测-讨论-A_lzy.xlsx")
output_file <- file.path(my_dir, "0908-第3组-S1S2-前测-讨论-A_lzy.csv")




df_arou_r1_fill <- fill_annotate(input_file, output_file)



file_name <- file.path(raw_dir, "0908-第3组-S1S2-前测-讨论-A_lzy.xlsx")
df_arou_r1 <- import(file_name)
var_names <- c(
  "id", "date", "desc", "duration", "type", "source", "offset", "mediaDur",
  "fps", "subject", "behavior", "category", "behaviorType", "time",
  "mediaFile", "imgIdx", "imgPath", "comment"
)

colnames(df_arou_r1) <- var_names

df_arou_r1 <- df_arou_r1 %>%
  select(id, duration, behavior, subject, time, imgIdx)

max_img <- ceiling(30*df_arou_r1$duration[1])
df_max <- tibble(seq(0, max_img))
colnames(df_max) <- "imgIdx"

df_arou_r1_s1 <- df_arou_r1 %>%
  filter(subject == "S1")
df_arou_r1_s2 <- df_arou_r1 %>%
  filter(subject == "S2")

df_arou_r1_s1 <- df_max %>%
  left_join(df_arou_r1_s1, by = "imgIdx")

replace_na <- function(x) {
  non_na_values <- na.omit(x)  # 提取非NA值
  first_non_na <- non_na_values[1]  # 获取第一个非NA值
  last_non_na <- non_na_values[length(non_na_values)]  # 获取最后一个非NA值
  
  # 替换规则：
  # 1）开始部分的NA值，使用第一个非NA值
  x[is.na(x) & seq_along(x) <= which(!is.na(x))[1]] <- first_non_na
  # 2）末尾部分的NA值，使用最后一个非NA值
  x[is.na(x) & seq_along(x) > which(!is.na(x))[length(non_na_values)]] <- last_non_na
  # 3）中间部分的NA值，使用第一个非NA值
  for (i in 2:length(x)) {
    if (is.na(x[i]) && !is.na(x[i - 1])) {
      x[i] <- x[i - 1]
    }
  }
  return(x)
}

df_arou_r1_s1[] <- lapply(df_arou_r1_s1, replace_na)
df_arou_r1_s1 <- df_arou_r1_s1 %>%
  mutate(time = imgIdx / 30)
file_name <- file.path(my_dir, "0908-第3组-S1-前测-讨论-A_lzy.csv")
export(df_arou_r1_s1, file_name)
print("0908-第3组-S1-前测-讨论-A_lzy.csv saved.")

# 2 import data2 ----
file_name <- file.path(raw_dir, "0908-第3组-S1S2-前测-讨论-A_wny.xlsx")
df_arou_r2 <- import(file_name)
var_names <- c(
  "id", "date", "desc", "duration", "type", "source", "offset", "mediaDur",
  "fps", "subject", "behavior", "category", "behaviorType", "time",
  "mediaFile", "imgIdx", "imgPath", "comment"
)

colnames(df_arou_r2) <- var_names

df_arou_r2 <- df_arou_r2 %>%
  select(id, duration, behavior, subject, time, imgIdx)

max_img <- ceiling(30*df_arou_r2$duration[1])
df_max <- tibble(seq(0, max_img))
colnames(df_max) <- "imgIdx"

df_arou_r2_s1 <- df_arou_r2 %>%
  filter(subject == "S1")
df_arou_r2_s2 <- df_arou_r2 %>%
  filter(subject == "S2")

df_arou_r2_s1 <- df_max %>%
  left_join(df_arou_r2_s1, by = "imgIdx")

replace_na <- function(x) {
  non_na_values <- na.omit(x)  # 提取非NA值
  first_non_na <- non_na_values[1]  # 获取第一个非NA值
  last_non_na <- non_na_values[length(non_na_values)]  # 获取最后一个非NA值
  
  # 替换规则：
  # 1）开始部分的NA值，使用第一个非NA值
  x[is.na(x) & seq_along(x) <= which(!is.na(x))[1]] <- first_non_na
  # 2）末尾部分的NA值，使用最后一个非NA值
  x[is.na(x) & seq_along(x) > which(!is.na(x))[length(non_na_values)]] <- last_non_na
  # 3）中间部分的NA值，使用第一个非NA值
  for (i in 2:length(x)) {
    if (is.na(x[i]) && !is.na(x[i - 1])) {
      x[i] <- x[i - 1]
    }
  }
  return(x)
}

df_arou_r2_s1[] <- lapply(df_arou_r2_s1, replace_na)
df_arou_r2_s1 <- df_arou_r2_s1 %>%
  mutate(time = imgIdx / 30)
file_name <- file.path(my_dir, "0908-第3组-S1-前测-讨论-A_wny.csv")
export(df_arou_r2_s1, file_name)
print("0908-第3组-S1-前测-讨论-A_wny.csv saved.")

# 3 merge data and calculate kappa ----
df_arou_s1 <- df_arou_r1_s1 %>%
  left_join(df_arou_r2_s1, by = "imgIdx") 

kappa_result <- kappa2(df_arou_s1[, c("behavior.x", "behavior.y")])
print(kappa_result)

ggplot(df_arou_s1) +
  geom_point(aes(x = imgIdx, y = behavior.x), color = "blue") +   # behavior.x的散点图
  geom_point(aes(x = imgIdx, y = behavior.y), color = "red") +    # behavior.y的散点图
  labs(x = "Image Index (imgIdx)", y = "Behavior", title = "Scatter Plot of imgIdx vs Behavior") +
  theme_minimal()