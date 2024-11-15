# 定义函数：将Excel列名转换为列号 ----
col2num <- function(col_name) {
  # 将列名转换为大写（确保统一格式）
  col_name <- toupper(col_name)
  
  # 初始化列号
  col_num <- 0
  
  # 遍历列名的每个字母
  for (i in 1:nchar(col_name)) {
    # 获取当前字母的位置 (A=1, B=2, ..., Z=26)
    current_char_value <- utf8ToInt(substr(col_name, i, i)) - utf8ToInt("A") + 1
    # 计算列号（相当于进制转换，26进制）
    col_num <- col_num * 26 + current_char_value
  }
  
  return(col_num)
}