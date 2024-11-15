# 0. Info ----

# Project: annotated data process
# Author: LiuZiyu
# Created date: 2024/11
# Last edited date: 2024/11/12

process_and_save_data <- function(file1, file2, output_dir) {
  
  # Helper function to replace NA values
  replace_na <- function(x) {
    non_na_values <- na.omit(x)  # Extract non-NA values
    first_non_na <- non_na_values[1]  # First non-NA value
    last_non_na <- non_na_values[length(non_na_values)]  # Last non-NA value
    
    # Replace rules for NA values
    x[is.na(x) & seq_along(x) <= which(!is.na(x))[1]] <- first_non_na
    x[is.na(x) & seq_along(x) > which(!is.na(x))[length(non_na_values)]] <- last_non_na
    
    for (i in 2:length(x)) {
      if (is.na(x[i]) && !is.na(x[i - 1])) {
        x[i] <- x[i - 1]
      }
    }
    
    return(x)
  }
  
  # Helper function to process the data for each file
  process_file <- function(file_name) {
    df <- import(file_name)
    var_names <- c(
      "id", "date", "desc", "duration", "type", "source", "offset", "mediaDur",
      "fps", "subject", "behavior", "category", "behaviorType", "time",
      "mediaFile", "imgIdx", "imgPath", "comment"
    )
    colnames(df) <- var_names
    
    # Select necessary columns
    df <- df %>%
      select(id, duration, behavior, subject, imgIdx)
    
    # Generate imgIdx sequence
    max_img <- ceiling(30 * df$duration[1])
    df_max <- tibble(seq(0, max_img))
    colnames(df_max) <- "imgIdx"
    
    # Separate data by subject (S1, S2)
    df_s1 <- df %>%
      filter(subject == "S1")
    df_s2 <- df %>%
      filter(subject == "S2")
    
    # Merge with imgIdx sequence
    df_s1 <- df_max %>%
      left_join(df_s1, by = "imgIdx")
    
    # Replace NAs
    df_s1[] <- lapply(df_s1, replace_na)
    
    # Calculate time
    df_s1 <- df_s1 %>%
      mutate(time = imgIdx / 30)
    
    return(df_s1)
  }
  
  # Process both input files
  df_arou_r1_s1 <- process_file(file1)
  df_arou_r2_s1 <- process_file(file2)
  
  # Merge both datasets on imgIdx
  df_arou_s1 <- df_arou_r1_s1 %>%
    left_join(df_arou_r2_s1, by = "imgIdx")
  
  # Calculate Kappa value
  kappa_result <- kappa2(df_arou_s1[, c("behavior.x", "behavior.y")])
  print(kappa_result)
  
  # Save merged data to CSV
  merged_file_name <- file.path(output_dir, "merged_data.csv")
  export(df_arou_s1, file = merged_file_name, row.names = FALSE)
  print(paste("Merged data saved to", merged_file_name))
  
  # Save Kappa result to CSV
  kappa_file_name <- file.path(output_dir, "kappa_result.csv")
  kappa_df <- data.frame(Kappa_Value = kappa_result$value)
  export(kappa_df, file = kappa_file_name, row.names = FALSE)
  print(paste("Kappa result saved to", kappa_file_name))
  
  # Create and save scatter plot
  plot_file_name <- file.path(output_dir, "scatter_plot.jpg")
  p <- ggplot(df_arou_s1) +
    geom_point(aes(x = imgIdx, y = behavior.x), color = "blue") +  # behavior.x scatter
    geom_point(aes(x = imgIdx, y = behavior.y), color = "red") +   # behavior.y scatter
    labs(x = "Image Index (imgIdx)", y = "Behavior", title = "Scatter Plot of imgIdx vs Behavior") +
    theme_minimal()
  ggsave(plot_file_name, plot = p)
  
  print(paste("Plot saved to", plot_file_name))
  
  # Return file paths
  return(merged_file_name)
}

