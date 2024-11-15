# 0. Info ----

# Project: annotated data process
# Author: LiuZiyu 
# Created date: 2024/11
# Last edited date: 2024/11/12

# This script is for
#   1) import annotated valence and arousal data,
#   2) fill in v and a data,
#   3) calculate rater consistency 

library(tidyverse)
library(rio)
library(paletteer)
library(skimr)
library(zoo)
library(irr)

# define working directories ----
root_dir <- "D:/# Library/0 Academic/2_Programs/2_fNIRS/process_a_emotion"

raw_dir <- file.path(root_dir, "0_rawdata")
my_dir <- file.path(root_dir, "1_mydata")
des_dir <- file.path(root_dir, "2_description")

func_dir <- file.path(root_dir, "9_myscript", "function")

# load functions ---- 
files <- list.files(func_dir, pattern = "\\.R$", full.names = TRUE)
lapply(files, source)

# define paletteers ----
scale_color <- scale_color_paletteer_d("ggprism::black_and_white")
scale_fill <- scale_fill_paletteer_d("ggprism::black_and_white")