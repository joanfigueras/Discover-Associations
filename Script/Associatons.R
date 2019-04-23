###############################################################################

# Packages:
pacman::p_load(rstudioapi, readr, caret, corrplot, e1071, dplyr, car, GGally)

# Load data ---------------------------

# Datasets:
current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)

transact_2017 <- read.csv("datasets/ElectronidexTransactions2017.csv",
                          header = FALSE)

