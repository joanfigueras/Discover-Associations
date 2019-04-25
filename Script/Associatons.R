###############################################################################

# Packages:
pacman::p_load(rstudioapi, readr, caret, corrplot, e1071, dplyr, car, GGally, arules, arulesViz)

# Load data ---------------------------

# Datasets:
current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)

#Data Frame
transact_2017 <- read.csv("datasets/ElectronidexTransactions2017.csv",
                          header = FALSE)
#Transactions
transactions <- read.transactions(file = "Datasets/ElectronidexTransactions2017.csv",
                                 format = "basket",
                                 sep = ",",
                                 header = FALSE,
                                 cols = 1)




