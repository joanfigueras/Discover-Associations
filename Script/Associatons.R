###############################################################################

# Packages:
pacman::p_load(rstudioapi, readr, caret, corrplot, e1071, dplyr, car, GGally, arules, arulesViz)

# Load data ---------------------------

# Datasets:
current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)

#Transactions
transactions <- read.transactions(file = "datasets/ElectronidexTransactions2017.csv",
                                 format = "basket",
                                 sep = ",",
                                 header = FALSE,
                                 cols = 1)

#Get to know dataset
inspect(transactions[1:1000])
length(transactions)
mean(size(transactions))
LIST(transactions)
itemLabels(transactions)


