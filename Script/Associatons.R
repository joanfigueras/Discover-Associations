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
                                 header = FALSE)

#Get to know dataset
# inspect(transactions[1:1000])
# length(transactions)
# mean(size(transactions))
# LIST(transactions)
# itemLabels(transactions)

#Convert transactions to dataframe
transactions_mat <- as(transactions,"matrix")
transactions_df <- as.data.frame(transactions_mat)


items <- c(transactions@itemInfo$labels)
freq <- c()
#Convert logical to integer (0 or 1)
for (i in 1:ncol(transactions_df)){
  transactions_df[ ,i] <- as.integer(transactions_df[ ,i])
  freq <- cbind(freq,sum(transactions_df[ ,i]))}




