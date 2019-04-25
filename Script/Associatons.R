###############################################################################

# Packages:
pacman::p_load(rstudioapi, readr, RColorBrewer, dplyr, car, GGally, 
               arules, arulesViz)

# Load data ---------------------------

# Datasets:
current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)

#Transactions
transactions <- read.transactions(file = "Datasets/ElectronidexTransactions2017.csv",
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

# Plotting
barplot(sort(itemFrequency(transactions), decreasing=TRUE))
itemFrequencyPlot(transactions, topN = 10, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Top10 - Absolute Item Frequency Plot" )

image(sample(transactions,500))
