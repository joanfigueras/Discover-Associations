#Settings ----------------
# Packages:
pacman::p_load(rstudioapi, readr, RColorBrewer, dplyr, car, GGally, shiny,
               arules, arulesViz, ggplot2)

# Load data ---------------------------
# Datasets:
current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)
Item_Cat <- read.csv("datasets/ItemsCategory.csv") # This data set allows us to aggregate the category for each of the items.

#Transactions
transactions <- 
  read.transactions(file = "Datasets/ElectronidexTransactions2017.csv",
                                 format = "basket",
                                 sep = ",",
                                 header = FALSE)

transactions@itemInfo$category <- Item_Cat[ ,2]
str(transactions)
by_category <- aggregate(transactions, by = "category")

transactions@itemInfo$labels <- paste(transactions@itemInfo$category,
                                      transactions@itemInfo$labels)

# Matrix and Data Frame from the "transactions":
transactions_mat <- as(transactions,"matrix")
transactions_df <- as.data.frame(transactions_mat)

for (i in 1:ncol(transactions_df)) {
  transactions_df[ ,i] <- as.integer(transactions_df[ ,i])
}

transactions_df$client_type <- #Here is where we need to create the new variable for the type of customers

# Plotting -------------------------------------------------------
barplot(sort(itemFrequency(transactions), decreasing=TRUE))
itemFrequencyPlot(transactions, topN = 10, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Top10 - Absolute Item Frequency Plot" )

image(sample(transactions,500))
image(sample(by_category,100))

itemFrequencyPlot(by_category, topN = 20, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Top10 - Absolute Item Frequency Plot" )

itemFrequencyPlot(subset(by_category = "Laptops"), topN = 20, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Top10 - Absolute Item Frequency Plot" )

 # Rules -----------------------------------------------------------------
association_rules <- apriori (transactions, 
                              parameter = list(supp = 0.0015, conf = 0.9, 
                                               minlen = 2)) #We use minlen to set the minimun size for the baskets


redundant_rules <- is.redundant(association_rules)
association_rules <- association_rules[!redundant_rules] # remove redundant rules.

ruleExplorer(association_rules) # Cool stuff
inspect(association_rules)

top_confidence <- sort(association_rules, decreasing = TRUE, na.last = NA,
                       by = "confidence")
inspect(top_confidence)
a <- c("Laptops", "Desktop")
category_rules <- apriori(by_category, # All the baskets that are more likely to buy an iMac
                      parameter = list(conf = 0.3), 
                      appearance = list(default = "rhs", lhs = a))

inspect(category_rules)

ruleExplorer(category_rules)

# subset(association_rules, items %in% "iMac")
# plot(subset(association_rules, items %in% "Acer Desktop"), method = "graph", control = list(type = "items"))

