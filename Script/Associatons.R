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
transactions@itemInfo$bkw_category <- Item_Cat[ ,3]
str(transactions)
by_category <- aggregate(transactions, by = "category")
by_bkw_category <- aggregate(transactions, by = "bkw_category")

transactions2 <- transactions
transactions2@itemInfo$labels <- paste(transactions2@itemInfo$category,
                                      transactions2@itemInfo$labels)

# Matrix and Data Frame from the "transactions": ----
transactions_df <- as.data.frame(as(transactions2,"matrix"))
transactions_df2 <- as.data.frame(as(transactions,"matrix"))

for (i in 1:ncol(transactions_df)) {
  transactions_df[ ,i] <- as.integer(transactions_df[ ,i])
}

for (i in 1:ncol(transactions_df2)) {
  transactions_df2[ ,i] <- as.integer(transactions_df2[ ,i])
}

x <- names(transactions_df)
for (i in 1:ncol(transactions_df)){
  for (j in 1:nrow(transactions_df)){
    if (transactions_df[j,i]==1){
      transactions_df[j,i] <- x[i]
  }
  }
}

# Classify the types of customers ----
v1 <- c()
v2 <- c()
v3 <- c()
v4 <- c()
v5 <- c()
client_type <- c()

for (i in 1:nrow(transactions_df)){
  v1[i] <- sum(grepl("Laptops", transactions_df[i, ]))
  v2[i] <- sum(grepl("Desktop", transactions_df[i, ]))
  v3[i] <- sum(grepl("Printer", transactions_df[i, ]))
  v4[i] <- sum(grepl("Monitor", transactions_df[i, ]))
  v5[i] <- sum(transactions_df2[i,1:125])
  if (sum(v1[i]+v2[i]) >=2 | sum(v3[i]) >=2 | sum(v4[i] + v1[i]) >=2 |
      v5[i] >=6) 
    {
    client_type[i] <- "Business"
  } else {
    client_type[i] <- "Retail"
  }
}

transactions_df2$client_type <- client_type
table(transactions_df2$client_type)

 # Spliting the datasets and transactions by type of customer ----

retail_df <- subset(transactions_df2[ ,1:125], client_type == "Retail")
business_df <- subset(transactions_df2[ ,1:125], client_type == "Business")
retail_mat <- as(retail_df, "matrix")
business_mat <- as(business_df, "matrix")

retail <- as(retail_mat, "transactions")
business <- as(business_mat, "transactions")

retail@itemInfo$category <- Item_Cat[ ,2]
retail@itemInfo$bkw_category <- Item_Cat[ ,3]
retail_by_cat <- aggregate(retail, by = "category")
retail_by_bkw_cat <- aggregate(retail, by = "bkw_category")

business@itemInfo$category <- Item_Cat[ ,2]
business@itemInfo$bkw_category <- Item_Cat[ ,3]
business_by_cat <- aggregate(business, by = "category")
business_by_bkw_cat <- aggregate(business, by = "bkw_category")

# Plotting -------------------------------------------------------
 # Overall plotting
barplot(sort(itemFrequency(transactions), decreasing=TRUE))
itemFrequencyPlot(transactions, topN = 10, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Top10 - Absolute Item Frequency Plot" )

image(sample(transactions,500))
image(sample(by_category,100))

itemFrequencyPlot(by_category, topN = 17, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Frequency plot by category")

itemFrequencyPlot(by_bkw_category, topN = 7, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Frequency plot by Blackwell's category")

 # Business Plotting
itemFrequencyPlot(business, topN = 10, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Top10 - Business Item Frequency Plot")

itemFrequencyPlot(business_by_cat, topN = 17, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Business Frequency plot by category")

itemFrequencyPlot(business_by_bkw_cat, topN = 7, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Business Frequency plot by Blackwell's category")
 # Retail Plotting
itemFrequencyPlot(retail, topN = 10, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Top10 - Retail Item Frequency Plot" )

itemFrequencyPlot(retail_by_cat, topN = 20, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Retail Frequency plot by category" )

itemFrequencyPlot(retail_by_bkw_cat, topN = 7, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Retail Frequency plot by Blackwell's category")

 # Rules -----------------------------------------------------------------
 # Business
business_rules <- apriori(business, 
                              parameter = list(supp = 0.0025, conf = 0.9, 
                                               minlen = 2)) #We use minlen to set the minimun size for the baskets

redundant_rules <- is.redundant(business_rules)
business_rules <- business_rules[!redundant_rules] # remove redundant rules.

ruleExplorer(business_rules) # Cool stuff
inspect(business_rules)

business_top_confidence <- sort(business_rules, decreasing = TRUE, 
                                na.last = NA, by = "confidence")

inspect(business_top_confidence[1:10])

business_cat_rules <- apriori(business_by_bkw_cat, # All the baskets that are more likely to buy an iMac
                      parameter = list(supp = 0.005, conf = 0.9, 
                                       minlen = 2))

business_top_confidence <- sort(business_cat_rules, decreasing = TRUE, 
                                na.last = NA, by = "lift")

inspect(business_cat_rules)

ruleExplorer(business_cat_rules)

# plot(subset(association_rules, items %in% "Acer Desktop"), method = "graph", control = list(type = "items"))

