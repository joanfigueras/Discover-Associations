#Settings ----------------
# Packages:
pacman::p_load(rstudioapi, readr, RColorBrewer, dplyr, car, GGally, shiny,
               arules, arulesViz, ggplot2, grid, gridExtra, lattice)

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
v6 <- c()
client_type <- c()

for (i in 1:nrow(transactions_df)){
  v1[i] <- sum(grepl("Laptops", transactions_df[i, ]))
  v2[i] <- sum(grepl("Desktop", transactions_df[i, ]))
  v3[i] <- sum(grepl("Printers", transactions_df[i, ]))
  v4[i] <- sum(grepl("Monitor", transactions_df[i, ]))
  v5[i] <- sum(transactions_df2[i,1:125])
  v6[i] <- sum(grepl("Gam", transactions_df[i, ]))
  if (v1[i]+v2[i] >=2 | v3[i] >=2 | v4[i] + v1[i] >=2 | v5[i] >=6) 
  {
    client_type[i] <- "Business"
  } else
    if (v6[i]>=2) {
      client_type[i] <- "Gamer" 
    } else {
      client_type[i] <- "Retail"
    }}

transactions_df2$client_type <- client_type
table(transactions_df2$client_type)

# Spliting the datasets and transactions by type of customer ----

retail_df <- subset(transactions_df2[ ,1:125], client_type == "Retail")
business_df <- subset(transactions_df2[ ,1:125], client_type == "Business")
gamer_df <- subset(transactions_df2[ ,1:125], client_type == "Gamer")
retail_mat <- as(retail_df, "matrix")
business_mat <- as(business_df, "matrix")
gamer_mat <- as(gamer_df, "matrix")

retail <- as(retail_mat, "transactions")
business <- as(business_mat, "transactions")
gamer <- as(gamer_mat, "transactions")

retail@itemInfo$category <- Item_Cat[ ,2]
retail@itemInfo$bkw_category <- Item_Cat[ ,3]
retail_by_cat <- aggregate(retail, by = "category")
retail_by_bkw_cat <- aggregate(retail, by = "bkw_category")

business@itemInfo$category <- Item_Cat[ ,2]
business@itemInfo$bkw_category <- Item_Cat[ ,3]
business_by_cat <- aggregate(business, by = "category")
business_by_bkw_cat <- aggregate(business, by = "bkw_category")

gamer@itemInfo$category <- Item_Cat[ ,2]
gamer@itemInfo$bkw_category <- Item_Cat[ ,3]
gamer_by_cat <- aggregate(retail, by = "category")
gamer_by_bkw_cat <- aggregate(retail, by = "bkw_category")

# Plotting -------------------------------------------------------
# Overall plotting
itemFrequencyPlot(transactions, topN = 10, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Top10 - Absolute Item Frequency Plot" )

itemFrequencyPlot(by_category, topN = 17, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Frequency plot by category")

# itemFrequencyPlot(by_bkw_category, topN = 7, type = "absolute", 
#                   col = brewer.pal(8, "Pastel2"),
#                   main = "Frequency plot by Blackwell's category")

## Business Plotting
# itemFrequencyPlot(business, topN = 10, type = "absolute", 
#                   col = brewer.pal(8, "Pastel2"),
#                   main = "Top10 - Business Item Frequency Plot")

itemFrequencyPlot(business_by_cat, topN = 17, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Business Frequency plot by category")

itemFrequencyPlot(business_by_bkw_cat, topN = 7, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Business Frequency plot by Blackwell's category")
## Retail Plotting
# itemFrequencyPlot(retail, topN = 10, type = "absolute", 
#                   col = brewer.pal(8, "Pastel2"),
#                   main = "Top10 - Retail Item Frequency Plot" )

itemFrequencyPlot(retail_by_cat, topN = 17, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Retail Frequency plot by category" )

itemFrequencyPlot(retail_by_bkw_cat, topN = 7, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Retail Frequency plot by Blackwell's category")

# Gaming Plotting
itemFrequencyPlot(gamer, topN = 10, type = "absolute",
                  col = brewer.pal(8, "Pastel2"),
                  main = "Top10 - Gaming Item Frequency Plot" )

itemFrequencyPlot(gamer_by_cat, topN = 17, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Gaming Frequency plot by category" )

itemFrequencyPlot(gamer_by_bkw_cat, topN = 7, type = "absolute", 
                  col = brewer.pal(8, "Pastel2"),
                  main = "Gaming Frequency plot by Blackwell's category")

# Rules -----------------------------------------------------------------

plot_supp_conf <- function(trans_obj,s1,s2,s3,s4) {
  
  supportLevels <- c(s1,s2,s3,s4)
  confidenceLevels <- c(0.9,0.8,0.7,0.6)
  
  rules_sup2 <- integer(length = 4) 
  rules_sup3 <- integer(length = 4) 
  rules_sup4 <- integer(length = 4) 
  rules_sup5 <- integer(length = 4) 
  
  #Apriori algorithm with support of 5%
  for (i in 1:length(confidenceLevels)) {
    rules_sup2[i] <- length(apriori(trans_obj, parameter = 
                                      list(sup = supportLevels[1], 
                                           conf = confidenceLevels[i], 
                                           target = "rules", minlen = 2)))
  }
  
  #Apriori algorithm with support of 1%
  for (i in 1:length(confidenceLevels)) {
    rules_sup3[i] <- length(apriori(trans_obj, parameter = 
                                      list(sup = supportLevels[2], 
                                           conf = confidenceLevels[i], 
                                           target = "rules", minlen = 2)))
  }
  
  #Apriori algorithm with support of 0.5%
  for (i in 1:length(confidenceLevels)) {
    rules_sup4[i] <- length(apriori(trans_obj, parameter = 
                                      list(sup = supportLevels[3], 
                                           conf = confidenceLevels[i], 
                                           target = "rules", minlen = 2)))
  }
  
  #Apriori algorithm with support of 0.1%
  for (i in 1:length(confidenceLevels)) {
    rules_sup5[i] <- length(apriori(trans_obj, parameter = 
                                      list(sup = supportLevels[4], 
                                           conf = confidenceLevels[i], 
                                           target = "rules",minlen = 2)))
  }
  
  #Plotting all confidence - support levels
  num_rules <- data.frame(rules_sup2,rules_sup3,rules_sup4,rules_sup5)
  
  plot_all <- ggplot(data = num_rules, aes(x = confidenceLevels)) + 
    geom_line(aes(y = rules_sup2, colour = paste("Supp Level of ", 
                                                 toString(s1)))) +
    geom_point(aes(y = rules_sup2, colour = paste("Supp Level of ", 
                                                  toString(s1)))) +
    geom_line(aes(y = rules_sup3, colour = paste("Supp Level of ", 
                                                 toString(s2)))) +
    geom_point(aes(y = rules_sup3, colour = paste("Supp Level of ", 
                                                  toString(s2)))) +
    geom_line(aes(y = rules_sup4, colour = paste("Supp Level of ", 
                                                 toString(s3)))) +
    geom_point(aes(y = rules_sup4, colour = paste("Supp Level of ", 
                                                  toString(s3)))) +
    geom_line(aes(y = rules_sup5, colour = paste("Supp Level of ",
                                                 toString(s4)))) +
    geom_point(aes(y = rules_sup5, colour = paste("Supp Level of ", 
                                                  toString(s4)))) +
    labs(x="Confidence levels", y="Number of rules found", 
         title="Apriori algorithm with different support levels") +
    theme_bw() + theme(legend.title=element_blank())
  
  return(plot_all)}

plot_supp_conf(transactions,0.001,0.002,0.003,0.004)

# Business
plot_supp_conf(business_by_bkw_cat, 0.03, 0.027, 0.025, 0.024)

business_cat_rules <- apriori(business_by_bkw_cat, # All the baskets that are more likely to buy an iMac
                              parameter = list(supp = 0.025, conf = 0.9, 
                                               minlen = 2))

redundant_rules <- is.redundant(business_cat_rules)
business_cat_rules <- business_cat_rules[!redundant_rules] # remove redundant rules.

business_top_confidence <- sort(business_cat_rules, decreasing = TRUE, 
                                na.last = NA, by = "confidence")

inspect(business_top_confidence[1:10])

ruleExplorer(business_top_confidence[1:10])

# Retail
plot_supp_conf(retail_by_bkw_cat, 0.01, 0.007, 0.005, 0.003)

retails_cat_rules <- apriori(retail_by_bkw_cat, # All the baskets that are more likely to buy an iMac
                             parameter = list(supp = 0.003, conf = 0.65, 
                                              minlen = 2))

redundant_rules <- is.redundant(retail_cat_rules)
retail_cat_rules <- retail_cat_rules[!redundant_rules] # remove redundant rules.

retail_top_confidence <- sort(retail_cat_rules, decreasing = TRUE, 
                              na.last = NA, by = "confidence")

inspect(retail_top_confidence)

ruleExplorer(retail_top_confidence)
# Gaming
plot_supp_conf(gamer_by_bkw_cat, 0.01, 0.007, 0.005, 0.003)

gamer_cat_rules <- apriori(gamer_by_bkw_cat, # All the baskets that are more likely to buy an iMac
                           parameter = list(supp = 0.003, conf = 0.65, 
                                            minlen = 2))

redundant_rules <- is.redundant(gamer_cat_rules)
gamer_cat_rules <- gamer_cat_rules[!redundant_rules] # remove redundant rules.

gamer_top_confidence <- sort(gamer_cat_rules, decreasing = TRUE, 
                             na.last = NA, by = "confidence")

inspect(gamer_top_confidence)

ruleExplorer(gamer_top_confidence)

# plot(subset(association_rules, items %in% "Acer Desktop"), method = "graph", control = list(type = "items"))

plot(retail_top_confidence, method = "graph",  engine = "htmlwidget")