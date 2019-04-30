#Settings ----------------
# Packages:
pacman::p_load(rstudioapi, readr, RColorBrewer, dplyr, car, GGally, shiny,
               arules, arulesViz, ggplot2,grid,gridExtra,lattice)

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

transactions2 <- transactions
transactions2@itemInfo$labels <- paste(transactions2@itemInfo$category,
                                      transactions2@itemInfo$labels)

# Matrix and Data Frame from the "transactions": ----
transactions_mat <- as(transactions2,"matrix")
transactions_df <- as.data.frame(transactions_mat)
transactions_mat2 <- as(transactions,"matrix")
transactions_df2 <- as.data.frame(transactions_mat2)

for (i in 1:ncol(transactions_df)) {
  transactions_df[ ,i] <- as.integer(transactions_df[ ,i])
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
  v3[i] <- sum(grepl("Printer", transactions_df[i, ]))
  v4[i] <- sum(grepl("Monitor", transactions_df[i, ]))
  v5[i] <- sum(transactions_df2[i,1:125])
  if (sum(v1[i]+v2[i]) >=2 | sum(v3[i]) >=2 | sum(v4[i] + v1[i]) >=2 | v5[i] >=6) {
    client_type[i] <- "Business"
        } else {
          client_type[i] <- "Retail"
        }
      }


for (i in 1:nrow(transactions_df)){
    v1[i] <- sum(grepl("Laptops", transactions_df[i, ]))
    v2[i] <- sum(grepl("Desktop", transactions_df[i, ]))
    v3[i] <- sum(grepl("Printer", transactions_df[i, ]))
    v4[i] <- sum(grepl("Monitor", transactions_df[i, ]))
    v5[i] <- sum(transactions_df2[i,1:125])
    if (sum(v1[i]+v2[i]) >=2 ) {
      client_type[i] <- "Business"
    } else {
        if (sum(v3[i]) >=2) {
        client_type[i] <- "Business"
        } else {
          if (sum(v4[i] + v2[i]) >=2) {
            client_type[i] <- "Business"
          } else {
            if (v5[i] >=5) {
              client_type[i] <- "Business"
            } else {
                client_type[i] <- "Retail"
         }
    }}}}

for (i in 1:ncol(transactions_df2)) {
  transactions_df2[ ,i] <- as.integer(transactions_df2[ ,i])
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
retail_by_cat <- aggregate(retail, by = "category")

business@itemInfo$category <- Item_Cat[ ,2]
business_by_cat <- aggregate(business, by = "category")

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

plot_supp_conf <- function(trans,s1,s2,s3,s4) {
  
  supportLevels <- c(s1,s2,s3,s4)
  confidenceLevels <- c(0.9,0.8,0.7,0.6)
  
  rules_sup2 <- integer(length = 4) 
  rules_sup3 <- integer(length = 4) 
  rules_sup4 <- integer(length = 4) 
  rules_sup5 <- integer(length = 4) 
  
  #Apriori algorithm with support of 5%####
  for (i in 1:length(confidenceLevels)) {
    rules_sup2[i] <- length(apriori(trans, parameter = list(sup = supportLevels[1],
                                                                   conf=confidenceLevels[i],
                                                                   target = "rules",
                                                                   minlen = 2)))
  }
  
  #Apriori algorithm with support of 1%####
  for (i in 1:length(confidenceLevels)) {
    rules_sup3[i] <- length(apriori(trans, parameter = list(sup = supportLevels[2],
                                                                   conf=confidenceLevels[i],
                                                                   target = "rules",
                                                                   minlen = 2)))
  }
  
  #Apriori algorithm with support of 0.5%####
  for (i in 1:length(confidenceLevels)) {
    rules_sup4[i] <- length(apriori(trans, parameter = list(sup = supportLevels[3],
                                                                   conf=confidenceLevels[i],
                                                                   target = "rules",
                                                                   minlen = 2)))
  }
  
  #Apriori algorithm with support of 0.1%####
  for (i in 1:length(confidenceLevels)) {
    rules_sup5[i] <- length(apriori(trans, parameter = list(sup = supportLevels[4],
                                                                   conf=confidenceLevels[i],
                                                                   target = "rules",
                                                                   minlen = 2)))
  }
  
  
  #Plotting all confidence - support levels####
  num_rules <- data.frame(rules_sup2,rules_sup3,rules_sup4,rules_sup5)
  
  plot_all <- ggplot(data = num_rules, aes(x = confidenceLevels)) + 
    geom_line(aes(y = rules_sup2, colour = paste("Supp Level of ",toString(s1)))) +
    geom_point(aes(y = rules_sup2, colour = paste("Supp Level of ",toString(s1)))) +
    geom_line(aes(y = rules_sup3, colour = paste("Supp Level of ",toString(s2)))) +
    geom_point(aes(y = rules_sup3, colour = paste("Supp Level of ",toString(s2)))) +
    geom_line(aes(y = rules_sup4, colour = paste("Supp Level of ",toString(s3)))) +
    geom_point(aes(y = rules_sup4, colour = paste("Supp Level of ",toString(s3)))) +
    geom_line(aes(y = rules_sup5, colour = paste("Supp Level of ",toString(s4)))) +
    geom_point(aes(y = rules_sup5, colour = paste("Supp Level of ",toString(s4)))) +
    labs(x="Confidence levels", y="Number of rules found", 
         title="Apriori algorithm with different support levels") +
    theme_bw() +
    theme(legend.title=element_blank())


return(plot_all)
}

plot_supp_conf(transactions,0.001,0.002,0.003,0.004)

#----------------------------------------------------------------------------------------------
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

