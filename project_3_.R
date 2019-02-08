install.packages("readxl")
install.packages("dummies")
install.packages("caTools")
install.packages("reshape")
library('dummies')
library("fastDummies")
library("readxl")
library('caTools')
library("reshape")
library('qcc')
function_grouping <- function(pivot_table) {
  pivot_table_rows <- dim(pivot_table)[1]
  pivot_table['parent'] <- pivot_table[1]
  for(i in 1:(pivot_table_rows-1)){
    for(j in (i+1):pivot_table_rows){
      if(abs(pivot_table[i,2] - pivot_table[j,2]) < 0.025){
        pivot_table[j, 'parent'] <- pivot_table[i, 'parent']
      } 
    }
  }
  return (pivot_table)
}
sub_values_replace <- function(original_df, parentTable, coloum) {
  rows_df = dim(original_df)[1]
  rows_parent = dim(parentTable)[1] 
  for(i in 1:rows_df){
    for(j in 1:rows_parent){
      if(parentTable[j, coloum] == original_df[i, coloum]){
        original_df[i, coloum] = parentTable[j, 'parent']
      }
    }
  }
  
  return (original_df)
}

data_ebay_auction <- read_excel("eBayAuctions.xls")
data_ebay_auctionMelt <- melt(data_ebay_auction, measure.vars = "Competitive?")
data_ebay_auctionCastCategory <- cast(data_ebay_auctionMelt, Category  ~ variable , mean)
data_ebay_auctionCastCurrency <- cast(data_ebay_auctionMelt, currency  ~ variable , mean)
data_ebay_auctionCastDuration <- cast(data_ebay_auctionMelt, Duration  ~ variable , mean)
data_ebay_auctionCastEndDay <- cast(data_ebay_auctionMelt, endDay  ~ variable , mean)
data_ebay_auctionCastCategoryPub= function_grouping(data_ebay_auctionCastCategory)
data_ebay_auctionCastCurrencyPub= function_grouping(data_ebay_auctionCastCurrency)
data_ebay_auctionCastDurationPub= function_grouping(data_ebay_auctionCastDuration)
data_ebay_auctionCastEndDayPub= function_grouping(data_ebay_auctionCastEndDay)
data_ebay_auction <- sub_values_replace(data_ebay_auction, data_ebay_auctionCastCategoryPub, 'Category')
data_ebay_auction <- sub_values_replace(data_ebay_auction, data_ebay_auctionCastCurrencyPub, 'currency')
data_ebay_auction <- sub_values_replace(data_ebay_auction, data_ebay_auctionCastDurationPub, 'Duration')
data_ebay_auction <- sub_values_replace(data_ebay_auction, data_ebay_auctionCastEndDayPub, 'endDay')
# Performance: as.factor > factor when input is a factor
# Performance: as.factor > factor when input is integer
# Unused levels or NA levels
data_ebay_auction$Category <- as.factor(data_ebay_auction$Category)
data_ebay_auction$currency <- as.factor(data_ebay_auction$currency)
data_ebay_auction$Duration <- as.factor(data_ebay_auction$Duration)
data_ebay_auction$endDay <- as.factor(data_ebay_auction$endDay)
data_ebay_auctionDummy <- dummy_cols(data_ebay_auction)
data_ebay_auctionDummy <- data_ebay_auctionDummy[ , -which(names(data_ebay_auctionDummy) %in% c("Category","currency", "Duration", "endDay"))]
set.seed(140)
data_ebay_auctionDummy$spl=sample.split(data_ebay_auctionDummy$OpenPrice, SplitRatio=0.6)
train=subset(data_ebay_auctionDummy, spl==TRUE) 
test=subset(data_ebay_auctionDummy, spl==FALSE)
fit.all = glm(`Competitive?` ~ ., data = train, family = "binomial")
fit.all$coefficients
summary(fit.all)
## Question 1 ##
high_pred_est <- max(abs(fit.all$coefficients),na.rm = TRUE)
# The value 2.26 corresponds to `Category_Health/Beauty`. Therefore, fitting the model manually using `Category_Health/Beauty` variable
single_fit = glm(`Competitive?` ~ `Category_Health/Beauty` + currency_GBP  , data = train, family = "binomial")
single_fit$coefficients
## Question 2 ##
four_fit = glm(`Competitive?` ~ `Category_Health/Beauty` + currency_GBP + `Category_Coins/Stamps` + Category_EverythingElse , data = train, family = "binomial")
four_fit$coefficients
summary(four_fit)
## Question 4 ##
reduced_fit = glm(`Competitive?` ~ ClosePrice + OpenPrice + currency_GBP + endDay_Mon + `Category_Health/Beauty` , data = train, family = "binomial")
reduced_fit$coefficients
summary(reduced_fit)
anova(fit.all,reduced_fit,test="Chisq")
## Question 5 ##
tempSize <- rep(length(train$`Competitive?`), length(train$`Competitive?`))
qcc.overdispersion.test(train$`Competitive?`, size=tempSize, type="binomial")
