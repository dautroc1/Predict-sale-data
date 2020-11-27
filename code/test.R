library(ggcorrplot)
library(ggplot2)
library(reshape2)

library(dplyr)
library(RColorBrewer)
library(forcats)
sale <- read.csv("summer-products-with-rating-and-performance_2020-08.csv")

#data cleaning
drop <- c('title','title_orig','currency_buyer','merchant_id','product_id', 'theme', 'crawl_month','merchant_name','merchant_info_subtitle','merchant_title','urgency_text','shipping_option_name')
sale <-  sale[,!(names(sale) %in% drop)]

#sale[, c('uses_ad_boosts', 'badge_local_product','badge_product_quality', 'badge_fast_shipping', 'has_urgency_banner')] <- as.factor(sale[, c('uses_ad_boosts', 'badge_local_product','badge_product_quality', 'badge_fast_shipping', 'has_urgency_banner')])

sale$merchant_profile_picture <- ifelse(sale$merchant_profile_picture == "", 0,1)
sale$product_url <- ifelse(sale$product_url == "", 0,1)
sale$product_picture <- ifelse(sale$product_picture == "", 0,1)

drop <- c('sale$merchant_profile_picture', 'sale$product_url', 'sale$product_picture')
sale <-  sale[,!(names(sale) %in% drop)]

#exploratory data
sale$origin_country <- as.character(sale$origin_country)
index <- sale$origin_country == ''
sale$origin_country[index] <- "Other"

a <- levels(fct_infreq(sale$origin_country))
g <- ggplot(data = sale) + geom_bar(aes(x=fct_infreq(origin_country), fill = fct_infreq(origin_country)),stat="count") + xlab('Country')
g <- g + scale_fill_brewer("OrRd", direction = -1)

index <- sale$origin_country %in% c('VE', 'SG', 'AT', 'GB')
sale$origin_country[index] <- "Other"
g <- ggplot(data = sale) + geom_bar(aes(x=fct_infreq(origin_country), fill = fct_infreq(origin_country)),stat="count") + xlab('Country')
g <- g + scale_fill_brewer("OrRd", direction = -1)

sale$product_variation_size_id <- as.character(sale$product_variation_size_id)
index <- sale$product_variation_size_id %in% c('33', '34', '2XL', '32/L','35','36','EU 35','EU39(US8)','L.','Round','Size-L','Size-L','SizeL')
sale$product_variation_size_id[index] <- "L"

index <- sale$product_variation_size_id %in% c('1m by 3m', '20pcs', '20PCS-10PAIRS', '25','25-S','26(Waist 72cm 28inch)','29','3 layered anklet','30 cm','B','Base Coat','daughter 24M','first  generation','H01','M.','One Size','Pack of 1','US 6.5 (EU 37)','US5.5-EU35','White','Women Size 36','Women Size 37','Size M')
sale$product_variation_size_id[index] <- "M"

index <- sale$product_variation_size_id %in% c('10pcs', '1pc', '2', '2pcs','4','4-5 Years','5','5PAIRS','AU plug Low quality','Baby Float Boat','Base & Top & Matte Top Coat','choose a size','Floating Chair for Kid','pants-S','s','S (waist58-62cm)','S Diameter 30cm','S Pink','S(bust 88cm)','S(Pink & Black)','S.','S..','S/M(child)','Size--S','Size-S','Size-S','Size-XS','Size XXS','SIZE XXS','Size/S','Suit-S','US-S','XS','XS.','XXS','XXXS','1','1 pc.','10 ml','17','Size-XXS','SIZE-XXS','Size -XXS','size S','Size S','Size S.','SIZE XS','','SIZE S','')
sale$product_variation_size_id[index] <- "S"

index <- sale$product_variation_size_id == ""
sale$product_variation_size_id[index] <- "S"

index <- sale$product_variation_size_id == "SIZE S"
sale$product_variation_size_id[index] <- "S"

index <- sale$product_variation_size_id %in% c('100 cm', '100 x 100cm(39.3 x 39.3inch)', '100pcs', '04-3XL','1 PC - XL','2XL','3XL','40 cm','4XL','5XL','60','6XL','80 X 200 CM','SIZE-4XL','Size-5XL','SIZE-4XL','Size-5XL','Size4XL','X   L','XXL','XXXL','XXXXL','XXXXXL')
sale$product_variation_size_id[index] <- "XL"

g <- ggplot(data = sale) + geom_bar(aes(x=fct_infreq(product_variation_size_id), fill = fct_infreq(product_variation_size_id)),stat="count") + xlab('Size')
g <- g + scale_fill_brewer("OrRd", direction = -1)


sale$rating_five_count[is.na(sale$rating_five_count)] <- 0
sale$rating_four_count[is.na(sale$rating_four_count)] <- 0
sale$rating_three_count[is.na(sale$rating_three_count)] <- 0
sale$rating_two_count[is.na(sale$rating_two_count)] <- 0
sale$rating_one_count[is.na(sale$rating_one_count)] <- 0
sale$has_urgency_banner[is.na(sale$has_urgency_banner)] <- 0


sale$product_color <- as.character(sale$product_color)

index <- sale$product_color == '' 
sale$product_color[index] <- "other"

index <- sale$product_color %in% c("applegreen",'apricot','army','army green','Army green','armygreen','camel','camel','camouflage','claret','darkgreen','floral','fluorescentgreen','ivory','jasper','light green','lightgreen','mintgreen')
sale$product_color[index] <- "green"

index <- sale$product_color %in% c('beige','brown & yellow','burgundy','coffee','khaki','lightkhaki')
sale$product_color[index] <- "brown"

index <- sale$product_color %in% c('Black','black & blue','black & green','black & stripe','black & white','black & yellow','blackwhite','coolblack','offblack')
sale$product_color[index] <- "black"

index <- sale$product_color %in% c('Blue','blue & pink','coralred','darkblue','denimblue','lakeblue','lightblue','navy','navy blue','navyblue','navyblue & white','prussianblue','skyblue')
sale$product_color[index] <- "blue"

index <- sale$product_color %in% c('dustypink','lightpink','Pink','pink & black','pink & blue','pink & blue','pink & grey','pink & white')
sale$product_color[index] <- "pink"

index <- sale$product_color %in% c('gold','leopard','leopardprint','lightyellow','rosegold')
sale$product_color[index] <- "yellow"
index <- sale$product_color %in% c('gray & white','greysnakeskinprint','lightgray','lightgrey')
sale$product_color[index] <- "gray"

index <- sale$product_color %in% c('lightpurple','violet')
sale$product_color[index] <- "purple"

index <- sale$product_color %in% c('lightred','RED','red & blue','rose','Rose red','rosered','watermelonred','wine','wine red','winered','winered & yellow')
sale$product_color[index] <- "red"

index <- sale$product_color %in% c('multicolor','nude','rainbow','silver','star','tan')
sale$product_color[index] <- "other"

index <- sale$product_color %in% c('offwhite','White','white & black','white & green','white & red','whitefloral','whitestripe')
sale$product_color[index] <- "white"

index <- sale$product_color %in% c('orange-red','orange & camouflage')
sale$product_color[index] <- "orange"

nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(8, "OrRd"))(nb.cols)
g <- ggplot(data = sale) + geom_bar(aes(x=fct_infreq(product_color), fill = fct_infreq(product_color)),stat="count") + xlab('Color')
g <- g + scale_fill_manual(values = rev(mycolors))


index <- sale$units_sold < 10
sale$units_sold[index] <- 10
sale$units_sold <- as.factor(sale$units_sold)
nb.cols <- 9
mycolors <- colorRampPalette(brewer.pal(8, "OrRd"))(nb.cols)
g <- ggplot(data = sale) + geom_bar(aes(x=fct_infreq(units_sold), fill = fct_infreq(units_sold)),stat="count") + xlab('Color') + labs(fill='Count')
g <- g + scale_fill_manual(values = rev(mycolors))

popular_tag <- read.csv('unique-categories.sorted-by-count.csv')
top50 <- as.character(head(popular_tag$keyword, 50))
top50 <- tolower(top50)
top50 <- as.list(top50)
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(8, "OrRd"))(nb.cols)
g <- ggplot(data = head(popular_tag, 10)) + geom_bar(aes(x=reorder(keyword, -count),y = count, fill = as.factor(count)),stat="identity") + xlab('Color') + labs(fill='Count')
g <- g + scale_fill_manual(values = mycolors)


replacetag <- function(tag, top50)
{
  tag <- as.character(tag)
  temp <- strsplit(tolower(tag), ",")
  inter <- intersect(temp[[1]], top50)
  return (length(inter) / length(top50))
}
sale$tag_count <- sapply(sale$tags, FUN = replacetag, top50)

#Model
drop <- c('tags','rating_count')
sale <-  sale[,!(names(sale) %in% drop)]



library(caret)
set.seed(2000)
trainindex <- createDataPartition(y = sale$units_sold,p = 0.70, list = FALSE)
train <- sale[trainindex,]
devtest <- sale[-trainindex,]
devindex <- createDataPartition(y = devtest$units_sold,p = 0.5, list = FALSE)
dev <- devtest[devindex,]
test <- devtest[-devindex,]
model <- readRDS("DecisionTree.rds")
model
model1 <- readRDS("SVC.rds")
model1
model2 <- readRDS("RandomForest.rds")
model2
model3 <- readRDS("gbm.rds")
model3
p <- predict(model, dev)
c <- confusionMatrix(p, dev$units_sold)
c
p <- predict(model2, dev)
c2 <- confusionMatrix(p, dev$units_sold)
c2
p <- predict(model3, dev)
c3 <- confusionMatrix(p, dev$units_sold)
c3
c <- c$overall["Accuracy"]
c1 <- c1$overall["Accuracy"]
c2 <- c2$overall["Accuracy"]
c3 <- c3$overall["Accuracy"]
x <- data.frame("Decision tree" = c, "SVC" = c1, "Random Forest"= c2, "GBM" = c3)
x
#tune

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
modelFit1 <- train(units_sold~.,method = customRF, data = train, brox = TRUE,tuneGrid=tunegrid)