# Importing the data


train_data <- read.table('dengue_features_train.csv', sep = ',', header = TRUE)

# adding id column
id <- rownames(train_data)
train_data <- cbind(id=id, train_data)

# Checking the missing values in columns
miss_data <- sapply(train_data, function(train_data) sum(is.na(train_data)))
miss_data

# downloading the csv with id
write.csv(train_data, "ID_Train.csv")

# Importing data without city, year, weekofyear and week_start_date columns
imputation<- read.table('ID_Train.csv', sep = ',', header = TRUE)
#importing data just with city, year, weekofyear and week_start_date_columns
year_week <- read.table('city_year_week.csv', sep = ',', header = TRUE)

# Checking the structure
summary(train_data)
str(train_data)


# installing amelia package
install.packages('Amelia')
install.packages('Rcpp')
library(Amelia)

#running Amelia imputation
amelia(imputation)
imp_out <- amelia(imputation)
str(imp_out$imputations)
# saving the imputated dataframes
write.amelia(obj=imp_out, file.stem = "outdata", separate = FALSE)

#loading the imputated dataframes
outdata1 <- read.table('outdata1.csv', sep = ',', header = TRUE)
outdata1 <- outdata1 [-c(1)]

outdata2 <- read.table('outdata2.csv', sep = ',', header = TRUE)
outdata2 <- outdata2 [-c(1)]

outdata3 <- read.table('outdata3.csv', sep = ',', header = TRUE)
outdata3 <- outdata3 [-c(1)]

outdata4 <- read.table('outdata4.csv', sep = ',', header = TRUE)
outdata4 <- outdata4 [-c(1)]

outdata5 <- read.table('outdata5.csv', sep = ',', header = TRUE)
outdata5 <- outdata5 [-c(1)]

train_data1 <- merge.data.frame(year_week, outdata1, by= 'id')
train_data2 <- merge.data.frame(year_week, outdata2, by= 'id')
train_data3 <- merge.data.frame(year_week, outdata3, by= 'id')
train_data4 <- merge.data.frame(year_week, outdata4, by= 'id')
train_data5 <- merge.data.frame(year_week, outdata5, by= 'id')


labels_train <- read.table('dengue_labels_train.csv', sep = ',', header = TRUE)

# Data for first imputation
train_labeled_data1 <- merge(train_data1,labels_train, by = c("city","year","weekofyear"))
miss_data1 <- sapply(train_labeled_data1, function(train_labeled_data1) sum(is.na(train_labeled_data1)))
str(train_labeled_data1)

train_labeled_data1$city <- as.factor(train_labeled_data$city)
train_labeled_data1$week_start_date <- as.ordered(train_labeled_data$week_start_date)
train_labeled_data1$year <- as.ordered(train_labeled_data$year)
train_labeled_data1$weekofyear <- as.ordered(train_labeled_data$weekofyear)

# Data for 2nd imputation
train_labeled_data2 <- merge(train_data2,labels_train, by = c("city","year","weekofyear"))
miss_data2 <- sapply(train_labeled_data2, function(train_labeled_data2) sum(is.na(train_labeled_data2)))
str(train_labeled_data2)

train_labeled_data2$city <- as.factor(train_labeled_data2$city)
train_labeled_data2$week_start_date <- as.ordered(train_labeled_data2$week_start_date)
train_labeled_data2$year <- as.ordered(train_labeled_data2$year)
train_labeled_data2$weekofyear <- as.ordered(train_labeled_data2$weekofyear)

#Data for 3rd imputation
train_labeled_data3 <- merge(train_data3,labels_train, by = c("city","year","weekofyear"))
miss_data3 <- sapply(train_labeled_data3, function(train_labeled_data3) sum(is.na(train_labeled_data3)))
str(train_labeled_data3)

train_labeled_data3$city <- as.factor(train_labeled_data3$city)
train_labeled_data3$week_start_date <- as.ordered(train_labeled_data3$week_start_date)
train_labeled_data3$year <- as.ordered(train_labeled_data3$year)
train_labeled_data3$weekofyear <- as.ordered(train_labeled_data3$weekofyear)

#Data for 4th imputation
train_labeled_data4 <- merge(train_data4,labels_train, by = c("city","year","weekofyear"))
miss_data4 <- sapply(train_labeled_data4, function(train_labeled_data4) sum(is.na(train_labeled_data4)))
str(train_labeled_data4)

train_labeled_data4$city <- as.factor(train_labeled_data4$city)
train_labeled_data4$week_start_date <- as.ordered(train_labeled_data4$week_start_date)
train_labeled_data4$year <- as.ordered(train_labeled_data4$year)
train_labeled_data4$weekofyear <- as.ordered(train_labeled_data4$weekofyear)

#Data for 5th imputation
train_labeled_data5 <- merge(train_data5,labels_train, by = c("city","year","weekofyear"))
miss_data5 <- sapply(train_labeled_data5, function(train_labeled_data5) sum(is.na(train_labeled_data5)))
str(train_labeled_data5)

train_labeled_data5$city <- as.factor(train_labeled_data5$city)
train_labeled_data5$week_start_date <- as.ordered(train_labeled_data5$week_start_date)
train_labeled_data5$year <- as.ordered(train_labeled_data5$year)
train_labeled_data5$weekofyear <- as.ordered(train_labeled_data5$weekofyear)