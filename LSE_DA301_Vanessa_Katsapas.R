# 1. Import the necessary package

# Import the tidyverse package.
library('tidyverse')


# Import the data set.
turtle_sales <- read.csv("turtle_sales.csv", header=T)


## Explore the data set.
head(turtle_sales)

##########################################################################

# 2. View the data set

# Convert data frame to a tibble.
as_tibble(turtle_sales)


# Use the glimpse() function.
glimpse(turtle_sales)


# Use the summary() function.
summary(turtle_sales)


###############################################################################

# 3. Explore the data set

# Return a frequency table for the 'sex' column.
table(turtle_sales$Platform)


# Return a frequency table for the 'region' column.
table(turtle_sales$Genre)


# Remove the 'Ranking'  column.
turtle_sales2 <- select(turtle_sales, -Ranking)


# Check the new data frame.
head(turtle_sales2)

################################################################################
# 3. Prepare the Data

# View as a tibble.
as.tibble(turtle_sales2)

# View summary
summary(turtle_sales2)

# Add another column to calculate revenue from other Geographies
turtle_sales2$Other <- (turtle_sales2$Global_Sales - (turtle_sales2$NA_Sales + turtle_sales2$EU_Sales))

#-- Create a new datafram with EU & NA sales
# Step 1 of 3: Create a dataframe with for NA Sales
NA_only <- select(turtle_sales2, -EU_Sales, -Global_Sales, -Other)
NA_only$Market <- "NA"
# Rename column to Sales so that both dataframes have the same headers
NA_only <- rename(NA_only, Sales = NA_Sales)

# Step 2 of 1: Create a dataframe with for EU Sales
EU_only <- select(turtle_sales2, -NA_Sales, -Global_Sales, -Other)
EU_only$Market <- "EU"
# Rename column to Sales so that both dataframes have the same headers
EU_only <- rename(EU_only, Sales = EU_Sales)

# Step 3 of 1: Create a dataframe with for Other Sales
Other <- select(turtle_sales2, -NA_Sales, -Global_Sales, -EU_Sales)
Other$Market <- "Other"
# Rename column to Sales so that both dataframes have the same headers
Other <- rename(Other, Sales = Other)


# Merge EU and NA and Other Data Frames
Sales_Market <- rbind(EU_only,NA_only, Other)


# Check the new dataframe
table(Sales_Market$Market)
summarise(Sales_Market)


#################### Vis 1 Platform by Market ###################

# Create a dataframe to group Platform and Market and aggregate price
Turtle_Stack <-Sales_Market%>%
  group_by(Platform, Market)%>%
  summarise(Sum_grp = sum(Sales))

head(Turtle_Stack)

# Plot the Turtle_Stack DataFrame 
ggplot(Turtle_Stack, aes(x = reorder(Platform, -Sum_grp), y = Sum_grp, fill = Market))+
  geom_bar(stat="identity") +
  ggtitle("Plot1: Proportions of Sales by Market") +
  #theme(axis.text.x=element_text(angle=45, hjust=0.9))
  xlab("Platform") + ylab("Sales")



#################### Vis 2 Platform by Market ###################

# Plot the Turtle_Stack DataFrame % Barchart
ggplot(Turtle_Stack, aes(x = reorder(Platform, -Sum_grp), y = Sum_grp, fill = Market))+
  geom_bar(position="fill", stat="identity") +
  ggtitle("Plot 2: Proportions of Sales by Market") +
  #theme(axis.text.x=element_text(angle=45, hjust=0.9))
  xlab("Platform") + ylab("Sales")

#Summary: This shows the proportion of sales by Europe and North America and remaining regions by Platform
# The % of sales by platform

#################### Vis 3 One Bar ###################

Sales_Market_G <- Sales_Market

Sales_Market_G$Global_Sales <- "Global_Sales"

Turtle_Stack2 <- Sales_Market_G%>%
  group_by(Market, Global_Sales)%>%
  summarise(Sum_grp = sum(Sales))

ggplot(Turtle_Stack2, aes(Global_Sales, Sum_grp, group=Market,fill = Market))+
  geom_bar(stat="identity") +
  ggtitle("Plot 3: Total Sales by Region") +
  xlab(NULL) + ylab("Sales")

#Summary: This shows the proportion of sales by Europe and North America

#################### Vis 4 Scatterplot ###################

qplot(x=NA_Sales,EU_Sales, data=turtle_sales2,colour=Year)


#################### Vis 5a & 5b Scatterplot ###################

#----------------- Compare Sales by Platform EU and NA Sales Start ----------------- #
turtle_sales2 %>%
  mutate(class = fct_reorder(Platform, NA_Sales, .fun='length' )) %>%
  ggplot( aes(x=Platform, y=NA_Sales)) + 
  geom_boxplot() +
  ggtitle("Plot 5a: Sales Variance by Platform in NA") +
  xlab("Platform") +
  theme(legend.position="none") +
  xlab("") +
  xlab("")

turtle_sales2 %>%
  mutate(class = fct_reorder(Platform, EU_Sales, .fun='length' )) %>%
  ggplot( aes(x=Platform, y=EU_Sales)) + 
  geom_boxplot() +
  ggtitle("Plot 5b: Sales Variance by Platform in EU") +
  xlab("Platform") +
  theme(legend.position="none") +
  xlab("") +
  xlab("")

#----------------- Compare Sales by Platform EU and NA Sales End----------------- #

#################### Vis 6 Popularity by Platform ###################
# Analysis on sales

# The number of products per platform
turtle_sales2 %>%
  drop_na(Platform) %>%
  ggplot(aes(fct_infreq(Platform)))+ # Sort with -> fct_infreq
  geom_bar(fill = "#97B3C6")+
  #coord_flip()+
  theme_bw()+
  labs(x = "Platform",
       y = NULL,
       title= "Plot 6: Platform Popularity (Unique Games sold per platform)")

#################### Vis 7 Popularity by Platform ###################

turtle_sales2 %>%
  drop_na(Genre) %>%
  ggplot(aes(fct_infreq(Genre)))+ # Sort with -> fct_infreq
  geom_bar(fill = "#97B3C6")+
  #coord_flip()+
  theme_bw()+
  labs(x = "Platform",
       y = NULL,
       title= "Plot 7: Genre Popularity (Count)")


# The number of Games for the platform

############################################################################
# Could some games be sold for nothing


# WEEK 5
############################################################################
# 5.1 Sense Check the data

head(turtle_sales)
tail(turtle_sales)
View(turtle_sales)
dim(turtle_sales)
# 4.1 view column names
colnames(turtle_sales)
names(turtle_sales)
#4.2 view structure of the dataset
# These show column names, data types and dimensions
str(turtle_sales)
glimpse(turtle_sales)
as_tibble(turtle_sales)
#4.3 Check for missing data
# Check for missing values
sum(is.na(turtle_sales))
sum(is.na(turtle_sales$column))
# check for missing values in a dataframe
turtle_sales[is.na(turtle_sales)]

# Replace missing values with 0
turtle_sales[is.na(turtle_sales)] <- 0

# get rid of rows with missing values
na.omit(turtle_sales)


# check for missing values in a specific column
is.na(turtle_sales$column)
#4.4 Descriptive Statistics
# Should be able to tell from summary data if the data is normalised

# Show for each value if it is missing(True) or not (False)
is.na(turtle_sales)
# count of missing values in each column
colSums(is.na(turtle_sales))
# which column has missing values
which(colSums(is.na(turtle_sales))>0)
# get the name of the column with missing values
names(which(colSums(is.na(turtle_sales))>0))

summary(turtle_sales) # Character collumns only return length, class & mode
skim(turtle_sales) # Pro: Works well with grouped dataframes
# Creates a file that can be downloaded
# Contains basic statistics, data structure, univariate distribution, correlation analysis, and principal component analysis.
DataExplorer::create_report(turtle_sales) 




############################################################################
# 5.2 Explore the data

# keep only numeric columns
sales_only <- turtle_sales %>% 
  keep(is.numeric) %>%
  
table(sales_only)

# Drop unnecessary columns (e.g. column X).
sales_only_col <- subset(sales_only, select=-c(Ranking, Product, Year))

# View new dataframe
as_tibble(sales_only_col)

# Descriptive data for all the sales columns
summary(sales_only_col)



# Create a new dataframe in rows to visualise data easier
# Find the sum of the sales per product and by market

# convert Product from int to char
Sales_Market_id$Product <- as.character(Sales_Market_id$Product)
head(Sales_Market_id)

head(turtle_sales)
by_pid <- turtle_sales %>% 
  group_by(Product) %>% 
  summarise(
    EU_sum = sum (EU_Sales),
    NA_sum = sum (NA_Sales),
    Global_sum = (Global_Sales)
  )
head(by_pid)
dim(by_pid)
table(by_pid$Product)

# Remove duplicate rows
pid_unique <- aggregate(. ~ Product, data = by_pid, sum)
head(pid_unique)
dim(pid_unique)

#**


Perc <- pid_unique %>%
  mutate(
    EU_perc = round((EU_sum/sum(Global_sum))*100, 3),
    NA_perc = round((NA_sum/sum(Global_sum))*100, 3))
head(Perc)  

# Sort EU_perc and extract the Top products
Perc %>% arrange(desc(EU_perc))
Perc_EU <- Perc[1:10,]
# Check new dataframe
Perc_EU

# Visualise Perc
ggplot(Perc_EU, aes(x = reorder(Product, -EU_perc), y = EU_perc))+
  geom_bar(stat="identity") +
  ggtitle("Top 10 EU Products by proportion of Sales") +
  #theme(axis.text.x=element_text(angle=45, hjust=0.9))
  xlab("Product ID") + ylab("Proportion of Sales")

# Sort EU_perc and extract the Top products
Perc %>% arrange(desc(NA_perc))
Perc_NA <- Perc[1:10,]
# Check new dataframe
Perc_NA

# Visualise Perc
ggplot(Perc_NA, aes(x = reorder(Product, -NA_perc), y = NA_perc))+
  geom_bar(stat="identity") +
  ggtitle("Top 10 NA Products by proportion of Sales") +
  #theme(axis.text.x=element_text(angle=45, hjust=0.9))
  xlab("Product ID") + ylab("Proportion of Sales")



pid_unique$Product <- as.character(pid_unique$Product)
head(by_pid)



# SAVE
#-- Create a new datafram with EU & NA sales
# Step 1 of 3: Create a dataframe with for NA Sales
NA_pid <- select(pid_unique, -EU_sum, -Global_sum)
NA_pid$Market <- "NA"

# Rename column to Sales so that both dataframes have the same headers
NA_pid <- rename(NA_pid, Sales = NA_sum)
head(NA_pid)

# Step 2 of 1: Create a dataframe with for EU Sales
EU_pid <- select(pid_unique, -NA_sum, -Global_sum)
EU_pid$Market <- "EU"
head(EU_pid)
# Rename column to Sales so that both dataframes have the same headers
EU_pid <- rename(EU_pid, Sales = EU_sum)
head(EU_pid)
dim(EU_pid)


# Step 3 of 1: Create a dataframe with for Other Sales
Global_pid <- select(pid_unique, -NA_sum, -EU_sum)
Global_pid$Market <- "Global"
head(Global_pid)
# Rename column to Sales so that both dataframes have the same headers
Global_pid <- rename(Global_pid, Sales = Global_sum)
head(Global_pid)
dim(Global_pid)

# Merge EU and NA and Other Data Frames
Sales_Market_id <- rbind(NA_pid,EU_pid, Global_pid)



head(Sales_Market_id)
tail(Sales_Market_id)
dim(Sales_Market_id)
# Sort by Sales and Find Top 10 Global
Global_Top <- Global_pid %>% arrange(desc(Sales))
head(Global_Top)
Global_Top <- Global_Top[1:10,]
as_tibble(Global_Top)
 
dim(Global_Top)


# Sort by Sales and Find Top 10 NA
NA_Top <- NA_pid %>% arrange(desc(Sales))
head(NA_Top)
NA_Top <- NA_Top[1:10,]
as_tibble(NA_Top)



# Sort by Sales and Find Top 10 NA
EU_pid %>% arrange(desc(Sales)) %>%
  group_by(Product)
EU_Top <- EU_pid[1:10,]
as_tibble(EU_Top)

EU_Top <- EU_pid %>% arrange(desc(Sales))
head(EU_Top)
EU_Top <- EU_Top[1:10,]
as_tibble(EU_Top)


test <- lapply(EU_Top, sum)

# Merge Top 10 in all margets 

ggplot(Global_Top, aes(x=Product, y=Sales)) +
  # Specify the geom_boxplot function.
  geom_boxplot() 

All_Top = rbind(Global_Top,NA_Top, EU_Top)
dim(All_Top)
as_tibble(All_Top)


# --

# Create a dataframe to group Platform and Market and aggregate price
Turtle_StackP <-All_Top%>%
  group_by(Product, Market)%>%
  summarise(Sum_grp = sum(Sales))

head(Turtle_StackP)
dim(Turtle_StackP)

# Plot the Turtle_Stack DataFrame 
ggplot(Turtle_StackP, aes(x = reorder(Product, -Sum_grp), y = Sum_grp, fill = Market))+
  geom_bar(stat="identity") +
  ggtitle("Proportions of Produc Sales by Market") +
  #theme(axis.text.x=element_text(angle=45, hjust=0.9))
  xlab("Product ID") + ylab("Sales")

#--


# Visualise Top 10
ggplot(All_Top, aes(x=Product, y=Sales)) +
  # Specify the geom_boxplot function.
  ggtitle("Top 10 products by market") +
  xlab("Product_ID") + ylab("Sales")+
  geom_boxplot() 


# Visualise Top 10 --> Work with this
ggplot(All_Top, aes(x=Product,y=Sales)) + 
  
  ggtitle("Top 10 products by market") +
  #theme(axis.text.x=element_text(angle=45, hjust=0.9))
  xlab("Product_ID") + ylab("Sales")+
  geom_point(aes(size=Sales, colour=Market))


###############################################################################

# 5. Explore the data

                              
                                
# Determine the min, max, median, mode of the sales columns
min(sales_only$NA_Sales)
max(sales_only$NA_Sales)
max(sales_only$NA_Sales) - (sales_only$NA_Sales) #range
# Calculate Q1
quantile(sales_only$NA_Sales, 0.25)
# Calculate Q3
quantile(sales_only$NA_Sales, 0.75)
# Function to calculate IQR
IQR(sales_only$NA_Sales) #(interquartile range):
# Function to calculate variance
var(sales_only$NA_Sales)
# Function to return standard deviate
sd(sales_only$NA_Sales)



############################################################################
# 5.5 Visually Determine the Normality of the data

# ----------------
# Calculate mode to make it easier to evaluate normalisation from 
# summary() which does not include mode.

# Function to calculate the mode
# Create the function.
getmode <- function(sales_only_col) {
  uniqv <- unique(sales_only_col)
  uniqv[which.max(tabulate(match(sales_only_col, uniqv)))]
}
# Calculate the mode using the user function.
result <- getmode(sales_only_col$NA_Sales) 
print(paste0("Mode for NA Sales:  ", result))

# Calculate the mode using the user function.
result <- getmode(sales_only_col$EU_Sales)
print(paste0("Mode for EU Sales:  ", result))

# Calculate the mode using the user function.
result <- getmode(sales_only_col$Global_Sales) 
print(paste0("Mode for Global Sales:  ", result))  

# Plot to check normalisation                        
hist(turtle_sales$NA_Sales)
hist(turtle_sales$EU_Sales)
hist(turtle_sales$Global_Sales)
head(turtle_sales)

# Define normality with qqplots
# Specify qqnorm function (draw a qqplot).
qqnorm(turtle_sales$NA_Sales, main="NA Sales")
# Specify qqline function.
qqline(turtle_sales$NA_Sales) 
# Specify qqnorm function (draw a qqplot).
qqnorm(turtle_sales$EU_Sales, main="EU Sales")
# Specify qqline function.
qqline(turtle_sales$EU_Sales) 
# Specify qqnorm function (draw a qqplot).
qqnorm(turtle_sales$Global_Sales, main="Global Sales")
# Specify qqline function.
qqline(turtle_sales$Global_Sales) 

# Specify shapiro.test function (Shapiro-Wilk test).
shapiro.test(turtle_sales$NA_Sales)
shapiro.test(turtle_sales$EU_Sales)
shapiro.test(turtle_sales$Global_Sales)

# Check Skewness and Kurtosis
# Install the moments package and load the library.
install.packages('moments') 
library(moments)

# Specify the skewness and kurtosis functions.
skewness(turtle_sales$NA_Sales) 
kurtosis(turtle_sales$NA_Sales)

skewness(turtle_sales$EU_Sales) 
kurtosis(turtle_sales$EU_Sales)

skewness(turtle_sales$Global_Sales) 
kurtosis(turtle_sales$Global_Sales)

head(turtle_sales)
as_tibble(turtle_sales)

############################################################################
# 5.6 Create plots to determine insights into sales
turtle_sales$Product <- as.character(turtle_sales$Product)
head(sales_only_col)



as_tibble(sales_only)
sales_only$Product <- as.character(sales_only$Product)
as_tibble(sales_only)


ggplot(sales_only, aes(Product, NA_Sales)) + 
  # Note: this object’s geom layer function.
  geom_boxplot()

head(sales_only)


#Looked at proportions by platform
# Plot the Turtle_Stack DataFrame % Barchart
ggplot(Turtle_Stack, aes(x = reorder(Platform, -Sum_grp), y = Sum_grp, fill = Market))+
  geom_bar(position="fill", stat="identity") +
  ggtitle("Plot 2: Proportions of Sales by Market") +
  #theme(axis.text.x=element_text(angle=45, hjust=0.9))
  xlab("Platform") + ylab("Sales")

#Summary: This shows the proportion of sales by Europe and North America and remaining regions by Platform
# The % of sales by platform

############################################################################
# 5.7 Save Script


######################### Week 6

head(sales_only)
dim(sales_only)

# Use sales_only dataframe
plot(sales_only$NA_Sales, sales_only$EU_Sales)

# Transform the data set to limit errors.
# Improve linearity of data set and increase R^2.
SqrtEU_Sales<- sqrt(sales_only$EU_Sales)

# Visualise the result of transformed data.
plot(sales_only$NA_Sales, sales_only$EU_Sales)

# Compare 2 predictive models

# Test the relationship between EU and NA Sales

# Create a linear regression model.
model1 <- lm(sales_only$NA_Sales ~ sales_only$EU_Sales)

# View the summary stats.
summary(model1)

# Create a visualisation to determine normality of data set.
qqnorm(residuals(model1))
qqline(residuals(model1), col='red')

# Test the relationship between the transformed EU and BA

# Create a linear regression model.
model2 <- lm(sales_only$NA_Sales ~ sales_only$EU_Sales)

# View the summary stats.
summary(model2)


# Create a visualisation to determine normality of data set.
qqnorm(residuals(model2))
qqline(residuals(model2), col='blue')

###############################################################################

# 4. Compare the two models

# Arrange plot with the par(mfrow) function.
par(mfrow=c(2, 1))


# Compare both graphs (model1 and model2).
plot(sales_only$NA_Sales, sales_only$EU_Sales)
abline(coefficients(model1), col='red')

plot(sales_only$NA_Sales, sales_only$EU_Sales)
abline(coefficients(model2), col='blue') 



