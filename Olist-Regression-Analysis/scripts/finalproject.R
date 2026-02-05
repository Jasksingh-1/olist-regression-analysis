                                          #######GROUP PROJECT 205#########

                 ###Dr. Fernanda Maciel California State University, Sacramento###


               #BY : JASKIRAT SINGH, VANSHIKA BHARDWAJ, JOSEPH GRANDE , SULEMAN AHMED
ol <- read.csv("olistf.csv") ###### reading data through read.csv function
                                        
####Exploratory Data Analysis (EDA) and Visualization##
### we have imported the datset and named as (ol)
ol <- na.omit(ol) #### so. here we have created a variable to clean the data as to remove the null values and empty rows before analysis


ol <- unique(ol)   #### to see the unique values , we can use the unique function
ol$product_category_name <- as.factor(ol$product_category_name) #  We are Converting categorical text columns to factors for modeling and analysis


  # This helps regression models treat them as categories, not as strings
ol$payment_type <- as.factor(ol$payment_type)


# Generated summary statistics here  for each variable in the dataset.
# This includes min, max, mean, median, and quartiles for numeric variables,
# and frequency counts for categorical variables.
# Useful for getting a quick overview of the data distribution and detecting anomalies.
summary(ol)


library(psych)# Loading the psych package to get more detailed stats like mean, SD, and skewness.
# It helps to understand the distribution of variables better using describe() and other functions.
describe(ol) #### here we have used describe function to check the mean median std etc...
ncol(ol)
dim(ol)
 

  #######IDENTIFICATION OF VARIABLES TYPES####
str(ol)  ##3by usng str function we can see the structure of the dataset
glimpse(ol)

sapply(ol, class) ### identification of datatype of each column here

hist(ol$price,
     main = "Price Distribution",
     xlab = "Price (R$)",   ### # Showing the distribution of price and payment value to understand how product and transaction amounts vary.
     col = "skyblue")
hist(ol$payment_value,
     main = "Payment Value Distribution",
     xlab = "Payment Value",
     col = "skyblue",
     border = "white")

######
#The bar chart illustrates how average delivery time varies across different product weight bins
#, divided into deciles. While there is some fluctuation across bins, we observe that products in
#the heaviest weight bin (bin 9) tend to have the longest average delivery times, exceeding 13 days
#. Lighter and medium-weight products generally have shorter or similar delivery durations, mostly around
#11 to 12 days. Interestingly, the trend is not perfectly linear, suggesting that factors other than weight
#(like shipping method or distance) might also influence delivery time. However, the slightly elevated 
#delivery duration for the heaviest products does support the operational expectation that heavier items 
#may require more time to transport and deliver.


ol$weight_group <- cut(ol$product_weight_g,
                       breaks = quantile(ol$product_weight_g, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                       labels = c("Light", "Medium", "Heavy"),   ## Grouped product weight into bins to analyze its effect on delivery time
                       include.lowest = TRUE)

install.packages("dplyr")  # Run only if not already installed
library(dplyr)
ol$weight_bin <- cut(ol$product_weight_g,
                     breaks = quantile(ol$product_weight_g, probs = seq(0, 1, 0.1), na.rm = TRUE),## Created weight categories and plotted average delivery time by weight bin
                     labels = 1:10,
                     include.lowest = TRUE)

avg_by_bin <- ol %>%
  group_by(weight_bin) %>%
  summarise(avg_delivery = mean(delivery_time_days, na.rm = TRUE))


barplot(avg_by_bin$avg_delivery,
        names.arg = as.character(avg_by_bin$weight_bin),
        col = c("skyblue", "dodgerblue", "steelblue", "cornflowerblue", "blue",
                "lightgreen", "darkgreen", "orange", "tomato", "red"),
        main = "Average Delivery Time by Product Weight Bin",
        xlab = "Product weight",             ##### using barplot function to visuualize the analysis
        ylab = "Average Delivery Time (Days)",
        las = 1)





 ##### here we are showing some inferences through the box plot
boxplot(ol$freight_value, main="Freight Value", col="blue")
boxplot(ol$price, main="price", col="red")
boxplot(ol$review_score, main="review_score", col="green")
boxplot(ol$delivery_time_days, main="delivery_time_days", col="orange")


###### payemnt trend####
# Convert to factor if it's not already
ol$payment_type <- as.factor(ol$payment_type)

# Create a numeric version of the payment_type column
ol$payment_type_num <- as.numeric(ol$payment_type)

# View result
table(ol$payment_type, ol$payment_type_num) ### use table to count the  each category in the form of table
levels(ol$payment_type)
table(ol$payment_type)

barplot(table(ol$payment_type_num),   ### here we have used barplot to show the trend of payment method
        names.arg = levels(ol$payment_type),  # #### we can clearly see through the bargraph that creditcard is the most used payment method
        col = "steelblue",
        main = "Payment Type Distribution",
        xlab = "Payment Type",
        ylab = "Count")




library(ggplot2)  ##### installing a library essential for the visualisation
############

boxplot(payment_value ~ payment_type, data = ol,
        main = "Payment Value by Payment Type",
        xlab = "Payment Type",
        ylab = "Payment Value",
        col = "skyblue")  #### here we are sowing that the payment through credit card shows the greatest value to the customers with very few outliers

avg_review_city <- ol %>%. ###### calculating the avaerage review score using inner join and  group function.
  count(customer_city, sort = TRUE) %>%
  top_n(10, n) %>%
  select(customer_city) %>%
  inner_join(ol, by = "customer_city") %>%
  group_by(customer_city) %>%
  summarise(avg_review = mean(review_score, na.rm = TRUE))

barplot(avg_review_city$avg_review,   ### using bar plot we are describing average review score by the city
        names.arg = avg_review_city$customer_city,
        col = "lightgreen",
        main = "Average Review Score by Top 10 Cities",
        xlab = "Customer City",
        ylab = "Average Review Score",
        las = 2,
        cex.names = 0.7)



city_delivery <- ol %>%
  group_by(customer_city) %>%
  summarise(avg_delivery = mean(delivery_time_days, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(avg_delivery))



top_cities <- head(city_delivery, 10)

barplot(top_cities$avg_delivery,
        names.arg = top_cities$customer_city,
        main = "Average Delivery Time by City",
        col = "tomato",
        ylab = "Avg Delivery Time (Days)",
        las = 2,
        cex.names = 0.6)


ol$actual_time <- as.numeric(difftime(ol$order_approved_at,
                                         ol$order_purchase_timestamp,
                                         units = "hours"))


sapply(ol[, c("order_purchase_timestamp", "order_approved_at")], class)


ol$actual_time <- as.numeric(difftime(ol$order_approved_at,
                                         ol$order_purchase_timestamp,
                                         units = "hours"))


cor(ol$delay_days, ol$review_score, use = "complete.obs") #### we are trying to fetch a correlation between the review score and how many days the order get delayed
### and its showing nearly no relationship as there are other factors which are driving a review score







#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


##### correlation between price fright value and delivery time



str(ol[, c("price", "delivery_time_days")]) ##using str function to get the structure of the two columns price and delivery_times




str(ol[, c("price", "delivery_time_days")])#### so we have applied some data transformation techniques such as we have changed the columns into date time format and created two additional variables called order_handlingtime and delay days
#Moreover  we have used omit and .na function to remove null value and rows
ol$price <- as.numeric(ol$price)
ol$delivery_time_days <- as.numeric(ol$delivery_time_days).  ### using as.numeric function to convert the format
ol$freight_value <- as.numeric(ol$freight_value)

cor(ol$price, ol$delivery_time_days, use = "complete.obs")
cor(ol$price, ol$freight_value, use = "complete.obs")



library(ggplot2)

ggplot(ol, aes(x = price, y = delivery_time_days)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(title = "Price vs Delivery Time",  #### price vs delivery timer
       
       x = "Product Price",
       y = "Delivery Time (Days)") +
  theme_minimal()


ggplot(ol, aes(x = price, y = freight_value)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  labs(title = "Price vs Freight Value",
       x = "Product Price",
       y = "Freight Value") +
  theme_minimal()



######## LINEAR REGRESSION#####


###we will be performing the linear regression####

Reg <- lm(freight_value ~ product_weight_g + 
            product_length_cm + 
            product_height_cm + 
            product_width_cm + 
            delivery_time_days + 
            product_photos_qty +
            price, data = ol)# ##### here we have used freight value as a response variable using some prdeictors 


summary(Reg) ######: For every 1 unit increase in price, the predicted freight increases by about 4.38 units, holding other variables constant.
##n the regression model, only freight_value had a statistically significant p-value (< 0.001), indicating a reliable relationship with price. Other predictors had p-values greater than 0.05, 
#suggesting that their effects may not be statistically meaningful and could be due to random variation.
###(p < 0.001)he p-value for freight_value is 6.62e-11 (very close to 0), which is far below the 0.001 threshold. That means freight cost is a statistically significant predictor of price.
####. that also means that there are other factors drivinbg the price of product might be the cost and 
#Our regression analysis reveals that freight value — not delivery speed or size — is the most important factor driving product pricing on ol. This suggests sellers might be adjusting prices to offset
# shipping costs. Weight has some influence, while size and delivery time have minimal to no impact.


### analysis for showing supply chain or logistics analysis.


library(broom) ### we have used broom library to clean up the models

tidy_model <- tidy(Reg)

ggplot(tidy_model, aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  coord_flip() +
  labs(title = "Regression Coefficients for Freight Value Prediction",
       x = "Predictor",
       y = "Coefficient Estimate")






####### using delay days as to predict how many days the order can be late is signifying that product dimesnions does not significantly causing order delay as there
#### is a management of orders, order approval window , geolocation and some other factors contributing in the order delay
ol <- na.omit(ol[, c("delay_days", ######### here we have used omit function to remove values
                           "product_weight_g", 
                           "product_length_cm", 
                           "product_height_cm", #####
                           "product_width_cm", 
                           "freight_value", 
                           "product_photos_qty", 
                           "price")])

Reg3 <- lm(delay_days ~ product_weight_g + 
             product_length_cm + 
             product_height_cm + 
             product_width_cm + 
             freight_value + 
             product_photos_qty + 
             price,
           data = ol)

summary(Reg3)
tidy_Reg3 <- tidy(Reg3) #### used tiddy function to get the clean and summarize data which is helpful in visualization


ggplot(tidy_Reg3, aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "orange") +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  coord_flip() +
  labs(title = "Regression Coefficients for Delay Days Prediction",
       x = "Predictor",
       y = "Coefficient Estimate")



#“Our regression models show that freight cost (‘freight_value’) is a key driver of both product pricing and delivery delays
#. In contrast, physical product features such as weight, size, or number of photos have minimal impact. This indicates
# a need for ol to reevaluate its pricing and shipping strategy—possibly adopting a system like Amazon that combines 
#delivery type (e.g., Prime) and fulfillment optimization, rather than simply basing freight cost on price.”


plot(Reg, which = 1)
plot(Reg, which = 2)

plot(Reg, which = 3)



library(car)
###### multicollinearity
vif(Reg).   #### we have used vif function to ascertain if there is any dependency between the

vif(Reg3)
names(ol)

#### here we have created a variable REG2 to make a equation with the variables to ascertain the payment value by the customers
Reg2 <- lm(log(payment_value) ~ price + freight_value + payment_installments +  #### to improve the homoscedasticity and assumptions we have used the log function to transform the data   for the payment value  
             product_weight_g + delivery_time_days, data = ol)





plot(Reg2, which = 1, main = "Residuals vs Fitted")     

#The Residuals vs. Fitted plot shows that most residuals are randomly scattered around zero, 
#indicating that the model satisfies the linearity assumption fairly well. The red smoothed line is mostly flat, 
#suggesting that the log transformation of payment_value improved linearity. However, there is some increased spread 
#among lower fitted values and a few noticeable outliers, which may affect model accuracy. Overall, the plot suggests that
# the assumptions of linear regression are reasonably met.
library(ggplot2)
library(tidyr). #### 
library(dplyr) 
delivery_data <- ol %>%
select(delivery_time_days, product_weight_g, product_length_cm, product_height_cm, product_width_cm) %>%
drop_na()

delivery_long <- pivot_longer(delivery_data,
                              cols = -delivery_time_days,
                              names_to = "Dimension",
                              values_to = "Value")


ggplot(delivery_long, aes(x = Value, y = delivery_time_days)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  facet_wrap(~ Dimension, scales = "free_x") +     ####.     here i wanna show that if the product dimensions have any impact on the delivery time
  labs(
    title = "Delivery Time vs. Product Dimensions",
    x = "Product Dimension Value",
    y = "Delivery Time (Days)"
  ) +
  theme_minimal()


#Visual Insight: Delivery Time vs Product Dimensions
#To explore the relationship between product specifications and delivery performance, we plotted scatterplots comparing delivery time 
#(in days) with each of the following product attributes: height, length, width, and weight. Each plot includes a fitted trend line
# to visualize the general direction of association.

#The plots reveal that although most products fall within a compact size and weight range, there is no strong linear 
#relationship between any single dimension and delivery time. The trend lines remain mostly flat, indicating that product height,
# length, width, and weight have minimal influence on how long a product takes to be delivered. This supports our regression findings, 
#suggesting that external logistics factors, rather than physical product features, are likely more responsible for variations in delivery 
#delays.


group_by(category) %>%
  summarise(avg_delay = mean(delay_days, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(avg_delay))

 ggplot(category_delay, aes(x = reorder(category, avg_delay), y = avg_delay)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Delivery Delay by Product Category",
       x = "Product Category",
       y = "Average Delay (Days)") +
  theme_minimal()




### as the analysis shows the weak relationship between it means the company offering discounts and delivery charge reliefs to the customers as there is no significant difference in the expensive and cheap products delivery cost
#The scatterplot of Product Price vs. Delivery Time shows that most products are priced below $1,000
#and delivered within 30 days, with a few extreme outliers. 




###############





#######LINEARITY.    
par(mfrow = c(1, 1))
plot(Reg)

##The linear regression model provides a statistically significant framework for predicting customer payment value,
#with payment_installments emerging as the most impactful factor. While other variables such as price, freight_value, and 
#delivery_time_days showed weaker individual effects, the overall model satisfies key assumptions of linearity, normality, and homoscedasticity. 
#Multicollinearity is not a concern, as all VIF values are within acceptable limits. The model can support business decisions related to payment behavior 
#and delivery trends, although further refinement may improve predictive power.




#####To better understand the relationships between the variables used in our regression model
#, we created a correlation heatmap. First, we selected the relevant numeric variables 
#(payment_value, price, freight_value, payment_installments, product_weight_g, and delivery_time_days) 
#and removed any missing values using the na.omit() function. We then calculated the correlation matrix 
#and visualized it using a color-coded heatmap with the corrplot function. This visual representation helps
#the strength and direction of linear relationships between variables. Positive correlations are shown in
#warmer colors, and negative correlations in cooler tones. We also displayed the correlation coefficients 
#ctly on the plot for easy interpretation. This step allows us to detect potential multicollinearity or strong linear
#associations that could influence the regression results.

par(mfrow = c(1,1))



ol$delay_days <- as.numeric(difftime(ol$order_delivered_customer_date,
                                     ol$order_estimated_delivery_date,
                                     units = "days"))

vars <- ol[, c("price", "freight_value", "payment_installments", 
               "delay_days", "product_weight_g", "delivery_time_days")]

vars <- na.omit(vars)
cor_matrix <- cor(vars)

library(corrplot)
corrplot(cor_matrix,
         method = "color",
         addCoef.col = "black",
         tl.col = "black",
         number.cex = 0.8,
         title = "Correlation Heatmap for Regression Variables",
         mar = c(0, 0, 2, 0))


     #######Thankyou#################




















