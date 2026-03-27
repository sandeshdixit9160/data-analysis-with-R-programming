# Practical 2 : Data Structures and Control Structures in R

# Data Frame Setup
set.seed(123)

sales_data <- data.frame(
  OrderID = 1:100,
  CustomerName = sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),100,replace=TRUE),
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),100,replace=TRUE),
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),100,replace=TRUE),
  Category = sample(c("Electronics","Fashion"),100,replace=TRUE),
  Quantity = sample(1:10,100,replace=TRUE),
  Price = sample(seq(500,50000,500),100,replace=TRUE),
  Discount = sample(c(0,5,10,15,20),100,replace=TRUE),
  PaymentMethod = sample(c("Cash","Card","UPI"),100,replace=TRUE)
)

sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount / 100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount

# ------------------------------------------------------------------

# Q1) Filtering Rows

# 1. Show orders where Price > 20000.
subset(sales_data, Price > 20000)

# 2. Show orders where City = Pune.
subset(sales_data, City == "Pune")

# 3. Show orders where Quantity > 5.
subset(sales_data, Quantity > 5)

# 4. Show orders where Category = Electronics.
subset(sales_data, Category == "Electronics")

# 5. Combine two conditions (e.g., Electronics in Pune).
subset(sales_data, Category == "Electronics" & City == "Pune")


# Q2) Sorting Data

# 1. Sort dataset by Price (Ascending).
sales_data[order(sales_data$Price), ]

# 2. Sort dataset by Revenue (Ascending).
sales_data[order(sales_data$Revenue), ]

# 3. Sort by City alphabetically.
sales_data[order(sales_data$City), ]

# 4. Sort by Quantity descending.
sales_data[order(-sales_data$Quantity), ]

# 5. Sort by multiple columns (City then Revenue descending).
sales_data[order(sales_data$City, -sales_data$Revenue), ]


# Q3) Aggregation

# 1. Total revenue by city.
aggregate(Revenue ~ City, data = sales_data, sum)

# 2. Average price by category.
aggregate(Price ~ Category, data = sales_data, mean)

# 3. Total quantity sold by product.
aggregate(Quantity ~ Product, data = sales_data, sum)

# 4. Maximum order value by city.
aggregate(FinalAmount ~ City, data = sales_data, max)

# 5. Minimum order value by product.
aggregate(FinalAmount ~ Product, data = sales_data, min)


# Q4) Conditional Statements

# 1. If discount > 10 mark "High Discount".
# Initializing the Data Frame
set.seed(123)

sales_data <- data.frame(
  OrderID = 1:100,
  CustomerName = sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),100,replace=TRUE),
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),100,replace=TRUE),
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),100,replace=TRUE),
  Category = sample(c("Electronics","Fashion"),100,replace=TRUE),
  Quantity = sample(1:10,100,replace=TRUE),
  Price = sample(seq(500,50000,500),100,replace=TRUE),
  Discount = sample(c(0,5,10,15,20),100,replace=TRUE),
  PaymentMethod = sample(c("Cash","Card","UPI"),100,replace=TRUE)
)

sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount / 100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount

# ------------------------------------------------------------------

# Q1) Filtering Rows

# 1. Show orders where Price > 20000.
subset(sales_data, Price > 20000)

# 2. Show orders where City = Pune.
subset(sales_data, City == "Pune")

# 3. Show orders where Quantity > 5.
subset(sales_data, Quantity > 5)

# 4. Show orders where Category = Electronics.
subset(sales_data, Category == "Electronics")

# 5. Combine two conditions (e.g., Electronics in Pune).
subset(sales_data, Category == "Electronics" & City == "Pune")


# Q2) Sorting Data

# 1. Sort dataset by Price (Ascending).
sales_data[order(sales_data$Price), ]

# 2. Sort dataset by Revenue (Ascending).
sales_data[order(sales_data$Revenue), ]

# 3. Sort by City alphabetically.
sales_data[order(sales_data$City), ]

# 4. Sort by Quantity descending.
sales_data[order(-sales_data$Quantity), ]

# 5. Sort by multiple columns (City then Revenue descending).
sales_data[order(sales_data$City, -sales_data$Revenue), ]


# Q3) Aggregation

# 1. Total revenue by city.
aggregate(Revenue ~ City, data = sales_data, sum)

# 2. Average price by category.
aggregate(Price ~ Category, data = sales_data, mean)

# 3. Total quantity sold by product.
aggregate(Quantity ~ Product, data = sales_data, sum)

# 4. Maximum order value by city.
aggregate(FinalAmount ~ City, data = sales_data, max)

# 5. Minimum order value by product.
aggregate(FinalAmount ~ Product, data = sales_data, min)


# Q4) Conditional Statements

# 1. If discount > 10 mark "High Discount".
sales_data$DiscountTag <- ifelse(sales_data$Discount > 10, "High Discount", "Low/No Discount")

# 2. Classify orders into High / Medium / Low value by using if else.
sales_data$OrderValueClass <- ifelse(sales_data$FinalAmount > 30000, "High",
                                     ifelse(sales_data$FinalAmount > 10000, "Medium", "Low"))

# 3. Print revenue of each order by using for loop.
for(i in 1:nrow(sales_data)) {
  print(paste("Order ID:", sales_data$OrderID[i], "- Revenue:", sales_data$Revenue[i]))
}

# 4. Calculate cumulative revenue by using while loop.
total_cum_rev <- 0
i <- 1
while(i <= nrow(sales_data)) {
  total_cum_rev <- total_cum_rev + sales_data$Revenue[i]
  i <- i + 1
}
print(total_cum_rev)

# 5. Skip fashion category (using next in loop).
for(i in 1:nrow(sales_data)) {
  if(sales_data$Category[i] == "Fashion") {
    next
  }
  print(sales_data[i, ])
}


# Q5) Discount Analysis

# 1. Find Average discount.
mean(sales_data$Discount)

# 2. Orders with discount > 10%.
subset(sales_data, Discount > 10)

# 3. Total discount amount.
sum(sales_data$DiscountAmount)

# 4. City with highest total discount.
discount_by_city <- aggregate(DiscountAmount ~ City, data = sales_data, sum)
discount_by_city[which.max(discount_by_city$DiscountAmount), ]

# 5. Count discount levels.
table(sales_data$Discount)


# Q6) Multi Condition Filtering

# 1. Electronics + Price > 20000.
subset(sales_data, Category == "Electronics" & Price > 20000)

# 2. Fashion + Quantity > 5.
subset(sales_data, Category == "Fashion" & Quantity > 5)

# 3. Pune + Discount > 10.
subset(sales_data, City == "Pune" & Discount > 10)

# 4. Revenue > 50000 + Cash payment.
subset(sales_data, Revenue > 50000 & PaymentMethod == "Cash")

# 5. High price + high quantity (e.g., Price > 25000 and Qty > 5).
subset(sales_data, Price > 25000 & Quantity > 5)

# 2. Classify orders into High / Medium / Low value by using if else.
sales_data$OrderValueClass <- ifelse(sales_data$FinalAmount > 30000, "High",
                                     ifelse(sales_data$FinalAmount > 10000, "Medium", "Low"))

# 3. Print revenue of each order by using for loop.
for(i in 1:nrow(sales_data)) {
  print(paste("Order ID:", sales_data$OrderID[i], "- Revenue:", sales_data$Revenue[i]))
}

# 4. Calculate cumulative revenue by using while loop.
total_cum_rev <- 0
i <- 1
while(i <= nrow(sales_data)) {
  total_cum_rev <- total_cum_rev + sales_data$Revenue[i]
  i <- i + 1
}
print(total_cum_rev)

# 5. Skip fashion category (using next in loop).
for(i in 1:nrow(sales_data)) {
  if(sales_data$Category[i] == "Fashion") {
    next
  }
  print(sales_data[i, ])
}


# Q5) Discount Analysis

# 1. Find Average discount.
mean(sales_data$Discount)

# 2. Orders with discount > 10%.
subset(sales_data, Discount > 10)

# 3. Total discount amount.
sum(sales_data$DiscountAmount)

# 4. City with highest total discount.
discount_by_city <- aggregate(DiscountAmount ~ City, data = sales_data, sum)
discount_by_city[which.max(discount_by_city$DiscountAmount), ]

# 5. Count discount levels.
table(sales_data$Discount)


# Q6) Multi Condition Filtering

# 1. Electronics + Price > 20000.
subset(sales_data, Category == "Electronics" & Price > 20000)

# 2. Fashion + Quantity > 5.
subset(sales_data, Category == "Fashion" & Quantity > 5)

# 3. Pune + Discount > 10.
subset(sales_data, City == "Pune" & Discount > 10)

# 4. Revenue > 50000 + Cash payment.
subset(sales_data, Revenue > 50000 & PaymentMethod == "Cash")

# 5. High price + high quantity (e.g., Price > 25000 and Qty > 5).
subset(sales_data, Price > 25000 & Quantity > 5) 