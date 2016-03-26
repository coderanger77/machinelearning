training_data = read.csv("../data/dunnhumby hack reduce product launch challenge - training set.csv") 
plot(training_data$Product_Category,training_data$Units_that_sold_that_week,type="i",xlab="product category",ylab)


TrailMixSnaks <- training_data [ which(training_data$Product_Category=='Trail Mix & Snacks'), ]str()
sales26week=training_data[which(training_data$Weeks_Since_Launch==26),]
newdata <- training_data[ which(training_data$Product_Category=='Ice Cream Ice Milk & Sherbets' ), ]

#sales vs number of stores @26th week. 
sales26week=training_data[which(training_data$Weeks_Since_Launch==26),]
plot(sales26week$Stores_Selling,sales26week$Units_that_sold_that_week,xlab = 'Number of stores',ylab = 'Number of units soled')

#for a category week vs sales.
plotProductCategory <- function(category)
{
  catData=training_data[which(training_data$Product_Category==category),]
  count=0
  for(i in catData$Product_Launch_Id)
  {
    if(count==0)
    {
      plot(catData$Weeks_Since_Launch[catData$Product_Launch_Id==i],catData$Units_that_sold_that_week[catData$Product_Launch_Id==i],type='l',ylim=c(0,2500),xlim=c(0,30),col=count,ylab='Number of sales',xlab = 'Week number',main=category)
      count=1
    }else{
      lines(catData$Weeks_Since_Launch[catData$Product_Launch_Id==i],catData$Units_that_sold_that_week[catData$Product_Launch_Id==i],type='l',col=count,ylab='Number of sales',xlab = 'Week number',main=category)
    }
    count=count+1
  }
}

plotProductCategory('Frzn Prepared Chicken')

training_data[,"Distinct_Customers_Buying_At_Least_Once_"]<-NA_integer_

#sales vs custome type
  for(product_id in unique(training_data$Product_Launch_Id))
  {
    for(i in 1:26)
    {
      if(i==1)
      {
        training_data[which(training_data$Product_Launch_Id==product_id & training_data$Weeks_Since_Launch==i),]["Distinct_Customers_Buying_At_Least_Once_"]=training_data[which(training_data$Product_Launch_Id==product_id & training_data$Weeks_Since_Launch==i),]["Distinct_Customers_Buying_At_Least_Once_Cumulative"]
      }else{
        prev=training_data[which(training_data$Product_Launch_Id==product_id & training_data$Weeks_Since_Launch==i-1),]["Distinct_Customers_Buying_At_Least_Once_Cumulative"]
        current=training_data[which(training_data$Product_Launch_Id==product_id & training_data$Weeks_Since_Launch==i),]["Distinct_Customers_Buying_At_Least_Once_Cumulative"]
        training_data[which(training_data$Product_Launch_Id==product_id & training_data$Weeks_Since_Launch==i),]["Distinct_Customers_Buying_At_Least_Once_"]=current-prev
        
      }
      
    }
  }

