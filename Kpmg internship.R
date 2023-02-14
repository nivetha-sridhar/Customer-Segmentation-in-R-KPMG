mypackages <- c("tidyverse","rfm","xlsx","writexl","magrittr","lubridate"
                ,"readxl","openxlsx","ggplot2","plotrix")

lapply(mypackages,require,character.only = TRUE)


#Data Cleaning
tran <- readxl::read_excel("D:/KPMG_final.xlsx","Transactions")
View(tran)


#checking for null values 
as.data.frame(colSums(is.na(tran)))

#dropping all the null values
tran <- tran%>%
  drop_na()

#re-checking
as.data.frame(colSums(is.na(tran)))

#no.of rows and columns
dim(tran)

length(unique(tran$transaction_id))

#adding a profit column to Transactions
tran$Profit <- tran$list_price - tran$standard_cost

write_xlsx(tran,"D:/KPMG_internship.xlsx")

#-----------------------------#

Cus_demo <- readxl::read_excel("D:/KPMG_final.xlsx","CustomerDemographic")

View(Cus_demo)
glimpse(Cus_demo)

#Checking for null values
as.data.frame(colSums(is.na(Cus_demo)))

#dropping null values
Cus_demo <- Cus_demo%>%drop_na()

as.data.frame(colSums(is.na(Cus_demo)))

#Removing default column as it makes no sense
Cus_demo$default <- NULL

#changing (F,femal),Male -> Female, Male 

Cus_demo$gender <- ifelse(Cus_demo$gender == "F" | Cus_demo$gender == "Femal","Female",Cus_demo$gender)
Cus_demo$gender <- ifelse(Cus_demo$gender == "M","Male",Cus_demo$gender)
sum(is.na(Cus_demo))

wb <- loadWorkbook("D:/KPMG_internship.xlsx")
writeData(wb, sheet = "CustomerDemographic", Cus_demo)
saveWorkbook(wb,"D:/KPMG_internship.xlsx",overwrite = T)

#-----------------------------#

new_cus <- readxl::read_excel("D:/KPMG_final.xlsx","NewCustomerList")


#removing cols 17 to 21 as they don't mean anything
new_cus[17:21] <- NULL
View(new_cus)

#null values
as.data.frame(colSums(is.na(new_cus)))
new_cus<- new_cus%>% drop_na()


wb1 <- loadWorkbook("D:/KPMG_internship.xlsx")
writeData(wb1, sheet = "NewCustomerList", new_cus)
saveWorkbook(wb1,"D:/KPMG_internship.xlsx",overwrite = T)

#-----------------------------#

cus_add <- readxl::read_excel("D:/KPMG_final.xlsx","CustomerAddress")

sum(is.na(cus_add))

cus_add$state <- ifelse(cus_add$state == "New South Wales","NSW",cus_add$state)
cus_add$state <- ifelse(cus_add$state == "Victoria","VIC",cus_add$state)

View(cus_add)

wb2 <- loadWorkbook("D:/KPMG_internship.xlsx")
writeData(wb2,sheet = "CustomerAddress",cus_add)
saveWorkbook(wb2,"D:/KPMG_internship.xlsx",overwrite = T)

#RFM ANALYSIS -> for Customer Segmentation 
#Transactions

tran_rfm <- readxl::read_excel("D:/KPMG_internship.xlsx","Transactions")
View(tran_rfm)

analysis_date <- lubridate::as_date("2017-12-30",tz = "UTC")

r_f_m <- tran_rfm%>%
  mutate(
    customer_id <- as.factor(customer_id),
    Recency_days = as.numeric(as.Date(analysis_date) - date(transaction_date))
)%>%
  group_by(customer_id)%>%
  summarise(
    Frequent_orders = n(),
    Total_revenue = sum(list_price),
    Recency_days = min(Recency_days))%>%
  ungroup()

View(r_f_m)

rfm_score <- rfm_table_customer(
                    data = r_f_m,
                    customer_id = customer_id,
                    n_transactions = Frequent_orders,
                    recency_days = Recency_days,
                    total_revenue = Total_revenue,
                    analysis_date = analysis_date,
                    recency_bins = 5,
                    frequency_bins = 5,
                    monetary_bins = 5)

View(rfm_score$rfm)
summary(rfm_score$rfm)

write_xlsx(rfm_score$rfm,"D:/RFM_KPMG_internship.xlsx")

segment_titles <- c("Champions","Loyal Customers","Potencial Loyalist",
                    "Recent Customers","Promising","Customers Needing Attention",
                    "About To Sleep","At Risk","Can't Lose Them","Hibernating",
                    "Lost")

r_low <- c(4,3,3,3,3,2,2,1,1,1,1)
r_high <- c(5,5,5,5,5,3,3,2,2,2,1)
f_low <- c(4,3,2,1,1,2,1,2,1,1,1)
f_high <- c(5,5,5,2,2,3,3,5,5,2,3)
m_low <- c(4,3,1,1,1,2,1,2,3,1,1)
m_high <- c(5,5,3,2,5,3,3,5,5,2,1)

segments <- rfm_segment(rfm_score, segment_titles,r_low,r_high,f_low,f_high,m_low,m_high)

write_xlsx(segments,"D:/Segments_KPMG_internship.xlsx")

segments %>%
  count(segment) %>% arrange(desc(n)) %>%
  rename(Segment = segment,Count = n)

#median Recency
rfm_plot_median_recency(segments)

#median frequency
rfm_plot_median_frequency(segments)

#median monetary
rfm_plot_median_monetary(segments)

#heatmap
rfm_heatmap(rfm_score)

#histogram

rfm_histograms(rfm_score)

#distribution of customers across orders based on transaction counts
rfm_order_dist(rfm_score)

#scatter plot
#Recency vs Monetary Value
rfm_rm_plot(rfm_score)

#Frequency vs Monetary Value
rfm_fm_plot(rfm_score)

#Recency vs Frequency
rfm_rf_plot(rfm_score)



plot <- ggplot(segments) + geom_bar(aes(x = segment, fill = segment))+theme(axis.text.x=element_text(angle=90,hjust=1)) +labs(title = "Barplot for Segments of customers")
plot_h <- plot + coord_flip()
plot_h

x <- table(segments$segment)
x
piepercent<- round(100*x/sum(x), 1)
lbls = paste(names(x), " ", piepercent,"%")
plotrix::pie3D(x, labels = lbls, main = "Pie chart for Customer Segments", explode = 0.1,labelcol = "#030303",labelcex = 0.70,radius=0.9 ,labelpos = NULL)

#---------------------------------------#
#For Task 3

df <- readxl::read_excel("D:/Segments_KPMG_internship.xlsx")

wbb <- loadWorkbook("D:/KPMG_internship.xlsx")
writeData(wbb, sheet = "Segments", df)
saveWorkbook(wbb,"D:/KPMG_internship.xlsx",overwrite = T)

