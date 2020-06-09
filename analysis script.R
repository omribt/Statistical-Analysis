install.packages("plyr")
install.packages("dplyr")
install.packages("expss")
install.packages("psych")
install.packages("lubridate")
install.packages("zoo")
install.packages("WriteXLS")
install.packages('openxlsx')

avg_nikion = mean(transactions$total[transactions$deal_type == "nikion"])
avg_loan = mean(transactions$total[transactions$deal_type == "loan"])
avg_converet = mean(transactions$total[transactions$deal_type == "convert"])
avg_transfer = mean(transactions$total[transactions$deal_type == "transfer"])
deal_count_nikion = length(unique(transactions$deal_number[transactions$deal_type == "nikion"]))
deal_count_loan = length(unique(transactions$deal_number[transactions$deal_type == "loan"]))
deal_count_convert = length(unique(transactions$deal_number[transactions$deal_type == "convert"]))
deal_count_transfer = length(unique(transactions$deal_number[transactions$deal_type == "transfer"]))
sum_nikion = sum(transactions$total[transactions$deal_type == "nikion"])
sum_loan = sum(transactions$total[transactions$deal_type == "loan"])
sum_convert = sum(transactions$total[transactions$deal_type == "convert"])
sum_transfer = sum(transactions$total[transactions$deal_type == "transfer"])

transactions$`deal date` = as.Date(transactions$`deal date`)
transactions$monthly =  as.yearmon(transactions$`deal date`)

monthly_sum <- aggregate(transactions$total,
                         by = list(month =
                                     transactions$monthly),
                         FUN = sum)


monthly_deals = aggregate(transactions$deal_number,
                          by = list(transactions$monthly),
                          FUN = n_distinct)


fiftyK = subset(transactions, total >= 50000)
monthly_deals_50K = aggregate(fiftyK$deal_number,
                              by = list(fiftyK$monthly),
                              FUN = n_distinct)

percentile_90 = data.frame(matrix(c(quantile(transactions$total[transactions$deal_type == 'nikion'], 0.9, na.rm = TRUE),
                      quantile(transactions$total[transactions$deal_type == 'transfer'], 0.9, na.rm = TRUE),
                      quantile(transactions$total[transactions$deal_type == 'convert'], 0.9, na.rm = TRUE),
                      quantile(transactions$total[transactions$deal_type == 'loan'], 0.9, na.rm = TRUE)),
                      nrow = 1,ncol = 4))

colnames(percentile_90) = c("nikion", "transfer", "convert", "loan")                      


nikion_transactions = subset(transactions, subset = transactions$deal_type == "nikion")

transfer_transactions = subset(transactions, subset = transactions$deal_type == "transfer")

convert_transactions = subset(transactions, subset = transactions$deal_type == "convert")

loan_transactions = subset(transactions, subset = transactions$deal_type == "loan")

nikion_deals = tryCatch (
  aggregate(
    nikion_transactions$deal_number,
    by = list(nikion_transactions$customer_name),
    FUN = n_distinct
  ),
  error = function(e) {
  }
)
Harigim_nikion = tryCatch(
  nikion_deals$Group.1[nikion_deals$x > 4],
  error = function(e) {
  }
)

transfer_deals = tryCatch(
  aggregate(
    transfer_transactions$deal_number,
    by = list(transfer_transactions$customer_name),
    FUN = n_distinct
  ),
  error = function(e) {
  }
)
Harigim_transfer = tryCatch(
  transfer_deals$Group.1[transfer_deals$x > 4],
  error = function(e) {
  }
)

convert_deals = tryCatch(
  aggregate(
    convert_transactions$deal_number,
    by = list(convert_transactions$customer_name),
    FUN = n_distinct
  ),
  error = function(e) {
  }
)
Harigim_convert = tryCatch(
  convert_deals$Group.1[convert_deals$x > 4],
  error = function(e) {
  }
)


loan_deals = tryCatch(
  aggregate(
    loan_transactions$deal_number,
    by = list(loan_transactions$customer_name),
    FUN = n_distinct
  ),
  error = function(e) {
  }
)
Harigim_loan = tryCatch(
  loan_deals$Group.1[loan_deals$x > 4],
  error = function(e) {
  }
)


harigim_total = c(tryCatch(
  n_distinct(Harigim_nikion),
  error = function(e) {
  }
),
tryCatch(
  n_distinct(Harigim_transfer),
  error = function(e) {
  }
),
tryCatch(
  n_distinct(Harigim_convert),
  error = function(e) {
  }
),
tryCatch(
  n_distinct(Harigim_loan),
  error = function(e) {
  }
))


nikion_cus = n_distinct(transactions$customer_name[transactions$deal_type == 'nikion'])
transfer_cus = n_distinct(transactions$customer_name[transactions$deal_type == 'transfer'])
convert_cus = n_distinct(transactions$customer_name[transactions$deal_type == 'convert'])
loan_cus = n_distinct(transactions$customer_name[transactions$deal_type == 'loan'])



daily_deals = aggregate(transactions$deal_number,
                          by = list(transactions$`deal date`,transactions$customer_name),
                          FUN = n_distinct)
n_distinct(daily_deals$Group.2[daily_deals$x > 1])/n_distinct(transactions$customer_name)

transfer_dest = tryCatch(
  aggregate(
    transfer_transactions$total,
    by = list(transactions$destination[transactions$deal_type == 'transfer']),
    FUN = sum
  ) ,
  error = function(e) {
    
  }
)

output = matrix(
  c(
    avg_nikion,
    sum_nikion,
    deal_count_nikion,
    avg_transfer,
    sum_transfer,
    deal_count_transfer,
    avg_converet,
    sum_convert,
    deal_count_convert,
    avg_loan,
    sum_loan,
    deal_count_loan
  )
  ,
  ncol = 3,
  nrow = 4,
  byrow = TRUE
)

rownames(output) = c("nikion", "transfer", "convert", "loan")
colnames(output) = c("avg", "sum", "count")
output = as.table(output)
    


seperate = ' '


output
monthly_sum
monthly_deals
monthly_deals_50K
percentile_90

harigim_total
tryCatch(n_distinct(Harigim_nikion)/nikion_cus,error = function(e){})
tryCatch(n_distinct(Harigim_transfer)/transfer_cus,error = function(e){})
tryCatch(n_distinct(Harigim_convert)/convert_cus,error = function(e){})
tryCatch(n_distinct(Harigim_loan)/loan_cus,error = function(e){})

n_distinct(transactions$customer_name)
sum(harigim_total, na.rm = TRUE)
n_distinct(transactions$customer_name) - sum(harigim_total, na.rm = TRUE)

daily_deals = aggregate(transactions$deal_number,
                        by = list(transactions$`deal date`,transactions$customer_name),
                        FUN = n_distinct)
n_distinct(daily_deals$Group.2[daily_deals$x > 1])/n_distinct(transactions$customer_name)

View(transfer_dest)
