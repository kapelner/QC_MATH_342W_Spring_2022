#problem 1
set.seed(1)
n = 10
x = rnorm(n, 0, 4)
px = 1 / (1 + exp(-x))
px
y = rbinom(n, 1, px)
y

log_mod = glm(y ~ x, family = binomial(link = "probit"))
round(coef(log_mod), 3)

pacman::p_load(xtable)
xtable(rbind(x, y, px))
mean(y * log(px) + (1 - y) * log(1 - px))

yhat = as.integer(px > 0.5)
table(y, yhat)

yhat = as.integer(px > 0.05)
table(y, yhat)


set.seed(2)
n = 50
table(sample(c("M", "F", "NA"), size = n, replace = TRUE, prob = c(.5,.35,.15)))


rm(list = ls())
options(java.parameters = "-Xmx4g")
pacman::p_load(R.utils, data.table, YARF)
bills = fread("bills_dataset/bills.csv.bz2")
payments = fread("bills_dataset/payments.csv.bz2")
discounts = fread("bills_dataset/discounts.csv.bz2")
setnames(bills, "amount", "tot_amount")
setnames(payments, "amount", "paid_amount")
head(bills)
head(payments)
head(discounts)

bills_with_payments = merge(bills, payments, all.x = TRUE, by.x = "id", by.y = "bill_id")
bills_with_payments[, id.y := NULL]
bills_with_payments_with_discounts = merge(bills_with_payments, discounts, all.x = TRUE, by.x = "discount_id", by.y = "id")
setorder(bills_with_payments_with_discounts, id)
bills_with_payments_with_discounts[, total_paid := sum(paid_amount, na.rm = TRUE), by = id]
bills_with_payments_with_discounts[, paid_bill := total_paid >= tot_amount, by = id]
bills_with_payments_with_discounts[, discount_num_days := factor(num_days, exclude = NULL)]
bills_with_payments_with_discounts[, discount_pct_off := factor(pct_off, exclude = NULL)]
bills_with_payments_with_discounts[, discount_days_until_discount := factor(days_until_discount, exclude = NULL)]
#compute number of days bill is due in
pacman::p_load(lubridate)
bills_with_payments_with_discounts[, num_days_to_pay := as.integer(ymd(due_date) - ymd(invoice_date))]
#now build the final dataframe
bills_data = bills_with_payments_with_discounts[, .(
  paid_in_full = as.integer(any(paid_bill)), 
  customer_id = first(customer_id),
  tot_amount = first(tot_amount),
  num_days_to_pay = first(num_days_to_pay),
  discount_num_days = first(discount_num_days),
  discount_pct_off = first(pct_off),
  discount_days_until_discount = first(days_until_discount)
), by = id]
#how many bills did this customer have previously?
bills_data[, num_previous_bills := 0 : (.N - 1), by = customer_id]
#and how many of those did he pay on time?
bills_data[, num_previous_bills_paid_on_time := cumsum(paid_in_full), by = customer_id]
bills_data[, customer_id := NULL] #no need for customer id anymore; it won't be a feature
#maybe some other derived features may be important
# bills_data[, pct_previous_payments := num_previous_bills_paid_on_time / num_previous_bills]
bills_data[num_days_to_pay == 0, num_days_to_pay := 1]
bills_data[, dollars_owed_per_day := tot_amount / num_days_to_pay]
#to force classification, set y to be a factor
bills_data[, paid_in_full := factor(paid_in_full)]
bills_data[, id := NULL]

setnames(bills_data, "discount_num_days", "disc_days")
setnames(bills_data, "discount_days_until_discount", "disc_delay")
setnames(bills_data, "num_previous_bills_paid_on_time", "num_prev_bills_yes")
setnames(bills_data, "dollars_owed_per_day", "owed_per_day")

head(bills_data)

set.seed(1984)
nsamp = 2000
bills_data_samp = bills_data[sample(1 : .N, nsamp)]
bills_data_samp[, disc_days := as.numeric(disc_days)]

head(bills_data_samp)

pacman::p_load(missForest)
bills_data_samp = missForest(bills_data_samp)$ximp

yarf_mod = YARF(bills_data_samp[, -c("paid_in_full")], bills_data_samp$paid_in_full, nodesize = 400, seed = 1984)
yarf_mod
illustrate_trees(yarf_mod, max_depth = 4, trees = 1, length_in_px_per_half_split = 40)
cart_mod = YARF(bills_data_samp[, -c("paid_in_full")], bills_data_samp$paid_in_full, nodesize = 1, num_trees = 1, seed = 1984)
cart_mod
illustrate_trees(cart_mod, max_depth = 4, trees = 1, length_in_px_per_half_split = 40)
