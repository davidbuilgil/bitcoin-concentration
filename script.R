################################
#
# Offending concentration on the internet
#
# D Buil-Gil, P Saldaña-Taboada, J Aldridge
#
################################

rm(list=ls())

#load packages
library(dplyr)
library(readr)
library(ineq)
library(tidyr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(here)

#load crime data
crimes_1 <- read_csv(here("data/records_forever_151020_1.csv"), 
                   col_types = cols(from_country = col_character(), 
                                    from_country_code = col_character()))
crimes_2 <- read_csv(here("data/records_forever_151020_2.csv"), 
                     col_types = cols(from_country = col_character(), 
                                      from_country_code = col_character()))
crimes <- bind_rows(crimes_1, crimes_2)

#table countries
countries <- crimes %>%
  group_by(from_country) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count/sum(count), 3)*100) %>%
  arrange(-count)

#extract date
crimes <- crimes %>%
  mutate(day = as.Date(created_at, format = "%d/%m/%Y"))

#table reports per day
days <- crimes %>%
  mutate(day = ymd(day)) %>%
  group_by(day) %>% 
  summarise(count = n()) %>%
  complete(day = seq.Date(min(day),
                          max(day), by = "day")) %>%
  mutate(count = replace_na(count, 0))

#visualise reports per day
ggplot(days, aes(x = day, y = count)) +
  geom_line() +
  geom_smooth(lwd = 1.2, col = "red") +
  theme_bw() +
  xlab("Days") +
  ylab("Number of reports")

#table accounts
#accounts <- crimes %>%
#  group_by(address) %>% # group based on users unique id
#  summarise(count = n(), # print the number of votes by user
#            day1 = min(day),
#            day2 = max(day),
#            time = difftime(day2, day1, units = "days"), #days between first and last report
#            time = time + 1)
#accounts <- accounts %>%
#  dplyr::select(address, count, time)

#save data from accounts
#write.csv(accounts, here("data/accounts.csv"))

#load data from accounts
accounts <- read.csv(here("data/accounts.csv"))

#calculate number countries victimised by account
accounts_countries <- crimes %>%
  group_by(address) %>%
  summarise(count       = n(),
            countries   = n_distinct(from_country),
            countries.c = count / countries,
            types       = n_distinct(abuse_type_id))

#merge both addresses files
accounts <- accounts %>%
  left_join(accounts_countries, by = "address") %>%
  dplyr::select(address, count = count.x, time, countries, types)

# subset top 1% of most prolific accounts
top_1pc <- subset(accounts, count > quantile(count, prob = 1 - 1/100))

# proportion of reports by top 1% accounts
sum(top_1pc$count) / sum(accounts$count) * 100
#31.09% of reports are related to the top 1% accounts

# subset top 10% of most prolific accounts
top_10pc <- subset(accounts, count > quantile(count, prob = 1 - 10/100))

# proportion of reports by top 10% accounts
sum(top_10pc$count) / sum(accounts$count) * 100
#60.74% of reports are related to the top 10% accounts

#obtain Gini index
Gini(accounts$count) # print Gini index
#for context, the Gini index in the US is 0.39

#calculate Gini as in Bernasco and Steenbeek (2017)
#install.packages("remotes")
#remotes::install_github("wsteenbeek/lorenzgini")
library(lorenzgini)
gini(accounts$count)

#print Lorenz curve
plot(Lc(accounts$count), # plot Lorenz curve
     main = "Lorenz curve of cybercrime reports by Bitcoin address",
     xlab = "Cumulative share of addresses from lowest to highest reports",
     ylab = "Cumulative share of reports", col = "darkred", lwd = 2, lty = 2)
legend(0.01, 0.99,
       c("Bitcoin-related cybercrime", "1:1 diagonal"),
       lty = c(2, 1),
       lwd = c(2, 1),
       col = c("darkred", "black"))

#print Lorenz curve amb compare with traditional crimes (as in Martinez et al)
Lorenz_cyber <- Lc(accounts$count, 
                   n = rep(1,length(accounts$count)), 
                   plot = F)
plot(Lorenz_cyber,
     col = "black",
     lty = 2,
     lwd = 2,
     main = "Lorenz curve of Bitcoin-related cybercrime and traditional crime",
     xlab = "Cumulative share of offenders/Bitcoin addresses from lowest to highest reports",
     ylab = "Cumulative share of crimes")
traditional <- exp(((0:100) + 13.761) / 23.914)
Lorenz_darknet <- Lc(traditional, 
                     n = rep(1,length(traditional)), 
                     plot = F)
lines(Lorenz_darknet, lty = 3, lwd = 2, col = "grey34")
legend(0.01, 0.99,
       c("Bitcoin-related cybercrime", "Traditional crime", "1:1 diagonal"),
       lty = c(2, 3, 1),
       lwd = c(2, 2, 1),
       col = c("black", "grey34", "black"))

#recode names of crime types
crimes <- crimes %>%
  mutate(abuse_type_id = recode(abuse_type_id,
                                '1' = "ransomware",
                                '2' = "darknet_market",
                                '3' = "bitcoin_tumbler",
                                '4' = "blackmail scam",
                                '5' = "sextorsion",
                                '99' = "other"))

#table crime type
type <- crimes %>%
  group_by(abuse_type_id) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count/sum(count), 3)*100) %>%
  arrange(-count)

#number reports by address by crime type
accounts_ransom <- crimes %>%
  filter(abuse_type_id == "ransomware") %>%
  group_by(address) %>% # group based on users unique id
  summarise(ransomware = n()) # print the number of votes by user
accounts_darknet <- crimes %>%
  filter(abuse_type_id == "darknet_market") %>%
  group_by(address) %>% # group based on users unique id
  summarise(darknet = n()) # print the number of votes by user
accounts_tumblr <- crimes %>%
  filter(abuse_type_id == "bitcoin_tumbler") %>%
  group_by(address) %>% # group based on users unique id
  summarise(tumblr = n()) # print the number of votes by user
accounts_blackmail <- crimes %>%
  filter(abuse_type_id == "blackmail scam") %>%
  group_by(address) %>% # group based on users unique id
  summarise(blackmail = n()) # print the number of votes by user
accounts_sextorsion <- crimes %>%
  filter(abuse_type_id == "sextorsion") %>%
  group_by(address) %>% # group based on users unique id
  summarise(sextorsion = n()) # print the number of votes by user
accounts_other <- crimes %>%
  filter(abuse_type_id == "other") %>%
  group_by(address) %>% # group based on users unique id
  summarise(other = n()) # print the number of votes by user
accounts_types <- plyr::join_all(list(accounts_ransom, accounts_darknet,
                                      accounts_tumblr, accounts_blackmail,
                                      accounts_sextorsion, accounts_other), 
                                 by = 'address', type='full')

#Lorenz curve by crime types
Lorenz_ransom <- Lc(accounts_types$ransomware[!is.na(accounts_types$ransomware)], 
                    n = rep(1,length(accounts_types$ransomware[!is.na(accounts_types$ransomware)])), 
                    plot = F)
Lorenz_darknet <- Lc(accounts_types$darknet[!is.na(accounts_types$darknet)], 
                     n = rep(1,length(accounts_types$darknet[!is.na(accounts_types$darknet)])), 
                     plot = F)
Lorenz_tumblr <- Lc(accounts_types$tumblr[!is.na(accounts_types$tumblr)], 
                    n = rep(1,length(accounts_types$tumblr[!is.na(accounts_types$tumblr)])), 
                    plot = F)
Lorenz_blackmail <- Lc(accounts_types$blackmail[!is.na(accounts_types$blackmail)], 
                       n = rep(1,length(accounts_types$blackmail[!is.na(accounts_types$blackmail)])), 
                       plot = F)
Lorenz_sextorsion <- Lc(accounts_types$sextorsion[!is.na(accounts_types$sextorsion)], 
                        n = rep(1,length(accounts_types$sextorsion[!is.na(accounts_types$sextorsion)])), 
                        plot = F)

#plot all
plot(Lorenz_ransom,
     col = "black",
     lty = 1,
     lwd = 2,
     main = "Lorenz curve of cybercrime types by Bitcoin address",
     xlab = "Cumulative share of addresses from lowest to highest reports",
     ylab = "Cumulative share of reports")
lines(Lorenz_darknet, lty = 2, lwd = 2, col = "grey6")
lines(Lorenz_tumblr, lty = 3, lwd = 2, col = "grey21")
lines(Lorenz_blackmail, lty = 4, lwd = 2, col = "grey34")
lines(Lorenz_sextorsion, lty = 5, lwd = 2, col = "grey41")
legend(0.01, 0.99,
       c("Ransomware", "Darknet market", "Bitcoin tumbler",
         "Blackmail", "Sextorsion", "1:1 diagonal"),
       lty = c(1, 2, 3, 4, 5, 1),
       lwd = c(2, 2, 2, 2, 2, 1),
       col = c("black", "grey6", "grey21", "grey34", "grey41", "black"))

#obtain Gini indices
Gini(accounts_types$ransomware)
Gini(accounts_types$darknet)
Gini(accounts_types$tumblr)
Gini(accounts_types$blackmail)
Gini(accounts_types$sextorsion)
#try also with generalised Gini
gini(accounts_types$ransomware[!is.na(accounts_types$ransomware)])
gini(accounts_types$darknet[!is.na(accounts_types$darknet)])
gini(accounts_types$tumblr[!is.na(accounts_types$tumblr)])
gini(accounts_types$blackmail[!is.na(accounts_types$blackmail)])
gini(accounts_types$sextorsion[!is.na(accounts_types$sextorsion)])

#create ID variable in accounts
accounts <- accounts %>%
  mutate(ID = 1:nrow(accounts)) %>%
  dplyr::select(ID, address, count, time, countries, types)

#save number of accounts
N <- nrow(accounts)

#load package
#library(RJSONIO)

#generate empty dataframe
#accounts_bit <- as.data.frame(NULL)

#download data for each account from blockchain
#for(i in 1:N) {
#  
#  print(i)
#    
#  skip_to_next <- FALSE
#  
#  account_i <- accounts %>%
#    filter(ID == i)
#  
#  address_i <- account_i$address
#  
#  URL_random <- "https://blockchain.info/address/1LfYcbCsssB2niF3VWRBTVZFExzsweyPGQ?format=json"
#  
#  URL_i <- sub("address/1LfYcbCsssB2niF3VWRBTVZFExzsweyPGQ", paste0("address/", address_i), URL_random)
#  
#  data_i <- tryCatch(RJSONIO::fromJSON(URL_i), error = function(e) { skip_to_next <<- TRUE})
#  
#  if(skip_to_next) { next }
#  
#  account_i <- account_i %>%
#    mutate(total_tra = data_i$n_tx,
#           bits_rec  = data_i$total_received,
#           bits_sent = data_i$total_sent,
#           balance   = data_i$final_balance)
#  
#  accounts_bit <- rbind(accounts_bit, account_i)
#
#}

#save data from accounts
#write.csv(accounts_bit, here("data/accounts_bit.csv"))

#load data from accounts
accounts_bit <- read.csv(here("data/accounts_bit.csv"))

#calculate Bitcoins
accounts_bit <- accounts_bit %>%
  mutate(bits_rec = bits_rec/100000000,
         bits_sent= bits_sent/100000000,
         balance  = balance/100000000)

#merge blockchain data with bitcoinabuse data
accounts <- accounts %>%
  left_join(accounts_bit, by = "address") %>%
  dplyr::select(ID.x, address, count.x, total_tra, bits_rec, 
                bits_sent, balance, time, countries, types) %>%
  rename(ID = ID.x,
         count = count.x)

#order by number of reports
accounts <- accounts %>%
  arrange(-count)

#split dataset in deciles
accounts <- accounts %>%
  mutate(decile = dplyr::ntile(-count, 10))

#print average results by decile
accounts %>%
  group_by(decile) %>%
  summarise(count.m      = format(round(mean(count, na.rm = T),3),3),
            total_tra.m  = format(round(mean(total_tra, na.rm = T),3),3),
            bits_rec.m   = format(round(mean(bits_rec, na.rm = T),3),3),
            bits_sent.m  = format(round(mean(bits_sent, na.rm = T),3),3),
            balance.m    = format(round(mean(balance, na.rm = T),3),3),
            time.m       = format(round(mean(time, na.rm = T),3),3),
            countries.m  = format(round(mean(countries, na.rm = T),3),3),
            types.m      = format(round(mean(types, na.rm = T),3),3),
            count.sd     = format(round(sd(count, na.rm = T),3),3),
            total_tra.sd = format(round(sd(total_tra, na.rm = T),3),3),
            bits_rec.sd  = format(round(sd(bits_rec, na.rm = T),3),3),
            bits_sent.sd = format(round(sd(bits_sent, na.rm = T),3),3),
            balance.sd   = format(round(sd(balance, na.rm = T),3),3),
            btime.sd     = format(round(sd(time, na.rm = T),3),3),
            countries.sd = format(round(sd(countries, na.rm = T),3),3),
            types.sd     = format(round(sd(types, na.rm = T),3),3))

#split dataset in deciles by sum of values
accounts <- accounts %>%
  mutate(decile.sum = as.numeric(cut(cumsum(count), breaks = 10)))

#print average results by cumulative decile
accounts %>%
  group_by(decile.sum) %>%
  summarise(count.m      = format(round(mean(count, na.rm = T),3),3),
            total_tra.m  = format(round(mean(total_tra, na.rm = T),3),3),
            bits_rec.m   = format(round(mean(bits_rec, na.rm = T),3),3),
            bits_sent.m  = format(round(mean(bits_sent, na.rm = T),3),3),
            balance.m    = format(round(mean(balance, na.rm = T),3),3),
            time.m       = format(round(mean(time, na.rm = T),3),3),
            countries.m  = format(round(mean(countries, na.rm = T),3),3),
            types.m      = format(round(mean(types, na.rm = T),3),3),
            count.sd     = format(round(sd(count, na.rm = T),3),3),
            total_tra.sd = format(round(sd(total_tra, na.rm = T),3),3),
            bits_rec.sd  = format(round(sd(bits_rec, na.rm = T),3),3),
            bits_sent.sd = format(round(sd(bits_sent, na.rm = T),3),3),
            balance.sd   = format(round(sd(balance, na.rm = T),3),3),
            btime.sd     = format(round(sd(time, na.rm = T),3),3),
            countries.sd = format(round(sd(countries, na.rm = T),3),3),
            types.sd     = format(round(sd(types, na.rm = T),3),3))
accounts %>%
  group_by(decile.sum) %>%
  summarise(n = n(),
            sum = sum(count))

#print average for all
accounts %>%
  summarise(count.m      = format(round(mean(count, na.rm = T),3),3),
            total_tra.m  = format(round(mean(total_tra, na.rm = T),3),3),
            bits_rec.m   = format(round(mean(bits_rec, na.rm = T),3),3),
            bits_sent.m  = format(round(mean(bits_sent, na.rm = T),3),3),
            balance.m    = format(round(mean(balance, na.rm = T),3),3),
            time.m       = format(round(mean(time, na.rm = T),3),3),
            countries.m  = format(round(mean(countries, na.rm = T),3),3),
            types.m      = format(round(mean(types, na.rm = T),3),3),
            count.sd     = format(round(sd(count, na.rm = T),3),3),
            total_tra.sd = format(round(sd(total_tra, na.rm = T),3),3),
            bits_rec.sd  = format(round(sd(bits_rec, na.rm = T),3),3),
            bits_sent.sd = format(round(sd(bits_sent, na.rm = T),3),3),
            balance.sd   = format(round(sd(balance, na.rm = T),3),3),
            btime.sd     = format(round(sd(time, na.rm = T),3),3),
            countries.sd = format(round(sd(countries, na.rm = T),3),3),
            types.sd     = format(round(sd(types, na.rm = T),3),3))

#obtain correlation matrix
account_comp <- accounts %>%
  filter(complete.cases(.)) %>%
  rename('Reports*' = count,
         'Transactions**' = total_tra,
         'Bitcoin received**' = bits_rec,
         'Bitcoin sent**' = bits_sent,
         'Balance in Bitcoin**' = balance,
         'Time offending*' = time,
         'Countries*' = countries,
         'Types of crime*' = types)
cor <- Hmisc::rcorr(as.matrix(account_comp[,3:10]), type = "spearman")
cor

#obtain descriptive stats
summary(account_comp)

#visualise correlation matrix
corrplot(cor$r, type = "lower", order = "hclust", tl.col = "black", tl.srt = 30,
         cl.lim = c(0,1), tl.cex=0.8,
         col = colorRampPalette(c("white", "grey", "black"))(100))
