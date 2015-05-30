library(httr)
library(rvest)
library(magrittr)
library(stringr)

target_output <- "tx_cash_5_2014_data.csv"

urls <- scan(file = "tx_cash_5_2014_urls.txt",what="character")

final <- rep("",length(urls))
for(counter in 1:length(urls)){
    tx_cash_5 <- html_session(urls[counter])
    date <- tx_cash_5 %>% html_nodes(".large-9 .sans") %>% html_text() %>% str_sub(start=31,end=40)
    n1 <- (tx_cash_5 %>% html_nodes("li:nth-child(1) span") %>% html_text())[20] 
    n2 <- (tx_cash_5 %>% html_nodes("li:nth-child(2) span") %>% html_text())[20]
    n3 <- (tx_cash_5 %>% html_nodes("li:nth-child(3) span") %>% html_text())[18]
    n4 <- (tx_cash_5 %>% html_nodes("li:nth-child(4) span") %>% html_text())[14]
    n5 <- (tx_cash_5 %>% html_nodes("li:nth-child(5) span") %>% html_text())[10]
    prizes <- tx_cash_5 %>% html_nodes("td") %>% html_text()
    prize_5 <- str_replace(prizes[2],",","") %>% str_replace(.,"\\$","")
    prize_4 <- str_replace(prizes[5],",","") %>% str_replace(.,"\\$","")
    prize_3 <- str_replace(prizes[8],",","") %>% str_replace(.,"\\$","") 
    winner_5 <- str_replace(prizes[3],",","") 
    winner_4 <- str_replace(prizes[6],",","") 
    winner_3 <- str_replace(prizes[9],",","")     
    final[counter] <- str_c(date,
                            n1,n2,n3,n4,n5,
                            prize_5,prize_4,prize_3,
                            winner_5,winner_4,winner_3,
                            sep=",")
}

write(final,file = target_output)

rm(target_output,urls,final,tx_cash_5,date,
   n1,n2,n3,n4,n5,
   prizes,prize_5,prize_4,prize_3,
   winner_5,winner_4,winner_3,
   counter)