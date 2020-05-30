#web scraping 

install.packages("rvest")
library(rvest)
library(dplyr)

market_wbpg<-read_html("https://www.marketwatch.com/story/bitcoin-jumps-after-credit-scare-2018-10-15")

market_wbpg%>%html_node("title")%>%html_text()

market_wbpg%>%html_nodes("p")%>%html_text()


market_bitcoin_articles<-read_html("https://www.marketwatch.com/search?q=bitcoin&Link=MW_SearchAC")


urls<-market_bitcoin_articles%>%html_nodes("div.searchresult a")%>%html_attr("href")
#15 articles related to bitcoin



date_time<-market_bitcoin_articles%>%html_nodes("div.deemphasized span.invisible")%>%html_text()
#some dates might be visible some might not so we use that nodes to help with that.
#since all are visible we rerun the previous code again and modeify it

date_time<-market_bitcoin_articles%>%html_nodes("div.deemphasized span")%>%html_text()

library(lubridate)#to help format the date 

date_clean<-gsub("\\.","",date_time)

date_clean<-parse_date_time(date_clean,"%I:%M %p %m/%d/%y")#standard date time format

datetime_convert <- ymd_hms(date_clean, tz = "US/Eastern")

datetime_convert <- with_tz(datetime_convert, "Asia/Kolkata")


market_watch<-data.frame(webPg=urls,DateTime=datetime_convert)

diff_in_hours <- difftime(
  Sys.time(), market_watch$DateTime, units = "hours"
)


diff_in_hours <- as.double(diff_in_hours)


market_watch$diff<-diff_in_hours


market_latest<-subset(market_watch,diff<18)



titles <- c()
bodies <- c()
for(i in market_latest$webPg){
  
  marketwatch_latest_wbpg <- read_html(i)
  title <- marketwatch_latest_wbpg %>%
    html_node("title") %>%
    html_text()
  titles <- append(titles, title)
  
  marketwatch_latest_wbpg <- read_html(i)
  body <- marketwatch_latest_wbpg %>%
    html_nodes("p") %>%
    html_text()
  one_body <- paste(body, collapse=" ")
  bodies <- append(bodies, one_body)
  
}


market_latest$title<-titles
market_latest$body<-bodies

#to remove basic items in the body text
library(stringr)
clean_bodies<-str_squish(market_latest$body)

library(LSAfun)
summary <- c()
for(i in clean_bodies){
  top_info <- genericSummary(i,k=3);
  one_summary <- paste(top_info, collapse=" ")
  summary <- append(summary, one_summary)
}


market_latest$summary<-summary


library(sendmailR)


marketwatch<- c()
for(i in 1:length(market_latest$summary)){
  marketwatch <- append(marketwatch, market_latest$title[i])
  marketwatch<- append(marketwatch, market_latest$summary[i])
}

sender <- "<siddhesh@gmail.com>"
to <- "<siddhesh@gmail.com>"
subject <- "Hourly Summary of Bitcoin Events"
body <- marketwatch            
mailControl <- list(smtpServer="ASPMX.L.GOOGLE.COM")
sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)




