rm(list=ls())


# Load packages
library(xml2) 
library(httr) 


url<-GET("https://www.pro-football-reference.com/officials/index.htm")

# Parse page
page<-read_html(url) # Parse HTML

# Extract official names
officials<-xml_text(xml_find_all(page, "//table[@id='officials']//tbody/tr/th"))

# Extract table data (first regular <td> element)
games<-xml_text(xml_find_all(page, "//table[@id='officials']//tbody/tr/td[1]"))
positions<-xml_text(xml_find_all(page, "//table[@id='officials']//tbody/tr/td[2]"))
years<-xml_text(xml_find_all(page, "//table[@id='officials']//tbody/tr/td[3]"))

#Getting links for crawling
links<-xml_attr(xml_find_all(page, "//table[@id='officials']//tbody/tr/th/a"),
                attr="href")
#Completing partial links
urls<-paste("https://www.pro-football-reference.com",links,sep="")


# Combine into data frame
officials_df<-data.frame(officials,games,positions,years,urls)
str(officials_df) # 502 rows/5 columns
# all data is stored as character or factor

# Save as .rda
save(officials_df, file="officials_overview.rda")
# Save as .csv
write.csv(officials_df, "officals_overview.csv", row.names=FALSE)