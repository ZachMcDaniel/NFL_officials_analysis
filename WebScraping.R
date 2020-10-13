rm(list=ls())

### Web Scraping (Template) ###

## Getting Started ##
# Step 1: Consult robots.txt or terms of service
# Step 2: Open the page where you want to collect data
# (https://www.pro-football-reference.com/)
# Step 3: Show the HTML source code (View Page Source shows
# HTML for entire page, Inspect opens an interactive viewer)
# Step 4: Identify the nodes where data is located (e.g., all
# data is in a table with the id="officials")
# Step 5: Figure out the name, attributes, and/or path (e.g., official
# names are the first <td> element in each row, enclosed in <a> tag)
# Step 6: Parse the page and use xpathSApply() to extract data

## Scrape From One Page ##
# Load packages
library(xml2) # New package--I hope this solves the problem!
library(httr) # Need httr and GET function if page is HTTPS

# HTTPS pages must be "requested" before being parsed
url<-GET("https://www.pro-football-reference.com/officials/index.htm")

# Parse page
page<-read_html(url) # Parse HTML
# NOTE: if page is http, then can apply read_html() directly to URL
# (i.e., you would not need the GET() function)

# Data is arranged in <table> tags. But some pages have more
# than 1 table. Need to be specific:
table<-xml_find_all(page, "//table[@id='officials']")
table # Print HTML to console to confirm you're getting data

# Dig deeper by building onto that base XPath
# Navigate to each row of data:
rows<-xml_find_all(page, "//table[@id='officials']//tbody/tr")
length(rows) # 502 elements

# Dig deeper to official names (<th> within each row)
officials<-xml_find_all(page, "//table[@id='officials']//tbody/tr/th")
length(officials) # 502 elements--important to check that all data
# you're scraping has the same number of elements

# Wrap the result in xml_text to extract value from between the start/end tags
officials<-xml_text(xml_find_all(page, "//table[@id='officials']//tbody/tr/th"))
length(officials)
officials[1:10] # Looks good!

# Extract games (first regular <td> element)
# Use index in [ ] to specify order
games<-xml_text(xml_find_all(page, "//table[@id='officials']//tbody/tr/td[1]"))
length(games)
games[1:10] # Looks good!

# Repeat for positions
positions<-xml_text(xml_find_all(page, "//table[@id='officials']//tbody/tr/td[2]"))
length(positions)
positions[1:10] # Can split multivalued fields later

# Repeat for years active
years<-xml_text(xml_find_all(page, "//table[@id='officials']//tbody/tr/td[3]"))
length(years)
years[1:10] # Clean up the data by splitting start and end year later

# The last bit of data to collect from this table is the hyperlinks
# under referee name. Use xmlGetAttr to extract attribute from within
# HTML tag
links<-xml_attr(xml_find_all(page, "//table[@id='officials']//tbody/tr/th/a"),
                attr="href")
length(links)
links[1:10] # Notice that links are relative/partial

# Use paste() to combine the base domain with the relative link
# to make a full URL
urls<-paste("https://www.pro-football-reference.com",links,sep="")
length(urls)
urls[1:10] # Looks good!

# Combine into data frame
officials_df<-data.frame(officials,games,positions,years,urls)
str(officials_df) # 502 rows/5 columns
# Notice that all data is stored as character or factor
# Can clean data types later

# Write output to RDA file to save
save(officials_df, file="officials_overview.rda")
# Can also save to csv, tab-delimited text, etc.
write.csv(officials_df, "officals_overview.csv", row.names=FALSE)
