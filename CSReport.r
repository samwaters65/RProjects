######################################################################################################
#                                                                                                    #
# Purpose:                                                                                           #
#	Create a series of RMarkdown documents with dynamic text, calculations, and visualizations.      # 
#	Then the documents are emailed to specific contacts via GMail. Once the emails have been sent,   #
# 	The files are then archived. Coupled with ReportMkdn.rmd File.                                   #
#                                                                                                    #
######################################################################################################


#################
# Load packages #
#################

library(dplyr)
library(RODBC)
library(gmailr)
library(magrittr)


#################
# New Functions #
#################

# Function for moving files to different directories (delete from current location)
my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}


#############
# Load Data #
#############

obj1 <- read.csv('...Obj1.csv')
obj2 <- read.csv('...bj2.csv')
obj3 <- read.csv('...Obj3.csv')
obj4 <- read.csv('...Obj4.csv')

###################
# Manipulate Data #
###################


obj2$Weeks <- as.Date(obj2$Weeks, format = '%m/%d/%Y')
#obj2 <- filter(obj2, obj2$Category != 'Category1')
#obj1 <- filter(obj1, obj1$Category != 'Category1')
#obj3 <- filter(obj3, obj3$Category != 'Category1')
#obj4 <- filter(obj4, obj4$Category != 'Category1')

###############
# Create PDFs #
###############


for (ident in unique(obj1$Ident)){
  rmarkdown::render(input = "ReportMkdn.Rmd", 
                    output_format = "pdf_document",
                    output_file = paste(ident, "Summary", Sys.Date(), ".pdf", sep=''),
                    output_dir = "...Report")
}






### Pull together the email list

# Establish SQL table connection
channel2<- odbcDriverConnect(connection="Driver={SQL Server Native Client 10.0}; Server=ServerName; Database=DBName;Trusted_Connection=yes;")

# Query SQL table to get contacts
emails <- 
  sqlQuery(channel2, "
           Select *
           from dbo.ReportEmails
           ")


# Loop through the various files and email them to the appropriate contacts
for (ident in unique(obj1$Ident[obj1$CustomerName != 'CustomerNameToExclude'])){
  to <- as.character(emails[emails$CustomerCategory == ident,2])
  file <- file.path(paste0("...\\Report\\", ident, "Summary", Sys.Date(), ".pdf", collapse=''))
  test_email <- mime() %>%
    to(to) %>%
    from("email@email.com") %>%
    subject(paste0(as.character(ident),"Metric Summary ", as.character(Sys.Date()))) %>%
    html_body("Please see attached for this week's file") %>%
    attach_file(file) %>%
    send_message()
}



# Loop to remove all of the files from the directory and store them into an Archive folder
for (ident in unique(obj1$Ident)) {
  my.file.rename(from = file.path(paste0("...CSReport\\", ident, "Summary", Sys.Date(), ".pdf", collapse='')),
                 to = file.path(paste0("...Archive\\", ident, "Summary", Sys.Date(), ".pdf", collapse='')))
}

