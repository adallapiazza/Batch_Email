library(data.table)
library(dplyr)
library(mailR)

## SET UP -------------------------------------------------------------------------------------------------------------

# function to check for valid emails
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

# email addresses 
load_emails <- read.csv('fake_emails.csv')

# clean data
email_addresses <- load_emails  %>% 
  dplyr::filter(!is.na(Email),
                Email != "") %>% 
  dplyr::distinct(Email) %>% 
  dplyr::mutate(Valid = isValidEmail(Email)) %>% 
  dplyr::filter(Valid == TRUE)


## SEND EMAILS WITH ATTACHMENT ----------------------------------------------------------------------------------------
body <- "To Whom It May Concern,
This is an automated email.
Thank You.
"

# set up email function
functionMail <- function(File = "testPDF.pdf",
                         to = "recipient@example.com"){
  
  mailR::send.mail(from = "sender@example.com",
                   to = to,
                   bcc = "bcc_recipient@example.com",
                   subject = "Important Message for All",
                   body = body,
                   smtp = list(host.name = "smtp.gmail.com", 
                               port = 465,
                               user.name = "YOURUSERNAME@gmail.com",
                               passwd = "YOURPASSWORD", 
                               ssl = TRUE),
                   authenticate = TRUE,
                   send = FALSE, # Set to TRUE to Run
                   attach.files = File,
                   debug = FALSE)
  print("SUCCESS")
}


Ops <- unique(email_addresses$Email)

for (i in 1:nrow(email_addresses)) {
  
  TO <- paste(Ops[i])
  print(paste(i, TO, collapse = ", " ))
  filename <- "testPDF.pdf"
  
  functionMail(File = filename, to = TO)
  
  tmsleep<-sample(3:7,1)
  Sys.sleep(tmsleep)
}
