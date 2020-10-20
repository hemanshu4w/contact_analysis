setwd(r'{D:\Himanshu\All_files\Other_contacts}')
library(data.table)
data <- fread('frame.csv')
data$contact_number <- as.character(data$contact_number)
data_1$contact_number <- as.character(data_1$contact_number)
fi <- data[!(data$contact_number %in% data_1$contact_number),]
setwd(r'{D:\Himanshu\All_files\New_contacts\check_1}')
library(data.table)
rufilo <- fread('Rufilo_FA_Data.csv')
setwd(r'{D:\Himanshu\All_files\New folder}')
data_1 <- fread('30_09_KBCB_retargeting2.csv')
data_2 <- fread('30_09_FA12_Retargeting.csv')
data_1$mobile_number <- as.character(data_1$mobile_number)
data_2$mobile_number <- as.character(data_2$mobile_number)
rufilo$mobile_number <- as.character(rufilo$mobile_number)
data_1 <- data_1[!(data_1$mobile_number %in% rufilo$mobile_number),]
data_2 <- data_2[!(data_2$mobile_number %in% rufilo$mobile_number),]
write.csv(data_1,'30_09_KBCB_retargeting2_v2.csv')
write.csv(data_2,'30_09_FA12_Retargeting_v2.csv')
getwd()

rufilo$mobile_number <- as.character(rufilo$mobile_number)

rufilo$mobile_number <- as.character(rufilo$mobile_number)
data_1$contact_number <- as.character(data_1$contact_number)




library(tidyverse)
fii <- fi %>% distinct(contact_number)
fii$char = nchar(fii$contact_number)
fiii <- fii %>% filter(char ==10)
fiii$contact_number_1 <- fiii$contact_number
library(tidyverse)

check_1 <- fiii %>% separate(contact_number_1,c('first_digit','other_digit'),sep = 1)
check_1 <- check_1 %>% filter(first_digit>5)
check_2 <- check_1[!(check_1$contact_number %in% mobile$mobile_number),]
check_3 <- check_2[!(check_2$contact_number %in% d_1$contact_number),]
check_4 <- check_3[!(check_3$contact_number %in% d_2$contact_number),]
check_5 <- check_4[!(check_4$contact_number %in% d_3$contact_number),]
check_6 <- check_5[!(check_5$contact_number %in% rufilo$mobile_number),]
rufilo$mobile_number <- as.character(rufilo$mobile_number)
check_6 <- check_6[1:900000,]
check_7 <- check_6[!(check_6$contact_number %in% rufilo$mobile_number),]


check_6 <- check_6 %>% distinct(contact_number)

write.csv(check_6,'fa_linked_mobile_22_09_2020.csv',row.names = F)
getwd()
head(koin_mo)

write.csv(fiii,'Non_FA_contact_base_1.csv',row.names = F)


setwd(r'{D:\Himanshu\All_files\New_contacts}')
library(data.table)
data <- fread('UPI_contacts_2.csv')


mobile <- fread('FA_KOINO_MobileNo_data.csv')
data$contact_number <- as.character(data$contact_number)
mobile$mobile_number <- as.character(mobile$mobile_number)
check <- data[!(data$contact_number %in% mobile$mobile_number),]

getwd()
setwd(r'{D:\Himanshu\All_files\08-09-2020}')
base_for_contact_campaign <- fread('Base_for_contact_campaign.csv')
base_for_contact_campaign$contact_number <- as.character(base_for_contact_campaign$contact_number)
tree_base$contact_number <- as.character(tree_base$contact_number)
setwd(r'{D:\Himanshu\All_files\08-09-2020\contacts_outcome_analysis\good_contacts}')
base <- fread('Tree good network delivery report.csv')
base$Mobile <- as.character(base$Mobile)

tree_base_1 <- tree_base[!(tree_base$contact_number %in% base$Mobile),]
class(base$Mobile)
class(base$)
setwd(r'{D:\Himanshu\All_files\08-09-2020}')
tree_base <- fread('tree_extracted_.csv')
library(tidyverse)
tree_base_1 <- tree_base %>% filter(new_converted_flag==0)

tree_base_1$contact_number <- as.character(tree_base_1$contact_number)
onboarding$`Mobile Number` <- as.character(onboarding$`Mobile Number`)

colnames(onboarding)[4] <- 'contact_number'
table($`Campaign Label`)
check_1 <- left_join(tree_base_1,onboarding,by = 'contact_number')
check_1$created_at <- as.POSIXct(check_1$created_at)
table(check_1$Status)
check_11 <- check_1 %>% filter(Status=='FINAL_APPROVED')
table(check_11$good_count)


setwd(r'{D:\Himanshu\All_files\08-09-2020}')

base_data <- fread('Base_for_contact_campaign.csv')
setwd(r'{D:\Himanshu\All_files\05-10-2020}')
base_2 <- fread('good_network_tree_2_22_09_2020.csv')

tree_base$contact_number <- as.character(tree_base$contact_number)
base_2$contact_number <- as.character(base_2$contact_number)
check <- tree_base[tree_base$contact_number %in% base_2$contact_number,]

base_data$contact_number <- as.character(base_data$contact_number)
final <- left_join(base_data,onboarding,by = 'contact_number')
final_1 <- final %>% distinct(contact_number,.keep_all = T)
head(final_1)
write.csv(final,'Base_1_for_tree.csv',row.names = F)
getwd()









setwd(r'{D:\Himanshu\All_files\New_contacts\check_1}')
d_2 <- fread('40K FA.csv')


d_2$`Mobile Number` <- as.character(d_2$`Mobile Number`)
check <- data[!(data$contact_number %in% d_2$`Mobile Number`),]
table(check$upi_number_match_count)
table(d_3$upi_number_match_count)
check <- d_3 %>% filter(upi_number_match_count==2 )


d_1 <- fread('54k_base_15_09_2020.csv')
d_2 <- fread('base_2_15_09_2020_upi.csv')
d_3 <- fread('UPI_transaction_users.csv')
d_1$contact_number <- as.character(d_1$contact_number)
d_2$contact_number <- as.character(d_2$contact_number)
d_3$contact_number <- as.character(d_3$contact_number)

check_1 <- check[!(check$contact_number %in% d_2$`Mobile Number`),]

check_2 <- check_1[!(check_1$contact_number %in% d_1$contact_number),]
check_3 <- check_2[!(check_2$contact_number %in% d_2$contact_number),]
check_4 <- check_3[!(check_3$contact_number %in% d_3$contact_number),]
check_5 <- check_4[!(check_4$contact_number %in% d_2$`Mobile Number`),]

library(tidyverse)

check_6 <- check_5 %>% filter(upi_number_match<=2)
check_7 <- check_6[!(check_6$contact_number %in% rufilo$mobile_number),]
getwd()
upi_11 <- fread('fa_linked_mobile_22_09_2020.csv')
upi_12 <- fread('UPI_transaction_base_21_09_2020.csv')
upi_11$contact_number <- as.character(upi_11$contact_number)
upi_12$contact_number <- as.character(upi_12$contact_number)
class(upi_11$contact_number)
class(check_7$contact_number)
nrow(upi_11 %>% distinct(contact_number))
check_8 <- check_7[!(check_7$contact_number %in% upi_11$contact_number),]

check_9 <- check_8[0:200000,] %>% select(contact_number)
write.csv(check_9,'UPI_contacts_29_09_2020.csv',row.names = F)
write.csv(check_8,'UPI_remaining_base.csv',row.names = F)



check_6 <- check_5[0:9000000,]
getwd()
write.csv(check_6,'UPI_transaction_base_22_09_2020.csv',row.names = F)
getwd()

check_4 <- check_4 %>% distinct(contact_number)
library(tidyverse)
check_1 <- check %>% filter(upi_number_match<=2)

write.csv(check_1,'UPI_transaction_base.csv',row.names = F)
getwd()






check_1 <- data %>% mutate(bands = ifelse(upi_number_match==1,'aa. 1',
                                    ifelse(upi_number_match==2,'ab. 2',
                                          ifelse(upi_number_match<6 & upi_number_match>2,'b.2_5',
                                           ifelse(upi_number_match<11 & upi_number_match>5,'c.5_10',
                                                  ifelse(upi_number_match<21 & upi_number_match>10,'d.10_20',
                                                         ifelse(upi_number_match<51 & upi_number_match>20,'e.20_50',
                                                                ifelse(upi_number_match>50,'f. GT50','error'))))))))
write.csv(table(check_1$bands),'UPI_transaction_bands_1.csv',row.names = F)
getwd()



















