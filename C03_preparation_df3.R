#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarize(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

#!!! NOTE:  there are duplicates !!!#

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()

#### CLEANING DATA TYPES in df_3 ####

## format string as factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.factor(CAP))

#### CLEANING MISSING VALUES in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))

## let examine in details some of these missing cases
df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarize(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#

#### EXPLORE COLUMNS of df_3 ####

#### ???? TO DO df_3 ???? ####
# EXPLORE the df_3_cli_address_clean relevant variables

### Factoring the variables PRV and REGION ###
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  mutate(PRV = as.factor(PRV),
         REGION = as.factor(REGION))

### Variable REGION ###
df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarize(TOT_ADDRESS = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDRESS/sum(TOT_ADDRESS)) %>%
  arrange(desc(PERCENT))
ggplot(df_3_cli_address_clean, aes(x=REGION, y= ..count../sum(..count..))) + 
  geom_bar(fill = "gold2", color="black") +         
  labs(x="Region", y="Relative frequency of addresses", 
       title="Frequency of addresses by region") +
  theme(axis.text.x = element_text(angle=45, hjust=1)
  )
    
### Variable PRV ###
df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarize(TOT_ADDRESS = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDRESS/sum(TOT_ADDRESS)) %>%
  arrange(desc(PERCENT))
ggplot(df_3_cli_address_clean, aes(x=PRV, y= ..count../sum(..count..))) + 
  geom_bar(fill = "orange", color="black") +         
  labs(x="Provincia", y="Relative frequency of addresses", 
       title="Frequency of addresses by province") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) 

#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)
