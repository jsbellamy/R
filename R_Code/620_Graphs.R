library('ggplot2')
library('treemap')

df <- read.csv('1. Master H1B Dataset.csv')
summary(df$CASE_STATUS)

#Bar graph for each case per thousand
df_case <- df %>% group_by(CASE_STATUS) %>% 
  summarise(count=n()) %>% 
  mutate(rate=count/1000)

ggplot(data = df_case, aes(x = reorder(CASE_STATUS,rate), y = rate)) +  
  geom_bar(stat="identity", fill="steelblue", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="Visa Status", x ="Case status", y = "Number of applications (thousands)")

#Bar Graph for Job type per thousand
df_job <- df %>% group_by(SOC_NAME) %>% 
  summarise(count=n()) %>% 
  mutate(rate=count/1000) %>%
  top_n(n=20)

ggplot(data = df_job, aes(x = reorder(SOC_NAME,rate), y = rate)) +  
  geom_bar(stat="identity", fill="chocolate", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="Top 20 Jobs", x ="Speciality", y = "Number of applications (thousands)")

#Bar Graph for employers per thousand
df_employ <- df %>% group_by(EMPLOYER_NAME) %>% 
  summarise(count=n()) %>% 
  mutate(rate=count/1000) %>%
  top_n(n=20)

ggplot(data = df_employ, aes(x = reorder(EMPLOYER_NAME,rate), y = rate)) +  
  geom_bar(stat="identity", fill="lightblue", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="Top 20 Employers", x ="Employer name", y = "Number of applications (thousands)")

#Line graph for applications per state over time
states_df <-  count(df, WORKSITE_STATE, sort = TRUE)
top_state_num <-  10
top_states_id <- as.vector(states_df[1:top_state_num,]$WORKSITE_STATE)
top_states <-  data.frame(df[df$WORKSITE_STATE %in% top_states_id,])
top_states_by_year <-  group_by(top_states, CASE_SUBMITTED_YEAR) %>% 
  count(CASE_SUBMITTED_YEAR, WORKSITE_STATE)

ggplot(top_states_by_year, aes(CASE_SUBMITTED_YEAR, n, color =  WORKSITE_STATE, group = WORKSITE_STATE)) +
  geom_line() + 
  ylab("Number of Applications") +
  ggtitle("Trend of H1B Petitions of the Top 10 States")

#Tree Graph for application per state
dstate <- df %>% group_by(WORKSITE_STATE) %>% 
  summarise(count=n()/1000)

dstate$WORKSITE_STATE <- tolower(dstate$WORKSITE_STATE)

colnames(dstate) <- c("region","value")
treemap(dstate, 
        index=c("region"), 
        type="value",
        vSize = "value",  
        vColor = "value",
        palette = "RdBu",  
        title=sprintf("Applications per state"), 
        title.legend = "Applications (thousands)",
        fontsize.title = 14  
)

        