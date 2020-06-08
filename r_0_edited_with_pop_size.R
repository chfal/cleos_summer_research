tb = read_csv("time_series_covid19_confirmed_US.csv")

pop_est <- read_csv("pop_est.csv") %>%
  dplyr::select(c(NAME,POPESTIMATE2019)) %>%
  rename(Province_State=NAME)

tb3 <- left_join(tb, pop_est) %>%
  group_by(Province_State) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)

tb2 = tb3[,7:118]
tb2 <- as.matrix(tb2)
rownames(tb2) = tb3$Province_State
tb4 <- apply(tb2,1, diff) %>%
  t()

tb4[tb4<0]=0

head(tb4)

#moving window from start to end
#go over each state
mGT= generation.time("gamma", c(3, 1.5))

new_york <- tb4["New York",]
R0EG = estimate.R(new_york, GT=mGT,  methods=c('EG'), pop.size = 1244690849, nsim=10, begin=40, end=54)
R0EG$estimates$EG$R


arkansas <- tb4["Arkansas",]
R0EG = estimate.R(arkansas, GT=mGT,  methods=c('EG'), pop.size = 232366865, nsim=10, begin=40, end=54)
R0EG$estimates$EG$R


west_virginia <- tb4["West Virginia",]
R0EG = estimate.R(west_virginia, GT=mGT,  methods=c('EG'), pop.size = 232366865, nsim=10, begin=40, end=54)

# result_matrix <- matrix(NA, 58*7,3, dimnames = list(NULL, c("State","Week","R0")))
# 
# result_data_frame <- data.frame(result_matrix)

fortnight <- seq(0,112, by=14)
i=1
k=1
j=1
l=1

result_data_frame2 <- data.frame(State=character(), Date=character(),R0=double())

for (i in 1:nrow(tb4)){
  for (j in 1:7){
    x=tb4[i,(fortnight[j]+1):fortnight[j+1]]
    if (sum(x)<=0) {
      R0=0 
    }
    else{
      R0EG = estimate.R(tb4[i,], GT=mGT,  methods=c('EG'), pop.size = tb4[,111], nsim=10, begin=fortnight[j]+1, end=fortnight[j+1])
      R0 = R0EG$estimates$EG$R
    }
    result_data_frame2[l,1]=rownames(tb4)[i]
    result_data_frame2[l,2]=j
    result_data_frame2[l,3]=R0
    l=l+1
  }
}