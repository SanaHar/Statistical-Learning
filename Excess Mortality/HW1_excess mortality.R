Sys.setlocale(locale = 'persian')

library(data.table)
library(ggplot2)

d = fread('C:/Users/Beethoven/Downloads/iranprovs_mortality_monthly.csv', encoding = 'UTF-8')
d = d[y<1401]

d$ym_num = d$y + d$m / 12 - 1/24

dy1 = d[ym_num>1398 + 10/12 -1/24]
dy1 = dy1[, .(n = sum(n)), .(age_group)]

dy2 = d[ym_num<1398 + 10/12 -1/24]
dy2 = dy2[, .(n = sum(n)), .(age_group)]

ggplot() +
  # after corona
  geom_point(data=dy1, aes(x=age_group, y=n), fill="blue",colour="darkblue") + 
  # before corona
  geom_point(data=dy2, aes(x=age_group, y=n), fill="red",colour="red")+
  theme(legend.position = "bottom",legend.justification = c("after corona", "before corona"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dy = d[, .(n = sum(n)), .(age_group)]

ggplot() +
  geom_point(data=dy, aes(x=age_group, y=n), fill="blue",colour="darkblue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ds1 = d[age_group=="0" | age_group=="01-04" | age_group=="05-09" | 
          age_group=="10-14"]

ds2 = d[age_group=="15-19" | age_group=="20-24" | age_group=="25-29" | age_group=="30-34" | 
          age_group=="35-39" | age_group=="40-44" | age_group=="45-49" | 
          age_group=="50-54" | age_group=="55-59" | age_group=="60-64" |
          age_group=="65-69"]

ds3 = d[age_group=="70-74" | age_group=="75-79" | age_group=="80-84" | 
          age_group=="85-89" | age_group=="90-94" | age_group=="95+"]

dss1 = ds1[, .(n = sum(n)), .(y, m, ym_num)]
dss2 = ds2[, .(n = sum(n)), .(y, m, ym_num)]
dss3 = ds3[, .(n = sum(n)), .(y, m, ym_num)]

dss = d[, .(n = sum(n)), .(y, m, ym_num)]

ggplot(dss, aes(ym_num, n))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = 1389:1401)+
  scale_y_continuous(limits = c(0, 71000))+
  geom_vline(xintercept = 1398 + 10/12 -1/24, linetype = 'dashed')+
  ggtitle(label = "Number of mortality for all group ages")

ggplot(dss1, aes(ym_num, n))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = 1389:1401)+
  scale_y_continuous(limits = c(0, 71000))+
  geom_vline(xintercept = 1398 + 10/12 -1/24, linetype = 'dashed')+
  ggtitle(label = "Number of mortality for group age 0-14")

ggplot(dss2, aes(ym_num, n))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = 1389:1401)+
  scale_y_continuous(limits = c(0, 71000))+
  geom_vline(xintercept = 1398 + 10/12 -1/24, linetype = 'dashed')+
  ggtitle(label = "Number of mortality for group age 15-69")

ggplot(dss3, aes(ym_num, n))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = 1389:1401)+
  scale_y_continuous(limits = c(0, 71000))+
  geom_vline(xintercept = 1398 + 10/12 -1/24, linetype = 'dashed')+
  ggtitle(label = "Number of mortality for group age 70-95+")

################### Question1

ds1 = ds1[, .(n = sum(n)), .(y, m, ym_num, prov)]
ds2 = ds2[, .(n = sum(n)), .(y, m, ym_num, prov)]
ds3 = ds3[, .(n = sum(n)), .(y, m, ym_num, prov)]

ym_num_covid = 1398 + 10/12 - 1/24
# to avoid dealing with non-linear patterns we only look at the last 5 years
years_num = 7
ym_num_start = ym_num_covid - years_num+2

provs = unique(ds1$prov)
table = data.table()
tab = data.table()

Q4table1 = data.table()
Q4tab1 = data.table()

Q4table2 = data.table()
Q4tab2 = data.table()

Q4table3 = data.table()
Q4tab3 = data.table()

pvalue_threshold = 0.1

mycolor = colorRampPalette(c("blue","green", "yellow", "red"))

for (p  in 1:length(provs)){
  p=30
  PROV = provs[p]
  for (M  in 1:12){
    dsm1 = ds1[prov == PROV & m == M,]
    dsm1 = dsm1[ym_num > ym_num_start]
    dsm12fit = dsm1[ym_num < ym_num_covid]
    fit1 = lm(n ~ ym_num, dsm12fit)
    
    dsm2 = ds2[prov == PROV & m == M,]
    dsm2 = dsm2[ym_num > ym_num_start]
    dsm22fit = dsm2[ym_num < ym_num_covid]
    fit2 = lm(n ~ ym_num, dsm22fit)
    
    dsm3 = ds3[prov == PROV & m == M,]
    dsm3 = dsm3[ym_num > ym_num_start]
    dsm32fit = dsm3[ym_num < ym_num_covid]
    fit3 = lm(n ~ ym_num, dsm32fit)
    
    pValue = summary(fit1)$coefficients[2,4]

    if(pValue<=pvalue_threshold){
      dsm1$n_predicted = predict(fit1 ,dsm1)
      std1 = summary(fit1)$sigma
    } else {
      dsm1$n_predicted = mean(dsm12fit$n)
      std1 = sd(dsm12fit$n)
    }
    
    pValue = summary(fit2)$coefficients[2,4]
    
    if(pValue<=pvalue_threshold){
      dsm2$n_predicted = predict(fit2 ,dsm2)
      std2 = summary(fit2)$sigma
    } else {
      dsm2$n_predicted = mean(dsm22fit$n)
      std2 = sd(dsm22fit$n)
    }
    
    pValue = summary(fit3)$coefficients[2,4]
    
    if(pValue<=pvalue_threshold){
      dsm3$n_predicted = predict(fit3 ,dsm3)
      std3 = summary(fit3)$sigma
    } else {
      dsm3$n_predicted = mean(dsm32fit$n)
      std3 = sd(dsm32fit$n)
    }
    for(i in 1:years_num){
      dsm1$excessMortality[i] = 0
      dsm2$excessMortality[i] = 0
      dsm3$excessMortality[i] = 0
      
      if(abs(dsm1$n[i]-dsm1$n_predicted[i])>2*std1){
        dsm1$excessMortality[i] = abs(dsm1$n[i]-dsm1$n_predicted[i])
      }
      
      if(abs(dsm2$n[i]-dsm2$n_predicted[i])>2*std2){
        dsm2$excessMortality[i] = abs(dsm2$n[i]-dsm2$n_predicted[i])
      }
      
      if(abs(dsm3$n[i]-dsm3$n_predicted[i])>2*std3){
        dsm3$excessMortality[i] = abs(dsm3$n[i]-dsm3$n_predicted[i])
      }
        
    }

    
    Q4tab1 = dsm1[ , c("y","m", "ym_num", "prov", "n_predicted", "excessMortality")]
    Q4table1 = rbindlist(list(Q4table1, Q4tab1)) 
    
    Q4tab2 = dsm2[ , c("y","m", "ym_num", "prov", "n_predicted", "excessMortality")]
    Q4table2 = rbindlist(list(Q4table2, Q4tab2)) 
    
    Q4tab3 = dsm3[ , c("y","m", "ym_num", "prov", "n_predicted", "excessMortality")]
    Q4table3 = rbindlist(list(Q4table3, Q4tab3)) 
    
    tab = dsm1[ , c("y","m", "ym_num", "prov", "n_predicted", "excessMortality")] 
    tab$excessMortality = tab$excessMortality+dsm2$excessMortality+dsm3$excessMortality
    table = rbindlist(list(table, tab)) 
  }
  
  ggplot(table[prov==PROV], aes(x=y, y=m))+                          
    geom_tile(aes(fill = excessMortality))+
    scale_fill_gradientn(colors = mycolor(100))+
    ggtitle(label = PROV)

}

ggplot(table, aes(ym_num, prov))+                          
  geom_tile(aes(fill = excessMortality))+
  scale_fill_gradientn(colors = hcl.colors(50, "RdGy")) 


################### Question2

table_country = table[, .(excessMortality = sum(excessMortality)), .(y,m,ym_num)]

ggplot(table_country, aes(x=y, y=m))+                          
  geom_tile(aes(fill = excessMortality))+
  scale_fill_gradientn(colors = mycolor(100))+
  ggtitle(label = "Iran")

ggplot(table_country)+
  geom_point(aes(ym_num, excessMortality), size = 2, fill="red",colour="red")+
  scale_x_continuous(breaks = 1389:1400)+
  geom_vline(xintercept = 1398 + 10/12 -1/24, linetype = 'dashed')+
  ggtitle(label = "Iran")

ggplot(data=table_country, aes(x=y, y=excessMortality, fill=m)) +
  geom_bar(stat = "identity")+
  scale_fill_gradientn(colors = hcl.colors(120, "Inferno"))+
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle(label = "Iran")

################### Question3

table_prov = table[, .(excessMortality = sum(excessMortality)), .(y,prov)]

ggplot(table_prov, aes(x = prov, y = excessMortality)) +
  geom_bar(aes(color = y, fill = y),stat = "identity", position = position_dodge(0.8),width = 0.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(table_prov, aes(prov,excessMortality)) +
  geom_linerange(aes(x = prov, ymin = 0, ymax = excessMortality, group = y),
                 color = "lightgray", position = position_dodge(0.3))+
  geom_point(aes(color = y),position = position_dodge(0.3), size = 3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

################### Question4

t1 = Q4table2[, .(n_predicted = sum(n_predicted)), .(prov)]

t2 = Q4table2[, .(excessMortality = sum(excessMortality)), .(prov)]

t2$ratio = t2$excessMortality/t1$n_predicted

   ggplot(t2)+
     geom_smooth(aes(prov, 1-ratio), method = 'lm')+
     geom_point(aes(prov, 1-ratio), size = 2 , fill="red",colour="red")+
     ggtitle(label = "The performance of the provinces in controlling Corona")+
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
   
t1 = Q4table3[, .(n_predicted = sum(n_predicted)), .(prov)]
   
t2 = Q4table3[, .(excessMortality = sum(excessMortality)), .(prov)]
   
t2$ratio = t2$excessMortality/t1$n_predicted
   
   ggplot(t2)+
     geom_smooth(aes(prov, 1-ratio), method = 'lm')+
     geom_point(aes(prov, 1-ratio), size = 2 , fill="red",colour="red")+
     ggtitle(label = "The performance of the provinces in controlling Corona")+
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

t1 = Q4table1[, .(n_predicted = sum(n_predicted)), .(prov)]
   
t2 = Q4table1[, .(excessMortality = sum(excessMortality)), .(prov)]
   
t2$ratio = t2$excessMortality/t1$n_predicted
   
   ggplot(t2)+
     geom_smooth(aes(prov, 1-ratio), method = 'lm')+
     geom_point(aes(prov, 1-ratio), size = 2 , fill="red",colour="red")+
     ggtitle(label = "The performance of the provinces in controlling Corona")+
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))