library(readxl)
library(lubridate)

######################### Liability Projections ################################
#Maximum gratuity
M<-2000000
#Demographic details
emp_data<-read_excel("Emp Data.xlsx",col_names = TRUE,sheet = 1)
str(emp_data)
emp_data<-data.frame(emp_data)
emp_data


#The Life Table
mortality<-read_excel("Emp Data.xlsx",col_names = TRUE,sheet = 2)
str(mortality)
mortality<-data.frame(mortality,row.names = 1)

#Actuarial Assumptions
assump<-read_excel("Emp Data.xlsx",col_names = FALSE,sheet = 3)
str(assump)
assump<-data.frame(assump)
assump

# Calculations carried out on employee data
#Age on relevant date
emp_data$age<-floor(as.numeric(difftime(emp_data$Relevant...Date,emp_data$DATE.OF.BIRTH,units = "days")/365.242))
#Years of service
emp_data$PS<-round(lubridate::time_length(difftime(emp_data$Relevant...Date,emp_data$date.of.joining,units = "weeks")+1,"years"))



#Individual gratuity calculations

Individual_calc<-function(curr)
{
  #'curr' is the current employee whose benefits are being computed 
  duration<-seq(0,43)
  #Extracting Employee information from curr
  sal=as.numeric(curr$Total.Salary.for.Gratuity)
  PS=as.numeric(curr$PS)
  age_r=as.numeric(curr$age)
  attrit<-as.numeric(assump[4,2])
  
#curr_calc<-as.data.frame(duration,row.names = 1)
#Age of the employee through the period of projection
age<-age_r+duration
#Years of service
service<-duration+PS
#mortlity as per the life table applicable to the employee
mort<-mortality[age-13,1]
mort<-mort[1:43]
#Expected Salary increments
sal_inc<-(1+assump[3,2])^duration
#Discount rates for duration
disc<-(1+assump[1,2])^-duration

Gratuity_Benefit<-pmin(M,15/26*sal*PS*sal_inc)
#Probability that the employee doesnot leave the company, projection year wise
surv<-rep(1,44)
#Attrition and death are exclusive  
for (i in 2:44)
{
  surv[i]=surv[i-1]*(1-mort[i-1]-attrit)
}
surv

#The PV of future benefit payable if the benefit payable is 1 unit per year.
d_l<-(mort*(1-attrit)+attrit)*disc*surv
d_l
#Present Value of the benefit upon maturity if benefits payable was 1 unit per year.   
maturity<-rep(0,44)
for (i in 1:43)
{
 if (duration[i]+age_r==assump[2,2]-1)
   maturity[i]=surv[i+1]*disc[i]

 }  
maturity
#Scaling for a given salary
Gratuity_Benefit[duration+age_r>=62]<-0
total=(maturity+d_l)*Gratuity_Benefit

mul=rep(1,44)
#mul[age>=assump[2,2]]<-0

PSL_with<-Gratuity_Benefit*disc*attrit*surv
PSL_with=mul*PSL_with
PSL_death<-Gratuity_Benefit*disc*(1-attrit)*mort*surv
PSL_death=mul*PSL_death
PSL_death+PSL_with
return(cbind(total,PSL_death,PSL_with))

}

future_ex_benefit=rep(0,length(emp_data$SL.NO))
Projections<-NULL

for (emp in 1:length(emp_data$SL.NO))
{
  o<-Individual_calc(emp_data[emp,])
  future_ex_benefit[emp]=sum(o[,1]) 
  PSL_death=o[,2]
  PSL_with=o[,3]
  Projections=cbind(Projections,o[,2]+o[,3])
}

future_ex_benefit
Projections

Liability=rep(0,44)

for(i in 1:44)  
  Liability[i]<-sum(Projections[i,])
  
plot(0:43,Liability,type = "l")

