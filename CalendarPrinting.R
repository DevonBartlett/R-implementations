#Comment the stuff/ you can add a catsprint for each month title/ you can try and make it a function of month and year
rm(list = ls())

#Asks for user to input a month. (1-12)
n <- as.numeric(readline("Enter the month as an integer "))

#Asks the user to input a year. 2019+
year <- as.numeric(readline("Enter the year "))

#Conditional statement for a new term 'month'. A vector containing
#                the number of days in each month. Every 4 years is leap year.
if(year%%4==0){
  month <- c(31,29,31,30,31,30,31,31,30,31,30,31)
}else{
  month <- c(31,28,31,30,31,30,31,31,30,31,30,31)
}

#This makes the basis of our code revert back to 2019.
setter_value <- (year-2019)

#Initial days is set to zero.
days <- 0

#This conditional + for loop determines the number of days in the inputted year.
if(year>2019){
  for (k in 1:setter_value) {
    days <- 365*k
  }
  days <- days + ((year-1)%/%4-504)
}

#This function determines the number of days in the inputted month.
current_days <- function(n){
  if (n==0){
    print("Zero is not a month")
  }else{
    nfact <- 0
    for(k in 1:n){
      nfact <- nfact+month[k]
    }
  }
  return(nfact)
}

#This makes sure that the code can understand the vector form of variable 'month'
current_month <- month[n]

#Creates a 4 by 7 matrix, with the numbers 1-28.
multi_table <- matrix(nrow = 4, ncol = 7,)
for(i in 1:1)
{
  for(j in 1:7)
  {
    multi_table[i,j] <- j
  }
}

for(i in 2:4)
{
  for(j in 1:7)
  {
    multi_table[i,j] <- j+(i-1)*7
  }
}

#This is the conditional statement that uses the previous variables
#     to determine the corrected starting day of inputted month.
if((days+current_days(n)+(month[1]- current_month)) %% 7 == 0){
  colnames(multi_table) <- c("Sat","Sun","Mon","Tue","Wed","Thr","Fri")
}else if((days+current_days(n)+(month[1]- current_month)) %% 7 == 1){
  colnames(multi_table) <- c("Sun","Mon","Tue","Wed","Thr","Fri","Sat")
}else if((days+current_days(n)+(month[1]- current_month)) %% 7 == 2){
  colnames(multi_table) <- c("Mon","Tue","Wed","Thr","Fri","Sat","Sun")
}else if((days+current_days(n)+(month[1]- current_month))  %% 7 == 3){
  colnames(multi_table) <- c("Tue","Wed","Thr","Fri","Sat","Sun","Mon")
}else if((days+current_days(n)+(month[1]- current_month))  %% 7 == 4){
  colnames(multi_table) <- c("Wed","Thr","Fri","Sat","Sun","Mon","Tue")
}else if((days+current_days(n)+(month[1]- current_month))  %% 7 == 5){
  colnames(multi_table) <- c("Thr","Fri","Sat","Sun","Mon","Tue","Wed")
}else if((days+current_days(n)+(month[1]- current_month))  %% 7 == 6){
  colnames(multi_table) <- c("Fri","Sat","Sun","Mon","Tue","Wed","Thr")
}
#Names each row to the week number.
rownames(multi_table) <- c("Week 1 |","Week 2 |","Week 3 |","Week 4 |")

if(n==1){
  print("January")
}else if(n==2){
  print("February")
}else if(n==3){
  print("March")
}else if(n==4){
  print("April")
}else if(n==5){
  print("May")
}else if(n==6){
  print("June")
}else if(n==7){
  print("July")
}else if(n==8){
  print("August")
}else if(n==9){
  print("September")
}else if(n==10){
  print("October")
}else if(n==11){
  print("November")
}else if(n==12){
  print("December")
}




#Displays the calendar.
print(multi_table)

#Because a black spot in a matrix is represented with an NA,
#        This code determine if the code has more than 28 days,
#        and prints off week 5.
if(current_month - 28 == 3){
  cat(sprintf("Week 5 |  29  30  31"))
}else if(current_month - 28 == 2){
  cat(sprintf("Week 5 |  29  30")) 
}else if(current_month - 28 == 1){
  cat(sprintf("Week 5 |  29"))
}
