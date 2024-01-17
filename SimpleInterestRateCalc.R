#Purpose: This code computes the compound interest by taking
# principle amount and interest rate using for loop.

#input
principle <- as.numeric(readline("Enter the principle amount: "))
int_rate <- as.numeric(readline("Enter annual interest rate in percent: "))
years <- as.numeric(readline("Enter the number of years of investment: "))

#Each year calculate interest rate earned on the principle amount.
#Computation
balance <- principle
count <- 1
while(count <= years)
{
  interestEarned <- balance * int_rate/100
  print(interestEarned)
  balance <- balance + interestEarned
  count <- count + 1
}

tot_interestEarned <- balance - principle

#Output
cat(sprintf("The principle amount $%.2f with %.2f interest rate yielded $%.2f earning after %d years.",principle,int_rate,tot_interestEarned,years))
