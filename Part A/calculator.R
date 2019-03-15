#Name: Anbarasan
#StudentID: 1508153

# # Write a function that prompts a user to choose an operation between six
# # available operations: 1) Add, 2) Subtract, 3) Multiply, 4) Divide, 5) Factors
# # and 6) Prime number. The first four operations will ask user to provide two
# # numbers and add, subtract, multiply and divide them accordingly. The fifth
# # operation calculates the factors of a number and sixth operation checks if a
# # number is prime. Please save your code to a file named calculator.R. The
# # output should look something like this when the user runs the function:
# 
# [1] "******Simple R Calculator - Select operation: ******"
# [1] "1.Add"
# [1] "2.Subtract"
# [1] "3.Multiply"
# [1] "4.Divide"
# [1] "5.Factors"
# [1] "6.Prime"
# 
# Enter choice [1/2/3/4/5/6]: 4
# Enter first number: 20
# Enter second number: 4 #prompting the user to enter the second number [1] "20
# / 4 = 5"
# 
# 
# [1] "******Simple R Calculator - Select operation: ******"
# [1] "1.Add"
# [1] "2.Subtract"
# [1] "3.Multiply"
# [1] "4.Divide"
# [1] "5.Factors"
# [1] "6.Prime"
# Enter choice [1/2/3/4/5/6]: 5
# Enter the number: 120
# [1] "The factors of 120 are:"
# [1] 1
# [1] 2
# [1] 3
# [1] 4
# [1] 5
# [1] 6
# [1] 8
# [1] 10
# [1] 12
# [1] 15
# [1] 20
# [1] 24
# [1] 30
# [1] 40
# [1] 60
# [1] 120
# #prompting the user to select an operation
# #prompting the user to enter the input

#Main Function Calculator
calculator <-function()
{
  print("******Simple R Calculator - Select operation: ******")
  print("1.Add")
  print("2.Subtract")
  print("3.Multiply")
  print("4.Divide")
  print("5.Factors")
  print("6.Prime")
  
  inputChoice <- readinput()
  
  opertationtype <-c("+","-","*","/","f","p")
  if(inputChoice>0 & inputChoice<5)
  {
    value<-keyInTwoValues()
    statement =paste(value[[1]],opertationtype[[inputChoice]],value[[2]])
    result = eval(parse(text=statement))
    print(paste(statement,"=",result))
    
  }
  else if(inputChoice==5)
  {
    value<-keyInOneValue(inputChoice)
    factor_cal(value)
  }
  else if(inputChoice==6)
  {
    value<-keyInOneValue(inputChoice)
    if(checkPrime(value)=="TRUE")
    {
      print(paste(value,"is a prime number."))
    }
    else
    {
      print(paste(value,"is not a prime number."))
    }
  }
}

#sub-function [readinput]: to read input choice
readinput <- function()
{ 
  input <- readline(prompt="Enter choice [1/2/3/4/5/6]:")
  if(!grepl("^[1-6]+$",input))
  {
    return(readinput())
  }
  as.integer(input)
}

#sub-function [2Numbers]: to key in 2 numbers
keyInTwoValues <-function()
{
  value_1<-suppressWarnings(as.numeric(readline(prompt = "Enter first number:")))
  while(is.na(value_1))
  {
    value_1<-suppressWarnings(as.numeric(readline(prompt = "Only number, Enter first number:")))
  }
  
  value_2<-suppressWarnings(as.numeric(readline(prompt = "Enter second number:")))
  while(is.na(value_2))
  {
    value_2<-suppressWarnings(as.numeric(readline(prompt = "Only number, Enter second number:")))
  }
  value_X_Y <-c(value_1,value_2)
}

#sub-function [1Number]: to key in 1 number
keyInOneValue <-function(x)
{
  value_1<-suppressWarnings(as.numeric(readline(prompt = "Enter the number:")))
  if(x==5)
  {
  while(is.na(value_1)||value_1<0)
  {
    value_1<-suppressWarnings(as.numeric(readline(prompt = "Only postive number, Enter the number:")))
  }
  }
  else
  {
    while(is.na(value_1)||value_1<2)
    {
      value_1<-suppressWarnings(as.numeric(readline(prompt = "Number must be greater than 1, Enter the number:")))
    }
  }
  value_1
}

#sub-function [Factor] 
factor_cal <- function(x) 
{
  print(paste("The factors of",x,"are:"))
  for(i in 1:x) {
    if((x %% i) == 0) {
      print(i)
    }
  }
}

#sub-function [Prime]
checkPrime <-function(x)
{
  x == 2L || all(x %% 2L:max(2,floor(sqrt(x))) != 0)
}
