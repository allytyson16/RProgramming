Names <- c("Alice", "Victor", "Vincent", "Kenneth", "Ama", "Aawo", "Jael", 
           "Greg" ,"Edd", "Russell", "Emelia", "Cal", "Catthy", "Ray")

Gender <- c("F", "M", "M", "M", "F", "F", "F", 
            "M", "M", "M", "F","M","F","M")

Weight <- runif(14, min = 30, max = 90)
Height <- runif(14, min = 1.2 , max = 3.0)

BMI <- Weight/ Height^2


Classification <- function(BMI)
{
  if(BMI < 16)
  {
    return("Severe Thinness")
  }
  else if(16 <= BMI && BMI < 17)
  {
    return("Moderate Thinness")
  }
  else if(17 <= BMI && BMI < 18.5)
  {
    return("Mild Thinness")
  }
  else if(18.5 <= BMI && BMI < 25)
  {
    return("Normal")
  }
  else if(25 <= BMI && BMI < 30)
  {
    return("Overweight")
  }
  else if(30 <= BMI && BMI < 35)
  {
    return("Obese Class I")
  }
  else if(35 <= BMI && BMI < 40)
  {
    return("Obese Class II")
  }
  else
  {
    return("Obese Class III")
  }
}


Table <- data.frame(Names, Gender, Weight, Height, BMI)

