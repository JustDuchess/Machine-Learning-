setwd('C:\\Users\\emmaa\\OneDrive\\Desktop\\NIIT TERM 2 SLIDES\\Chapter 8')
employee_performance <- read.csv(file.choose())
str(employee_performance)

head(employee_performance, n = 10)
tail(employee_performance, n =10)

summary(employee_performance)

nrow(employee_performance)
ncol(employee_performance)
colnames(employee_performance)

table(employee_performance$Department)


table(employee_performance$Gender)
table(employee_performance$Age)






















































##library(ggplot2)
ggplot(employee_performance, aes(x = Monthly_Salary, y = Performance_Score,)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "Bar Plot", x = "Salary", y = "Employee performance")

ggplot(employee_performance, aes(x = Monthly_Salary, y = Performance_Score,)) +
  geom_histogram(stat = "identity", fill = 'blue') +
  labs(title = "Pie chart", x = "Salary", y = "Employee performance")

ggplot(employee_performance, aes(x =Monthly_Salary, y = Performance_Score)) +
  geom_line(color = "red") +
  labs(title = "Line Plot", x = "Salary", y = 'Performance') +
  theme_light()

ggplot(employee_performance, aes(x = Gender, y = Monthly_Salary,)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "Bar Plot", x = "Gender", y = "Salary")

##ggplot(employee_performance, aes(x = Age, y = Monthly_Salary,)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "Bar Plot", x = "Age", y = "salary")

  ggplot(employee_performance, aes(x = Job_Title, y = Monthly_Salary,)) +
    geom_bar(stat = "identity", fill = 'blue') +
    labs(title = "Bar Plot", x = "job title", y = "salary")
  
  ggplot(employee_performance, aes(x = Monthly_Salary, y = Education_Level,)) +
    geom_bar(stat = "identity", fill = 'blue') +
    labs(title = "Bar Plot", x = "Salary", y = "education")

  ggplot(employee_performance, aes(x = Resigned, y = Overtime_Hours,)) +
    geom_bar(stat = "identity", fill = 'blue') +
    labs(title = "Bar Plot", x = "resigned", y = "overtime")

##ggplot(employee_performance, aes(x = Department, y = Performance_Score,)) +
    geom_bar(stat = "identity", fill = 'blue') +
    labs(title = "Bar Plot", x = "department", y = "Employee performance")
 
    
    ggplot(employee_performance, aes(x = Gender, y = Performance_Score,)) +
      geom_bar(stat = "identity", fill = 'blue') +
      labs(title = "Bar Plot", x = "gender", y = "Employee performance")
    

##ggplot(employee_performance, aes(x = Age, y = Promotions,)) +
      geom_bar(stat = "identity", fill = 'blue') +
      labs(title = "Bar Plot", x = "age", y = "promotion")

##ggplot(employee_performance, aes(x = Job_Title, y = Resigned,)) +
      geom_bar(stat = "identity", fill = 'blue') +
      labs(title = "Bar Plot", x = "job", y = "resigned")
    
##ggplot(employee_performance, aes(x = Work_Hours_Per_Week, y = Projects_Handled,)) +
      geom_bar(stat = "identity", fill = 'blue') +
      labs(title = "Bar Plot", x = "projects", y = "work hours")
      
ggplot(employee_performance, aes(x = Age, y = Education_Level,)) +
        geom_bar(stat = "identity", fill = 'blue') +
        labs(title = "Bar Plot", x = "age", y = "education")

##ggplot(employee_performance, aes(x = Job_Title, y = Age,)) +
  geom_bar(stat = "identity", fill = 'blue') +
  labs(title = "Bar Plot", x = "job", y = "age")

##ggplot(employee_performance, aes(x = Job_Title, y = Age + Gender)) +
    geom_bar(stat = "identity", fill = 'blue') +
    labs(title = "Bar Plot", x = "job", y = "age_gender")
  
##ggplot(employee_performance, aes(x = Age, y = Sick_Days, fill = Gender)) +
      geom_bar(stat = "identity", fill = 'blue') +
      labs(title = "Bar Plot", x = "age", y = "sick days")

##ggplot(employee_performance, aes(x = Monthly_Salary, y = Performance_Score,)) +
        geom_bar(stat = "identity", fill = 'blue') +
        labs(title = "Bar Plot", x = "Salary", y = "Employee performance")
  
ggplot(employee_performance, aes(x = Age, y = Years_At_Company,)) +
          geom_bar(stat = "identity", fill = 'blue') +
          labs(title = "Bar Plot", x = "age", y = "years worked")

ggplot(employee_performance, aes(x = Age, y = Sick_Days, fill = Gender)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Bar Plot of Sick Days by Age and Gender", x = "Age", y = "Sick Days") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink", "Others" = "purple"))

ggplot(employee_performance, aes(x = Job_Title, y = Age, fill = Gender)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Age Distribution by Job Title and Gender", x = "Job Title", y = "Age") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink", "Others" = "purple"))
