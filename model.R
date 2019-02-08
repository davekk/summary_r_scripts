setwd("~/module2_R_biostats-master/kenny_work/Models/")

library("MASS")
library("readxl") # for reading excel files

#load the raw files
applicants <- read_xlsx("./20190201141343applicants.xlsx")
loans <- read_xlsx("./20190201141350loans.xlsx")
# names(loans) <- gsub(" ", "_",names(loans), fixed = TRUE) # replace all white spaces with underscore


model.loan <- glm(formula =loans$`Good Risk`~ loans$`Number of Missed/Late Payments` + loans$`Lines of Credit`+ loans$`Age at First Credit`+ loans$`Age in Years`+ loans$`Marital Status`, family = binomial,data = loans) # build a model based on all the suplied information
model.loan # view the model summary
summary(model.loan) #
# predicting the applicant risk score
prediction <- predict(model.loan,applicants, type="response")

fitted.results <- ifelse(prediction > 0.5,1,0)

misClasificError <- mean(fitted.results != loans$`Good Risk`)
print(paste('Accuracy',1-misClasificError))
