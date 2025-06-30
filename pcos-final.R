library(dplyr)
library(caret)
library(ggplot2)
library(car)
library(ggcorrplot)
library(readxl)
library(writexl)
library(RColorBrewer)

# Reading and selecting data
pcos<-read_xlsx("/Users/quyenle/Desktop/stat1601/project/PCOS_data.xlsx")

pcos_cat<-pcos%>%
  select(c("Patient File No.", "Weight gain(Y/N)", "Hair loss(Y/N)", "Pimples(Y/N)", "Reg.Exercise(Y/N)", "PCOS (Y/N)", "Blood Group"))
pcos_numeric<-pcos%>%
  select(c("Patient File No.","Age (yrs)", "BMI", "Follicle No. (L)", "Follicle No. (R)", "Endometrium (mm)", "FSH(mIU/mL)", "LH(mIU/mL)", "Vit D3 (ng/mL)", "Hb(g/dl)", "Cycle length(days)", "Weight (Kg)", "Height(Cm)"))

pcos_data<-merge(pcos_cat, pcos_numeric)

# Renaming Columns
pcos_data<-pcos_data%>%
  rename("Hair_Loss"="Hair loss(Y/N)", "Acne"="Pimples(Y/N)", "Daily_Exercise"="Reg.Exercise(Y/N)", "PCOS"="PCOS (Y/N)",   "Weight_gain"="Weight gain(Y/N)", "Blood_Type"="Blood Group")

# Fixing Output to say words for categorical
pcos_data<-pcos_data%>%
  mutate("Hair_Loss"=ifelse(grepl("1", `Hair_Loss`), "Yes", "No"))%>%
  mutate("Acne"=ifelse(grepl("0", `Acne`), "No", "Yes"))%>%
  mutate("Daily_Exercise"=ifelse(grepl("1", `Daily_Exercise`), "Yes", "No"))%>%
  mutate("PCOS"=ifelse(grepl("1", `PCOS`), "Yes", "No"))%>%
  mutate("Weight_gain"=ifelse(grepl("1", `Weight_gain`), "Yes", "No"))

# Alterations to the data to make it easier to work with
pcos_data<-pcos_data %>%
  rename("Age"="Age (yrs)")%>%
  rename("Blood_Type" = "Blood_Type")%>%
  rename("FSH" = "FSH(mIU/mL)")%>%
  rename("cycle_length" = "Cycle length(days)")%>%
  rename("Hemoglobin" = "Hb(g/dl)")%>%
  rename("VitaminD" = "Vit D3 (ng/mL)")%>%
  rename("LH" = "LH(mIU/mL)")%>%
  rename("Follicle_L" = "Follicle No. (L)")%>%
  rename("Follicle_R" = "Follicle No. (R)")%>%
  rename("Endometrium"="Endometrium (mm)")%>%
  rename("Height"="Height(Cm)")%>%
  rename("Weight"="Weight (Kg)")%>%
  rename("Weight_gain"="Weight_gain")%>%
  rename("Hair_loss" = "Hair_Loss")%>%
  rename("Daily_Exercise" = "Daily_Exercise")

pcos_data = pcos_data %>%
  mutate(Age=as.numeric(Age))

# Adding a new column
pcos_data<-pcos_data%>%
  rowwise()%>%
  mutate("Average_Follicle"=sum(c_across("Follicle_R":"Follicle_L"))/2)

# Converting Blood Types (num) to actual blood type
pcos_data<-pcos_data%>%
  mutate("Blood_Type"=ifelse(Blood_Type == 11, "A+", ifelse(Blood_Type == 12, "A-", ifelse(Blood_Type == 13, "B+", ifelse(Blood_Type == 14, "B-", ifelse(Blood_Type == 15, "O+", ifelse(Blood_Type == 16, "O-", ifelse(Blood_Type == 17, "AB+","AB-"))))))))

# Grouping into Age Groups 
pcos_data <- pcos_data %>%
  mutate(Age = as.numeric(Age), 
         Age_Groups = case_when(
           Age >= 20 & Age <= 27 ~ "Age 20-27",
           Age >= 28 & Age <= 34 ~ "Age 28-34",
           Age >= 35 & Age <= 41 ~ "Age 35-41",
           Age >= 42 & Age <= 48 ~ "Age 42-48",
           TRUE ~ "Other"
         ))

# Numeric Summaries 
summary(pcos_data$Age)
summary(pcos_data$Average_Follicle)

pcos_data %>%
  group_by(Age_Groups) %>%
  summarize(avg_cycle = mean(cycle_length, na.rm = T))

pcos_data %>%
  group_by(PCOS) %>%
  summarize(avg_LH = mean(LH, na.rm = T))

# Categorical Summaries
table(pcos_data$PCOS, pcos_data$`Weight_gain`)
table(pcos_data$Blood_Type)
table(pcos_data$PCOS)

# Bar Graph of Blood Type
ggplot(pcos_data, aes(x=Blood_Type))+geom_bar(fill=brewer.pal(8, "Set3")) + labs(x="Blood Type of Patients", y="Number of Patients", title="Blood Type x Patients") + theme(plot.title= element_text(hjust = 0.5))

# Scatter Plot of Follicle L and R 

#creating correlation matrix
pcos_numeric<-pcos_data%>%
  select(c("Age", "Follicle_L", "Follicle_R", "VitaminD", "FSH","Endometrium", "LH", "Hemoglobin", "cycle_length", "Weight", "Height"))

cor_mat<-cor(pcos_numeric)

ggplot(pcos_data, aes(x=Follicle_L, y=Follicle_R)) + geom_point(color="indianred1") + labs(x="Number of Left Follicles", y="Number of Right Follicles", title="Scatter Plot of Left and Right Follicles") + theme(plot.title= element_text(hjust = 0.5))

# Creating scatter plot with PCOS
ggplot(pcos_data, aes(x=Follicle_L, y=Follicle_R, color=PCOS)) + geom_point()  + labs(x="Number of Left Follicles", y="Number of Right Follicles", title="Scatter Plot of Left and Right Follicles") + theme(plot.title= element_text(hjust = 0.5))

# Histogram of Age
ggplot(pcos_data, aes(x=Age)) + geom_histogram(bins=23, fill="darksalmon", color="white") + labs(x="Age of Patients", y="Number of Patients", title="Distribution of Age of Patients") + theme(plot.title= element_text(hjust = 0.5))

# Histogram of BMI
ggplot(pcos_data, aes(x=BMI)) + geom_histogram(fill="salmon", color="white") + labs(x="BMI of Patients", y="Number of Patients", title="Distribution of BMI of Patients") + theme(plot.title= element_text(hjust = 0.5)) 

# Side by Side Box Plot of PCOS and Cycle Length
ggplot(pcos_data, aes(x=PCOS, y=cycle_length)) + geom_boxplot(fill="pink", color="darksalmon") + coord_flip() + labs(y="Cycle Length (Days)", title = "PCOS x Cycle Length") + theme(plot.title= element_text(hjust = 0.5)) 

# HeatMap
ggplot(pcos_data, aes(x=Blood_Type, y=Age_Groups, fill=LH)) + geom_tile(color="white") + scale_fill_distiller(palette="RdPu", direction= 1) + labs(y="Age Groups",x= "Blood Type", title = "LH Levels by Age Group and Blood Type") + theme(plot.title= element_text(hjust = 0.5)) 

# Example MLR
model<-train(LH~+FSH+Age+Hemoglobin+cycle_length, pcos_data, method="lmStepAIC", trace=FALSE)
summary(model)

# Predicting using MLR 
MLRpred<-data.frame(FSH=4.96, Age=22, Hemoglobin=10, cycle_length=7)
predict(model, MLRpred)

# Example Logistic Regression
model2<-train(PCOS~cycle_length+Average_Follicle+FSH+BMI+Hemoglobin, pcos_data, method="glmStepAIC", trace=FALSE)
summary(model2)
vif(model2$finalModel)

# Predicting using Logistic Regression 
newdat<-data.frame(cycle_length=6, Average_Follicle=4, FSH=5, BMI=0.4, Hemoglobin=10)
predict(model2, newdat)

# KNN Model 
knn_model<-train(PCOS~FSH+Age+cycle_length+BMI+Average_Follicle, pcos_data, method = "knn" )

knn_model$finalModel
ggplot(knn_model)

# Predicting using KNN 
woman<-data.frame(FSH=4.96, Age=22, cycle_length=7, BMI=0.4, Average_Follicle=12)
predict(knn_model, woman)