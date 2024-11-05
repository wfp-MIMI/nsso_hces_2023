

## Testing sensitivity of food consumption+composition
# Leave-on-out

# Loading packages
library(dplyr)
library(ggplot2)


# Loading data

data.df <- read.csv(here::here("data", "nsso_subset_lucia.csv"))
names(data.df)

#food_list <- readxl::read_excel(here::here( "NSSO_INDB_20241023.xlsx"))


# Checking the data
unique(data.df$item_name)
length(unique(data.df$Item_Code))
length(unique(data.df$common_id))
data.df$item_name[data.df$Item_Code == "61"]
# Some mismatches between no. of unique items and codes
data.df %>% distinct(item_name, Item_Code) %>% count(item_name)
unique(data.df$Item_Code[data.df$item_name == "peas(Kg)"]) # peas has the same name different code, it may be dried vs fresh

# Selecting the nutrients 
#vars <- c("VITA_RAE", "VITB12", "FOLDE", "ZN", "FE")
vars <- names(data.df)[6:11]

# Getting the list of food sorted by least to most consumed (freq. (no. of HHs))
food_list <- data.df %>% filter(!is.na(quantity_100g)) %>%
  group_by(Item_Code, item_name) %>% 
  summarise(N = n(), 
            Mean_qty = mean(quantity_100g, na.rm = TRUE)) %>%
  arrange(N)

# A loop that generate a list of dataset w/ the mean, sd and median intakes
# excluding one food item for the variables selected

i = 1
test <- list()                   

for(i in 1:nrow(food_list)){
  
test[[i]] <- data.df %>% filter(!is.na(quantity_100g)) %>% 
  filter(!Item_Code %in% food_list[i,1 ]) %>% 
  group_by(common_id) %>%       
  summarise(across(vars,  list(Mean = mean, SD = sd,
                               Median = median), 
                na.rm = TRUE, .names = "{.fn}.{.col}")) %>% 
  # Adding a variable with the item excluded
  mutate(test_food = paste0(food_list[i,1 ], "_", gsub(" ", "", food_list[i,2])))

print(i)

}

# Adding the baseline (no food removed)
test[[nrow(food_list)+1]] <- data.df %>% filter(!is.na(quantity_100g)) %>% 
  # mutate(across(vars, ~as.numeric)) %>% 
  group_by(common_id) %>%       
  summarise(across(vars,  list(Mean = mean, SD = sd,
                               Median = median),
                   na.rm = TRUE, .names = "{.fn}.{.col}")) %>% 
  mutate(test_food = "baseline")

# Saving the output into spreadsheet
writexl::write_xlsx(test, 
  here::here("inter-output", paste0("sensitivity_output_", Sys.Date(), ".xlsx")))


#test1 <- test 

names(test[[1]])

# Getting the variables to compare
mean_vars <- grep("Mean.", names(test[[1]]), value = TRUE)
median_vars <- grep("Median.", names(test[[1]]), value = TRUE)

i =1
j=6

# Visualising the output 
test[[120]] %>% 
  ggplot() + geom_histogram(aes(Median.vita_mcg)) +
  geom_vline(xintercept = median(data.df$vita_mcg, na.rm = TRUE), colour = "red")

# Creating an empty dataframe
t_test <- as.data.frame(matrix( ncol = 4))

# A loop that generate a table with p-values for the comparison btween baseline and food excluded

for(i in 1:length(test)){
  
  x <- wilcox.test(as.numeric(unlist(test[[i]][, median_vars[j]])), as.numeric(unlist(test[[125]][, median_vars[j]])))
  
  t_test[i,] <- broom::tidy(x)
  t_test[nrow(t_test), "test_food"] <- paste0(food_list[i,1 ], "_", gsub(" ", "", food_list[i,2]))
  t_test[nrow(t_test), "test_nutrient"] <- mean_vars[j]
  
}

# Same loop for food and nutrient wilcox.test

t_test <- as.data.frame(matrix( ncol = 4))

for(i in 1:length(test)){

  for(j in 1:length(mean_vars)){
  
#x <- t.test(unlist(log(test[[i]][, mean_vars[j]])), unlist(log(test[[125]][, mean_vars[j]])))
x <- wilcox.test(as.numeric(unlist(test[[i]][, median_vars[j]])), as.numeric(unlist(test[[125]][, median_vars[j]])))

t_test[nrow(t_test)+1,]<- broom::tidy(x)
t_test[nrow(t_test), "test_food"] <- paste0(food_list[i,1 ], "_", gsub(" ", "", food_list[i,2]))
t_test[nrow(t_test), "test_nutrient"] <- median_vars[j]

  }
  
}


# Same loop for food and nutrient t.test (log-tranformed)

t_test <- as.data.frame(matrix( ncol = 4))

for(i in 1:length(test)){
  
  for(j in 1:length(mean_vars)){
    
    x <- t.test(log(as.numeric(unlist(test[[i]][, mean_vars[j]]))), 
                log(as.numeric(unlist(test[[125]][, mean_vars[j]]))))

    t_test[nrow(t_test)+1,]<- broom::tidy(x)
    t_test[nrow(t_test), "test_food"] <- paste0(food_list[i,1 ], "_", gsub(" ", "", food_list[i,2]))
    t_test[nrow(t_test), "test_nutrient"] <- median_vars[j]
    
  }
  
}

# Same loop for food and nutrient difference of means

diff_test <- as.data.frame(matrix( ncol = 1))

for(i in 1:length(test)){
  
  for(j in 1:length(mean_vars)){
    
    x <- mean((as.numeric(unlist(test[[125]][, mean_vars[j]])))) -mean(as.numeric(unlist(test[[i]][, mean_vars[j]])))
    
    diff_test[nrow(diff_test)+1,]<- x
    diff_test[nrow(diff_test), "test_food"] <- paste0(food_list[i,1 ], "_", gsub(" ", "", food_list[i,2]))
    diff_test[nrow(diff_test), "test_nutrient"] <- median_vars[j]
    
  }
  
}



