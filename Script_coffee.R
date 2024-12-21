
library(naniar) # For the analysis of missing data
library(e1071) # To calculate skewness
library(corrplot) # Correlation heatmap
library(dplyr) # Correlation
library(car) #VIF
library(ggplot2)
library(visdat)
library(maps)
library(mice)
library(Rtsne)
library(stats)
library(GGally)
# Import data
data = read.csv('EDAV_coffee_data.csv')

# Preliminary analysis
dim(data) # 945 rows 29 columns
summary(data)
head(data) # Altitude covariate is illegible. Various units, missing and inaccurate recors idicating big ranges of altitudes 
          # Further investigation of related altitude variables is needed


# Analysis of missing values

  # Missingness of Coffee data
data %>% abbreviate_vars(min_length = 10) %>% vis_miss(show_perc_col = FALSE) + labs(title = "Missingness of Coffee data")

gg_miss_upset(data) # There are 157 rows with missing data. All of them lack records in the three altitude covariates- Altitude_low/high/medium, which represent identical data.


gg_miss_fct(data, Country.of.Origin) + labs(title = "Missingness of Coffee data w.r.t. Country of Origin") # Most of the missing Altitude_low/high/medium data are from Hawaii and Peru - the vales of 
# altitude_low_meters  appear to follow MAR or MNAR
data %>%
  group_by(Country.of.Origin) %>%
  miss_var_summary() %>%
  filter(variable == "altitude_low_meters")
 
peru <- data[which(data$Country.of.Origin == 'Peru'),] 
altitude_proc_missing_Peru = nrow(peru[is.na(peru$altitude_low_meters),]) / nrow(peru) # 75% of records from Hawaii lack information about altitude 


Hawaii = data[which(data$Country.of.Origin == 'United States (Hawaii)'),]
altitude_proc_missing_Hawaii= nrow(Hawaii[is.na(Hawaii$altitude_low_meters),])/ nrow(Hawaii)  # 97% of records from Hawaii lack information about altitude- Data MAR or MNAR
 
 
 
# Handling missing data
 
  # Assign mean of the column to the single MCAR value
  data$Quakers[is.na(data$Quakers)] <- round(mean(data$Quakers, na.rm = TRUE)) 
  
  # Remove 2 records where both Harvest.Year and Altitude are Missing At Random. Both records are from the same plantation,
  # but given the insgnifficant number of missing values and untidy categorical variable 'Harvest.Year', listwise deletion will not have much impaxt on the overall statistics.
  data <- data[!is.na(data$Altitude),] 

  # Stochastic imputation method Predictive Mean Matching (PMM)
  # Correlation marix of altitude covarirates
  data_with_altitudes_and_outlier = data
  Altitude_numeric_covariates = cor(data.frame(data$altitude_high_meters, data$altitude_low_meters, data$altitude_mean_meters), use = "pairwise.complete.obs") # perfect correlation
  data_one_altitude_cov = subset(data, select = -c(altitude_low_meters, altitude_high_meters, Altitude)) # Delete multicollinaear variables to enable imputation of missing data. 
  imp <- mice(data_one_altitude_cov, seed = 3, method = c("pmm"), m = 5, maxit = 5)
  xyplot(imp, altitude_mean_meters ~ Total.Cup.Points + Cupper.Points + Sweetness, par.settings = list(superpose.symbol = list(
    col = c("pink", "blue"), pch=16)))
  # Update data frame with imputed data
  data<- complete(imp, action=1) 
  summary(data)

  

# Correlation between covariates Altitude_high/low/medium_meters and all other variables is approximately 0.

# Detecting outliers
  
  # Scatterplot Total Cup Points vs Flavour
  
  ggplot(data = data, mapping = aes(x = Total.Cup.Points, y = Flavour)) +geom_point() 
  length(unique(data$Flavour)) # Data points form horizontal lines as Favour variable has only 32 unique values
  data <- data[which(data$Total.Cup.Points != 0),] # Remove the recod where majority of covariates (incl. Total Cup Poins) is equal to 0
  
  # Modified Z-socre in numerical data

  # Subset of numeric data without coordinates
  numerical_data <- subset(numeric_covariates, select = -c(Latitude, Longitude))
  for(i in colnames(numerical_data)){
    med <- median(numerical_data[[i]])
    MAD <- median(abs(numerical_data[[i]] - med))
    
    m <- 0.6745 * (numerical_data[[i]] - med) / MAD
    outliers <- sum(abs(m) > 3.5, na.rm = TRUE)
    no_outliers_accross_covariates <- paste("Covariate ", i, "has ", outliers , "outliers")
    print(no_outliers_accross_covariates)# There are 44 outliers in the Total Cup Points
    
  }

  # QQ-plot of Total.Cup.Points
  qqnorm(data$Total.Cup.Points) # Non-Gaussian distribution as the scatterplot does not assume the shape of the diagonal line
                                # Log-transformation is in this case redundant.

  ggplot(data.frame(x = data$Total.Cup.Points), aes(x=x)) + geom_density()
  hist(data$Total.Cup.Points,100) # The lower tail of the distribution is longer. There is a slight asimmetric distribution
  # The asymmetry of the density of log-normal distributions means standard techniques for identifying outliers such as boxplots are 
  #not suitable. This is because the underlying assumption behind boxplots is that the spread of the data should be symmetric 
  #around its median.

gamma_YK <- function(x){
  summ = as.numeric(summary(x))
  num = (summ[5]-summ[3])-(summ[3]-summ[2])
  denom = summ[5] - summ[2]
  num/denom}

gamma_YK(data$Total.Cup.Points) # Skewness in Non-Gaussian distribuation might be biased, therefore we calculate Yule-Kendall indeces and its log-transformed variant.

# For the Total Cup points measurements in the coffee dataset, we see that the sample skewness estimate indicates negative skewness, which is visible in the corresponding histogram.
gamma_YK(log(data$Total.Cup.Points)) # Log-transformation does not remove the skewness


# MULTIVARIATE ANALYSIS

# Group the data by Country.Of.Origin and calculate the mean of Cupper Points of each group
gp <- data %>%
  group_by(Country.of.Origin) %>%
  summarize( mean_total_cup_points = mean(Cupper.Points))

# Create the barplot with fill based on Mean of cupper points
ggplot(gp, aes(x = Country.of.Origin, y = mean_total_cup_points)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "blue", high = "red") +  # Gradient color scale
  labs(x = "Country of Origin", y = "Mean of Cupper Points", fill = "Total Cup Points") +
  theme_minimal()
# Haiti has the lowest mean on Cupper Points (6.97) and Ecuador has the highest (7.67)


numeric_covariates <- select_if(data_with_altitudes_and_outlier, is.numeric) # Create subset of numeric predictors
# Calculate the correlation matrix
cor_matrix <- cor(numeric_covariates, use = "pairwise.complete.obs")
# Create a heatmap
corrplot(cor_matrix, method = 'color', type = 'upper', 
         order = 'hclust', addCoef.col = 'black', 
         tl.col = 'black', tl.srt = 45, 
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200))

numeric_covariates <- select_if(data, is.numeric) # Create subset of numeric predictors
# Calculate the correlation matrix
cor_matrix <- cor(numeric_covariates, use = "pairwise.complete.obs")
# Create a heatmap
corrplot(cor_matrix, method = 'color', type = 'upper', 
         order = 'hclust', addCoef.col = 'black', 
         tl.col = 'black', tl.srt = 45, 
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200))

# Thre is a very strong correlation of:
# 1 between all the altitude covariates;
# 0.85 between Flavour and Aftertaste; 0.9 with the outlier
# 0.81 and 0.82 between Cupper Points and Flavour, Aftertaste respecively; 0.87 with the outlier
# and 0.82, 0.80, 0.80 between Total Cup Points and Flavour, Aftertaste, Cupper Points respectively. 0.89, 0.87, 0.86 with the outlier
# This requires further investigation in respect to the possible multicollinearity
# Strong negative correlation of -0.79 between latitude and longitude
# No correlation between covaruates related to altitude with any other covariate

# Spearman’s Correlation Coefficients in order to confrm previous findings about the presence of outliers
cor(rank(data$Flavour), rank(data$Aftertaste)) # Outliers - The coefficient does differ form cor()
cor(rank(data$Aftertaste), rank(data$Cupper.Points)) # Outliers - The coefficient does differ form cor()


# Calculate Variance Inflation Factors in order to analyse the data against multicollinearity
# Fit a linear regression model
lm_model_Aftertaste <- lm(Aftertaste ~ Cupper.Points + Balance , data = data)
lm_model_Flavour <- lm(Flavour ~ Aftertaste + Cupper.Points, data = data)

# Calculate VIF
vif_values_Aftertaste <- vif(lm_model_Aftertaste)
vif_values_Flavour <- vif(lm_model_Flavour)


# Print VIF values
print(vif_values_Aftertaste)
print(vif_values_Flavour) # No multicollinearity in the data. After removing the oulier, Flavour is not multicollinear with Aftertaste and Cupper.Points.



p <- ggplot(mapping= aes(data$Flavour, data$Total.Cup.Points)) 
p + geom_point() + geom_smooth(method = 'lm') + labs(x = "Flavour",   
                                                     y = "Total Cup Points ",    
                                                     title = "Plot of Total Cup Points against Flavour") 
#Overall, we can see an upward trend, suggesting that Flavour is positively correlated with Total Cup Points.

aftertaste_factor = as.factor(data$Aftertaste)

p + geom_point(aes(col = aftertaste_factor)) + 
  geom_smooth(aes(col = aftertaste_factor), method = 'lm') +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.justification = c(0, 1),
        legend.position = c(0, 0.3))  + 
  labs(x = "Flavour",y = "Total Cup Points",
       title = "Total Cup Points vs Flavour, grouped by Aftertaste category") # Simpson's paradox - in genenral Total Cup Points variable is
# positively correlated with Flavour; however, they are negalively correlated for two nowest values of Aftertaste




# SPATIAL ANALYSIS



# Get world map data
world_map <-map_data("world")
americas <- subset(world_map, region %in% c("USA","Brazil","Mexico", "Colombia", "Argentina", "Canada",
                                        "Peru","Venezuela","Chile","Guatemala","Ecuador", "Bolivia", "Cuba",
                                        "Honduras", "Paraguay", "Nicaragua","El Salvador", "Costa Rica", "Panama",
                                        "Uruguay",  "Jamaica",  "Trinidad and Tobago", "Guyana", "Suriname", "Belize",
                                        "Barbados", "Saint Lucia", "Grenada", "Saint Vincent and the Grenadines", 
                                        "Antigua and Barbuda", "Saint Kitts and Nevis"))

# Plot the world map
plot_spatial <- ggplot(data=data, aes(x = Longitude, y = Latitude)) + geom_point(color = 'blue') +
 geom_polygon(data = americas, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  coord_fixed( xlim = c(-180,0), ylim=c(-40,40)) 


# Plot the Cupper.Points of coffee across the contries
plot_spatial + geom_point(aes(colour = cut(Cupper.Points, c( 0,6, 7,8,9,10))),
                          size = 2) +
  scale_color_manual(name = "Cupper.Points",
                     values = c(
                                "(0,6]" = "brown",
                                "(6,7]" = "red",
                                "(7,8]" = "pink",
                                "(8,9]" = "orange",
                                "(9,10]" = "yellow"),
                                
                     labels = c("0 < Cupper.Points <= 6", "6 < Cupper.Points <= 7", "7 < Cupper.Points <= 8", '8 < Cupper.Points <= 9 ','8 < Cupper.Points'))

# Aftertaste has been rated as better colser to the equator.
# It would be good to complete this analysis and confirm the trend by fitting a linear model and draw the residuals into the same map





 # CLUSTERING and FURTHER DIMENSION REDUCTION

# In order to reduce dimensionaly of numeric covaratietes, t-SNE embedding will be implemented, 
# which is capable of preserving local and global structure of the data, 
# so that points that are close together in the original dimension tend to be close also in the low dimension.

# Numerical predictors without Lat and Long and without Total.Cup.Points
numerical_predictors <- numerical_data #subset(numerical_data, select = -c(Total.Cup.Points, Cupper.Points))
numerical_predictors
nn <- select_if(data, is.numeric)




# Generalised pairs plot for the numerical predictors
ggpairs(data = nn,
        progress = FALSE, # suppress verbose progress bar output
        lower = list(continuous = wrap('points', alpha=0.2)))

# We can recognise three clusters in all scatterplots containg Moisture covariate.
# there are still some clusters masking one another

# Visualise of density estimates using contour plots.
ggplot(numerical_predictors, aes(x=Aftertaste, y=Balance)) + 
  geom_density_2d_filled()

set.seed(202)
tsne_out <- Rtsne(nn, pca=FALSE,perplexity=30,theta=0.0) # Run TSNE

Y1 <- tsne_out$Y[,1]
Y2 <- tsne_out$Y[,2]

Y_df <- data.frame(Y1,Y2)

# K-means clustering 
coffee_km2 <- kmeans(tsne_out$Y[,1:2], centers=4)
tsne_km_df <- bind_cols(Y_df,cluster=as.factor(coffee_km2$cluster))

ggplot(tsne_km_df, aes(x=Y1,y=Y2)) +
  geom_point(mapping = aes(colour=cluster)) +
  geom_density_2d(alpha=0.4) + 
  xlab("New representation of covariates Y1") + ylab("New representation of covariates Y2")

# Select number of clusters
# Plot the total within-cluster sum of squares (WCSS) 
WCSS_vec <- c()
for(k in 1:9){
  coffee_km2_k <- kmeans(tsne_out$Y[,1:2], centers=k, nstart=30)
  WCSS_vec[k] <- coffee_km2_k$tot.withinss
}

ggplot(data.frame(k=1:9,WCSS_vec)) + 
  geom_line(mapping=aes(x=k,y=WCSS_vec)) + geom_point(mapping=aes(x=k,y=WCSS_vec)) +
  ylab("WCSS")
#  k-means optimisation procedure coul de supprted with silouette analysis

library(cluster)

library(cluster)

library(cluster)

# Convert data to numeric
tsne_numeric <- as.data.frame(sapply(tsne_out$Y[, 1:2], as.numeric))


# Run kmeans clustering
coffee_km_k <- kmeans(tsne_numeric, centers = 5, nstart = 30)

# Compute silhouette widths
sil_width <- silhouette(coffee_km_k$cluster, dist(tsne_numeric))

# Get average silhouette width
avg_sil_width <- mean(sil_width[, "sil_width"])

# Print average silhouette width
print(avg_sil_width)





