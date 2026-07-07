
setwd("/Users/tasosrigos/Desktop/Business Analytics/Statistics for Business Analytics II/Project 2")

library(readxl)

#loading the data
country_facts <- read_excel("stat BA II project II March 2026.xlsx", sheet = 1)
votes <- read_excel("stat BA II project II March 2026.xlsx", sheet = 2)
dictionary <- read_excel("stat BA II project II March 2026.xlsx", sheet = 3)
print(dictionary, n = 60)

nrow(country_facts) 
ncol(country_facts)
str(country_facts)

nrow(votes)
ncol(votes)
str(votes)

state_aggregates <- subset(country_facts, fips %% 1000 == 0)
print(state_aggregates [, c("fips", "area_name")], n=53)
nrow(state_aggregates)
# 51 state aggregrates and the national one

# we keep only the country only data 
countries <- subset(country_facts, fips %% 1000 != 0)

# unique candicates
sort(unique(votes$candidate))

# we keep only trump's votes
trump_votes <- votes[votes$candidate == "Donald Trump", ]

# we check for na fips
sum(is.na(trump_votes$fips))

# we have 10 countries without fips
trump_votes[is.na(trump_votes$fips), ]
# all 10 observations are for state New Hampshire

# we find the fips from the county sheet
countries[countries$state_abbreviation == "NH", c("fips", "area_name")]

nh_fips <- data.frame(county = c("Belknap", "Carroll", "Cheshire", "Coos", "Grafton", 
                                 "Hillsborough", "Merrimack", "Rockingham", "Strafford", "Sullivan"),
  fips = c(33001, 33003, 33005, 33007, 33009,
                   33011, 33013, 33015, 33017, 33019)
)

nh_rows <- is.na(trump_votes$fips) & trump_votes$state == "New Hampshire"
trump_votes$fips[nh_rows] <- nh_fips$fips[match(trump_votes$county[nh_rows], nh_fips$county)]
# check if we have na fimps in the trump votes dataset
sum(is.na(trump_votes$fips))
# zero na fips after manual mapping

trump_votes$trump_majority <- ifelse(trump_votes$fraction_votes > 0.50, 1, 0)

merged_df <- merge(countries, trump_votes[, c("fips", "fraction_votes", "trump_majority")], by = "fips", all = FALSE)

nrow(merged_df)
# 2721 countries merged
nrow(countries) - nrow(merged_df)
# 422 countries were lost



# lost counties by state
lost_fips <- setdiff(countries$fips, trump_votes$fips)
lost <- countries[countries$fips %in% lost_fips, ]
sort(table(lost$state_abbreviation), decreasing = TRUE)

prop.table(table(merged_df$trump_majority))

####### EDA ########

library(ggplot2)
library(gridExtra)
options(scipen = 999)
# theme for the plots of the report
report_theme <- theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title = element_text(face = "bold"))

# barplot of trump majority
fig1 <- ggplot(merged_df, aes(x = factor(trump_majority, labels = c("No", "Yes")))) +
  geom_bar(fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_text(stat = "count", aes(label = paste0("n = ", after_stat(count), " (", round(after_stat(count)/nrow(merged_df)*100, 1), "%)")), 
            vjust = -0.5, size = 4.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Trump Received More Than 50%", y = "Number of Counties") +
  report_theme

fig1

#ggsave("images/figure1.pdf", width = 7.1, height = 3, units = 'in', plot = fig1)

# density plot of the fractional vote
fig2 <- ggplot(merged_df, aes(x = fraction_votes)) +
  geom_density(fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_vline(xintercept = 0.50, linetype = "dashed", colour = "red", linewidth = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x = "Trump's Fraction of Votes", y = "Density") +
  report_theme

fig2
#ggsave("images/figure2.pdf", width = 7.1, height = 3, units = 'in', plot = fig2)


# population is very skewed, so we use log scale
p1 <- ggplot(merged_df, aes(x = PST045214)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(colour = "black", linewidth = 1) +
  scale_x_log10(labels = scales::comma) +
  labs(x = "Population (2014 estimate)", y = "Density", title = "(a)") +
  report_theme

p2 <- ggplot(merged_df, aes(x = POP060210)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(colour = "black", linewidth = 1) +
  scale_x_log10(labels = scales::comma) +
  labs(x = "Population per Square Mile", y = "Density", title = "(b)") +
  report_theme

fig3 <- grid.arrange(p1, p2, ncol = 2)
#ggsave("images/figure3.pdf", width = 7.1, height = 3, units = 'in', plot = fig3)

# race and ethnicities
p1 <- ggplot(merged_df, aes(x = RHI825214)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "White Not Hispanic (%)", y = "Density", title = "(a)") +
  report_theme

p2 <- ggplot(merged_df, aes(x = RHI225214)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "Black (%)", y = "Density", title = "(b)") +
  report_theme

p3 <- ggplot(merged_df, aes(x = RHI725214)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "Hispanic or Latino (%)", y = "Density", title = "(c)") +
  report_theme

p4 <- ggplot(merged_df, aes(x = POP645213)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "Foreign Born (%)", y = "Density", title = "(d)") +
  report_theme

fig4 <- grid.arrange(p1, p2, p3, p4, ncol = 2)
#ggsave("images/figure4.pdf", width = 7.1, height = 3.5, units = 'in', plot = fig4)


# education plot 
p1 <- ggplot(merged_df, aes(x = EDU685213)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "Bachelor's Degree or Higher (%)\nPersons Age 25+", y = "Density", title = "(a)") +
  report_theme

p2 <- ggplot(merged_df, aes(x = EDU635213)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "High School Graduate or Higher (%)\nPersons Age 25+", y = "Density", title = "(b)") +
  report_theme

p3 <- ggplot(merged_df, aes(x = AGE775214)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "Persons 65 and Over (%)", y = "Density", title = "(c)") +
  report_theme

p4 <- ggplot(merged_df, aes(x = AGE295214)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "People Under 18 (%)", y = "Density", title = "(d)") +
  report_theme

fig5 <- grid.arrange(p1, p2, p3, p4, ncol = 2)
#ggsave("images/figure5.pdf", width = 7.1, height = 3.5, units = 'in', plot = fig5)

# poverty and income plot
p1 <- ggplot(merged_df, aes(x = INC110213)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "Median Household Income ($)", y = "Density", title = "(a)") +
  report_theme

p2 <- ggplot(merged_df, aes(y = INC110213)) +
  geom_boxplot(fill = "grey70") +
  labs(y = "Median Household Income ($)", title = "(b)") +
  report_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p3 <- ggplot(merged_df, aes(x = PVY020213)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "Persons Below Poverty (%)", y = "Density", title = "(c)") +
  report_theme

p4 <- ggplot(merged_df, aes(y = PVY020213)) +
  geom_boxplot(fill = "grey70") +
  labs(y = "Persons Below Poverty (%)", title = "(d)") +
  report_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

fig6 <- grid.arrange(p1, p2, p3, p4, ncol = 2)
#ggsave("images/figure6.pdf", width = 7.1, height = 3.5, units = 'in', plot = fig6)

key_vars <- c("PST045214", "POP060210", "RHI825214", "RHI225214", 
              "RHI725214", "POP645213", "EDU685213", "EDU635213", 
              "AGE775214", "AGE295214", "INC110213", "PVY020213")

library(psych)
describe(merged_df[, key_vars ])
summary(merged_df[, key_vars ])

# housing 
p1 <- ggplot(merged_df, aes(x = HSG445213)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "grey70", colour = "black", linewidth = 0.3) +
  geom_density(linewidth = 0.6) +
  labs(x = "Homeownership Rate (%)", y = "Density", title = "(a)") +
  report_theme

p2 <- ggplot(merged_df, aes(y = HSG445213)) +
  geom_boxplot(fill = "grey70") +
  labs(y = "Homeownership Rate (%)", title = "(b)") +
  report_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p3 <- ggplot(merged_df, aes(x = HSG495213)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "grey70", colour = "black", linewidth = 0.3) +
  scale_x_log10(labels = scales::comma) +
  geom_density(linewidth = 0.6) +
  labs(x = "Median Housing Value ($)", y = "Density", title = "(c)") +
  report_theme

p4 <- ggplot(merged_df, aes(y = HSG495213)) +
  geom_boxplot(fill = "grey70") +
  labs(y = "Median Housing Value ($)", title = "(d)") +
  report_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

fig7 <- grid.arrange(p1, p2, p3, p4, ncol = 2)
#ggsave("images/figure7.pdf", width = 7.1, height = 3.5, units = 'in', plot = fig7)

describe(merged_df[, c("HSG445213", "HSG495213")])
summary(merged_df[, c("HSG445213", "HSG495213")])


###### bivariate eda #########
merged_df$trump_majority_f <- factor(merged_df$trump_majority, levels = c(0, 1), labels = c("No", "Yes"))
prop.table(table(merged_df$trump_majority))

bi_fill <- scale_fill_manual(values = c("grey70", "steelblue"))

#################################################

by(merged_df[c("PST045214", "POP060210")], merged_df$trump_majority_f, function(subset) sapply(subset, median))

# very skewed Wilcoxon
wilcox.test(PST045214 ~ trump_majority_f, data = merged_df)
wilcox.test(POP060210 ~ trump_majority_f, data = merged_df)

p1 <- ggplot(merged_df, aes(x = PST045214, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white") + bi_fill +
  scale_x_log10(labels = scales::comma) +
  labs(x = "Population (log scale)", y = "Density", fill = "Trump > 50%", title = "(a)") +
  report_theme + theme(legend.position = c(0.2, 0.85))

p2 <- ggplot(merged_df, aes(x = POP060210, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white", show.legend = FALSE) + bi_fill +
  scale_x_log10(labels = scales::comma) +
  labs(x = "Population per Square Mile (log scale)", y = "Density", title = "(b)") +
  report_theme

fig8 <- grid.arrange(p1, p2, ncol = 2)
#ggsave("images/figure8.pdf", width = 7.1, height = 3.5, units = 'in', plot = fig8)

####################################

p1 <- ggplot(merged_df, aes(x = RHI825214, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white") + bi_fill +
  labs(x = "White Not Hispanic (%)", y = "Density", fill = "Trump > 50%", title = "(a)") +
  report_theme + theme(legend.position = c(0.2, 0.85))

p2 <- ggplot(merged_df, aes(x = RHI225214, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white", show.legend = FALSE) + bi_fill +
  labs(x = "Black (%)", y = "Density", title = "(b)") +
  scale_x_continuous(limits = c(0, 50)) +
  report_theme

p3 <- ggplot(merged_df, aes(x = RHI725214, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white", show.legend = FALSE) + bi_fill +
  labs(x = "Hispanic or Latino (%)", y = "Density", title = "(c)") +
  scale_x_continuous(limits = c(0, 30)) +
  report_theme

p4 <- ggplot(merged_df, aes(x = POP645213, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white", show.legend = FALSE) + bi_fill +
  labs(x = "Foreign Born (%)", y = "Density", title = "(d)") +
  scale_x_continuous(limits = c(0, 25)) +
  report_theme

fig9 <- grid.arrange(p1, p2, p3, p4, ncol = 2)
#ggsave("images/figure9.pdf", width = 7.1, height = 3.5, units = 'in', plot = fig9)

# wilcocox tests
# all variables distributions are right-skewed, the mean in not a meause for central tendency
wilcox.test(RHI825214 ~ trump_majority_f, data = merged_df)
wilcox.test(RHI225214 ~ trump_majority_f, data = merged_df)
wilcox.test(RHI725214 ~ trump_majority_f, data = merged_df)
wilcox.test(POP645213 ~ trump_majority_f, data = merged_df)

# median by group
by(merged_df[c("RHI825214", "RHI225214", "RHI725214", "POP645213")], 
   merged_df$trump_majority_f, 
   function(subset) sapply(subset, median))
# mean by group
by(merged_df[c("RHI825214", "RHI225214", "RHI725214", "POP645213")], 
   merged_df$trump_majority_f, 
   function(subset) sapply(subset, mean))

# bachelor's degree skewness 1.61 wilcoxon
wilcox.test(EDU685213 ~ trump_majority_f, data = merged_df)

# high school graduate skewness -0.82 t-test
t.test(EDU635213 ~ trump_majority_f, data = merged_df)

# age 65+ skewness 0.81 t-test
t.test(AGE775214 ~ trump_majority_f, data = merged_df)

# age under 18 skewness 0.48 t-test
t.test(AGE295214 ~ trump_majority_f, data = merged_df)

p1 <- ggplot(merged_df, aes(x = EDU685213, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white") + bi_fill +
  labs(x = "Bachelor's Degree or Higher (%)", y = "Density", fill = "Trump > 50%", title = "(a)") +
  report_theme + theme(legend.position = c(0.75, 0.85))

p2 <- ggplot(merged_df, aes(x = EDU635213, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white", show.legend = FALSE) + bi_fill +
  labs(x = "High School Graduate or Higher (%)", y = "Density", title = "(b)") +
  report_theme

p3 <- ggplot(merged_df, aes(x = AGE775214, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white", show.legend = FALSE) + bi_fill +
  labs(x = "Persons 65 and Over (%)", y = "Density", title = "(c)") +
  report_theme

p4 <- ggplot(merged_df, aes(x = AGE295214, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white", show.legend = FALSE) + bi_fill +
  labs(x = "Persons Under 18 (%)", y = "Density", title = "(d)") +
  report_theme

fig10 <- grid.arrange(p1, p2, p3, p4, ncol = 2)
#ggsave("images/figure10.pdf", width = 7.1, height = 3.5, units = 'in', plot = fig10)

###############################
# median by group
by(merged_df[c("INC110213", "PVY020213")], merged_df$trump_majority_f, function(subset) sapply(subset, median))

# both skewed, use Wilcoxon
wilcox.test(INC110213 ~ trump_majority_f, data = merged_df)
wilcox.test(PVY020213 ~ trump_majority_f, data = merged_df)

p1 <- ggplot(merged_df, aes(x = trump_majority_f, y = INC110213, fill = trump_majority_f)) +
  geom_boxplot(show.legend = FALSE) + bi_fill +
  labs(x = "Trump > 50%", y = "Median Household Income ($)", title = "(a)") +
  report_theme

p2 <- ggplot(merged_df, aes(x = trump_majority_f, y = PVY020213, fill = trump_majority_f)) +
  geom_boxplot(show.legend = FALSE) + bi_fill +
  labs(x = "Trump > 50%", y = "Persons Below Poverty (%)", title = "(b)") +
  report_theme

fig11 <- grid.arrange(p1, p2, ncol = 2)
#ggsave("images/figure11.pdf", width = 7.1, height = 3.5, units = 'in', plot = fig11)



# homeownership t-test
t.test(HSG445213 ~ trump_majority_f, data = merged_df)

# housing value very skewed wilcoxon
wilcox.test(HSG495213 ~ trump_majority_f, data = merged_df)

p1 <- ggplot(merged_df, aes(x = trump_majority_f, y = HSG445213, fill = trump_majority_f)) +
  geom_boxplot(show.legend = FALSE) + bi_fill +
  labs(x = "Trump > 50%", y = "Homeownership Rate (%)", title = "(a)") +
  report_theme


p2 <- ggplot(merged_df, aes(x = HSG495213, fill = trump_majority_f)) +
  geom_density(alpha = 0.6, colour = "white") + bi_fill +
  labs(x = "Median Housing Value ($)", y = "Density", fill = "Trump > 50%", title = "(b)") +
  report_theme + theme(legend.position = c(0.75, 0.85))

fig12 <- grid.arrange(p1, p2, ncol = 2)
#ggsave("images/figure12.pdf", width = 7.1, height = 3.5, units = 'in', plot = fig12)

# spearman correlation between predictors

demographic_vars <- c(
  "PST045214", "PST040210", "PST120214", "POP010210",
  "AGE135214", "AGE295214", "AGE775214", "SEX255214",
  "RHI125214", "RHI225214", "RHI325214", "RHI425214",
  "RHI525214", "RHI625214", "RHI725214", "RHI825214",
  "POP715213", "POP645213", "POP815213", "EDU635213",
  "EDU685213", "VET605213"
)
id_cols <- c("fips", "area_name", "state_abbreviation", 
             "fraction_votes", "trump_majority", "trump_majority_f")
pred_vars <- setdiff(names(merged_df), id_cols)
economic_vars <- setdiff(pred_vars, demographic_vars)

cor_mat <- cor(merged_df[, pred_vars], method = "spearman")

# rename columns and rows with dictionary descriptions
dict_map <- setNames(dictionary$description, dictionary$column_name)
new_names <- ifelse(colnames(cor_mat) %in% names(dict_map), dict_map[colnames(cor_mat)], colnames(cor_mat))
colnames(cor_mat) <- new_names
rownames(cor_mat) <- new_names

# pairs with |r| > 0.50
pairs <- which(abs(cor_mat) > 0.50 & upper.tri(cor_mat), arr.ind = TRUE)
cor_pairs <- data.frame(
  var1 = rownames(cor_mat)[pairs[,1]],
  var2 = colnames(cor_mat)[pairs[,2]],
  r = round(cor_mat[pairs], 2)
)
cor_pairs <- cor_pairs[order(-abs(cor_pairs$r)), ]

# we need the original names to classify
orig_names <- dictionary$column_name[match(cor_pairs$var1, dictionary$description)]
orig_names2 <- dictionary$column_name[match(cor_pairs$var2, dictionary$description)]

cor_pairs$type <- ifelse(
  orig_names %in% demographic_vars & orig_names2 %in% demographic_vars, "Demographic",
  ifelse(orig_names %in% economic_vars & orig_names2 %in% economic_vars, "Economic", "Cross-group")
)

cor_pairs[cor_pairs$type == "Demographic", ]
cor_pairs[cor_pairs$type == "Economic", ]
cor_pairs[cor_pairs$type == "Cross-group", ]

################ CLASSIFICATION PART ##############################

library(caret)
library(MASS)
library(randomForest)
library(e1071)

drop_cols <- c("fips", "area_name", "state_abbreviation", 
               "fraction_votes", "trump_majority_f")
model_data <- merged_df[, !(names(merged_df) %in% drop_cols)]
model_data$trump_majority <- as.factor(model_data$trump_majority)
target_col <- which(names(model_data) == "trump_majority")

n <- nrow(model_data)

set.seed(1) # for reproducbility
# 5 folds, 2 times 
# 2 times supervised 10 fold cv
folds <- createMultiFolds(model_data$trump_majority, k = 5, times = 2)

methods <- c("logistic", "svm", "random_forest")
accuracy <- matrix(NA, nrow = length(methods), ncol = length(folds))
sensitivity <- matrix(NA, nrow = length(methods), ncol = length(folds))
specificity <- matrix(NA, nrow = length(methods), ncol = length(folds))
precision <- matrix(NA, nrow = length(methods), ncol = length(folds))
rownames(precision) <- methods
rownames(accuracy) <- methods 
rownames(sensitivity) <- methods 
rownames(specificity) <- methods

svm_best_params <- data.frame(cost = rep(NA, length(folds)), gamma = rep(NA, length(folds)))

selected_vars_logistic_regression <- list()
var_imp_gini <- matrix(0, nrow = ncol(model_data) - 1, ncol = length(folds))
var_imp_acc  <- matrix(0, nrow = ncol(model_data) - 1, ncol = length(folds))
rownames(var_imp_gini) <- rownames(var_imp_acc) <- names(model_data)[-target_col]
random_f_best_params <- data.frame(mtry = rep(NA, length(folds)), ntree = rep(NA, length(folds)))

calc_metrics <- function(cm, pos, neg) {
  acc <- sum(diag(cm)) / sum(cm)
  sens <- cm[pos, pos] / sum(cm[pos, ])
  spec <- cm[neg, neg] / sum(cm[neg, ])
  prec <- cm[pos, pos] / sum(cm[, pos])
  c(acc, sens, spec, prec)
}

# it runs for couple of minutes
for (f in 1:length(folds)) {
  
  train_idx <- folds[[f]]
  train <- model_data[train_idx, ]
  test  <- model_data[-train_idx, ]
  test_labels <- test$trump_majority
  pos <- levels(train$trump_majority)[2]
  neg <- levels(train$trump_majority)[1]
  
  # logistic regression
  full <- glm(trump_majority ~ ., data = train, family = binomial)
  step_model <- step(full, direction = "backward", trace = 0)
  pr_prob <- predict(step_model, newdata = test, type = "response")
  pr <- factor(ifelse(pr_prob > 0.5, pos, neg), levels = levels(train$trump_majority))
  cm <- table(Actual = test_labels, Predicted = pr)
  m <- calc_metrics(cm, pos, neg)
  accuracy["logistic", f] <- m[1]
  sensitivity["logistic", f] <- m[2]
  specificity["logistic", f] <- m[3]
  precision["logistic", f] <- m[4]
  selected_vars_logistic_regression[[f]] <- names(coef(step_model))[-1]
  
  # SVM
  tune_out <- tune(svm, trump_majority ~ ., data = train,
                   kernel = "radial",
                   ranges = list(cost = c(1, 10), gamma = c(0.01, 0.1)))
  svm_model <- tune_out$best.model
  pr <- predict(svm_model, newdata = test)
  cm <- table(Actual = test_labels, Predicted = pr)
  m <- calc_metrics(cm, pos, neg)
  accuracy["svm", f] <- m[1]
  sensitivity["svm", f] <- m[2]
  specificity["svm", f] <- m[3]
  precision["svm", f] <- m[4]
  svm_best_params[f, ] <- c(tune_out$best.parameters$cost, tune_out$best.parameters$gamma)
  
  # random forest 
  best_oob <- 1
  rf_model <- NULL
  best_m <- NA
  best_nt <- NA
  
  for (m_try in c(5, 7, 10)) {
    for (nt in c(100, 300, 500)) {
      rf_temp <- randomForest(trump_majority ~ ., data = train, 
                              mtry = m_try, ntree = nt, importance = TRUE)
      oob_err <- rf_temp$err.rate[nt, "OOB"]
      if (oob_err < best_oob) {
        best_oob <- oob_err
        rf_model <- rf_temp
        best_m <- m_try
        best_nt <- nt
      }
    }
  }
  
  pr <- predict(rf_model, newdata = test)
  cm <- table(Actual = test_labels, Predicted = pr)
  m <- calc_metrics(cm, pos, neg)
  accuracy["random_forest", f] <- m[1]
  sensitivity["random_forest", f] <- m[2]
  specificity["random_forest", f] <- m[3]
  precision["random_forest", f] <- m[4]
  var_imp_gini[, f] <- importance(rf_model)[, "MeanDecreaseGini"]
  var_imp_acc[, f]  <- importance(rf_model)[, "MeanDecreaseAccuracy"]
  random_f_best_params[f, ] <- c(best_m, best_nt)
  
  cat(names(folds)[f])
}

# mean accuracy 
round(rowMeans(accuracy), 4)
# SD of accuracy
round(apply(accuracy, 1, sd), 4)
# mean sensitivity
round(rowMeans(sensitivity), 4)
# mean specificity
round(rowMeans(specificity), 4)
# mean precision
round(rowMeans(precision), 4)

acc_df <- data.frame(
  Accuracy = as.vector(t(accuracy)),
  Method = rep(c("Logistic Regression", "SVM Radial Kernel", "Random Forest"), each = length(folds))
)
acc_df$Method <- factor(acc_df$Method, levels = c("Logistic Regression", "SVM Radial Kernel", "Random Forest"))

ggplot(acc_df, aes(x = Method, y = Accuracy)) +
  geom_boxplot(fill = "grey70") +
  labs(x = "", y = "Predictive Accuracy") +
  report_theme

#ggsave("images/boxplot_accuracy.pdf", width = 7, height = 4)


dict_map <- setNames(dictionary$description, dictionary$column_name)
var_counts_log_regr <- sort(table(unlist(selected_vars_logistic_regression)), decreasing = TRUE)
names_log <- dict_map[names(var_counts_log_regr)]
names(var_counts_log_regr) <- names_log

var_counts_log_regr

never_selected <- setdiff(names(model_data)[-target_col], names(var_counts_log_regr))
dict_map[never_selected]

# mapping for plot
short_names <- c(
  "LND110210" = "Land Area (sq mi)",
  "HSG495213" = "Median Housing Value ($)",
  "AGE295214" = "Persons Under 18 (%)",
  "RHI725214" = "Hispanic (%)",
  "RHI225214" = "Black (%)",
  "LFE305213" = "Mean Travel Time (min)",
  "EDU635213" = "High School Grad. (%)",
  "INC110213" = "Median Household Income ($)",
  "POP645213" = "Foreign Born (%)",
  "POP060210" = "Pop. per Sq Mile",
  "RHI325214" = "American Indian (%)",
  "POP815213" = "Non-English at Home (%)",
  "EDU685213" = "Bachelor's Degree (%)",
  "PST120214" = "Pop. Change (%)",
  "RHI825214" = "White not Hispanic (%)",
  "RHI125214" = "White Alone (%)",
  "RHI625214" = "Two or More Races (%)",
  "AGE775214" = "Persons 65+ (%)"
)
# Mean Decrease Gini (top 15)
avg_gini <- sort(rowMeans(var_imp_gini), decreasing = TRUE)
df_gini <- data.frame(Variable = short_names[names(head(avg_gini, 15))], 
                      Importance = head(avg_gini, 15))
df_gini$Variable <- factor(df_gini$Variable, levels = rev(df_gini$Variable))

p1 <- ggplot(df_gini, aes(x = Variable, y = Importance)) +
  geom_segment(aes(xend = Variable, y = 0, yend = Importance), colour = "grey70") +
  geom_point(size = 3, colour = "steelblue") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Gini", title = "(a)") +
  report_theme + theme(axis.text.y = element_text(size = 6))

# Mean Decrease Accuracy (top 15)
avg_acc <- sort(rowMeans(var_imp_acc), decreasing = TRUE)
df_acc <- data.frame(Variable = short_names[names(head(avg_acc, 15))], 
                     Importance = head(avg_acc, 15))
df_acc$Variable <- factor(df_acc$Variable, levels = rev(df_acc$Variable))

p2 <- ggplot(df_acc, aes(x = Variable, y = Importance)) +
  geom_segment(aes(xend = Variable, y = 0, yend = Importance), colour = "grey70") +
  geom_point(size = 3, colour = "steelblue") +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Accuracy", title = "(b)") +
  report_theme + theme(axis.text.y = element_text(size = 6))

fig14 <- grid.arrange(p1, p2, ncol = 2)
#ggsave("images/rf_importance.pdf", width = 7, height = 4, plot = fig14)
random_f_best_params
svm_best_params

############# Clustering part ###################

library(mclust)
# package for variable selection in model based clustering using bic
library(clustvarsel)

# spearman correlations among the 22 demographic variables
demo_cor <- cor(countries[, demographic_vars], method = "spearman")
pairs <- which(abs(demo_cor) > 0.70 & upper.tri(demo_cor), arr.ind = TRUE)
cor_demo <- data.frame(
  var1 = colnames(demo_cor)[pairs[,1]],
  var2 = colnames(demo_cor)[pairs[,2]],
  r = round(demo_cor[pairs], 2)
)
cor_demo[order(-abs(cor_demo$r)), ]

# We have to drop the unnecessary variables
# PST040210, POP010210, PST045214 have r = 1 (keep PST045214)
# VET605213 (veterans) r = 0.98 with PST045214 (drop VET605213)
# AGE135214 (under 5) r = 0.83 with AGE295214 (under 18), drop AGE135214
# RHI125214 (white alone) r = 0.85 with RHI825214 (white not hispanic), drop RHI125214
# POP815213 (non english speakers) has r = 0.86 with POP645213 (foreign born), drop POP815213

drop_corr <- c("PST040210", "POP010210", "VET605213",
               "AGE135214", "RHI125214", "POP815213")

cluster_16_variables <- setdiff(demographic_vars, drop_corr)
length(cluster_16_variables)

# model based with 16 variables
cluster_data_16_variables <- countries[, cluster_16_variables]

# we select a range of clusters from 2 to 5 in order to be interpretable
set.seed(1)
mb_cl_16 <- Mclust(cluster_data_16_variables, G = 2:5)
summary(mb_cl_16)
plot(mb_cl_16, what = "BIC")

table(mb_cl_16$classification)
round(aggregate(cluster_data_16_variables, by = list(Cluster = mb_cl_16$classification), FUN = mean), 1)

# From the cluster means above, 4 variables do not differ across the 5 clusters
# SEX255214 48.8 - 50.2 (the same in all clusters)
# RHI525214 0.0 - 1.0 (near zero in all clusterss)
# RHI625214 1.2 - 4.5 (small range)
# POP715213 84.3 - 88.5 (small varibibility)
set.seed(1)
bic_16 <- Mclust(cluster_data_16_variables, G = 5)$bic

# start with 16 variables
current_vars <- cluster_16_variables

# test SEX255214
set.seed(1)
bic_current <- Mclust(countries[, current_vars], G = 5)$bic
set.seed(1)
bic_without <- Mclust(countries[, setdiff(current_vars, "SEX255214")], G = 5)$bic
cat("SEX255214", round(bic_current), round(bic_without), round(bic_without - bic_current), "\n")
# positive so drop it
current_vars <- setdiff(current_vars, "SEX255214")

# test RHI625214 on 15 variables
bic_current <- bic_without
set.seed(1)
bic_without <- Mclust(countries[, setdiff(current_vars, "RHI625214")], G = 5)$bic
cat("RHI625214", round(bic_current), round(bic_without), round(bic_without - bic_current), "\n")
# positive, drop it
current_vars <- setdiff(current_vars, "RHI625214")

# test POP715213 on 14 variables
bic_current <- bic_without
set.seed(1)
bic_without <- Mclust(countries[, setdiff(current_vars, "POP715213")], G = 5)$bic
cat("POP715213", round(bic_current), round(bic_without), round(bic_without - bic_current), "\n")
#  drop it
current_vars <- setdiff(current_vars, "POP715213")

# test RHI525214 on 13 variables
bic_current <- bic_without
set.seed(1)
bic_without <- Mclust(countries[, setdiff(current_vars, "RHI525214")], G = 5)$bic
cat("RHI525214", round(bic_current), round(bic_without), round(bic_without - bic_current), "\n")
# negative, keep it

length(current_vars)
current_vars

cluster_data_13_variables <- countries[, current_vars]

# try to find the best number of clusters to interpreter
set.seed(1)
m_13_var_5_clust <- Mclust(cluster_data_13_variables, G = 5)
summary(m_13_var_5_clust)
table(m_13_var_5_clust$classification)
round(aggregate(cluster_data_13_variables, by = list(Cluster = m_13_var_5_clust$classification), FUN = mean), 1)

set.seed(1)
m_13_var_3_clust <- Mclust(cluster_data_13_variables, G = 3)
summary(m_13_var_3_clust)
table(m_13_var_3_clust$classification)
round(aggregate(cluster_data_13_variables, by = list(Cluster = m_13_var_3_clust$classification), FUN = mean), 1)

set.seed(1)
m_13_var_4_clust <- Mclust(cluster_data_13_variables, G = 4)
summary(m_13_var_4_clust)
table(m_13_var_4_clust$classification)
round(aggregate(cluster_data_13_variables, by = list(Cluster = m_13_var_4_clust$classification), FUN = mean), 1)
# 4 clusters are the most interpretable despite having worst icl and bic in comaprison with 5


# validate the fitted model
varidation_model <- clustvarsel(cluster_data_13_variables, G = 4, direction = "backward")
varidation_model$subset
# all the 13 variables are included, so we are good

# classification table
table(m_13_var_4_clust$classification)
summary(m_13_var_4_clust)
# log-likelihood
m_13_var_4_clust$loglik
# mean unceratianty
mean(m_13_var_4_clust$uncertainty)
summary(m_13_var_4_clust$uncertainty)
# how many countries have uncerataining over 20%
sum(m_13_var_4_clust$uncertainty > 0.20)

# validation split in order for clusters to show up again 
set.seed(1)
idx <- sample(nrow(cluster_data_13_variables), size = nrow(cluster_data_13_variables) / 2)

set.seed(1)
mc_a <- Mclust(cluster_data_13_variables[idx, ], G = 4)
set.seed(1)
mc_b <- Mclust(cluster_data_13_variables[-idx, ], G = 4)

table(mc_a$classification)
round(aggregate(cluster_data_13_variables[idx, ], by = list(Cluster = mc_a$classification), FUN = mean), 1)

table(mc_b$classification)
round(aggregate(cluster_data_13_variables[-idx, ], by = list(Cluster = mc_b$classification), FUN = mean), 1)

# interpretentation with economic variables

table(m_13_var_4_clust$classification)
round(aggregate(cluster_data_13_variables, by = list(Cluster = m_13_var_4_clust$classification), FUN = mean), 1)

countries$cluster <- m_13_var_4_clust$classification

t(round(aggregate(countries[, economic_vars],
                  by = list(Cluster = countries$cluster),
                  FUN = mean, na.rm = TRUE), 1))

countries$cluster_f <- factor(countries$cluster, 
                              labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"))

econ_bar_vars <- c("INC110213", "PVY020213", "HSG445213", "HSG495213", 
                   "HSG096213", "LFE305213", "RTN131207", "SBO015207")
econ_bar_names <- c("Median Household Income ($)", "Poverty (%)", 
                    "Homeownership Rate (%)", "Median Housing Value ($)",
                    "Multi-unit Housing (%)", "Mean Travel Time (min)",
                    "Retail Sales per Capita ($)", "Women-owned Firms (%)")

plots <- list()
for (i in 1:length(econ_bar_vars)) {
  df <- aggregate(countries[, econ_bar_vars[i]], 
                  by = list(Cluster = countries$cluster_f), FUN = mean, na.rm = TRUE)
  colnames(df) <- c("Cluster", "Value")
  
  plots[[i]] <- ggplot(df, aes(x = Cluster, y = Value)) +
    geom_col(fill = "grey70") +
    labs(x = "", y = econ_bar_names[i]) +
    coord_flip() +
    report_theme
}

fig_econ1 <- grid.arrange(grobs = plots[1:4], ncol = 2)
#ggsave("images/econ_barplots1.pdf", width = 7, height = 4, plot = fig_econ1)

fig_econ2 <- grid.arrange(grobs = plots[5:8], ncol = 2)
#ggsave("images/econ_barplots2.pdf", width = 7, height = 4, plot = fig_econ2)



#  Voting in the clusters 
counties_votes <- merge(
  data.frame(fips = countries$fips, cluster = m_13_var_4_clust$classification),
  trump_votes[, c("fips", "fraction_votes", "trump_majority")],
  by = "fips", all.x = TRUE
)

round(tapply(counties_votes$trump_majority, counties_votes$cluster, mean, na.rm = TRUE), 3)
round(tapply(counties_votes$fraction_votes, counties_votes$cluster, mean, na.rm = TRUE), 3)

