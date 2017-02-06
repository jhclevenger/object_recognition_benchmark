
library(tidyverse)
library(reshape2)


#### for readable output and plotting

names_rows = c('Person','Car','Airplane','Bird','Dog','Elephant',
               'Bear','Zebra','Fork','Knife','Bowl','Apple','Chair','Table')

names_cols = c("Per", "Car", "Air", "Bir", "Dog", "Ele", "Bea", "Zeb", "For" ,
               "Kni", "Bow", "App", "Cha", "Tab")


#### load data

# 75% accuracy data (31 ms)
data_31 = read.csv("~/desktop/muri_benchmark/benchmark_31.csv")
data_31$rt = as.numeric(data_31$rt)

# 60 ms data
data_60 = read_csv("~/desktop/muri_benchmark/benchmark_60.csv")

# 100 ms data
data_100 = read_csv("~/desktop/muri_benchmark/benchmark_100.csv")

# cv data
confusion_cv = 
  as.data.frame(read_csv("~/desktop/muri_benchmark/cv_confusion.csv", col_names = FALSE, na = c("NaN", "NA")))
colnames(confusion_cv) = names_cols
rownames(confusion_cv) = names_rows

#### get human descriptives

descriptives_31 = data_31 %>% 
  group_by(subject) %>% 
  summarise(trials = n(), 
            accuracy = mean(acc, na.rm = TRUE))

descriptives_60 = data_60 %>% 
  group_by(subject) %>% 
  summarise(trials = n(), 
            accuracy = mean(acc, na.rm = TRUE), 
            counterbalance = max(counterbalance))

descriptives_100 = data_100 %>% 
  group_by(subject) %>% 
  summarise(trials = n(), 
            accuracy = mean(acc, na.rm = TRUE), 
            counterbalance = max(counterbalance))


#### get category-level results

# 31 ms
target_category_31 = 
  data_31 %>%
  group_by(target_category) %>%
  summarise(accuracy = mean(acc), rt = mean(rt[acc == 1], na.rm = TRUE)) 

target_category_31$target_category = names_rows

# 60 ms
target_category_60 = 
  data_60 %>%
  group_by(target_category) %>%
  summarise(accuracy = mean(acc), rt = mean(rt[acc == 1], na.rm = TRUE)) 

target_category_60$target_category = names_rows

# 100 ms
target_category_100 = 
  data_100 %>%
  group_by(target_category) %>%
  summarise(accuracy = mean(acc), rt = mean(rt[acc == 1], na.rm = TRUE)) 

target_category_100$target_category = names_rows

# cv
category_cv = tibble(names_rows, rowMeans(confusion_cv, na.rm = TRUE))
colnames(category_cv) = c('category', 'accuracy')

# aggregate accuracy
category_accuracy = 
  tibble(names_rows, target_category_31$accuracy, target_category_60$accuracy, 
         target_category_100$accuracy, category_cv$accuracy)
colnames(category_accuracy) = 
  c('category', 'human_31', 'human_60', 'human_100', 'cv')

# aggregate rt
category_rt = tibble(names_rows, target_category_31$rt, target_category_60$rt, target_category_100$rt)
colnames(category_rt) = 
  c('category', 'human_31', 'human_60', 'human_100')


############ Correlate human and CV performance at the category level

# Correlate it
cor(scale(category_accuracy$human_31, center = TRUE, scale = TRUE), 
    scale(category_accuracy$cv, center = TRUE, scale = TRUE), method = "pearson")


# Plot it
theme_set(theme_minimal(base_size = 35))
our_color = "grey30"
theme = theme_update(axis.text = element_text(color = our_color),
                     axis.title = element_text(color = our_color),
                     plot.title = element_text(color = our_color))

# scatter plot
ggplot(category_accuracy, aes(x = human_100, y = cv)) + 
  geom_smooth(method = "lm", se = TRUE, color = '#00C0AF') +
  geom_point(shape = 19, alpha = .3, size = 4, color='#F8766D') + 
  scale_x_continuous(limits = c(.6, 1)) +
  scale_y_continuous(limits = c(.6, 1)) + 
  theme(legend.position = "none") + 
  xlab("Humans (31 ms)") + 
  ylab("CNN") +
  theme(aspect.ratio = 1) + 
  ggtitle("Accuracy by Category")


############ Correlate human and CV confusions

# Get confusions
confusion_31_vector =
  data_31 %>%
  group_by(target_category, cue_category) %>%
  summarise(confusion = 1 - mean(acc[!is.na(rt)]))

confusion_60_vector =
  data_60 %>%
  group_by(target_category, cue_category) %>%
  summarise(confusion = 1 - mean(acc[!is.na(rt)]))

confusion_100_vector =
  data_100 %>%
  group_by(target_category, cue_category) %>%
  summarise(confusion = 1 - mean(acc[!is.na(rt)]))

confusion_cv_vector = gather(1 - as_tibble(t(confusion_cv)), na.rm = TRUE)
  
# z-score confusions
human_31_norm = scale(confusion_31_vector$confusion, center = TRUE, scale = TRUE)
human_60_norm = scale(confusion_60_vector$confusion, center = TRUE, scale = TRUE)
human_100_norm = scale(confusion_100_vector$confusion, center = TRUE, scale = TRUE)
cv_norm = scale(confusion_cv_vector$value, center = TRUE, scale = TRUE)

cor(c(human_100_norm), c(cv_norm), use = "pairwise.complete.obs", method = "pearson")

# put normed values back into confusion matrix form for plotting
confusions = c('confusion_31', 'confusion_60', 'confusion_100', 'confusion_cv')

for (i in 1:length(confusions)) {
  current = as_tibble(matrix(ncol = 14, nrow = 14))
  rownames(current) = names_rows
  colnames(current) = names_cols
  assign(confusions[i], current)
}

index = 0
for (i in 1:14) {
  for (j in 1:14) {
    if (i == j) {
      next
    }
    index = index + 1
    confusion_31[i, j] = human_31_norm[index]
    confusion_60[i, j] = human_60_norm[index]
    confusion_100[i, j] = human_100_norm[index]
    confusion_cv[i, j] = cv_norm[index]
  }
}


# modify structures for plotting confusion matrices
confusion_31_plot = as.data.frame(as.table(as.matrix(confusion_31)))
confusion_60_plot = as.data.frame(as.table(as.matrix(confusion_60)))
confusion_100_plot = as.data.frame(as.table(as.matrix(confusion_100)))
confusion_cv_plot = as.data.frame(as.table(as.matrix(confusion_cv)))

confusion_31_plot$Var1 = with(confusion_31_plot, factor(Var1, levels = rev(levels(Var1))))
confusion_60_plot$Var1 = with(confusion_60_plot, factor(Var1, levels = rev(levels(Var1))))
confusion_100_plot$Var1 = with(confusion_100_plot, factor(Var1, levels = rev(levels(Var1))))
confusion_cv_plot$Var1 = with(confusion_cv_plot, factor(Var1, levels = rev(levels(Var1))))


theme_set(theme_minimal(base_size = 35))
our_color = "grey30"
theme = theme_update(axis.text = element_text(color = our_color),
                     axis.title = element_text(color = our_color),
                     plot.title = element_text(color = our_color))

plot = ggplot(confusion_31_plot)
plot + geom_tile(aes(x = Var2, y = Var1, fill = Freq), colour = 'black') + 
  scale_x_discrete(name = "Labeled Category", 
                   labels = c("P", "C", "A", "B", "D", "E", "B", "Z", "F", "K", "B", "A", "C", "T")) + 
  scale_y_discrete(name = "Actual Category ") + 
  scale_fill_gradientn(colours = c("red", "orange", "yellow", "green", "cyan", "blue"),
                       limits = c(-2, 4.7), na.value = "grey70") +
  guides(fill = guide_colorbar(title = NULL,barwidth = 0.7, barheight = 17, draw.ulim = .75)) +
  ggtitle("") +
  ggtitle("Human (31 ms) Confusions") + coord_equal(ratio = 1)

plot = ggplot(confusion_60_plot)
plot + geom_tile(aes(x = Var2, y = Var1, fill = Freq),colour='black') + 
  scale_x_discrete(name = "Labeled Category", 
                   labels = c("P", "C", "A", "B", "D", "E", "B", "Z", "F", "K", "B", "A", "C", "T")) + 
  scale_y_discrete(name = "Actual Category ") + 
  scale_fill_gradientn(colours = c("red", "orange", "yellow", "green", "cyan", "blue"),
                       limits = c(-2, 4.7), na.value = "grey70") +
  guides(fill = guide_colorbar(title = NULL,barwidth = 0.7, barheight = 17, draw.ulim = .75)) +
  ggtitle("") +
  ggtitle("Human (60 ms) Confusions") + coord_equal(ratio = 1)

plot = ggplot(confusion_100_plot)
plot + geom_tile(aes(x = Var2, y = Var1, fill = Freq),colour='black') + 
  scale_x_discrete(name = "Labeled Category", 
                   labels = c("P", "C", "A", "B", "D", "E", "B", "Z", "F", "K", "B", "A", "C", "T")) + 
  scale_y_discrete(name = "Actual Category ") + 
  scale_fill_gradientn(colours = c("red", "orange", "yellow", "green", "cyan", "blue"),
                       limits = c(-2, 4.7), na.value = "grey70") +
  guides(fill = guide_colorbar(title = NULL,barwidth = 0.7, barheight = 17, draw.ulim = .75)) +
  ggtitle("") +
  ggtitle("Human (100 ma) Confusions") + coord_equal(ratio = 1)

plot = ggplot(confusion_cv_plot)
plot + geom_tile(aes(x = Var2, y = Var1, fill = Freq),colour='black') + 
  scale_x_discrete(name = "Labeled Category", 
                   labels = c("P", "C", "A", "B", "D", "E", "B", "Z", "F", "K", "B", "A", "C", "T")) + 
  scale_y_discrete(name = "Actual Category ") + 
  scale_fill_gradientn(colours = c("red", "orange", "yellow", "green", "cyan", "blue"),
                       limits = c(-2, 4.6), na.value = "grey70") +
  guides(fill = guide_colorbar(title = NULL,barwidth = 0.7, barheight = 17, draw.ulim = .75)) +
  ggtitle("") +
  ggtitle("CNN Confusions") + coord_equal(ratio = 1)

# Scatter plot
scatter = data.frame(cv_norm, human_norm_31, human_norm_60, human_norm_100)

# scatter plot
#theme_set(theme_gray(base_size = 25))
#theme = theme_set(theme_minimal(25))
#theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())
ggplot(scatter, aes(x = human_norm_60, y = human_norm_100)) + 
  geom_point(shape = 19, alpha = .3, size = 4, color='#F8766D') +
  geom_smooth(method = "lm", se = TRUE, color='#00C0AF') + 
  scale_x_continuous(limits=c(-1.7,4.7)) + coord_equal(ratio=1) +
  scale_y_continuous(limits=c(-1.7,4.7)) + theme(legend.position="none") + 
  ylab("Human (100 ms) Confusions") + xlab("Human (100 ms) Confusions") +
  coord_equal(ratio = 1)


# Split half reliability
human_corrs = as.data.frame(matrix(ncol = 1, nrow = 100))
for (k in 1:100) {
  split_human_one = matrix(ncol = 14, nrow = 14)
  split_human_two = matrix(ncol = 14, nrow = 14)
  for (i in 1:14) {
    for (j in 1:14) {
      if (i == j) {
        next()
      }
      current_human = data_100 %>% filter(target_category == i, cue_category == j)
      randomize = sample(1:nrow(current_human), nrow(current_human))
      if (length(randomize) %% 2) {
        randomize = randomize[1:(length(randomize)-1)]
      }
      split_human_one[i,j] = mean(current_human$acc[randomize[1:(length(randomize) / 2)]])
      split_human_two[i,j] = mean(current_human$acc[randomize[((length(randomize) / 2 + 1)):length(randomize)]])
    }
  }
  
  human_r = 
    cor(as.vector(split_human_one), as.vector(split_human_two), use = "pairwise.complete.obs",
        method = "pearson")
  
  # spearman-brown correction
  human_corrs[k,1] = 2 * human_r / (1 + human_r)
  
}

mean(human_corrs$V1)



# image level stuffs

image_accuracy_31 =
  data_31 %>%
  group_by(image_id) %>%
  summarise(accuracy = mean(acc), rt = mean(rt[acc == 1]))

image_accuracy_60 =
  data_60 %>%
  group_by(image_id) %>%
  summarise(accuracy = mean(acc), rt = mean(rt[acc == 1]))

image_accuracy_100 =
  data_100 %>%
  group_by(image_id) %>%
  summarise(accuracy = mean(acc), rt = mean(rt[acc == 1]))

cv_image_accuracy = 
  read_csv("~/desktop/muri_benchmark/image_cv_91.csv")

cv_image_accuracy = arrange(cv_image_accuracy, image_id)

# correlate image level performance
cor(image_accuracy_60$accuracy, cv_image_accuracy$accuracy)
cor(image_accuracy_60$rt, image_accuracy_100$rt)


# overall accuracy differences (relative to CV)
diffs_31 = image_accuracy_31$accuracy - cv_image_accuracy$accuracy
diffs_60 = image_accuracy_60$accuracy - image_accuracy_31$accuracy
diffs_100 = image_accuracy_100$accuracy - image_accuracy_60$accuracy

# images of interest
soa_cv = cv_image_accuracy %>% filter(image_id %in% soa_images$image_id)
soa_31 = image_accuracy_31 %>% filter(image_id %in% soa_images$image_id)
soa_60 = image_accuracy_60 %>% filter(image_id %in% soa_images$image_id)
soa_100 = image_accuracy_100 %>% filter(image_id %in% soa_images$image_id)

diffs_31_soa = soa_31$accuracy - soa_cv$accuracy
diffs_60_soa = soa_60$accuracy - soa_cv$accuracy
diffs_100_soa = soa_100$accuracy - soa_cv$accuracy

diffs_human = tibble(diffs_31, diffs_100)

# overall rt
rt_human = tibble(image_accuracy_31$rt, image_accuracy_60$rt, image_accuracy_100$rt)
#rt_human = tibble(soa_31$rt, soa_60$rt, soa_100$rt)
colnames(rt_human) = c("31", "60", "100")

# overall acurracy
acc_human = tibble(image_accuracy_31$accuracy, image_accuracy_60$accuracy, image_accuracy_100$accuracy)
#acc_human = tibble(soa_31$accuracy, soa_60$accuracy, soa_100$accuracy)
colnames(acc_human) = c("31", "60", "100")

#plot(diffs_60, diffs_100)
#cor(diffs_60_soa, diffs_100_soa)

plot_scale = 1

# scatter plot
#theme_set(theme_gray(base_size = 25))
theme = theme_set(theme_minimal(25))
#theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())
ggplot(diffs_human, aes(cv_image_accuracy$accuracy, image_accuracy_100$accuracy)) + 
  geom_point(shape = 19, alpha = .3, size = 4, color = '#F8766D') +
  geom_smooth(method = "lm", se = TRUE, color = '#00C0AF') + 
  #geom_abline(intercept = 0, slope = 1, color = '#00C0AF') +
  scale_x_continuous(limits = c(0, plot_scale), breaks = seq(0, plot_scale, .2)) + 
  scale_y_continuous(limits = c(0, plot_scale), breaks = seq(0, plot_scale, .2)) + 
  theme(legend.position = "none") + 
  coord_equal(ratio = 1) +
  xlab("CV") + ylab("Human (100 ms)") +
  coord_equal(ratio = 1)

plot_scale = .7

# scatter plot
#theme_set(theme_gray(base_size = 25))
theme = theme_set(theme_minimal(25))
#theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank())
ggplot(acc_human, aes(x = `60`, y = `100`)) + 
  geom_point(shape = 19, alpha = .3, size = 4, color = '#F8766D') +
  #geom_smooth(method = "lm", se = TRUE, color = '#00C0AF') + 
  geom_abline(intercept = 0, slope = 1, color = '#00C0AF') +
  scale_x_continuous(limits = c(-plot_scale, plot_scale), breaks = seq(-plot_scale, plot_scale, .2)) + 
  scale_y_continuous(limits = c(-plot_scale, plot_scale), breaks = seq(-plot_scale, plot_scale, .2)) + 
  theme(legend.position = "none") + 
  coord_equal(ratio = 1) +
  xlab("Human (31 ms) - CV") + ylab("Human (100 ms) - CV") +
  coord_equal(ratio = 1)


# Split half reliability
human_corrs = as.data.frame(matrix(ncol = 1, nrow = 100))

image_ids = 
  data_100 %>%
  group_by(image_id) %>%
  summarize(n = n())

image_ids = image_ids$image_id

for (k in 1:100) {
  split_human_one = matrix(ncol = 1, nrow = 1274)
  split_human_two = matrix(ncol = 1, nrow = 1274)
  
  for (i in 1:1274) {
      
      current_image = data_100 %>% filter(image_id == image_ids[i])
      randomize = sample(current_image$acc)
      split_human_one[i] = mean(randomize[1:(length(randomize) / 2)])
      split_human_two[i] = mean(randomize[((length(randomize) / 2) + 1):length(randomize)])
}
  
  human_r = 
    cor(as.vector(split_human_one), as.vector(split_human_two), use = "pairwise.complete.obs",
        method = "pearson")
  
  # spearman-brown correction
  human_corrs[k,1] = 2 * human_r / (1 + human_r)
  
}

mean(human_corrs$V1)










