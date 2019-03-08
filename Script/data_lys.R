library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
# import data 
setwd("./dropbox/G-soop/WIKI mining")
data <- read.csv("OSPD.csv", header = TRUE, sep = ";", colClasses = "character")
data1 <- as_tibble(data)
# make it into a numeric matrix 
dat <- data1[,2:10]
dat[dat != "0"] <- "1"
# tranform data types into numeric, and factor 
dat <- as.data.frame(sapply(dat, as.numeric))
# put names and OOM into the data frame 
dat <- mutate(dat, names = data[,1], OOM = data[,11])
dat[,11] <- as.factor(dat[,11])
# general analysis 

 

# Visualize the data 

# general distribuion of tools 
tool_dis <- as.data.frame(round(colSums(dat[,1:9])/nrow(dat), digits = 2))
tool_dis <- mutate(tool_dis, Tool = rownames(tool_dis))
names(tool_dis) <- c("Propotion", "Tool")
tool_dis <- tool_dis[,2:1]

# Plot a pie chart of this distribution 
p_tool_dis <- ggplot(tool_dis, aes(x = "", y = Propotion, fill = Tool)) + 
    geom_bar(width = 1, stat = "identity")
p_tool_dis <- p_tool_dis +
    coord_polar("y", start = 0) +
    scale_fill_brewer(palette = "Blues") +
    theme_minimal() +
    geom_text(aes(label = percent(Propotion)), position = position_stack(vjust = 0.5))

# group_by the OOM
dat_group <- group_by(dat, OOM)
summary <- summarise(dat_group, 
                     social_media = round(mean(Facebook.twitter), digits = 2),
                     Wiki = round(mean(Wiki), digits = 2),
                     Forum = round(mean(Forum), digits = 2),
                     Blog = round(mean(Blog), digits = 2),
                     Github = round(mean(Github_text), digits = 2),
                     Live_chat = round(mean(Live.chat), digits = 2),
                     Discussion = round(mean(Discussion), digits = 2),
                     Wevolver = round(mean(Wevolver), digits = 2),
                     Thingiverse = round(mean(Thingiverse), digits = 2),
                     count = n())

# number of projects according to OOM 
p_general <- ggplot(data = summary) + 
    geom_bar(aes(x = OOM, y = count, fill = OOM), stat = "identity") + 
    labs(title = "OSPD project distribution according to OOM", x = "OOM", y = "Number of projects") + 
    scale_fill_brewer(palette = "Blues") + 
    geom_text(aes(x = OOM, y = count, label = count), vjust = 1.6)

# Vertical analysis, compare tool usage vertically according to OOM



# Social media according to OOM
p_social_media <- ggplot(data = summary, aes(x = OOM, y = social_media, fill = OOM)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Social media distribution according to OOM", x = "OOM", y = "Propotion of social media usage") + 
    scale_fill_brewer(palette = "Blues") + 
    geom_text(aes(x = OOM, y = social_media, label = social_media), vjust = 1.6) +
    geom_line(stat = "identity", group = 1, color = "orange", size = 1.5) + 
    geom_point() +
    geom_hline(yintercept = tool_dis[1,2], linetype = "dashed", color = "blue") +
    annotate("text", x = 0.9, y = tool_dis[1,2] + 0.018, label = "mean")
    

# Wiki distribution 
p_wiki <- ggplot(data = summary, aes(x = OOM, y = Wiki, fill = OOM)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Wiki distribution according to OOM", x = "OOM", y = "Propotion of Wiki usage") + 
    scale_fill_brewer(palette = "Blues") + 
    geom_text(aes(x = OOM, y = Wiki, label = Wiki), vjust = 1.6) +
    geom_line(stat = "identity", group = 1, color = "orange", size = 1.5) + 
    geom_point() +
    geom_hline(yintercept = tool_dis[2,2], linetype = "dashed", color = "blue") +
    annotate("text", x = 0.9, y = tool_dis[2,2] + 0.018, label = "mean")

# Forum distribution 
p_forum <- ggplot(data = summary, aes(x = OOM, y = Forum, fill = OOM)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Forum distribution according to OOM", x = "OOM", y = "Propotion of Forum usage") + 
    scale_fill_brewer(palette = "Blues") + 
    geom_text(aes(x = OOM, y = Forum, label = Forum), vjust = 1.6) +
    geom_line(stat = "identity", group = 1, color = "orange", size = 1.5) + 
    geom_point() +
    geom_hline(yintercept = tool_dis[3,2], linetype = "dashed", color = "blue") +
    annotate("text", x = 0.9, y = tool_dis[3,2] + 0.018, label = "mean")

# Blog distribution 
p_blog <- ggplot(data = summary, aes(x = OOM, y = Blog, fill = OOM)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Blog distribution according to OOM", x = "OOM", y = "Propotion of Blog usage") + 
    scale_fill_brewer(palette = "Blues") + 
    geom_text(aes(x = OOM, y = Blog, label = Blog), vjust = 1.6) +
    geom_line(stat = "identity", group = 1, color = "orange", size = 1.5) + 
    geom_point() +
    geom_hline(yintercept = tool_dis[4,2], linetype = "dashed", color = "blue") +
    annotate("text", x = 0.9, y = tool_dis[4,2] + 0.018, label = "mean")

# Github distribution 
p_github <-  ggplot(data = summary, aes(x = OOM, y = Github, fill = OOM)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Github distribution according to OOM", x = "OOM", y = "Propotion of Github usage") + 
    scale_fill_brewer(palette = "Blues") + 
    geom_text(aes(x = OOM, y = Github, label = Github), vjust = 1.6) +
    geom_line(stat = "identity", group = 1, color = "orange", size = 1.5) + 
    geom_point() +
    geom_hline(yintercept = tool_dis[5,2], linetype = "dashed", color = "blue") +
    annotate("text", x = 0.9, y = tool_dis[5,2] + 0.018, label = "mean")

# live chat distribution 
p_live_chat <- ggplot(data = summary, aes(x = OOM, y = Live_chat, fill = OOM)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Live chat distribution according to OOM", x = "OOM", y = "Propotion of Live chat usage") + 
    scale_fill_brewer(palette = "Blues") + 
    geom_text(aes(x = OOM, y = Live_chat, label = Live_chat), vjust = 1.6) +
    geom_line(stat = "identity", group = 1, color = "orange", size = 1.5) + 
    geom_point() +
    geom_hline(yintercept = tool_dis[6,2], linetype = "dashed", color = "blue") +
    annotate("text", x = 0.9, y = tool_dis[6,2] + 0.018, label = "mean")

# Discussion distribution 
p_discussion <- ggplot(data = summary, aes(x = OOM, y = Discussion, fill = OOM)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Discussion distribution according to OOM", x = "OOM", y = "Propotion of Discussion usage") + 
    scale_fill_brewer(palette = "Blues") + 
    geom_text(aes(x = OOM, y = Discussion, label = Discussion), vjust = 1.6) +
    geom_line(stat = "identity", group = 1, color = "orange", size = 1.5) + 
    geom_point() +
    geom_hline(yintercept = tool_dis[7,2], linetype = "dashed", color = "blue") +
    annotate("text", x = 0.9, y = tool_dis[7,2] + 0.018, label = "mean")

# Wevolver distribution 
p_wevolver <- ggplot(data = summary, aes(x = OOM, y = Wevolver, fill = OOM)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Wevolver distribution according to OOM", x = "OOM", y = "Propotion of Wevolver usage") + 
    scale_fill_brewer(palette = "Blues") + 
    geom_text(aes(x = OOM, y = Wevolver, label = Wevolver), vjust = 1.6) +
    geom_line(stat = "identity", group = 1, color = "orange", size = 1.5) + 
    geom_point() +
    geom_hline(yintercept = tool_dis[8,2], linetype = "dashed", color = "blue") +
    annotate("text", x = 0.9, y = tool_dis[8,2] + 0.018, label = "mean")

# Thingiverse distribution 
p_thingiverse <- ggplot(data = summary, aes(x = OOM, y = Thingiverse, fill = OOM)) + 
    geom_bar(stat = "identity") + 
    labs(title = "Thingiverse distribution according to OOM", x = "OOM", y = "Propotion of Thingiverse usage") + 
    scale_fill_brewer(palette = "Blues") + 
    geom_text(aes(x = OOM, y = Thingiverse, label = Thingiverse), vjust = 1.6) +
    geom_line(stat = "identity", group = 1, color = "orange", size = 1.5) + 
    geom_point() +
    geom_hline(yintercept = tool_dis[9,2], linetype = "dashed", color = "blue") +
    annotate("text", x = 0.9, y = tool_dis[9,2] + 0.018, label = "mean")

# integrate everything together 
# First gather the data 
all <- gather(summary[,-11], 
              key = "Tool", 
              value = "Propotion", 
              2:10,  
              factor_key = TRUE )
geom_line(aes(color = Tool), size = 1, group = all$Tool) + 
# Plot the data 
p_all <- ggplot(all, aes(x = OOM, y = Propotion)) + 
    geom_line(aes(color = Tool), size = 1, group = all$Tool) + 
    geom_point(shape = all$Tool) + 
    scale_colour_brewer(palette = "Set1")


p_all_grid <- ggplot(all, aes(x = OOM, y = Propotion)) + 
#    geom_line(aes(color = Tool), size = 1, group = all$Tool) + 
    geom_point(shape = all$Tool) + 
    scale_colour_brewer(palette = "Set1") +
    facet_grid(all$Tool ~ .) +
    geom_smooth(method = lm, se = FALSE, size = 0.5)

p_all_grid



# Horizontal analysis, compare tool variety 
dat_var <- mutate(dat, var = rowSums(dat[,1:9]))
dat_group_var <- group_by(dat_var, OOM)
tool_var <- summarise(dat_group_var, var = mean(var))
