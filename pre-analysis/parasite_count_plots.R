#parasite_count_plots.R
#Generate plots for the parasite counts from the Visium HD 3' experiment
#Timepoints: 2 days, 5 days, 9 days, 12 days, 14 days (after 2nd BF)

library(ggplot2)
library(dplyr)
library(systemfonts)
library(here)

here::i_am("repo/pre-analysis/parasite_count_plots.R")
# --- Generate tacked bar plot of relative frequencies ---

#Load in the data
mean_counts <- read.csv(here("data", "parasite_counts_totals.csv"), stringsAsFactors=TRUE)
mean_counts$Day <- factor(mean_counts$Day)
mean_counts$Stage <- factor(mean_counts$Stage, levels=c("Metacyclic", "Leptomonad", "Nectomonad", "Procyclic"))

#Create a new df that tracks rel frequency of stages
df <- mean_counts |> group_by(Day) |> mutate(Freq = Count / sum(Count))

#Plot
ggplot(df, aes(x=Day,y=Freq,fill=Stage)) + 
  scale_fill_manual(values = c("#c5bedf","#fe8587","#b3e0f5","#ffd038")) +
  geom_bar(stat="identity") +
  theme_classic() +
  labs(title="Leishmania Counts by Timepoint", x="Days post-infection",
       y="Parasite stage (frequency)") +
  theme(plot.caption = element_text(family="Helvetica", hjust=0.5)) +
  theme(plot.title = element_text(family="Helvetica", face="bold", hjust=0.5)) + 
  theme(axis.text = element_text(family='Helvetica'))


# --- Generate a scatter plot of total parasites per gut ---
counts <- read.csv(here("data", "parasite_counts.csv"), stringsAsFactors=TRUE)
counts$Day <- factor(counts$Day)
counts$Stage <- factor(counts$Stage, levels=c("Metacyclic", "Leptomonad", "Nectomonad", "Procyclic"))

#Calculate the total counts of each stage per gut
counts <- mutate(counts, Total = Count * 10000 * Volume / 1000)

#Calculate the total # of promastigotes per gut
subset_counts <- counts |> 
  group_by(Gut) |>
  summarise(total_promastigote = sum(Total), Day = first(Day))

#Plot
ggplot(subset_counts, aes(x=Day, y=total_promastigote)) +
  geom_point(size=1) +
  theme_classic() +
  labs(y="# of promastigotes", x="Days post-infection",
       title="Total promastigotes per gut") +
  theme(plot.title = element_text(family="Helvetica", face="bold",hjust=0.5))
