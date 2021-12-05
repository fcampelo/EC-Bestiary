library(dplyr)
library(forcats)
library(ggplot2)
library(rlang)
library(ggforce)
library(rcrossref)

df <- readRDS("./data/00_bestiaryDF.rds")

## === fig 1
years <- c(2000, 2020)
df %>%
  filter(Year <= years[2]) %>%
  ggplot(aes(x = Year)) + 
  geom_bar(fill = "#6600bbaa", show.legend = FALSE) + 
  xlim(1999, 2021) + ylim(0, 30) +
  ylab("Papers proposing new metaphor-based methods") + xlab("") +
  theme_light() + 
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) 
#annotate("text", x = 2020, y = 30, 
#         label = paste0("2000 - ", years[2]), size = 5)

ggsave(paste0("./img/new_metaphors_2000-", years[2], ".png"), 
       width = 6, height = 6, bg = "transparent")

df %>%
  filter(Year >= years[1], Year <= years[2]) %>%
  arrange(Year) %>%
  select(Metaphor, SubMetaphor, Year) %>%
  summarise(Name = ifelse(is.na(SubMetaphor),
                          paste0(Metaphor, " (", Year, ")"),
                          paste0(Metaphor, ": ", SubMetaphor, " (", Year, ")")))


## === fig 2
df2 <- df %>%
  filter(Is.journal, Year >= 2000)

df2$Where.pub <- gsub("International", "Intl.", df2$Where.pub)
df2$Where.pub <- gsub("Journal of| JOURNAL OF", "J.", df2$Where.pub)
df2$Where.pub <- gsub("Information", "Inf.", df2$Where.pub)
df2$Where.pub <- gsub("Transactions on|Transactions of", "Trans.", df2$Where.pub)
df2$Where.pub <- gsub("Applied", "Appl.", df2$Where.pub)
df2$Where.pub <- gsub("Computer|Computational|Computing|Computation", "Comp.", df2$Where.pub)
df2$Where.pub <- gsub("Optimization|Optimisation", "Optim.", df2$Where.pub)
df2$Where.pub <- gsub("Proceedings of the Institution of Mechanical Engineers, Part B: ", "", df2$Where.pub)
df2$Where.pub <- gsub("Communications in", "Comms.", df2$Where.pub)
df2$Where.pub <- gsub("Technology", "Tech.", df2$Where.pub)
df2$Where.pub <- gsub("Engineering|ENGINEERING", "Eng.", df2$Where.pub)
df2$Where.pub <- gsub("Intelligence|Intelligent", "Intel.", df2$Where.pub)
df2$Where.pub <- gsub("Numerical", "Num.", df2$Where.pub)
df2$Where.pub <- gsub("Science\ |Science and\ ", "Sci.\ ", df2$Where.pub)
df2$Where.pub <- gsub("Mathematical|Mathematics", "Math.", df2$Where.pub)
df2$Where.pub <- gsub("Advances|Advanced", "Adv.", df2$Where.pub)
df2$Where.pub <- gsub("Research", "Res.", df2$Where.pub)
df2$Where.pub <- gsub("Planning", "Plan.", df2$Where.pub)
df2$Where.pub <- gsub("Management", "Manag.", df2$Where.pub)
df2$Where.pub <- gsub("Evolutionary", "Evol.", df2$Where.pub)
df2$Where.pub <- gsub("System|Systems", "Sys.", df2$Where.pub)

df2 <- df2 %>%
  mutate(Where.pub = factor(Where.pub, 
                            levels = names(sort(table(Where.pub), 
                                                decreasing = TRUE)))) %>%
  group_by(Where.pub) %>%
  summarise(Count = n())

df2$Labs <- df2$Where.pub
#df2$Labs[26:length(df2$Labs)] <- NA

myscale <- scale_fill_gradient(low = "#5500aa66", high = "#6600bbdd")

mp <- ggplot(df2, aes(x = as.integer(Where.pub), y = Count, fill = Count)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  theme_light() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  myscale +
  facet_zoom(xy = as.integer(Where.pub) <= 24, horizontal = FALSE) + 
  scale_x_continuous(breaks = seq_along(df2$Labs), 
                     labels = NULL)+#df2$Labs, na.value = NULL) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  xlab("") + ylab("Number of new metaphor-based methods")

mp
ggsave("./img/new_metaphors_by_venue1.png", width = 6, height = 8, 
       bg = "transparent")

mp + scale_x_continuous(breaks = seq_along(df2$Labs), 
                        labels = df2$Labs, na.value = NULL)
ggsave("./img/new_metaphors_by_venue2.png", width = 6, height = 8, 
       bg = "transparent")


## === fig 3
dlist <- readRDS("./data/00_bestiary_data_list.rds")
auth.list <- dlist$data$author
auth.list <- lapply(auth.list, 
                    function(x){
                      x$full <- paste(substr(x$given, 1, 1), x$family)
                      idx <- duplicated(x$full)
                      if(any(idx)) x <- x[-which(idx), ]
                      return(x)})

df3 <- dplyr::bind_rows(auth.list) %>% 
  select(full) %>%
  mutate(full = factor(full, 
                       levels = names(sort(table(full), 
                                           decreasing = TRUE)))) %>%
  group_by(full) %>%
  summarise(Count = n())

df3$Labs <- df3$full

ggplot(df3, aes(x = as.integer(full), 
                y = Count, fill = Count)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  theme_light() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  myscale +
  facet_zoom(xy = as.integer(full) < 41, horizontal = FALSE) + 
  scale_x_continuous(breaks = seq_along(df3$Labs), 
                     labels = df3$Labs,) + 
  scale_y_continuous(breaks = 2*(0:5), 
                     labels = 2*(0:5)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  xlab("") + ylab("Number of metaphor-based methods authored")
