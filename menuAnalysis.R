library(tidyverse)
library(ez)
library(ARTool)
library(dplyr)
library(RColorBrewer)
library(vistime)
library(ggpubr)

menuData <- read_csv("trials11-07-24")

# plot of 8x8 radial average 'chording' time 
# improvement shown by the end of each learning block of 8
menuData |> filter(pID > 16 & conditionID == "8x8 Radial") |>
  group_by(block) |>
  summarise(averageTime = mean(betweenLastTime)) |>
  ggplot(aes(x = block, y = averageTime)) +
    geom_line()



# plot of 8x8 chording average 'chording' time 
# improvement shown by the end of each block of 8
menuData |> filter(pID > 16 & conditionID == "8x8 Chording" & betweenLastTime < 500000) |>
  group_by(block) |>
  summarise(averageTime = mean(betweenLastTime)) |>
  ggplot(aes(x = block, y = averageTime)) +
  geom_line() +
  labs(y = "AverageChording Time (millis between last two keys pressed)", x = "Learning Block")+
  scale_y_continuous(limits=c(600, 1300), breaks = seq(600, 1300, by = 200)) +
  scale_x_continuous(limits=c(1, 40), breaks = seq(1,40,by = 10))


# plot of 8x8 chording average 'chording' time 
# improvement shown overall
# use trial number?? figure out math
menuData |> filter(pID > 16 & conditionID == "8x8 Chording" & betweenLastTime < 500000) |>
  mutate(learningBlock = floor(block / 5) + 1) |>
  select(betweenLastTime, block, learningBlock, conditionID, time) |>
  group_by(learningBlock) |>
  summarise(averageTime = mean(betweenLastTime)) |>
  ggplot(aes(x = learningBlock, y = averageTime)) +
  geom_line() +
  labs(y = "AverageChording Time (millis between last two keys pressed)", x = "Learning Block")+
  scale_x_continuous(limits=c(1, 8), breaks = seq(1,8,by = 1)) +
  scale_y_continuous(limits=c(600, 1300), breaks = seq(600, 1300, by = 200))


# plot of average trial time per condition
menuData |> filter(time < 10000) |>
  group_by(conditionID) |>
  summarise(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(type = substring(conditionID, 5, nchar(conditionID))) |>
  ggplot(aes(x = conditionID, y = averageTime, fill = type)) +
    geom_bar(stat = "identity") + 
    labs(x = "Condition ID", y = "Average Trial Time (millis)", fill = "Menu Type") +
    ggtitle("Menu Type vs. Trial Time") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.2, position=position_dodge(.9)) 
  

# plot of average chording time (between last two sticks) per condition
menuData |> group_by(conditionID) |>
  filter(betweenLastTime < 10000) |>
  summarise(averageTime = mean(betweenLastTime),
            sd = sd(betweenLastTime, na.rm = TRUE),
            se = sd/sqrt(length(betweenLastTime))) |>
  mutate(type = substring(conditionID, 5, nchar(conditionID))) |>
  print(n = 50) |>
  ggplot(aes(x = conditionID, y = averageTime, fill = type)) +
  geom_bar(stat = "identity") + 
  labs(x = "Condition ID", y = "Average Chording Time (millis)", fill = "Menu Type") +
  ggtitle("Menu Type vs. Chording Time") + 
  scale_y_continuous(limits=c(0, 1000), breaks = seq(0, 1000, by = 100)) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9)) 
  
#!!!
# stacked bar plot 
menuData |> filter(betweenLastTime < 10000) |>
  group_by(conditionID) |>
  summarise(averageSelection = mean(selectionTime), averageBetween = mean(betweenLastTime), averageBrowse = mean(browsingTime), na.rm = TRUE) |>
  mutate(averageSelection = averageSelection - averageBetween) |>
  select(averageBetween, averageBrowse, conditionID, averageSelection) |>
  pivot_longer(!conditionID, names_to = "timeType", values_to = "times") |>
  print(n = 30) |>
  ggplot(aes(x = conditionID, y = times, fill = timeType)) +
    geom_bar(position = 'stack',stat = "identity") + 
    labs(x = "Condition ID", y = "Average Time (millis)") +
    ggtitle("Menu Type vs. Chording Time") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete(name = '', labels = c("Chording", "AFK", "Browsing"))


# MENU POSITIONS 
menuData |>
  filter(conditionID == "8x8 Radial", time < 10000) |>
  group_by(menuTarget) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  ggplot(aes(x = menuTarget, y = averageTime)) +
  geom_bar(stat = "identity") + 
  ggtitle("8x8 Radial Target Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9)) 
  
# ITEM POSITIONS 
menuData |>
  filter(conditionID == "8x8 Radial", time < 10000) |>
  group_by(itemTarget) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  ggplot(aes(x = itemTarget, y = averageTime)) +
  geom_bar(stat = "identity") + 
  ggtitle("8x8 Radial Target Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9)) 

  

# TARGET POSITIONS CHORDING 8x8
menuData |>
  filter(conditionID == "8x8 Chording", time < 10000) |>
  unite(coords, c(menuTarget, itemTarget), sep = ",", remove = FALSE) |>
  group_by(coords) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menu = substring(coords, 0,1)) |>
  ggplot(aes(x = coords, y = averageTime, fill = menu)) +
  geom_bar(stat = "identity") + 
  ggtitle("8x8 Chording Target Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "darkgreen") +
  geom_text(aes(label = coords), vjust = 0.45, hjust = 4, size = 2.8, angle = 90, colour = "black") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  

# TARGET POSITIONS RADIAL 8x8
menuData |>
  filter(conditionID == "8x8 Radial", time < 10000) |>
  unite(coords, c(menuTarget, itemTarget), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  group_by(coords) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menu = substring(coords, 0,1)) |>
  ggplot(aes(x = coords, y = averageTime, fill = menu)) +
  geom_bar(stat = "identity") + 
  ggtitle("8x8 Radial Target Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "darkgreen") +
  geom_text(aes(label = coords), vjust = 0.45, hjust = 4, size = 2.8, angle = 90, colour = "black") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# TARGET POSITIONS CHORDING 8x4
menuData |>
  filter(conditionID == "8x4 Chording", time < 10000) |>
  unite(coords, c(menuTarget, itemTarget), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  group_by(coords) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menu = substring(coords, 0,1)) |>
  ggplot(aes(x = coords, y = averageTime, fill = menu)) +
  geom_bar(stat = "identity") + 
  ggtitle("8x4 Chording Target Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "darkgreen") +
  geom_text(aes(label = coords), vjust = 0.45, hjust = 3.3, size = 3.8, angle = 90, colour = "black") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# TARGET POSITIONS RADIAL 8x4
menuData |>
  filter(conditionID == "8x4 Radial", time < 10000) |>
  unite(coords, c(menuTarget, itemTarget), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  group_by(coords) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menu = substring(coords, 0,1)) |>
  ggplot(aes(x = coords, y = averageTime, fill = menu)) +
  geom_bar(stat = "identity") + 
  ggtitle("8x4 Radial Target Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "darkgreen") +
  geom_text(aes(label = coords), vjust = 0.45, hjust = 4, size = 3.8, angle = 90, colour = "black") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# TARGET POSITIONS CHORDING 4x4
menuData |>
  filter(conditionID == "4x4 Chording", time < 10000) |>
  unite(coords, c(menuTarget, itemTarget), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  group_by(coords) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menu = substring(coords, 0,1)) |>
  ggplot(aes(x = coords, y = averageTime, fill = menu)) +
  geom_bar(stat = "identity") + 
  ggtitle("4x4 Chording Target Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "darkgreen") +
  geom_text(aes(label = coords), vjust = 0.45, hjust = 3.3, size = 4.8, angle = 90, colour = "black") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# TARGET POSITIONS RADIAL 4x4
menuData |>
  filter(conditionID == "4x4 Radial", time < 10000) |>
  unite(coords, c(menuTarget, itemTarget), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  group_by(coords) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menu = substring(coords, 0,1)) |>
  ggplot(aes(x = coords, y = averageTime, fill = menu)) +
  geom_bar(stat = "identity") + 
  ggtitle("4x4 Radial Target Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "darkgreen") +
  geom_text(aes(label = coords), vjust = 0.45, hjust = 3.3, size = 4.8, angle = 90, colour = "black") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Average errors for conditions
menuData |> 
  group_by(conditionID) |>
  summarize(averageErr = mean(errors),
            sd = sd(errors, na.rm = TRUE),
            se = sd/sqrt(length(errors))) |>
  mutate(type = substring(conditionID, 5, nchar(conditionID))) |>
  ggplot(aes(x = conditionID, y = averageErr, fill = type)) +
  geom_bar(stat = "identity") + 
  ggtitle("Errors across all Conditions") +
  geom_errorbar(aes(ymin=averageErr-se, ymax=averageErr+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(y = "Average Errors per Trial", x = "Condition", fill = "Menu Type")

# TARGET NAMES 4x4 RADIAL
menuData |>
  filter(conditionID == "4x4 Radial", time < 10000) |>
  group_by(targetIcon) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menuIcon = if_else(targetIcon %in% c('Backpack','Satchel','Suitcase','Purse'), "Bags", 
                    if_else(targetIcon %in% c('Capris','Leggings','Shorts','Tights'), "Pants",
                    if_else(targetIcon %in% c('Cocktail','Grad','Summer dress','Wedding'), "Dresses",
                            "Coats")))) |>
  ggplot(aes(x = targetIcon, y = averageTime, fill = menuIcon)) +
  geom_bar(stat = "identity") + 
  ggtitle("4x4 Radial Target Icon Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-sd, ymax=averageTime+sd), width=.3, position=position_dodge(.9), colour = "black") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9))

# TARGET NAMES 4x4 CHORDING
menuData |>
  filter(conditionID == "4x4 Chording", time < 10000) |>
  group_by(targetIcon) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menuIcon = if_else(targetIcon %in% c('Duck','Flamingo','Owl','Parrot'), "Birds", 
                    if_else(targetIcon %in% c('Jaguar','Tabby','Lion','Leopard'), "Cats",
                    if_else(targetIcon %in% c('Jellyfish','Shark','Starfish','Tuna'), "Fish",
                    "Dogs")))) |>
  ggplot(aes(x = targetIcon, y = averageTime, fill = menuIcon)) +
  geom_bar(stat = "identity") + 
  ggtitle("4x4 Chording Target Icon Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-sd, ymax=averageTime+sd), width=.3, position=position_dodge(.9), colour = "black") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9))

# TARGET NAMES 8x4 RADIAL
menuData |>
  filter(conditionID == "8x4 Radial", time < 10000) |>
  group_by(targetIcon) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menuIcon = if_else(targetIcon %in% c('Butterfly','Caterpillar','Spider','Worm'), "Insects", 
                    if_else(targetIcon %in% c('Donkey','Horse','Sheep','Pig'), "Farm Animals",
                    if_else(targetIcon %in% c('Duck','Parrot','Owl','Flamingo'), "Birds",
                    if_else(targetIcon %in% c('Hadro','Ptero','Spino','T-rex'), "Dinosaurs", 
                    if_else(targetIcon %in% c('Hamster','Hedgehog','Rabbit','Squirrel'), "Small Mammals",
                    if_else(targetIcon %in% c('Leopard','Tabby','Jaguar','Lion'), "Cats",
                    if_else(targetIcon %in% c('Shiba','Yorkie','Sheperd','Poodle'), "Dogs", 
                    "Fish")))))))) |>
  ggplot(aes(x = targetIcon, y = averageTime, fill = menuIcon)) +
  geom_bar(stat = "identity") + 
  ggtitle("8x4 Radial Target Icon Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-sd, ymax=averageTime+sd), width=.3, position=position_dodge(.9), colour = "black") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9))

# TARGET NAMES 8x4 CHORDING
menuData |>
  filter(conditionID == "8x4 Chording", time < 10000) |>
  group_by(targetIcon) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menuIcon = if_else(targetIcon %in% c('Backpack','Purse','Satchel','Suitcase'), "Bags", 
                    if_else(targetIcon %in% c('Baseball','Cowboy','Toque','Farmers'), "Hats",
                    if_else(targetIcon %in% c('Belt','Fan','Sunnies','Watch'), "Accessories",
                    if_else(targetIcon %in% c('Blouse','T-shirt','Vest','Sweater'), "Tops", 
                    if_else(targetIcon %in% c('Boots','Heels','Sneakers','Flip flops'), "Shoes",
                    if_else(targetIcon %in% c('Cocktail','Grad','Summer dress','Wedding'), "Dresses",
                    if_else(targetIcon %in% c('Capris','Leggings','Shorts','Tights'), "Pants",
                    "Coats")))))))) |>
  ggplot(aes(x = targetIcon, y = averageTime, fill = menuIcon)) +
  geom_bar(stat = "identity") + 
  ggtitle("8x4 Chording Target Icon Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-sd, ymax=averageTime+sd), width=.3, position=position_dodge(.9), colour = "black") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9))

# TARGET NAMES 8x8 CHORDING
menuData |>
  filter(conditionID == "8x8 Chording", time < 10000) |>
  group_by(targetIcon) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  mutate(menuIcon = if_else(targetIcon %in% c('Apple','Mango','Banana','Pomegranate','Pineapple','Watermelon','Melon','Lime'), "Fruit", 
                    if_else(targetIcon %in% c('Bacon','Ham','Kebab','Lamb','Sausage','Steak','Turkey','Stuffing'), "Meat",
                    if_else(targetIcon %in% c('Bagel','Sliced Bread','Baguette','Croissant', 'Easter Bread','Bread','Naan','Pretzel'), "Bakery",
                    if_else(targetIcon %in% c('Banana split','Cheese cake','Chocolate','White chocolate', 'Cotton candy', 'Gummy', 'Icecream', 'Jelly'), "Dessert", 
                    if_else(targetIcon %in% c('Bokchoy','Broccoli','Carrot','Cauliflower', 'Corn', 'Peas','Salad', 'Zuccini'), "Veg",
                    if_else(targetIcon %in% c('Bubble tea','Champagne','Coffee','Cola', 'Coldbrew', 'Red wine', 'White wine', 'Tea'), "Drinks",
                    if_else(targetIcon %in% c('Butter','Milk','Yogurt','Eggs', 'Sunny side up', 'Whipped cream', 'Mozza', 'Cheese'), "Dairy",
                    "Fish")))))))) |>
  ggplot(aes(x = targetIcon, y = averageTime, fill = menuIcon)) +
  geom_bar(stat = "identity") + 
  ggtitle("8x8 Chording Target Icon Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-sd, ymax=averageTime+sd), width=.3, position=position_dodge(.9), colour = "black") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1.05, vjust = 1.05, size = 7))


# TARGET NAMES 8x8 RADIAL
menuData |>
  filter(conditionID == "8x8 Radial", time < 10000, pID > 17) |>
  mutate(menuIcon = if_else(targetIcon %in% c('Arrivals','Passenger','Departures','Traveler','Customs','Flight Crew','Luggage','Runway'), "Airport", 
                    if_else(targetIcon %in% c('ATM','Cheque','Debit Card','Banker','Savings','Paycheck','Withdrawl','Safe'), "Bank",
                    if_else(targetIcon %in% c('Totaled Car','Red Car','Hybrid','Limo', 'Black Cab','Pickup','Van','SUV'), "Cars",
                    if_else(targetIcon %in% c('Retail Worker','Shorts','Fitting Room','Suit', 'Dress', 'Sweater', 'Pants', 'Blouse'), "Clothing Store", 
                    if_else(targetIcon %in% c('Eggs','Grocery Bag','Produce Shelves','Grocery Basket', 'Grocery Cart', 'Veg','Fruit', 'Ingredients'), "Grocery Store",
                    if_else(targetIcon %in% c('Bush','Bench','Evergreen','Pond', 'Oak Tree', 'Path', 'Squirrel', 'Swing'), "Park",
                    if_else(targetIcon %in% c('Textbook','Chalkboard','Exam','Graduation', 'Music Book', 'Pencil Case', 'Lockers', 'Teached'), "School",
                    "Theatre")))))))) |>
  group_by(targetIcon, menuIcon) |>
  summarize(averageTime = mean(time),
            sd = sd(time, na.rm = TRUE),
            se = sd/sqrt(length(time))) |>
  ggplot(aes(x = targetIcon, y = averageTime, fill = menuIcon)) +
  geom_bar(stat = "identity") + 
  ggtitle("8x8 Chording Target Icon Times") +
  labs(y = "Average Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-sd, ymax=averageTime+sd), width=.3, position=position_dodge(.9), colour = "black") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1.05, vjust = 1.05, size = 7))



# Average items and menus selected for all conditions
menuData |> 
  group_by(conditionID) |>
  summarize(averageItem = mean(itemsPressed),
            averageMenu = mean(menusPressed),
            seItem = sd(itemsPressed, na.rm = TRUE)/sqrt(length(itemsPressed)),
            seMenu = sd(menusPressed, na.rm = TRUE)/sqrt(length(menusPressed))) |>
  pivot_longer(cols = averageItem:averageMenu, names_to = "type", values_to = "averageNumSelected") |>
  mutate(se = if_else(type == "averageItem", seItem, seMenu)) |>
  ggplot(aes(x = type, y = averageNumSelected, fill = conditionID)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  ggtitle("Number of Menu and Item Selections Across All Conditions") +
  geom_errorbar(aes(ymin=averageNumSelected-se, ymax=averageNumSelected+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(y = "Average Selections", x = "Condition", fill = "Menu Type") +
  scale_fill_manual(values = c("#7ba7ed", "#fa96a2","#4b8cf2", "#d94355", "#0c4cb0", "#a30316"))


#!!!
# NUMBER OF CHORDS PER CONDITION
menuData |>
  filter(betweenLastTime < 200 & menusPressed == 1 & itemsPressed == 1) |>
  group_by(conditionID) |>
  summarise(count = n()) |>
  mutate(count = count / if_else(conditionID == "4x4 Radial" | conditionID == "4x4 Chording",
                                 40*2*4, 1)) |>
  mutate(count = count / if_else(conditionID == "8x4 Radial" | conditionID == "8x4 Chording",
                                 40*4*4, 1)) |>
  mutate(count = count / if_else(conditionID == "8x8 Radial" | conditionID == "8x8 Chording",
                                 40*8*4, 1)) |>
  mutate(type = substring(conditionID, 5, nchar(conditionID))) |>
  ggplot(aes(x = conditionID, y = count, fill = type)) +
  geom_bar(stat = "identity") + 
  ggtitle("Ratio of Chords vs. Menu") +
  labs(y = "Average Chords per Trial", x = "Condition ID", fill = "Menu Type") +
  scale_y_continuous(limits=c(0, 0.5), breaks = seq(0, 0.5, by = 0.1)) 

#!!! added error bars
# CT vs. SECTIONS ALL MENUS
menuData |> 
  filter(time < 20000) |>
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, time, section) |>
  unite(group, c(conditionID, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(time),
            se = sd(time, na.rm = TRUE)/sqrt(length(time))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  ggplot(aes(x = block, y = averageTime, colour = conditionID)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Completion Time (millis)", colour = "Menu Type") +
  ggtitle("Completion Time vs. Block for ONE Learning Block (All Menus)") + 
  facet_wrap(. ~ menuType)


# CT vs. BLOCK 4x4 RADIAL MENU ITEM POSITIONS
menuData |> 
  filter(time < 10000, conditionID == "4x4 Radial") |>
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, time, menuTarget, itemTarget, section) |>
  unite(group, c(menuTarget, itemTarget, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(time)) |>
  mutate(block = as.numeric(trimws(substring(group, 4, nchar(group))))) |>
  mutate(menuPos = trimws(substring(group, 0, 2))) |>
  mutate(itemPos = trimws(substring(group, 2, 4))) |>
  unite(pos, c(itemPos, menuPos), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  ggplot(aes(x = block, y = averageTime, colour = pos)) +
  geom_line() + 
  labs(x = "Block (group of 8 items)", y = "Average Completion Time (millis)", colour = "Position (item, menu)") +
  ggtitle("Completion Time vs. Trial for 4x4 Radial Menu")


# CT vs. BLOCK 4x4 CHORDING MENU ITEM POSITIONS
menuData |> 
  filter(time < 10000, conditionID == "4x4 Chording") |>
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, time, menuTarget, itemTarget, section) |>
  unite(group, c(menuTarget, itemTarget, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(time)) |>
  mutate(block = as.numeric(trimws(substring(group, 4, nchar(group))))) |>
  mutate(menuPos = trimws(substring(group, 0, 2))) |>
  mutate(itemPos = trimws(substring(group, 2, 4))) |>
  unite(pos, c(itemPos, menuPos), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  ggplot(aes(x = block, y = averageTime, colour = pos)) +
  geom_line() + 
  labs(x = "Block (group of 8 items)", y = "Average Completion Time (millis)", colour = "Position (item, menu)") +
  ggtitle("Completion Time vs. Trial for 4x4 Chording Menu")
  


# CT vs. BLOCK 8x4 RADIAL MENU ITEM POSITIONS
menuData |> 
  filter(time < 10000, conditionID == "8x4 Radial") |>
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, time, menuTarget, itemTarget, section) |>
  unite(group, c(menuTarget, itemTarget, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(time)) |>
  mutate(block = as.numeric(trimws(substring(group, 4, nchar(group))))) |>
  mutate(menuPos = trimws(substring(group, 0, 2))) |>
  mutate(itemPos = trimws(substring(group, 2, 4))) |>
  unite(pos, c(itemPos, menuPos), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  ggplot(aes(x = block, y = averageTime, colour = pos)) +
  geom_line() + 
  labs(x = "Block (group of 8 items)", y = "Average Completion Time (millis)", colour = "Position (item, menu)") +
  ggtitle("Completion Time vs. Trial for 8x4 Radial Menu")


# CT vs. BLOCK 8x4 CHORDING MENU ITEM POSITIONS
menuData |> 
  filter(time < 10000, conditionID == "8x4 Chording") |>
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, time, menuTarget, itemTarget, section) |>
  unite(group, c(menuTarget, itemTarget, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(time)) |>
  mutate(block = as.numeric(trimws(substring(group, 4, nchar(group))))) |>
  mutate(menuPos = trimws(substring(group, 0, 2))) |>
  mutate(itemPos = trimws(substring(group, 2, 4))) |>
  unite(pos, c(itemPos, menuPos), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  ggplot(aes(x = block, y = averageTime, colour = pos)) +
  geom_line() + 
  labs(x = "Block (group of 8 items)", y = "Average Completion Time (millis)", colour = "Position (item, menu)") +
  ggtitle("Completion Time vs. Trial for 8x4 Chording Menu")


# CT vs. BLOCK 8x8 RADIAL MENU ITEM POSITIONS
menuData |> 
  filter(time < 10000, conditionID == "8x8 Radial") |>
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, time, menuTarget, itemTarget, section) |>
  unite(group, c(menuTarget, itemTarget, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(time)) |>
  mutate(block = as.numeric(trimws(substring(group, 4, nchar(group))))) |>
  mutate(menuPos = trimws(substring(group, 0, 2))) |>
  mutate(itemPos = trimws(substring(group, 2, 4))) |>
  unite(pos, c(itemPos, menuPos), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  ggplot(aes(x = block, y = averageTime, colour = pos)) +
  geom_line() + 
  labs(x = "Block (group of 8 items)", y = "Average Completion Time (millis)", colour = "Position (item, menu)") +
  ggtitle("Completion Time vs. Trial for 8x8 Radial Menu")


# CT vs. BLOCK 8x8 CHORDING MENU ITEM POSITIONS
menuData |> 
  filter(time < 10000, conditionID == "8x8 Chording") |>
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, time, menuTarget, itemTarget, section) |>
  unite(group, c(menuTarget, itemTarget, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(time)) |>
  mutate(block = as.numeric(trimws(substring(group, 4, nchar(group))))) |>
  mutate(menuPos = trimws(substring(group, 0, 2))) |>
  mutate(itemPos = trimws(substring(group, 2, 4))) |>
  unite(pos, c(itemPos, menuPos), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  ggplot(aes(x = block, y = averageTime, colour = pos)) +
  geom_line() + 
  labs(x = "Block (group of 8 items)", y = "Average Completion Time (millis)", colour = "Position (item, menu)") +
  ggtitle("Completion Time vs. Trial for 8x8 Chording Menu")


# !!!
# CHORDING TIME vs. SECTIONS ALL MENUS
menuData |> 
  filter(betweenLastTime < 20000) |>
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, betweenLastTime, section) |>
  unite(group, c(conditionID, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(betweenLastTime),
            se = sd(betweenLastTime, na.rm = TRUE)/sqrt(length(betweenLastTime))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  ggplot(aes(x = block, y = averageTime, colour = conditionID)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Chording Time (millis)", colour = "Menu Type") +
  ggtitle("Chording Time vs. Block for ONE Learning Block (All Menus)") + 
  facet_wrap(. ~ menuType)



#!!!
# NUM OF TOTAL SELECTIONS vs. SECTIONS ALL MENUS
menuData |> 
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, itemsPressed, menusPressed, section) |>
  mutate(totalSelected = itemsPressed + menusPressed) |>
  unite(group, c(conditionID, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(average = mean(totalSelected),
            se = sd(totalSelected, na.rm = TRUE)/sqrt(length(totalSelected))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  ggplot(aes(x = block, y = average, colour = conditionID)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=average-se, ymax=average+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Items + Menus Selected", colour = "Menu Type") +
  ggtitle("Chording Time vs. Block for ONE Learning Block (All Menus)") + 
  facet_wrap(. ~ menuType)



# NUM ITEMS SELECTED vs. SECTIONS ALL MENUS
menuData |> 
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, itemsPressed, menusPressed, section) |>
  unite(group, c(conditionID, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(average = mean(itemsPressed),
            se = sd(itemsPressed, na.rm = TRUE)/sqrt(length(itemsPressed))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  ggplot(aes(x = block, y = average, colour = conditionID)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=average-se, ymax=average+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Items Selected", colour = "Menu Type") +
  ggtitle("Average Num of Items Selected vs. Block for ONE Learning Block (All Menus)") + 
  facet_wrap(. ~ menuType)

#!!!
# NUM MENUS SELECTED vs. SECTIONS ALL MENUS
menuData |> 
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, learningBlock, block, itemsPressed, menusPressed, section) |>
  unite(group, c(conditionID, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(average = mean(menusPressed),
            se = sd(menusPressed, na.rm = TRUE)/sqrt(length(menusPressed))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  ggplot(aes(x = block, y = average, colour = conditionID)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=average-se, ymax=average+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Menus Selected", colour = "Menu Type") +
  ggtitle("Average Num of Menus Selected vs. Block for ONE Learning Block (All Menus)") + 
  facet_wrap(. ~ menuType)

# !!!
menuData |> 
  mutate(type = if_else(betweenLastTime < 200 & itemsPressed == 1 & menusPressed ==1,
                        "Chord",
                        if_else(betweenFirstTime < 200 & (itemsPressed > 1 | menusPressed > 1),
                                "Fire and Fix",
                                "Search"))) |>
  group_by(type) |>
  summarise(types = n()/4480) |>
  mutate(types = types * 100) |>
  #pivot_wider(names_from = type, values_from = types) 
  ggplot(aes(x = type, y = types, fill = type)) +
    geom_bar(stat = 'identity') + 
    theme(legend.position="none") 



# TARGET POSITIONS CHORDING 4x4 CHORDING TIMES
menuData |>
  filter(conditionID == "4x4 Chording", betweenLastTime < 10000) |>
  unite(coords, c(menuTarget, itemTarget), sep = ",", remove = FALSE) |>
  print(n = 50) |>
  group_by(coords) |>
  summarize(averageTime = mean(betweenLastTime),
            sd = sd(betweenLastTime, na.rm = TRUE),
            se = sd/sqrt(length(betweenLastTime))) |>
  mutate(menu = substring(coords, 0,1)) |>
  ggplot(aes(x = coords, y = averageTime, fill = menu)) +
  geom_bar(stat = "identity") + 
  ggtitle("4x4 Keyboard Target Times") +
  labs(y = "Average Chording Time", x = "Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "darkgreen") +
  geom_text(aes(label = coords), vjust = 0.45, hjust = 3.3, size = 4.8, angle = 90, colour = "black") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# TARGET POSITIONS CHORDING 8x4 CHORDING TIMES
menuData |>
  filter(conditionID == "8x4 Chording", betweenLastTime < 10000) |>
  group_by(itemTarget) |>
  summarize(averageTime = mean(betweenLastTime),
            sd = sd(betweenLastTime, na.rm = TRUE),
            se = sd/sqrt(length(betweenLastTime))) |>
  ggplot(aes(x = itemTarget, y = averageTime)) +
  geom_bar(stat = "identity") + 
  ggtitle("4x4 Chording Target Times") +
  labs(y = "Average Time", x = "Item Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "red") 

# TARGET POSITIONS CHORDING 8x4 CHORDING TIMES
menuData |>
  filter(conditionID == "4x4 Chording", betweenLastTime < 10000) |>
  group_by(menuTarget) |>
  summarize(averageTime = mean(betweenLastTime),
            sd = sd(betweenLastTime, na.rm = TRUE),
            se = sd/sqrt(length(betweenLastTime))) |>
  ggplot(aes(x = menuTarget, y = averageTime, fill = "lightblue")) +
  geom_bar(stat = "identity") + 
  ggtitle("4x4 Chording Target Times") +
  labs(y = "Average Time", x = "Item Target") +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "red") 



#FARREN TEST SECOND VERSION OF SYSTEM 

menuDataLonger <- read_csv("menuLongerTrial.csv")

# chording time over blocks
menuDataLonger |> 
  filter(conditionID == "8x8 Radial", betweenLastTime < 50000) |>
  group_by(block) |>
  summarise(chordingTime = mean(betweenLastTime)) |>
  ggplot(aes(x = block, y = chordingTime)) +
    geom_line() +
    scale_x_continuous(limits=c(1, 10), breaks = seq(1, 10, by = 1)) +
    geom_hline(yintercept = 200, color = 'red',linetype="dotted") +
    labs(x = "Block Number", y = "Average Chording Time")

#!!!
menuDataLonger |> 
  filter(betweenLastTime < 50000) |>
  select(conditionID, block, betweenLastTime) |>
  unite(group, c(conditionID, block), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(betweenLastTime),
            se = sd(betweenLastTime, na.rm = TRUE)/sqrt(length(betweenLastTime))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  ggplot(aes(x = block, y = averageTime, colour = conditionID)) +
  geom_line() + 
  #geom_point() +
  #geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Chording Time (millis)", colour = "Menu Type") +
  ggtitle("Chording Time vs. Block for All Menus (Farren's Test Data)") + 
  facet_wrap(. ~ menuType) + 
  geom_hline(yintercept = 200, color = 'black',linetype="dotted") +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1, 10, by = 1)) 

# !!!
menuDataLonger |>
  filter(betweenLastTime < 200 & menusPressed == 1 & itemsPressed == 1) |>
  select(conditionID, block, betweenLastTime) |>
  unite(group, c(conditionID, block), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(count = n()) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 1, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  mutate(type = substring(conditionID, 5, nchar(conditionID))) |>
  print(n = 30) |>
  ggplot(aes(x = block, y = count, color = type)) +
  geom_line() + 
  ggtitle("Number of chords over blocks") +
  facet_wrap(. ~ menuType) + 
  labs(y = "Average Chords per Menu (Farren's Test Data)", x = "Condition ID", fill = "Menu Type") +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1, 10, by = 1)) 

#!!!
# NUMBER OF CHORDS PER CONDITION
menuDataLonger |>
  filter(betweenLastTime < 200 & menusPressed == 1 & itemsPressed == 1) |>
  group_by(conditionID) |>
  summarise(count = n()) |>
  mutate(count = count / if_else(conditionID == "4x4 Radial" | conditionID == "4x4 Chording",
                                 80, 1)) |>
  mutate(count = count / if_else(conditionID == "8x4 Radial" | conditionID == "8x4 Chording",
                                 80, 1)) |>
  mutate(count = count / if_else(conditionID == "8x8 Radial" | conditionID == "8x8 Chording",
                                 80, 1)) |>
  mutate(type = substring(conditionID, 5, nchar(conditionID))) |>
  ggplot(aes(x = conditionID, y = count, fill = type)) +
  geom_bar(stat = "identity") + 
  ggtitle("Ratio of Chords per Trial (Farren's Test Data)") +
  labs(y = "Average Chords per Menu", x = "Condition ID", fill = "Menu Type") +
  scale_y_continuous(limits=c(0, 1), breaks = seq(0, 1, by = 0.1)) 
  
    
menuData_07_16 <- read_csv('menusTrial0716.csv')

menuData_07_16 |> filter(pID == 34, oneOffItem | oneOffMenu, conditionID == "8x8 Radial") |>
  group_by(block) |>
  summarise(count = n()) |>
  ggplot(aes(x = block, y = count)) +
    geom_line()


stream <- read_csv("menusStream0716.csv") 

stream |> filter(conditionID == "8x8 Radial", pID == 21, block == 1, trial == 2) |>
  select(event, time) |>
  mutate(toSub = as.numeric(time[1])) |>
  mutate(position = substring(event, nchar(event), nchar(event)))|>
  mutate(event = substring(event, 0, nchar(event)-2)) |>
  mutate(type = substring(event, 0, 4)) |>
  mutate(event = substring(event, 6, nchar(event))) |>
  mutate(event = if_else(event == "selected" | event == "opened", "start",
                         if_else(event == "stick released" | event == "closed", "end", event))) |>
  pivot_wider(names_from = event, values_from = time) |>
  mutate(end = if_else(is.na(end), lead(start), end)) |>
  mutate(position = if_else(position == 0, "E", if_else(position == 1, "SE", 
                    if_else(position == 2, "S", if_else(position == 3, "SW", 
                    if_else(position == 4, "W", if_else(position == 5, "NW",
                    if_else(position == 6, "N", "NE")))))))) |>
  mutate(start = (start - toSub) / 1000, end = (end - toSub)/1000) |>
  print(n = 50) |>
  gg_vistime(col.group = "type", col.event = "position", show_labels = TRUE, optimize_y = FALSE, title = "Block 1, Trial 2, 8x8 Radial Menu") + 
    scale_x_continuous(limits=c(0, 2.5), breaks = seq(0, 2.5, by = 0.5)) + 
    labs(x = "Time (secs)")
    

  
trialPlot1 <- stream |> filter(conditionID == "8x8 Chording", pID == 22, block == 1, trial == 1) |>
  select(event, time) |>
  mutate(toSub = time[1]) |>
  filter(event != "error") |>
  separate(event, c("position", "event"), ":") |>
  filter(position != "") |>
  pivot_wider(names_from = event, values_from = time) |>
  unnest() |>
  mutate(type = if_else(position %in% c('q','w','e','r','a','s','d','f'),"Menu","Item")) |>
  mutate(down = (down - toSub) / 1000, up = (up - toSub)/1000) |>
  print(n = 50) |>
  gg_vistime(col.event = "position", col.group = "type", col.start = "down", col.end = "up", show_labels = TRUE, optimize_y = FALSE, title = "1st time seeing item") +
  scale_x_continuous(limits=c(0, 8), breaks = seq(0, 8, by = 1)) + 
  labs(x = "Time (secs)") + 
  aes(color = event) +
  scale_color_manual(name = "issue", values = brewer.pal(12, "Set3"), guide = "none")



trialPlot2 <- stream |> filter(conditionID == "8x8 Chording", pID == 22, block == 5, trial == 7) |>
  select(event, time) |>
  mutate(toSub = time[1]) |>
  filter(event != "error") |>
  separate(event, c("position", "event"), ":") |>
  filter(position != "") |>
  pivot_wider(names_from = event, values_from = time) |>
  unnest() |>
  mutate(type = if_else(position %in% c('q','w','e','r','a','s','d','f'),"Menu","Item")) |>
  mutate(down = (down - toSub) / 1000, up = (up - toSub)/1000) |>
  print(n = 50) |>
  gg_vistime(col.event = "position", col.group = "type", col.start = "down", col.end = "up", show_labels = TRUE, optimize_y = FALSE, title = "5th time seeing item") +
  scale_x_continuous(limits=c(0, 8), breaks = seq(0, 8, by = 1)) + 
  labs(x = "Time (secs)") + 
  aes(color = event) +
  scale_color_manual(name = "issue", values = brewer.pal(12, "Set3"), guide = "none")


ggarrange(trialPlot1, trialPlot2, labels = c("pID: 22", "pID: 22"))

trialPlot4 <- stream |> filter(conditionID == "8x8 Chording", pID == 24, block == 5, trial == 3) |>
  select(event, time) |>
  mutate(toSub = time[1]) |>
  filter(event != "error") |>
  separate(event, c("position", "event"), ":") |>
  filter(position != "") |>
  pivot_wider(names_from = event, values_from = time) |>
  unnest() |>
  mutate(type = if_else(position %in% c('q','w','e','r','a','s','d','f'),"Menu","Item")) |>
  mutate(down = (down - toSub) / 1000, up = (up - toSub)/1000) |>
  print(n = 50) |>
  gg_vistime(col.event = "position", col.group = "type", col.start = "down", col.end = "up", show_labels = TRUE, optimize_y = FALSE, title = "5th time seeing item") +
  scale_x_continuous(limits=c(0, 4), breaks = seq(0, 4, by = 1)) + 
  labs(x = "Time (secs)") + 
  aes(color = event) +
  scale_colour_discrete(drop=TRUE, limits = levels(stream$event)) + 
  theme(legend.position="none")

trialPlot3 <- stream |> filter(conditionID == "8x8 Chording", pID == 24, block == 1, trial == 3) |>
  select(event, time) |>
  mutate(toSub = time[1]) |>
  filter(event != "error") |>
  separate(event, c("position", "event"), ":") |>
  filter(position != "") |>
  pivot_wider(names_from = event, values_from = time) |>
  unnest() |>
  mutate(type = if_else(position %in% c('q','w','e','r','a','s','d','f'),"Menu","Item")) |>
  mutate(down = (down - toSub) / 1000, up = (up - toSub)/1000) |>
  print(n = 50) |>
  gg_vistime(col.event = "position", col.group = "type", col.start = "down", col.end = "up", show_labels = TRUE, optimize_y = FALSE, title = "1st time seeing item") +
  scale_x_continuous(limits=c(0, 4), breaks = seq(0, 4, by = 1)) + 
  labs(x = "Time (secs)") + 
  aes(color = event) +
  scale_colour_discrete(drop=TRUE, limits = levels(stream$event)) + 
  theme(legend.position="none")

ggarrange(trialPlot3, trialPlot4, labels = c("pID: 24", "pID: 24"))




trialPlot5 <- stream |> filter(conditionID == "8x8 Chording", pID == 32, block == 1, trial == 4) |>
  select(event, time) |>
  mutate(toSub = time[1]) |>
  filter(event != "error") |>
  separate(event, c("position", "event"), ":") |>
  filter(position != "") |>
  pivot_wider(names_from = event, values_from = time) |>
  unnest() |>
  mutate(type = if_else(position %in% c('q','w','e','r','a','s','d','f'),"Menu","Item")) |>
  mutate(down = (down - toSub) / 1000, up = (up - toSub)/1000) |>
  print(n = 50) |>
  gg_vistime(col.event = "position", col.group = "type", col.start = "down", col.end = "up", show_labels = TRUE, optimize_y = FALSE, title = "1st time seeing item") +
  scale_x_continuous(limits=c(0, 8), breaks = seq(0, 8, by = 1)) + 
  labs(x = "Time (secs)") + 
  aes(color = event) +
  scale_color_manual(name = "issue", values = brewer.pal(12, "Set3"), guide = "none")

trialPlot6 <- stream |> filter(conditionID == "8x8 Chording", pID == 32, block == 5, trial == 4) |>
  select(event, time) |>
  mutate(toSub = time[1]) |>
  filter(event != "error") |>
  separate(event, c("position", "event"), ":") |>
  filter(position != "") |>
  pivot_wider(names_from = event, values_from = time) |>
  unnest() |>
  mutate(type = if_else(position %in% c('q','w','e','r','a','s','d','f'),"Menu","Item")) |>
  mutate(down = (down - toSub) / 1000, up = (up - toSub)/1000) |>
  print(n = 50) |>
  gg_vistime(col.event = "position", col.group = "type", col.start = "down", col.end = "up", show_labels = TRUE, optimize_y = FALSE, title = "5th time seeing item") +
  scale_x_continuous(limits=c(0, 8), breaks = seq(0, 8, by = 1)) + 
  labs(x = "Time (secs)") + 
  aes(color = event) +
  scale_color_manual(name = "issue", values = brewer.pal(12, "Set3"), guide = "none")


ggarrange(trialPlot5, trialPlot6, labels = c("pID: 32", "pID: 32"))










# LONGER TRIALS 8 ITEMS 10 BLOCKS 
# 4 PARTICIPANTS 

long_data <- read_csv("menusTrialData_2024-07-23.csv")

long_data |> filter(pID > 40 & betweenLastTime < 400 & menusPressed == 1 & itemsPressed == 1) |>
  group_by(conditionID) |>
  summarise(count = n()) |>
  mutate(count = count / (80*4)) |>
  mutate(type = substring(conditionID, 5, nchar(conditionID))) |>
  ggplot(aes(x = conditionID, y = count, fill = type)) +
  geom_bar(stat = "identity") + 
  ggtitle("Ratio of Chords vs. Menu") +
  labs(y = "Average Chords per Trial", x = "Condition ID", fill = "Menu Type") +
  scale_y_continuous(limits=c(0, 1), breaks = seq(0, 1, by = 0.2)) 


# AVERAGE CHORDS OVER TIME PER MENU TYPE
long_data |> filter(pID > 40 & betweenLastTime < 300 & menusPressed == 1 & itemsPressed == 1) |>
  group_by(block, conditionID) |>
  summarise(count = n()/24) |>
  mutate(type = substring(conditionID, 5, nchar(conditionID))) |>
  print(n = 50) |>
  ggplot(aes(x = block, y = count, color = conditionID)) +
  geom_line() + 
  ggtitle("Ratio of Chords vs. Menu") +
  labs(y = "Average Chords per Trial", x = "Block", fill = "Menu Type") +
  scale_y_continuous(limits=c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1, 10, by = 2)) 

# AVERAGE CHORDS OVER TIME PER PARTICIPANT
long_data |> filter(pID > 40 & betweenLastTime < 400 & menusPressed == 1 & itemsPressed == 1 & conditionID == "8x8 Radial") |>
  mutate(pID = as_factor(pID)) |>
  group_by(block, pID, .drop = FALSE) |>
  summarise(count = n()/6) |>
  print(n = 50) |>
  ggplot(aes(x = block, y = count, color = pID)) +
  geom_line() + 
  ggtitle("Ratio of Chords vs. Menu") +
  labs(y = "Average Chords per Trial", x = "Block", fill = "Menu Type") +
  scale_y_continuous(limits=c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1, 10, by = 1)) 



# KEYBOARD MENU
# different lines for participants 
# chording times 
long_data |> 
  filter(betweenLastTime < 10000, pID > 40) |>
  select(conditionID, block, betweenLastTime, pID) |>
  unite(group, c(conditionID, block), sep = " ", remove = FALSE) |>
  group_by(group, pID) |>
  summarise(averageTime = mean(betweenLastTime),
            se = sd(betweenLastTime, na.rm = TRUE)/sqrt(length(betweenLastTime))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  filter(conditionID == "Chording") |>
  mutate(pID = as_factor(pID)) |>
  ggplot(aes(x = block, y = averageTime, colour = pID)) +
  geom_line() + 
  #geom_point() +
  #geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Chording Time (millis)", colour = "Participant ID") +
  ggtitle("Participant ID vs. Chording Time over 10 blocks in Keyboard (4 participants)") + 
  facet_wrap(. ~ menuType) + 
  geom_hline(yintercept = 200, color = 'black',linetype="dotted") +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1, 10, by = 1)) + 
  #scale_y_continuous(limits=c(0, 1600), breaks = seq(0, 1600, by = 200)) + 
  scale_color_manual(name = "issue", values = brewer.pal(12, "Set1"))


# RADIAL MENU
# different lines for participants 
# chording times 
long_data |> 
  filter(betweenLastTime < 10000, pID > 40) |>
  select(conditionID, block, betweenLastTime, pID) |>
  unite(group, c(conditionID, block), sep = " ", remove = FALSE) |>
  group_by(group, pID) |>
  summarise(averageTime = mean(betweenLastTime),
            se = sd(betweenLastTime, na.rm = TRUE)/sqrt(length(betweenLastTime))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  filter(conditionID == "Radial") |>
  mutate(pID = as_factor(pID)) |>
  ggplot(aes(x = block, y = averageTime, colour = pID)) +
  geom_line() +  
  labs(x = "Block", y = "Average Chording Time (millis)", colour = "Participant ID") +
  ggtitle("Participant ID vs. Chording Time over 10 blocks in Radial (4 participants)") + 
  facet_wrap(. ~ menuType) + 
  geom_hline(yintercept = 200, color = 'black',linetype="dotted") +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1, 10, by = 1)) + 
  scale_color_manual(name = "issue", values = brewer.pal(12, "Set1"))

# RADIAL MENU
# different lines for participants 
# CT 
long_data |> 
  filter(betweenLastTime < 10000, pID > 40) |>
  select(conditionID, block, time, pID) |>
  unite(group, c(conditionID, block), sep = " ", remove = FALSE) |>
  group_by(group, pID) |>
  summarise(averageTime = mean(time),
            se = sd(time, na.rm = TRUE)/sqrt(length(time))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  filter(conditionID == "Radial") |>
  mutate(pID = as_factor(pID)) |>
  ggplot(aes(x = block, y = averageTime, colour = pID)) +
  geom_line() +  
  #geom_point() +
  #geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Chording Time (millis)", colour = "Participant ID") +
  ggtitle("Participant ID vs. Chording Time over 10 blocks in Radial (4 participants)") + 
  facet_wrap(. ~ menuType) + 
  scale_x_continuous(limits=c(1, 10), breaks = seq(1, 10, by = 1)) + 
  scale_color_manual(name = "issue", values = brewer.pal(12, "Set1"))

# KEYBOARD MENU
# different lines for participants 
# CT 
long_data |> 
  filter(betweenLastTime < 10000, pID > 40) |>
  select(conditionID, block, time, pID) |>
  unite(group, c(conditionID, block), sep = " ", remove = FALSE) |>
  group_by(group, pID) |>
  summarise(averageTime = mean(time),
            se = sd(time, na.rm = TRUE)/sqrt(length(time))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  filter(conditionID == "Chording") |>
  mutate(pID = as_factor(pID)) |>
  ggplot(aes(x = block, y = averageTime, colour = pID)) +
  geom_line() +  
  #geom_point() +
  #geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Chording Time (millis)", colour = "Participant ID") +
  ggtitle("Participant ID vs. Chording Time over 10 blocks in Keyboard (4 participants)") + 
  facet_wrap(. ~ menuType) + 
  scale_x_continuous(limits=c(1, 10), breaks = seq(1, 10, by = 1)) + 
  scale_color_manual(name = "issue", values = brewer.pal(12, "Set1"))


long_data |>
  filter(time < 20000) |>
  mutate(section = (block - 1) %% 5 + 1) |>
  select(conditionID, block, time, section) |>
  unite(group, c(conditionID, section), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(time),
            se = sd(time, na.rm = TRUE)/sqrt(length(time))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  ggplot(aes(x = block, y = averageTime, colour = conditionID)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Completion Time (millis)", colour = "Menu Type") +
  ggtitle("Completion Time vs. Block (All Menus)") + 
  facet_wrap(. ~ menuType)





long_data |> filter(pID > 40, conditionID == "8x8 Radial") |>
  mutate(pID = as_factor(pID)) |>
  group_by(block) |>
  summarise(oneOffCount = sum(oneOffItem == TRUE | oneOffMenu == TRUE),
            chordingCount = sum(betweenLastTime < 200 & menusPressed == 1 & itemsPressed == 1),
            fireFix = sum(betweenFirstTime < 200 & (menusPressed > 1 | itemsPressed > 1)),
            visualSearch = sum((menusPressed > 1 | itemsPressed > 1) & betweenFirstTime >= 200 )) |>
  print(n = 10) |>
  pivot_longer(cols = oneOffCount:visualSearch, names_to = "type", values_to = "count") |>
  
  ggplot(aes(x = block, y = count, color = type)) +
  geom_line() +
  scale_color_manual(name = "issue", values = brewer.pal(12, "Set1"))

long_data |> filter(pID > 40, conditionID == "8x8 Chording") |>
  mutate(pID = as_factor(pID)) |>
  mutate(strat = if_else((oneOffItem == TRUE | oneOffMenu == TRUE), "oneOff", 
                 if_else((betweenLastTime < 200 & menusPressed == 1 & itemsPressed == 1), "chording",
                 if_else((betweenFirstTime < 200 & (menusPressed > 1 | itemsPressed > 1)), "fireFix",
                         "visualSearch")))) |>
  mutate(strat = as_factor(strat)) |>
  group_by(block) |>
  summarise(count = n())
  ggplot(aes(x = block, y = strat, fill = strat)) +
  geom_bar(stat = "identity") +
  scale_color_manual(name = "issue", values = brewer.pal(12, "Set1"))



  
  
  
  
  
  
  
  
streamLong <- read_csv("menusStreamData_2024-07-23.csv") 

streamLongPlot1 <- streamLong |> filter(conditionID == "8x8 Radial", pID == 43, block == 1, trial == 2) |>
  select(event, time) |>
  filter(event != "error") |>
  mutate(toSub = as.numeric(time[1])) |>
  mutate(position = substring(event, nchar(event), nchar(event)))|>
  mutate(event = substring(event, 0, nchar(event)-2)) |>
  mutate(type = substring(event, 0, 4)) |>
  mutate(event = substring(event, 6, nchar(event))) |>
  mutate(event = if_else(event == "selected" | event == "opened", "start",
                         if_else(event == "stick released" | event == "closed", "end", event))) |>
  pivot_wider(names_from = event, values_from = time) |>
  mutate(end = if_else(is.na(end), lead(start), end)) |>
  mutate(position = if_else(position == 0, "E", if_else(position == 1, "SE", 
                                                        if_else(position == 2, "S", if_else(position == 3, "SW", 
                                                                                            if_else(position == 4, "W", if_else(position == 5, "NW",
                                                                                                                                if_else(position == 6, "N", "NE")))))))) |>
  mutate(start = (as.numeric(start) - as.numeric(toSub)), end = (as.numeric(end) - as.numeric(toSub))) |>
  gg_vistime(col.group = "type", col.event = "position", show_labels = TRUE, optimize_y = FALSE, title = "Block 1, Trial 2, 8x8 Radial Menu") + 
  scale_x_continuous(limits=c(0, 5000), breaks = seq(0, 5000, by = 500)) + 
  labs(x = "Time (secs)")

streamLongPlot2 <- streamLong |> filter(conditionID == "8x8 Radial", pID == 43, block == 10, trial == 2) |>
  select(event, time) |>
  filter(event != "error") |>
  mutate(toSub = as.numeric(time[1])) |>
  mutate(position = substring(event, nchar(event), nchar(event)))|>
  mutate(event = substring(event, 0, nchar(event)-2)) |>
  mutate(type = substring(event, 0, 4)) |>
  mutate(event = substring(event, 6, nchar(event))) |>
  mutate(event = if_else(event == "selected" | event == "opened", "start",
                         if_else(event == "stick released" | event == "closed", "end", event))) |>
  pivot_wider(names_from = event, values_from = time) |>
  mutate(end = if_else(is.na(end), lead(start), end)) |>
  mutate(position = if_else(position == 0, "E", if_else(position == 1, "SE", 
                                                        if_else(position == 2, "S", if_else(position == 3, "SW", 
                                                                                            if_else(position == 4, "W", if_else(position == 5, "NW",
                                                                                                                                if_else(position == 6, "N", "NE")))))))) |>
  mutate(start = (as.numeric(start) - as.numeric(toSub)), end = (as.numeric(end) - as.numeric(toSub))) |>
  gg_vistime(col.group = "type", col.event = "position", show_labels = TRUE, optimize_y = FALSE, title = "Block 10, Trial 2, 8x8 Radial Menu") + 
  scale_x_continuous(limits=c(0, 5000), breaks = seq(0, 5000, by = 500)) + 
  labs(x = "Time (secs)")


ggarrange(streamLongPlot1, streamLongPlot2, labels = c("pID: 32", "pID: 32"))








#ADAMS DATA 

adam_data <- read_csv("menusTrialData_2024-07-25.csv") |> print(n = 4)


# !!! chording time vs block
adam_data |> 
  filter(participantID == "Adam") |>
  select(conditionID, block, betweenLastTime) |>
  unite(group, c(conditionID, block), sep = " ", remove = FALSE) |>
  group_by(group) |>
  summarise(averageTime = mean(betweenLastTime),
            se = sd(betweenLastTime, na.rm = TRUE)/sqrt(length(betweenLastTime))) |>
  mutate(block = as.numeric(trimws(substring(group, nchar(group)-1, nchar(group))))) |>
  mutate(conditionID = trimws(substring(group, 4, nchar(group)-2))) |>
  mutate(menuType = substring(group, 0, 3)) |>
  ggplot(aes(x = block, y = averageTime, colour = conditionID)) +
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin=averageTime-se, ymax=averageTime+se), width=.3, position=position_dodge(.9), colour = "black") +
  labs(x = "Block", y = "Average Chording Time (millis)", colour = "Menu Type") +
  ggtitle("Chording Time vs. Block for All Menus (Adam)") + 
  facet_wrap(. ~ menuType) + 
  geom_hline(yintercept = 400, color = 'black',linetype="dotted") +
  scale_x_continuous(limits=c(1, 10), breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(limits=c(0, 1600), breaks = seq(0, 1600, by = 200)) 

adam_data |> filter(participantID == "Adam" & betweenLastTime < 200 & menusPressed == 1 & itemsPressed == 1) |>
  group_by(conditionID) |>
  summarise(count = n()) |>
  mutate(count = count / (80)) |>
  mutate(type = substring(conditionID, 5, nchar(conditionID))) |>
  ggplot(aes(x = conditionID, y = count, fill = type)) +
  geom_bar(stat = "identity") + 
  ggtitle("Ratio of Chords vs. Menu") +
  labs(y = "Average Chords per Trial", x = "Condition ID", fill = "Menu Type") +
  scale_y_continuous(limits=c(0, 1), breaks = seq(0, 1, by = 0.2)) 

