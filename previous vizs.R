#Visualizing stacked per day
ggplot(chat, aes(x = mday(time), fill = author)) + 
  stat_count(position = "stack", show.legend = TRUE) + 
  ggtitle("Conversations per day") + ylab("# of messages") + 
  xlab("time") + 
  theme(plot.title = element_text(face = "italic"))+ 
  scale_x_continuous(breaks=seq(0,31,1))

#Visualizing dodged per day
ggplot(chat, aes(x = mday(time), fill = author)) + 
  stat_count(position = "dodge", show.legend = TRUE) + 
  ggtitle("Conversations per day") + ylab("# of messages") + 
  xlab("time") + 
  theme(plot.title = element_text(face = "italic"))+ 
  scale_x_continuous(breaks=seq(0,31,1))

#Visualizing stacked per hour
ggplot(chat, aes(x = hour(time), fill = author)) + 
  stat_count(position = "stack", show.legend = TRUE) + 
  ggtitle("Conversations per day") + ylab("# of messages") + 
  xlab("time") + 
  theme(plot.title = element_text(face = "italic"))+ 
  scale_x_continuous(breaks=seq(0,23,1))

#Visualizing dodged per hour
ggplot(chat, aes(x = hour(time), fill = author)) + 
  stat_count(position = "dodge", show.legend = TRUE) + 
  ggtitle("Conversations per day") + ylab("# of messages") + 
  xlab("time") + 
  theme(plot.title = element_text(face = "italic"))+ 
  scale_x_continuous(breaks=seq(0,23,1))


# 1. Who messages the most.
# 2. What time are most messages sent
# 3. Distribution of time of message by person
# 4. Most used words and emojis by person.