ggplot(data = filter(models, training == "metaweb", nsample <= 3000), aes(x = nsample, y = auc.metaweb, colour = training)) +
  geom_point( alpha = 0.5, size = 2, color = "#ede7e3") +
  geom_smooth(method = "gam", size = 1, se = F, color = "#ede7e3") +
  ylim(0.7, 1) +
  scale_x_continuous(limits = c(0,3000), trans = "sqrt", breaks = c(0, 100, 1000, 3000)) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme(
    panel.border = element_rect(color = "#ede7e3", fill= NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 18, color = "#ede7e3"),
    axis.ticks = element_line(color = "#ede7e3"),
    panel.background = element_rect(fill = "#0B4F6C"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#0B4F6C", color = "#0B4F6C"),
    legend.position = "none"
  )

ggsave("figures/results1.png", width = 5, height = 4)

####

ggtree(tree_order, size = 1, ladderize = F, color = "#ede7e3") +
  theme_tree(bgcolor = "#0B4F6C")+
  ggplot(data = filter(df, Measure == "AUC", Type.of.interaction != "all"), aes(y = value, x = Order)) +
  geom_jitter(alpha = 0.5, size = 1, color = "#ede7e3") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0, color = "#ede7e3") +
  stat_summary(fun= "mean", geom="pointrange", shape = 21, fill = "white", size = 0.4, color = "#ede7e3") +
  coord_flip()+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), labels = c("0","0.25","0.5","0.75","1")) +
  labs(y = "AUC", x = NULL) +
  facet_wrap(~Type.of.interaction,
             labeller = labeller(Type.of.interaction = c("all" = "All Interactions", "in"= "Prey",
                                                         "out" = "Predators"))) +
  theme(
    panel.border = element_rect(color = "#ede7e3", fill= NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text = element_text(size = 18, color = "#ede7e3"),    
    axis.ticks = element_line(color = "#ede7e3"),
    panel.background = element_rect(fill = "#0B4F6C"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#0B4F6C", color = "#0B4F6C"),
    strip.background = element_rect(color = "#ede7e3", fill= NA),
    strip.text = element_text(size = 18, color = "#ede7e3")
  ) + 
  plot_layout(nrow = 1, widths = c(0.2, 0.8)) &
  theme(plot.background = element_rect(fill = "#0B4F6C", color = "#0B4F6C"))

ggsave("figures/results2.png", width = 6, height = 4)


####
species_performance <- filter(species_performance, Target == "Serengeti")
overall_performance <- filter(overall_performance, Target == "Serengeti")

# geographic distance
p1 <- ggplot(species_performance) +
  geom_jitter(aes(x = geo.dist/1000, y = auc, colour = Source), alpha = 0.2, height = 0, width = 0.3) +
  geom_point(data = overall_performance, aes(x = geo.dist/1000, y = auc, fill = Source), shape = 21, size = 3) +
  scale_fill_manual(values = c("deepskyblue","royalblue4", "red3", "chartreuse4")) +
  scale_y_continuous(breaks = c(0.25, 0.5,0.75,1.0))+
  scale_color_manual(values = c("deepskyblue","royalblue4", "red3", "chartreuse4")) +
  labs(y = "", x = "")+
  theme_minimal() +
  theme(panel.border = element_rect(color = NA, fill= NA),
        plot.background = element_rect(fill = "#ede7e3", color = "#ede7e3"),
        axis.text = element_text(size = 18, color = "#0B4F6C"),    
        axis.ticks = element_line(color = "#0B4F6C"),
        panel.background = element_rect(fill = "#ede7e3"),
        panel.grid = element_blank(),
        axis.line = element_line(size = 0.5), legend.position = "none")

# environmental distance
p2 <- ggplot(species_performance) +
  geom_jitter(aes(x = env.dist, y = auc, colour = Source), alpha = 0.2, height = 0, width = 0.2) +
  geom_point(data = overall_performance, aes(x = env.dist, y = auc, fill = Source), shape = 21, size = 3) +
  scale_fill_manual(values = c("deepskyblue","royalblue4", "red3", "chartreuse4")) +
  scale_color_manual(values = c("deepskyblue","royalblue4", "red3", "chartreuse4")) +
  scale_y_continuous(breaks = c(0.25, 0.5,0.75,1.0))+
  scale_x_continuous(n.breaks = 4)+
  labs(y = "", x = "", fill = "Source food web", colour = "Source food web") +
  theme_minimal() +
  theme(panel.border = element_rect(color = NA, fill= NA),
        plot.background = element_rect(fill = "#ede7e3", color = "#ede7e3"),
        axis.text = element_text(size = 18, color = "#0B4F6C"),    
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "#0B4F6C"),
        panel.background = element_rect(fill = "#ede7e3"),
        panel.grid = element_blank(),
        axis.line = element_line(size = 0.5), legend.position = "none")

# phylogenetic distance
p3 <- ggplot(species_performance) +
  geom_jitter(aes(x = phylo.dist, y = auc, colour = Source), alpha = 0.2, height = 0, width = 3) +
  geom_point(data = overall_performance, aes(x = phylo.dist, y = auc, fill = Source), shape = 21, size = 3) +
  scale_fill_manual(values = c("deepskyblue","royalblue4", "red3", "chartreuse4")) +
  scale_color_manual(values = c("deepskyblue","royalblue4", "red3", "chartreuse4")) +
  scale_y_continuous(breaks = c(0.25, 0.5,0.75,1.0))+
  labs(y = "", x = "", fill = "Source food web", colour = "Source food web") +
  theme_minimal()  +
  theme(panel.border = element_rect(color = NA, fill= NA),
        plot.background = element_rect(fill = "#ede7e3", color = "#ede7e3"),
        axis.text = element_text(size = 18, color = "#0B4F6C"),    
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "#0B4F6C"),
        panel.background = element_rect(fill = "#ede7e3"),
        panel.grid = element_blank(),
        axis.line = element_line(size = 0.5), legend.position = "none")

# everything together and save
p <- (p1 + p2 + p3)&
  theme(plot.background = element_rect(fill = "#ede7e3", color = "#ede7e3"))
ggsave("figures/results3.png", p, width = 7, height = 4) 

ggplot(subset(correlations_summary, role %in% c("indegree", "outdegree", "betweeness", "eigen", "TL", "OI")), aes(x = role, y = correlation_mean, colour = insample, fill = insample)) +
  geom_pointrange(aes(ymin = correlation_min, ymax = correlation_max, group = insample), position=position_dodge(width=0.75), shape= 21, size = 1) +
  scale_color_manual(values =  c("#ede7e3","#ffa62b")) +
  scale_fill_manual(values = c("#ede7e3","#ffa62b")) +
  geom_hline(yintercept = 0, color= "#ede7e3")+
  labs(y = "Correlation", x = "Species role", color = "Prediction", fill = "Prediction") +
  ylim(c(-0.5,1))+
  theme_bw() +
  theme(panel.border = element_rect(color = "#ede7e3", fill= NA),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 18, color = "#ede7e3"),
        axis.text.x = element_blank(),    
        axis.ticks = element_line(color = "#ede7e3"),
        panel.background = element_rect(fill = "#0B4F6C"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#0B4F6C", color = "#0B4F6C"), 
        legend.position = "none")

ggsave("figures/results4.png", width = 7, height = 4) 
