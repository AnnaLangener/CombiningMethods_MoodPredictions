#### Shapley plots #####

# Adapted from: https://github.com/mayer79/shapviz & https://rdrr.io/cran/SHAPforxgboost/man/shap.plot.summary.html & https://rdrr.io/cran/SHAPforxgboost/src/R/SHAP_funcs.R

###### Dataset needed that contains
# ID: ID of each observation
# variable: feature name
# value: SHAP value
# rfvalue: real variable value
# stdfvalue: standardized feature value: (rfvalue - min(rfvalue))/ (max(rfvalue)-min(rfvalue))  
# mean_value: mean absolute SHAP value for each feature (used for label)
set.seed(12361488)

library(forcats)
shapley_plot <- function(data, number) {
if(number != "all"){  
index <- Shapley_values[order(Shapley_values$mean_value, decreasing = TRUE),]
features <- unique(index$variable)[1:number]
Shapley_values <- Shapley_values[Shapley_values$variable %in% features, ]
}

return(ggplot(data = Shapley_values) +
  coord_flip() +
  geom_hline(yintercept = 0) + # the y-axis beneath
  geom_bar(aes(x = fct_reorder(variable,mean_value), y = mean_value), stat = "identity",position = 'dodge', fill = "#fca50a") +
  # sina plot:
  ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
                     method = "counts", maxwidth = 0.7, alpha = 0.7, position = "jitter") +
  # print the mean absolute value:
  geom_text(data = unique(Shapley_values[, c("variable", "mean_value")]),
            aes(x = variable, y=-Inf, label = sprintf("%.3f", mean_value)),
            size = 3, alpha = 0.7,
            hjust = -0.2,
            fontface = "bold") +
  scale_color_gradient(low="#FFCC33", high="#6600CC",
                       breaks=c(0,1), 
                       labels=c(" Low","High "),
                       guide = guide_colorbar(barwidth = 12, barheight = 0.3)) +
  theme_minimal() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(), # remove axis line
        legend.position="bottom",
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        axis.title.x= element_text(size = 10)) +
  # reverse the order of features, from high to low
  # also relabel the feature using `label.feature`
  scale_x_discrete(limits = rev(levels(Shapley_values$variable)))+
  labs(y = "Shapley value (impact on model output)", x = "", color = "Feature value  "))
}

