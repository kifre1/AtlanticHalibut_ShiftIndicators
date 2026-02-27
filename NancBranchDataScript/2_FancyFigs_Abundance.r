library(tidyverse)
library(broom)
library(dplyr)

library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)
library(RColorBrewer)
library(scales)

library(grid)  # For unit() function
#theme----
theme_replace(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour="black"),
              
              strip.background=element_rect(colour="black",fill="white"))

theme_set(theme_bw())

theme_replace(legend.key =element_rect(colour="black",fill="white"),
              
              plot.margin = unit(c(1.5,3,1.5,1), "cm"),
              
              #plot.margin=margin(3,4,4,0),
              
              #plot.margin=margin(3,4,4,0),
              
              plot.title=element_text(size=16,vjust=1.5,family="serif"),
              
              legend.background=element_rect(size=.9,colour="white",fill="white"),
              
              plot.tag = element_text(face = "plain", family = "serif", size = 12),
              strip.text=element_text(size=14,family="serif",angle=0),
              
              panel.border = element_rect(colour = "black",fill=NA),
              
              panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
              
              strip.background=element_rect(colour="black",fill="white"),
              
              axis.text.x = element_text(angle = 90, vjust = 0.2, hjust=1,size=12,family="serif"),
              
              axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=12,family="serif"),
              
              axis.title.x=element_text(size=12,hjust=0.5,vjust=-2,family="serif"))

pd <- position_dodge(.5)
# Solution 1: Transform your data beforehand and use natural scale
# This approach transforms the data before plotting
# Plot abuncance indexed regions ----

FigAbd.Region.Spring <- read.csv(here::here("2025-04-23/Output/IndexAbundance/abundance_ind_Region.Spring.csv"),row.names=NULL)
names(FigAbd.Region.Spring)


#To estimate the proportion ot toal abundance by Season, I have to do it here as teh script in 3.3Regional_Proportions can only easily be run if you have VAST
Reg_Prop<-read.csv(here::here("2025-04-23/Output/Proportions/proportions_and_density_Regional.csv"))
#isolate spring 
Reg_Prop_Spring <- subset(Reg_Prop, Reg_Prop$Season == "Spring")
names(Reg_Prop_Spring)
#plot check and adding vars to figure out proportion before and during warming.----
Reg_Prop_Spring$Period<-NULL
Reg_Prop_Spring$Period[Reg_Prop_Spring$Year<2006]<-"1990-2005"
Reg_Prop_Spring$Period[Reg_Prop_Spring$Year>2005]<-"2006-2023"
Reg_Prop_Spring$Region<-factor(Reg_Prop_Spring$Index_Region)
names(Reg_Prop_Spring)
library(dplyr)

popr <- Reg_Prop_Spring %>%
  group_by(Period) %>%
  mutate(Total_EstimateForPeriod = sum(Index_Estimate)) %>%
  ungroup() %>%
  group_by(Period, Region) %>%
  summarise(Total_EstimateForRegion = sum(Index_Estimate), 
            Total_EstimateForPeriod = first(Total_EstimateForPeriod)) %>%
  mutate(Prop = Total_EstimateForRegion / Total_EstimateForPeriod * 100) %>%
  ungroup()
names(Reg_Prop_Spring)
range(Reg_Prop_Spring$Proportion)
Reg_Prop_Spring %>%
  group_by(Region,Period) %>%
  summarise(
    min_prop = min(Proportion, na.rm = TRUE),
    max_prop = max(Proportion, na.rm = TRUE),
    .groups = "drop"
  )
####ENDprop----
Regiona_Proportion_Plot_Spring<-ggplot(Reg_Prop_Spring, aes(x = Year, y = Proportion, fill = Index_Region)) +
  geom_area() +
  scale_fill_manual(values = c("orange", "darkblue")) +
  labs(
    title = "Mean Proportion of Total Est. Abun. Spring",
    x = "Year",
    y = "Proportion of Total (%)",
    fill = NULL
  ) +
  geom_vline(xintercept = 2006, linetype = "dashed", color = "black", size = 1)
##END adding vars to figure out proportion before and during warming.----
popr
##Figure ABD Panel A----
regpal<- c("orange", "darkblue")
colbline<-brewer.pal(3, "Dark2")
custom_labels <- c(
  "Canada: Before (93.9%), During (94%)",
  "USA:    Before ( 6.1%), During ( 6%)"
)
CI <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x)) * qt(0.975, df = length(x) - 1)
}
#option 1----
FigAbd.Region.Spring.transformed <- FigAbd.Region.Spring %>%
  mutate(
    Index_Millions = Index_Estimate / 1000000,
    Index_SD_Millions = Index_SD / 1000000,
    ymin = pmax(Index_Millions - Index_SD_Millions, 0.001),  # Ensure positive values
    ymax = Index_Millions + Index_SD_Millions
  )
ARegionalPlot<- 
ggplot(data = FigAbd.Region.Spring.transformed, aes(x = Year, y = Index_Millions, group = Region)) +
  geom_vline(xintercept = 2005, lty = 2, lwd = 1.2) +
  scale_y_log10(
    breaks = c(0.1,0.2, 0.5, 1, 2, 3,4,5, 10, 20, 50),  # Custom breaks in natural units
    labels = c("0.1","0.2","0.5", "1", "2","3","4", "5", "10", "20", "50")  # Natural unit labels
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) +
  geom_ribbon(aes(x = Year, 
                  ymin = ymin, 
                  ymax = ymax, 
                  fill = Region), alpha = 0.3) +
  geom_line(aes(color = Region), linewidth = 1) +
  scale_fill_manual(values = regpal) +
  scale_color_manual(values = regpal) +
  guides(color = guide_legend(title = ""),
         fill = "none") + 
  labs(y = "Modelled Abundance (Millions)", x = "") +
  guides(color = guide_legend(title = "")) +
  annotate("text", x = 1996, y = 2.1, label = "Canada", color = "black", size = 5, family = "serif") +
  annotate("text", x = 1996, y = 0.13, label = "USA", color = "black", size = 5, family = "serif") +
  theme(text = element_text(family = "serif"),  
        legend.position = "none",
        plot.margin = margin(10, 5, 10, 15))#+
  # Faceting by region
 # facet_wrap(Region ~ ., 
           #  scales = "free",
           #  labeller = label_wrap_gen(width = 15)) 

#END Option 1----
# Solution 2: Use asymmetric error bars on log scale----
# This approach calculates proper confidence intervals for log-transformed data
 ggplot(data = FigAbd.Region.Spring, aes(x = Year, y = Index_Estimate/1000000, group = Region)) +
  geom_vline(xintercept = 2005, lty = 2, lwd = 1.2) +
  scale_y_log10(
    breaks = c(0.1,0.2, 0.5, 1, 2, 3,4,5, 10, 20, 50),  # Custom breaks in natural units
    labels = c("0.1","0.2","0.5", "1", "2","3","4", "5", "10", "20", "50")  #
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) +
  geom_ribbon(aes(x = Year, 
                  ymin = pmax((Index_Estimate - Index_SD)/1000000, 0.001), 
                  ymax = (Index_Estimate + Index_SD)/1000000, 
                  fill = Region), alpha = 0.3) +
  geom_line(aes(color = Region), linewidth = 1) +
  scale_fill_manual(values = regpal) +
  scale_color_manual(values = regpal) +
  guides(color = guide_legend(title = ""),
         fill = "none") + 
  labs(y = "Modelled Abundance (Millions)", x = "") +
  guides(color = guide_legend(title = "")) +
  annotate("text", x = 1996, y = 2.1, label = "Canada", color = "black", size = 5, family = "serif") +
  annotate("text", x = 1996, y = .13, label = "USA", color = "black", size = 5, family = "serif") +
  theme(text = element_text(family = "serif"),  
        legend.position = "none",
        plot.margin = margin(10, 5, 10, 15))
#END Solution 2: Use asymmetric error bars on log scale----
# Solution 3: Use multiplicative error bars (recommended for log scale)----
# This approach uses multiplicative confidence intervals which are more appropriate for log scales
ARegionalPlot<-ggplot(data = FigAbd.Region.Spring, aes(x = Year, y = Index_Estimate/1000000, group = Region)) +
  geom_vline(xintercept = 2005, lty = 2, lwd = 1.2) +
  scale_y_log10(
    breaks = c(0.1,0.2, 0.5, 1, 2, 3,4),  # Custom breaks in natural units
    labels = c("0.1","0.2","0.5", "1", "2","3","4")  #
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) +
  geom_ribbon(aes(x = Year, 
                  ymin = pmax((Index_Estimate/1000000) * exp(-Index_SD/Index_Estimate), 0.001), 
                  ymax = (Index_Estimate/1000000) * exp(Index_SD/Index_Estimate), 
                  fill = Region), alpha = 0.3) +
  geom_line(aes(color = Region), linewidth = 1) +
  scale_fill_manual(values = regpal) +
  scale_color_manual(values = regpal) +
  guides(color = guide_legend(title = ""),
         fill = "none") + 
  labs(y = "Modelled Abundance (Millions)", x = "") +
  guides(color = guide_legend(title = "")) +
  annotate("text", x = 1996, y = 2.1, label = "Canada", color = "black", size = 5, family = "serif") +
  annotate("text", x = 1996, y = .13, label = "USA", color = "black", size = 5, family = "serif") +
  theme(text = element_text(family = "serif"),  
        legend.position = "none",
        plot.margin = margin(10, 5, 10, 15))
##Plot Slopes----
Reg_SlopSpring <- read.csv(here::here("2025-04-23/Output/IndexAbundance/RegionAbdSlope.Spring.csv"),row.names=NULL)
names(Reg_SlopSpring)
#scaling slopes per region period
##Scale slope for Abundace----
RegionAbd_coefficients_df <- FigAbd.Region.Spring %>%
  group_by(Region,Period) %>%
  do({
    model <- lm(scale((log10(Index_Estimate))) ~ scale(Year), data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

RegionAbd_coefficients_df<- RegionAbd_coefficients_df%>%
  filter(term == "scale(Year)")
##END scaled slope for Abundace----
#Core Area Abundance----
#Part 2: Core Areas abundance trends

abundance_ind_CA <- read.csv(here::here("2025-04-23/Output/IndexAbundance/abundance_ind_CA.csv"),row.names=NULL)
names(abundance_ind_CA)
abundance_ind_CA.spr<- subset(abundance_ind_CA, abundance_ind_CA$Season == "Spring")

#2.1 CA plot abundance trends, spring
#assign colors----
abundance_ind_CA.spr$Index_Region <- factor(abundance_ind_CA.spr$Index_Region, levels = c("EGOM", "BOF", "CapeBreton", "HaliChan", "CapeCod", "Browns", "Gully", "GrandBanks", "Nantucket", "Georges", "Sable", "GBTail"))
region_colours <- c(
  "EGOM" ="#004995",
  "BOF"  = "#8C510A",
  "CapeBreton" ="#4D4D4D",
  "HaliChan" ="#00441B",
  "CapeCod" ="#2171B5",
  "Browns" ="#D8781D",
  "Gully"="#7F7F7F",
  "GrandBanks" ="#238B45",
  "Nantucket" = "#56B4E9",
  "Georges" ="#EDA752",
  "Sable"  ="#C0C0C0",
  "GBTail" = 	"#81C784"
)
#factor the order 
abundance_ind_CA.spr$Index_Region <- factor(abundance_ind_CA.spr$Index_Region, levels = names(region_colours))
abundance_ind_CA.spr$ordCoreArea<-factor(abundance_ind_CA.spr$Index_Region, levels=c("Nantucket","CapeCod","EGOM","Georges","BOF","Browns","Sable","Gully","CapeBreton","HaliChan","GrandBanks","GBTail"))
region_colours2 <- c(
  "Nantucket" = "#56B4E9",
  "CapeCod" = "#2171B5",
  "EGOM" = "#004995",
  "Georges" = "#EDA752",
  "BOF" = "#8C510A",
  "Browns" = "#D8781D",
  "Sable" = "black",
  "Gully" = "#7F7F7F",
  "CapeBreton" = "#4D4D4D",
  "HaliChan" = "#00441B",
  "GrandBanks" = "#238B45",
  "GBTail" = "#81C784"
)


abundance_ind_CA.spr$ordCoreArea<-factor(abundance_ind_CA.spr$ordCoreArea, levels=names(region_colours2))
#END #assign colors----
#log values and get standard deviation of log values
abundance_ind_CA.spr$LogAbd <- log10(abundance_ind_CA.spr$Index_Estimate + 1)  # Add 1 to avoid log(0)
CI <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x)) * qt(0.975, df = length(x) - 1)
}

length(region_colours2)
length(unique(abundance_ind_CA.spr$ordCoreArea))

names(abundance_ind_CA.spr)

# CA Plot with Solution 3: Multiplicative error bars for log scale----
CAPlot <- ggplot(data = abundance_ind_CA.spr,  # Filter for years >= 1990], 
                 aes(x = Year, y = Index_Estimate/1000000, color = ordCoreArea)) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) + # Set x-axis breaks
  scale_y_log10(
    breaks = c(0.001,0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10),  # Custom breaks in natural units
    labels = c("0.001","0.002", "0.005", "0.01", "0.02", "0.05", "0.1", "0.2", "0.5", "1", "2", "5", "10")  # Natural unit labels
  ) +
  geom_ribbon(aes(x = Year, 
                  ymin = pmax((Index_Estimate/1000000) * exp(-Index_SD/Index_Estimate), 0.001), 
                  ymax = (Index_Estimate/1000000) * exp(Index_SD/Index_Estimate), 
                  fill = ordCoreArea), alpha = 0.3, colour = NA) +  # Multiplicative error bars
  geom_line(aes(color = ordCoreArea), linewidth = .9) +                         # Line for the group
 scale_fill_manual(values = region_colours) +                    # Custom fill colors
 scale_color_manual(values = region_colours) +
  geom_vline(xintercept = 2005, linetype = "dashed", color = "black", size = 1) +
  # Combine legends
  guides(color = guide_legend(title = ""),
         fill = "none") + 
  labs(y = "Modelled Abundance (Millions)", x = "") +
  theme(text = element_text(family = "serif"),  
        legend.box.background = element_blank(), # Transparent legend box
        legend.position.inside = c(.15, .7),
        legend.text = element_text(size = 10, family = "serif"),
        plot.margin = margin(10, 5, 10, 15))
#END CA Plot with Solution 3: Multiplicative error bars for log scale----
CAPlot
abundance_ind_CA.spr$Subregion<-NULL
abundance_ind_CA.spr$Subregion[abundance_ind_CA.spr$ordCoreArea == "Nantucket" | abundance_ind_CA.spr$ordCoreArea=="Georges"|abundance_ind_CA.spr$ordCoreArea=="CapeCod" |abundance_ind_CA.spr$ordCoreArea=="EGOM"] <- "5ZY"
abundance_ind_CA.spr$Subregion[abundance_ind_CA.spr$ordCoreArea == "BOF" | abundance_ind_CA.spr$ordCoreArea=="Browns"] <- "4X"
abundance_ind_CA.spr$Subregion[abundance_ind_CA.spr$ordCoreArea == "Gully" | abundance_ind_CA.spr$ordCoreArea=="CapeBreton"| abundance_ind_CA.spr$ordCoreArea=="Sable"] <- "4VW"
abundance_ind_CA.spr$Subregion[abundance_ind_CA.spr$ordCoreArea == "HaliChan" | abundance_ind_CA.spr$ordCoreArea=="GrandBanks"| abundance_ind_CA.spr$ordCoreArea=="GBTail"] <- "3NOPs"

# Example of 12 bold colors (you can swap to any you like)
region_colours12 <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF",
  "#999999", "#66C2A5", "#FC8D62", "#8DA0CB"
)

CAPlotshort <- ggplot(
  #data = abundance_ind_CA.spr[abundance_ind_CA.spr$Year >= 2017, ],
  data = abundance_ind_CA.spr,
  aes(x = Year, y = Index_Estimate/1000000, color = ordCoreArea, fill = ordCoreArea)
) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) +
  scale_y_log10(
    breaks = c(0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10),
    labels = c("0.001","0.002","0.005","0.01","0.02","0.05","0.1","0.2","0.5","1","2","5","10")
  ) +
  geom_ribbon(
    aes(
      ymin = pmax((Index_Estimate/1000000) * exp(-Index_SD/Index_Estimate), 0.001),
      ymax = (Index_Estimate/1000000) * exp(Index_SD/Index_Estimate)
    ),
    alpha = 0.2, color = NA   # <-- faint ribbons
  ) +
  geom_line(linewidth = .9) +
  scale_color_manual(values = region_colours12) +
  scale_fill_manual(values = region_colours12) +
  guides(color = guide_legend(title = ""), fill = "none") +
  labs(y = "Modelled Abundance (Millions)", x = "") +
  facet_wrap(~Subregion, nrow = 1) +
  theme(
    text = element_text(family = "serif"),
    legend.box.background = element_blank(),
    legend.position.inside = c(.15, .7),
    legend.text = element_text(size = 10, family = "serif"),
    plot.margin = margin(10, 5, 10, 15)
  )

CAPlotshort



#Figure4: plot abundance trends with a map of the core areas 
#go to Sup2DataPlots.R to make CAMAP 
#CAMAP in C:\Users\shackelln\Documents\My_Program_Files\R\Hali_Shift_withNF\Rhttp://127.0.0.1:21785/graphics/8becb3ce-f8a5-4389-9221-5f8cb03ac05a.png\Sup2DataPlots.R
library(patchwork)
library(grid)  # for textGrob

ARegionalPlot_clean <- ARegionalPlot + 
  theme(
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(t=2, r=2, b=2, l=5)  # Tight margins
  )

CAPlot_clean <- CAPlot +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 14, family = "serif"),
    plot.margin = margin(t=2, r=10, b=2, l=2)  # Tight margins
  )

# Create shared y-axis label
shared_y <- textGrob("Modelled Abundance/n(log10)", 
                     rot = 90, 
                     gp = gpar(fontsize = 10, fontfamily = "serif"))


# Bottom row with shared y-axis title
bottom_row <- plot_grid(shared_y,plot_grid(CAPlot_clean,ARegionalPlot_clean, ncol = 2, rel_widths = c(1, 1)),
                        ncol = 2, 
                        rel_widths = c(0.05, 1)
)

# Extract legend using cowplot
legend_CAMAP <- get_legend(CAMAP + theme(legend.position = "right"))
CAMAP_2col <- CAMAP +
  guides(fill = guide_legend(ncol = 2),   # for fill legends
         color = guide_legend(ncol = 2)) +  # for color legends
  theme(legend.position = "right",legend.title = element_blank())
# Extract the legend from CAMAP
legend_CAMAP <- get_legend(CAMAP_2col)
CAMAP_clean <- CAMAP + theme(legend.position = "none")
#You use CAMAP_2col to get the 2-column legend, but use CAMAP_clean for the plot without the legend.
#Comben CAMAP no legend with 2 col legend()
camap_with_legend <- plot_grid(
  CAMAP_clean, legend_CAMAP,
  ncol = 2,
  rel_widths = c(.6, 0.4),
  align = "h"
)


#FEB- DOES NOT WORK WELL , therefore,just label afer
# 1. Manually label bottom plots
camap_with_legend_tagged<-camap_with_legend#+labs(tag = "(a)")+theme(plot.tag = element_text(face = "plain", family = "serif", size = 14))
CAPlot_tagged <- CAPlot_clean#+ labs(tag = "(b)")+legend.text = element_text(size = 14, family = "serif")+theme(plot.tag = element_text(face = "plain", family = "serif", size = 14))
ARegionalPlot_tagged <- ARegionalPlot_clean #+ labs(tag = "(c)")+theme(plot.tag = element_text(face = "plain", family = "serif", size = 14))

# 2. Create vertical shared axis
shared_y <- textGrob("Abundance (Millions)", 
                     rot = 90, 
                     gp = gpar(fontsize = 10, fontfamily = "serif"))

# 3. Arrange bottom row with shared y-axis and tags
bottom_row_with_y <- arrangeGrob(
  shared_y,
  CAPlot_tagged,
  ARegionalPlot_tagged,
  ncol = 3,
  widths = unit.c(unit(1.5, "lines"), unit(1, "null"), unit(1, "null"))
)

# 4. Wrap in patchwork
bottom_row_patchwork <- wrap_elements(full = bottom_row_with_y)


# 5. Combine with top plot
final_plot <- camap_with_legend_tagged / bottom_row_patchwork +
  plot_layout(heights = c(1.5, 1.2)) +
 #draw_label("Figure 2", x = 0.01, y = 1, hjust = 0, vjust = 1, size = 14,fontfamily="serif") +
    theme(
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10),
    # plot.tag.position = c(0.01, 0.98),
    #plot.tag = element_text(size = 14, face = "bold")
  )

# Display
final_plot2<-ggdraw(final_plot) +
  draw_plot_label(label = "(a)", x = 0.01, y = 0.89, size = 14,family="serif") +   # Top left
  draw_plot_label(label = "(b)", x = 0.01, y = 0.5, size = 14,,family="serif") +   # Top right  
  draw_plot_label(label = "(c)", x = 0.448, y = 0.5, size = 14,,family="serif") +    # Bottom (centered plot)
  draw_plot_label(label = "Figure 2", x = 0.001, y = 1, size = 11,family="serif")  # Top left
#END manual labels----
final_plot2
# Save the final plot with legend
ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/FigureAbundanceFAandMapTrends_Feb11_2026.tif"), plot = final_plot2, dpi = 600, width = 7, height = 6, units = "in", compression="lzw")

ggsave(here::here("FebFigs/FigureAbundanceFAandMapTrends_Feb11_2026.tif"), plot = final_plot2, dpi = 600, width = 7, height = 6, units = "in", compression="lzw")
