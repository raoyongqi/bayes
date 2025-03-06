
project_dir <- "path-to-output-directory"
data_dir <- "path-to-input-directory"
setwd(project_dir)

boot_path <- file.path(data_dir, "data/Diversity Data/bootstrap_results")
ML_path <- file.path(data_dir, "data/Diversity Data/alpha")
plot_path <- file.path(project_dir, "plots/final_plots/Fig_S1"_


# -------------------------------------------------------------------------------------
# set global ggplot theme

library(ggplot2)

theme_publication <- function(base_size = 12, base_family = "Helvetica", ...) {
      require(grid)
      require(ggthemes)
      (theme_foundation(base_size = base_size, base_family = base_family)
       + theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(color = NA),
               plot.background = element_rect(color = NA),
               panel.border = element_rect(color = "black", size = 1),
               axis.title = element_text(face = "plain", size = 9),
               axis.title.y = element_text(angle=90, vjust = 2, margin = margin(r=0)),
               axis.title.x = element_text(vjust = -0.2, margin = margin(t=7)),
               axis.text = element_text(size = 7), 
               #axis.line.y = element_line(color="black"),
               #axis.line.x = element_line(color="black"),
               axis.ticks = element_line(),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank() ,
                # explicitly set the horizontal lines (or they will disappear too)
                #panel.grid.major.x = element_line(size=.5, color="#f0f0f0"),
               #legend.key = element_rect(color = NA),
               legend.position = "top",
               legend.direction = "horizontal",
               legend.key.size = unit(0.4, "cm"),
               legend.spacing = unit(0, "cm"),
               legend.title = element_text(size = 8),
               legend.text = element_text(size = 7),
               plot.margin = unit(c(10,5,5,5),"mm"),
               strip.text = element_text(size = 8),
               strip.background = element_blank()
          ))
}


# -------------------------------------------------------------------------------------
# read in files and combine

# bootstrap
get_boot <- function(dir) {
    filenames <- list.files(file.path(boot_path, dir), pattern = "*.csv")
    dat_list <- lapply(file.path(boot_path, dir, filenames), read.csv)
    names(dat_list) <- paste0("boot", seq_along(filenames))
    dat <- do.call(rbind.data.frame, dat_list)
    dat$boot_num <- gsub("(\\w+)\\.\\d+", "\\1", rownames(dat)) 
    rownames(dat) <- NULL
    dat <- dat[, c("fips", "PD", "PD.s", "MPD", "MPD.s")]
    SE <- function(x) sd(x) / sqrt(sum(!is.na(x))) 
    require(plyr)
    std_err <- ddply(dat, .(fips), numcolwise(SE)) # calculate SEs
    std_err <- std_err[order(std_err$fips), ]
    return(std_err)
}
boot.alpha.ALL <- get_boot("alpha.all_species")
boot.alpha.INTRO <- get_boot("alpha.INTRO.county_diversity_INTROnull")
boot.alpha.NATIVE <- get_boot("alpha.NATIVE.county_diversity_NATnull")

# ML
get_ML <- function(dir) {
    ML_all <- read.csv(file.path(ML_path, dir))
    ML_all <- ML_all[, c("fips", "PD", "PD.s", "MPD", "MPD.s")]
    ML_all <- ML_all[order(ML_all$fips), ]
}
ML.alpha.ALL <- get_ML("all_species/ALL_county_diversity.ML.csv")
ML.alpha.INTRO <- get_ML("nonnative_species/INTRO.INTRO_county.diversity.ML.csv")
ML.alpha.NATIVE <- get_ML("native_species/NAT.NAT_county.diversity.ML.csv")


# -------------------------------------------------------------------------------------
# correlation

#pdf(file.path(plot_path, "bootstrap_error.pdf"), height = 3.5, width = 5.5)
#op <- par(mfrow = c(2, 2), mai = c(0.6, 0.6, 0.1, 0.1))
#plot(boot.alpha.ALL$PD, ML.alpha.ALL$PD)
#plot(boot.alpha.ALL$PD.s, ML.alpha.ALL$PD.s)
#plot(boot.alpha.ALL$MPD, ML.alpha.ALL$MPD)
#plot(boot.alpha.ALL$MPD.s, ML.alpha.ALL$MPD.s)
#par(op)
#dev.off()


# -------------------------------------------------------------------------------------
# calculate % error

#perc_err <- Reduce("/", list(boot.alpha.ALL[, -1], ML.alpha.ALL[, -1])) * 100
#perc_err$fips <- boot.alpha.ALL$fips


# -------------------------------------------------------------------------------------
# reshape boot data to long

boot.alpha.list <- list(
    ALL = boot.alpha.ALL, 
    INTRO = boot.alpha.INTRO, 
    NATIVE = boot.alpha.NATIVE
)
boot.alpha.df <- do.call(rbind.data.frame, boot.alpha.list)
boot.alpha.df$type <- gsub("(\\w+)\\.\\d+", "\\1", rownames(boot.alpha.df)) 
rownames(boot.alpha.df) <- NULL
    
library(reshape2)

boot.alpha.df_long <- melt(boot.alpha.df, id.var = c("fips", "type"))
# perc_err_long <- melt(perc_err, id.var = "fips")

boot.min.max <- ddply(boot.alpha.df_long, .(type, variable), summarize, Min = min(value), Max = max(value)) # most are variances, so min is zero


# -------------------------------------------------------------------------------------
# reshape ML data to long

ML.alpha.list <- list(
    ALL = ML.alpha.ALL, 
    INTRO = ML.alpha.INTRO,  
    NATIVE = ML.alpha.NATIVE
)
ML.alpha.df <- do.call(rbind.data.frame, ML.alpha.list)
ML.alpha.df$type <- gsub("(\\w+)\\.\\d+", "\\1", rownames(ML.alpha.df)) 
rownames(ML.alpha.df) <- NULL
    
library(reshape2)

ML.alpha.df_long <- melt(ML.alpha.df, id.var = c("fips", "type"))
# perc_err_long <- melt(perc_err, id.var = "fips")

ML.min.max <- ddply(ML.alpha.df_long, .(type, variable), summarize, Min = round(min(value)), Max = round(max(value)))
ML.min.max$MLrange <- with(ML.min.max, paste0("[", Min, ", ", Max, "]"))


# -------------------------------------------------------------------------------------
# merge MLrange with boot

boot.alpha.df_long <- merge(boot.alpha.df_long, ML.min.max[, c("type", "variable", "MLrange")], by = c("type", "variable"), all.x = TRUE)

boot.alpha.df_long$type <- factor(boot.alpha.df_long$type, 
    levels = c("ALL", "INTRO", "NATIVE"),
    labels = c("Total", "Introduced", "Native"))


# -------------------------------------------------------------------------------------
# plot

library(ggplot2)
library(ggthemes)

std_err_plot <- ggplot(boot.alpha.df_long, aes(x = value, fill = type)) +
    geom_density(color = NA,  alpha = 0.5) + # fill = "grey30",
    scale_fill_tableau(palette = "Tableau 10", name = "") +
    facet_wrap(~ variable + type, scales = "free", labeller = function(x) label_value(x, multi_line = FALSE)) +
    labs(x = "Standard error of the mean across counties (n=100 bootstrap samples)", y = "Density") +
    theme_publication() +
    theme(strip.text = element_text(size = 7, vjust = -0.5),
          #panel.border = element_rect(size = 0.5),
          panel.spacing.y = unit(-0.3, "lines"),
          legend.box.margin = margin(0, 0, -10, 0),
          legend.margin = margin(t = 0, b = 0, unit = "cm"))                 
ggsave(std_err_plot, file = file.path(plot_path, "Fig_S1_new.pdf"), height = 5.5, width = 7)


#perc_err_plot <- ggplot(perc_err_long, aes(x = value)) +
#    geom_density(color = NA, fill = "grey30", alpha = 0.3) +
#    geom_rug(color = "grey30", alpha = 0.3) +
#    facet_wrap(~ variable, scales = "free") +
#    labs(x = "Percentage error across counties (n=100 bootstrap samples)", y = "Density") +
#    theme_publication() 
#ggsave(perc_err_plot, file = file.path(plot_path, "perc_err_plot.pdf"), height = 4, width = 5)    

  
    
