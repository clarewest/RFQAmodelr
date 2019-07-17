#' Calculate classifier performance
#'
#' Calculates the pr, auc and performance for a given score on a set of models
#'
#'@param score vector of scores, one for each model (e.g. RFQAmodel)
#'@param labels vector of labels, one for each model (1 for correct, 0 for
#'incorrect)
#'@param plot_index
#'@param plot.colours
#'@param add_to_plot
#'@param do_not_plot
#'@export
calculate_performance <- function(score,labels,plot_index,plot.colours,add_to_plot=FALSE, do_not_plot=FALSE) {
  pr <- ROCR::prediction(score, as.factor(labels))
  perf <- ROCR::performance(pr, "tpr", "fpr")
  auc = ROCR::performance(pr, measure = "auc")
  if (!do_not_plot){
    ROCR::plot(perf,col=plot.colours[plot_index],lwd=2,lty=plot_index,add=add_to_plot)
  }
  return (c(pr,auc,perf))
}

#' Plot a ROC curve
#'
#' Plots a ROC curve for one or more methods
#' @param resultsdf data frame
#' @param featurelist vector. List of methods/features to plot curves for
#' @param topmodel boolean. When true, only the highest-ranking model per
#' target is considered
#' @param addhighconf boolean. When true, the TPR and FPR corresponding to the
#' high confidence cutoff (0.5) is indicated
#' @param truth string. Optional, the score to use to define a correct model
#' (default TMScore). This is only required if the Label column is not already
#' provided.
#' @param cutoff float. Optional, the cutoff to use to define a correct models.
#' See \code{truth}.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate select group_by slice ungroup summarise pull
#' @export
plot_roc <- function(resultsdf, featurelist, topmodel = FALSE, addhighconf = FALSE, truth = TMScore, cutoff = 0.5, rev=TRUE, relabel=TRUE) {
  ## Make the label column if it doesn't already exist
  if ((!"Label" %in% names(resultsdf)) || relabel) {
    resultsdf <- resultsdf %>% mutate(Label = RFQAmodelr::get_labels(!!as.name(truth), cutoff, rev))
  }
  roc.results <- list(length(featurelist))
  if (topmodel){
    roc_scores <- resultsdf %>%
      mutate(SAINT2=-SAINT2) %>%  # lower scores are better for SAINT2
      select(Target, Decoy, TMScore, Label, featurelist, {{truth}}) %>%
      group_by(Target) %>%
      tidyr::gather(key="Score", value="value", -c("Target","Decoy", "TMScore", "Label", truth))
    }
  else{
    roc_scores <- resultsdf %>%
      mutate(SAINT2=-SAINT2, EigenTHREADER=EigenTHREADER/ET_Max) %>%
      select(Target, Decoy, TMScore, Label, featurelist, {{truth}}) %>%
      group_by(Target) %>%
      tidyr::gather(key="Score", value="value", -c("Target","Decoy", "TMScore", "Label", truth))
  }
  roc_scores.top <- roc_scores %>%
    group_by(Target, Score) %>%
    slice(which.max(value))
  pdf(NULL)
  dev.control(displaylist="enable")
  par(pty="s", xpd=NA, bg = "transparent", mgp = c(2,1,0))
  plot.colours=gg_color_hue(length(featurelist))
  if (topmodel){
    plottitle = "Highest-ranking model per target"
    plotscores <- roc_scores.top
    legx = 0.5
  } else {
    plottitle = "All models"
    plotscores <- roc_scores
    legx=0.44
  }
  legend.labels <- vector()
  for (i in 1:length(featurelist)) {
    add = !i==1
    plotroc = topmodel*(featurelist[i]=="EigenTHREADER")  ## Remove EigenTHREADER because it's normalised so is not informative on this plot
    roc.results[[i]] <- RFQAmodelr::calculate_performance((subset(plotscores, Score==featurelist[i])$value),
                                                          subset(plotscores, Score==featurelist[i])$Label,
                                                          i,
                                                          plot.colours,
                                                          add,
                                                          plotroc)
    legend.labels[i] <- paste(featurelist[i]," (",round(roc.results[[i]][[2]]@y.values[[1]],2),")", sep="")
  }
  if (topmodel){
    legend.index = which(featurelist!="EigenTHREADER")} else {
      legend.index = 1:length(featurelist)
    }
  legend(x = c(legx, 1), y = c(length(featurelist)*0.05, 0), legend.labels[legend.index], lwd = 2, lty = legend.index,col=plot.colours[legend.index], cex=0.8,bty = "n", y.intersp=1.5)
  title(plottitle)
  if (addhighconf){
    ## Add lines to indicate values at RFQAmodel=0.5 cutoff
    perf <- roc.results[[1]][[3]]
    cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]],tpr=perf@y.values[[1]])
    high <- cutoffs %>% filter(abs(cut-0.5)==min(abs(cut-0.5)))
    abline(h=high$tpr, v=high$fpr, lty="dotted",col="gray85")
  }
  p <- recordPlot()
  invisible(dev.off())
  nmax <- resultsdf %>% group_by(Target) %>% slice(which.max(TMScore)) %>% ungroup %>% summarise(sum(Label)) %>% pull()
  return(list(p = p, results = roc.results, features = featurelist, nmax = nmax ))
}

#' Plot ROC-like Curve
#'
#' Plot a ROC-like curve, showing True Positives vs False Positive Rate. This
#' is necessary when the total number of true positives differs between
#' methods, i.e. when looking at the highest-ranking model per target.
#'
#' This function requires the object output from \code{plot_roc()}.
#'
#' @param rocresults The object output from \code{plot_roc()}, which includes
#' the ROC plot, results, list of features, and the maximum number of targets
#' with at least one correct model.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate select group_by slice ungroup summarise bind_rows mutate_if
#' @importFrom rlang .data
#' @import ggplot2
#' @export
plot_roclike <- function(rocresults){
  rocresultslist <- rocresults$results
  methods <- rocresults$features
  val_max <- rocresults$nmax
  ### ROC-like plot showing TP vs FPR
  colours=gg_color_hue(length(methods))
  tprs <- list()
  for (METHOD in methods){
    pred <- rocresultslist[[which(methods==METHOD)]][[1]]
    perf <- rocresultslist[[which(methods==METHOD)]][[3]]
    tprs[[which(methods==METHOD)]] <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]],tpr=perf@y.values[[1]], tp = pred@tp[[1]], n.pos.pred=pred@n.pos.pred[[1]],n.neg.pred=pred@n.neg.pred[[1]]) %>% mutate(Method=METHOD)
  }

  tprs %>% bind_rows() %>% mutate(recall=.data$tp/val_max) %>% mutate_if(is.numeric, round, 2) -> all
  all$Method <- factor(all$Method, levels=methods)

  ## Legend position
  if (length(methods)>7){
    legs = c(.2, .7)
  } else {
    legs = c(.8, .84)
  }

  ## Make the plot
  roclikep <- ggplot2::ggplot(all, aes(x=fpr,y=tp,linetype=Method, colour=Method)) +
    theme_bw() +
    geom_line(size=0.7) +
    theme_bw() +
    scale_x_continuous(trans="log10") +
    annotation_logticks(sides="b") +
    scale_linetype_manual(values=c(1:length(methods))) +
    scale_colour_manual(values=colours[1:length(methods)]) +
    geom_hline(yintercept=val_max, lty="dotted", colour="darkgrey") +
    theme(panel.border = element_rect(colour="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position=legs,
          legend.key.size = unit(0.8, "lines"),
          legend.background = element_blank(),
          legend.title=element_blank(),
          plot.title = element_text(size=14, hjust = 0.5, face="bold", margin = ggplot2::margin(0,0,20,0)),
          axis.text = element_text(size=12, colour="black"),
          axis.text.y = element_text(margin = ggplot2::margin(0,9,0,0)),
          axis.text.x = element_text(margin = ggplot2::margin(9,0,0,0)),
          axis.title = element_text(size=12),
          axis.ticks = element_line(colour="black"),
          axis.ticks.length = unit(0.5,"lines"), aspect.ratio = 1) +
    labs(x="False positive rate", y="True positives", title="Highest-ranking model per target") +
    scale_y_continuous(breaks = seq(0,200,ifelse(val_max > 100, 20, 5)), limits=c(0,val_max+ifelse(val_max > 100, 10, 5)))

  return(list(p=roclikep, results=all))
}

#' Get the Default ggplot2 Color Palette
#'
#' \code{gg_color_hue} returns the default ggplot color palette for a given
#' number of colors
#'
#' @author John Colby \url{http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette}
#
#' @param n Number of Coolors
#' @export
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}




