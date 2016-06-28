#! /usr/bin/env Rscript
library(argparse)
library(dplyr)
library(ggplot2)

parser <- ArgumentParser(description='Compare gene expressions')
parser$add_argument('--file_1',
                    help='Path of expressions file 1',
                    default='results/DGNWB_comparison.txt')
parser$add_argument('--file_2',
                    help='Path of expressions file 2',
                    default='results/PB8K_beta_comparison.txt')

parser$add_argument('--result_prefix',
                    help='comparison prefix',
                    default='results/comparison')

arguments <- parser$parse_args(commandArgs(TRUE))

load_results <- function(path) {
    df <- read.table(path, stringsAsFactors=FALSE)
    cols <- df[1,]
    rows <- df[,1]
    d <- df[2:length(rows),2:length(cols)]
    colnames(d) <- cols[2:length(cols)]
    rownames(d) <- rows[2:length(rows)]
    d[, 1:6] <- sapply(d[, 1:6], as.numeric)
    d
}

plot_pearson <- function(d1, file1, d2, file2, plot_path) {
    data <- data.frame(f1 = d1$R, f2 = d2$R)
    #colnames(data) <- c(file1, file2)

    p<-ggplot(data, aes(x=f1,y=f2))+
            geom_point(pch=1,cex=1.5)

    p2<- p +
        geom_abline(intercept=0, slope=1) +
        xlab(file1) +
        ylab(file2) +
        theme_bw(20)

    png(file=plot_path,height=640,width=640)
    print(p2)
    dev.off()
}

file1 <-arguments$file_1
file2 <-arguments$file_2

d1 <- load_results(file1)
d2 <- load_results(file2)

d1 <- d1[rownames(d1) %in% rownames(d2), ]
d1 <- d1[order(rownames(d1)),]

d2 <- d2[rownames(d2) %in% rownames(d1), ]
d2 <- d2[order(rownames(d2)),]

p1 <- basename(file1)
p2 <- basename(file2)

plot_path <- paste(arguments$result_prefix, "_", p1, "_", p2, ".png", sep="", collapse="")
plot_pearson(d1, p1, d2, p2, plot_path)
