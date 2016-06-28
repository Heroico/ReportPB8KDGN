#! /usr/bin/env Rscript
library(argparse)
library(dplyr)
library(ggplot2)

parser <- ArgumentParser(description='Compare gene expressions')
parser$add_argument('--file_1',
                    help='Path of expressions file 1',
                    default='data/Whole_Blood_Analysis.expr.txt.gz')
parser$add_argument('--file_2',
                    help='Path of expressions file 2',
                    default='results/PB8K_expression.txt.gz')

parser$add_argument('--result_prefix',
                    help='comparison prefix',
                    default='results/PB8K_comparison')

arguments <- parser$parse_args(commandArgs(TRUE))

source("ExpressionUtilities.R")

print("Loading file 1")
d1 <- load_file(arguments$file_1)

print("Loading file 2")
d2 <- load_file(arguments$file_2)

print("Preparing data")
d1 <- d1[rownames(d1) %in% rownames(d2), colnames(d1) %in% colnames(d2)]
d1 <- d1[order(rownames(d1)),]

d2 <- d2[rownames(d2) %in% rownames(d1), colnames(d2) %in% colnames(d1)]
d2 <- d2[order(rownames(d2)),]

print("Doing regression")

n <- length(colnames(d2))

results <- build_results(d1, d2)
save_stuff(results, n, arguments$result_prefix)

