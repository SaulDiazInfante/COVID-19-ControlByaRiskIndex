library(ggplot2)
library(vistime)
pres <- data.frame(
    Position = rep(c("R_t >1", "Risk>0.5"), each = 3),
    Name = c("Washington", rep(c("Adams", "Jefferson"), 2), "Burr"),
    start = c("1", "1797-02-03", "1801-02-03"),
    end = c("1797-02-03", "1801-02-03", "1809-02-03"),
    color = c('#cbb69d', '#603913', '#c69c6e'),
    fontcolor = c("black", "white", "black"))

gg_vistime(
    pres,
    col.event = "Position",
    col.group = "Name",
    title = "Presidents of the USA"
)
