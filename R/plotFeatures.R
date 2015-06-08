plotFeatures <- function(df, col=NULL, col.start="start", col.end="end",
                         col.id="label", col.type="type", box.height=4,
                         box.sep=2, pos.label=c("middle", "top", "bottom", "no"),
                         new=TRUE, legend=TRUE, ...)
{
    if (!all(c(col.start, col.end, col.id, col.type) %in% names(df))) {
        stop("Missing columns in the input data.frame")
    }

    args <- list(...)
    pos.label <- match.arg(pos.label)

    f2n <- function(x) as.numeric(as.character(x))

    if (new) {
        if (is.null(args[["xlim"]])) {
            args[["xlim"]] <- c(min(f2n(df[[col.start]])),
                                max(f2n(df[[col.end]])))
        }
        if (is.null(args[["ylim"]])) {
            args[["ylim"]] <- c(0, 1)
        }
        if (is.null(args[["xlab"]])) {
            args[["xlab"]] <- "coordinates"
        }
        if (is.null(args[["ylab"]])) {
            args[["ylab"]] <- ""
        }
        if (is.null(args[["yaxt"]])) {
            args[["yaxt"]] <- "n"
        }
        if (is.null(args[["frame.plot"]])) {
            args[["frame.plot"]] <- FALSE
        }
        do.call(plot.default,
                c(list(x=args[["xlim"]], y=args[["ylim"]], type="n"),
                  args))
    } else {
        if (dev.cur() == 1) {
            stop("Graphical device not found (and new=FALSE)")
        }
    }
    par <- par()

    pcy <- (par$usr[4] - par$usr[3]) * 0.01
    rans <- IRanges(f2n(df[[col.start]]),
                    f2n(df[[col.end]]))
    bins <- disjointBins(rans)

    if (is.null(col)) {
        col <- rainbow(length(levels(df[[col.type]])))
    }
    if (is.null(names(col))) {
        names(col) <-  levels(df[[col.type]])
    }

    fullh <- box.height + box.sep

    rect(start(rans),
         (fullh * (bins-1) + box.sep) * pcy,
         end(rans),
         fullh*bins*pcy,
         col=col[df[[col.type]]])

    #limx <- sapply((start(rans)+end(rans))/2, function(x) max(x, par$usr[1]))
    # limx <- sapply(limx, function(x) min(x, par$usr[2]))
    limx <- sapply((start(rans)+end(rans)) / 2, max, par$usr[1])
    limx <- sapply(limx, min, par$usr[2])

    if (pos.label == "middle") {
        pos <- (fullh*(bins-1) + box.sep + box.height/2) * pcy
        adj <- c(0.5,0.5)
        text(limx, pos, df[[col.id]], font=2, adj=adj)
    }
    if (pos.label == "top") {
        pos <- fullh*bins*pcy + pcy
        adj <- c(0.5,0)
        text(limx, pos, df[[col.id]], font=2, adj=adj)
    }
    if (pos.label == "bottom") {
        pos <- (fullh*bins - box.height)*pcy - pcy
        adj <- c(0.5, 1)
        text(limx, pos, df[[col.id]], font=2, adj=adj)
    }

    if (legend) {
        legend("topright", names(col), fill=col, bty="n")
    }
}
