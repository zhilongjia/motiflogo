#' A new representation of DNA motif logo
#' 
#' A new representation of DNA motif logo via ggplot2. Besides, the xlabel could
#' be DNA or SNP.
#' 
#' @param pwm position weight matrix
#' @param xlabel text of label in x-axis
#' @param ylabel The default is Information Content
#' @param tit The default is Motif
#' @param addpoint add point to the position of nt. The default is FALSE
#' @param scaleSize the scale size of nt. ref \code{\link[ggplot2]{scale_size}}
#' @author Zhilong Jia
#' @export
#' @examples
#' pfm <- matrix(data=c(5, 3, 16, 1, 0, 17, 17, 0, 0, 16, 12, 8,
#' 6, 9, 1, 1, 18,  1, 0, 0, 18,  1, 0, 2,
#' 2,3, 1, 0, 0, 0, 0, 1, 0, 0, 1, 2,
#' 5, 3, 0, 16, 0, 0, 1, 17,  0, 1, 5, 6), 
#' byrow=TRUE,nrow=4,dimnames=list(c('A','C','G','T')))
#' 
#' pwm <- apply(pfm, 2, function(x) x/sum(x))
#' motiflogo(pwm)
#' 
#' xlabel <- c("T\nT", "G\nG", "A\nA", "T\nT", "C\nC", "A\nC", "A\nA", "A\nA", "C\nC","A\nA", "A\nA", "T\nT")
#' motiflogo(pwm, xlabel=xlabel)
#' motiflogo(pwm, xlabel=xlabel, addpoint=TRUE)
#' 
#' 
motiflogo<-function(pwm, xlabel=NULL, ylabel="Information Content", tit="Motif", addpoint=FALSE, 
                   scaleSize=c(5,20)){  
    
    ic <- apply(pwm, 2, function (x) { x[which(x==0)] = 0.000001; 2 + sum(x*log2(x))})
    ic <- rep(ic, each=4 )
    data <- melt(pwm, varnames=c("nt","Position"))
    data$ic <- data$value * ic
    
    p <- ggplot(data, aes(x=Position, y=ic, label=nt)) + ylim(-0.1,2.2)
    if (addpoint) {p <- p + geom_point()}
    #cope with the xlabel.
    if (!is.null(xlabel)) {
        if (length(xlabel) == ncol(pwm))
            {p1 <- p+ theme(axis.text.x=element_text(size=sapply(xlabel, nts2size),
                                                 face=sapply(xlabel,nts2fnot),
                                                 colour=sapply(xlabel, nts2col)),
                      axis.title=element_text(size=14), 
                      axis.ticks.x = element_line(size = 0.5, colour = "black", linetype = "solid")) +
            scale_x_continuous(breaks=unique(data$Position), labels=xlabel)
        p2 <- p1 + geom_text(colour=sapply(data$nt, AGCT2col), aes(size=ic, fontface="bold", family="URWHelvetica"),  
                            position = position_jitter(w = 0.15, h = 0.01))}
        else{
            stop("error xlabel")
        }
            
    } else {
        p2 <- p + geom_text(aes(colour=sapply(nt, AGCT2col), size=ic, fontface="bold", family="URWHelvetica"),  
                            position = position_jitter(w = 0.15, h = 0.01)) +
            scale_x_continuous(breaks=1:ncol(pwm))     
    }
    
    p2 + scale_size(range=scaleSize) + ylab(ylabel) + ggtitle(tit) +
        theme(axis.line = element_line(size = 0.5, colour = "black", linetype = "solid"),
              axis.line.y=element_blank(),
              legend.position="none") 
}

