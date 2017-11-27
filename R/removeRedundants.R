`removeRedundants` <-
function(implicants, noflevels) {
    mbase <- rev(c(1, cumprod(rev(noflevels))))[-1]
    .Call("removeRedundants", implicants, noflevels - 1, mbase, package = "QCA")
}
