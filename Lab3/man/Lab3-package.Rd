\name{Lab3-package}
\alias{Lab3-package}
\alias{Lab3}
\docType{package}
\title{
\packageTitle{Lab3}
}
\description{
\packageDescription{Lab3}
}
\details{
Package: Lab3
Type: Package
Title: Euclidian algorithm and Dijkstra algorithm
Version: 1.0
Date: 2015-09-09
Author: Niclas Lovsjo, Maxime Bonneau
Maintainer: Niclas Lovsjo <niclas.lovsjo@me.com>
Description: Euclidian finds GCD's and Dijkstra's find shortest paths in graphs
License: GPL-2

\packageDESCRIPTION{Lab3}
\packageIndices{Lab3}
Euclidian finds GCD's of two scalars and Dijkstra's algorithm finds shortest paths in a graph.
}
\author{
\packageAuthor{Lab3}

Maintainer: \packageMaintainer{Lab3}
}
\references{
https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
}

\keyword{ package }
\seealso{
Course homepage:
https://www.ida.liu.se/~732A50/info/courseinfo.en.shtml
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
> dijkstra(wiki_graph,1)
[1]  0  7  9 20 20 11
> euclidian(2425,5)
[1] 5
}
