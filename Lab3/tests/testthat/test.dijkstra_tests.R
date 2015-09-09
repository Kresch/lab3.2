#test dijkstra

library(stringr)

test1 <- list(v1=c(1,1,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6),
                   v2=c(2,3,1,3,4,1,2,4,5,2,3,5,6,3,4,6,1,4),
                   w=c(4,2,4,1,5,2,1,8,10,5,8,2,6,10,2,3,6,3))
                        #not DF

test2 <- data.frame(v1=c(1,1,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6),
                   v2=c(2,3,1,3,4,1,2,4,5,"a",3,5,6,3,4,6,4,5),
                   w=c(4,2,4,1,5,2,1,8,10,5,8,2,6,10,2,3,6,3))
                        #inputed char


test_that("It checks for correct input-type.",
          {
                expect_error(dijkstra(wiki_graph,c(1,2)))
                expect_error(dijkstra(test1,1))
                expect_error(dijkstra(test2,1))
          })
test_that("Expected values for calling Dijkstra", {
        expect_equal(dijkstra(wiki_graph,1),c(0,7,9,20,20,11))
        expect_equal(dijkstra(wiki_graph,3),c(9,10,0,11,11,2))
})