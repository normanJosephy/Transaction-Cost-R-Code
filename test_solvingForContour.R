# test_g.R

# Update
# From command line:
# test_file(path='test_solvingForContour.R',reporter='tap')

source('solvingForContour.R')

test_that("g computation",{
 u=1.2
 d=1/u
 g0 = g(c(u,d),r=0,n=1,s0=100,K=100)
 g1 = g(c(u,d),r=0,n=2,s0=100,K=100)
 ans = 100/11
 g2 = g(c(1.2,0.8),r=0,n=1,s0=100,K=90)
 g3 = g(c(1.2,0.8),r=0,n=2,s0=100,K=90)
 g4 = g(c(1.2,0.8),r=0.1,n=1,s0=100,K=90)
 expect_equal(g0,ans,label='At the money u=1.2,d=1/u one coin flip')
 expect_equal(g1,ans,label='At the money d=1/u two coin flips')
 expect_equal(g2,15,label='In the money u=1.2,d=.8, one coin flip')
 expect_equal(g3,33/2,label='In the money u=1.2,d=.8, two coin flips')
 expect_equal(g4,90/4.4,label='In the money u=1.2,d=0.8,r=0.1,one coin flip')
})

