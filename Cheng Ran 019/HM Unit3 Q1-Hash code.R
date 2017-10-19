#install.packages("digest")

library(digest)

digest("I learn a lot from this class when I am proper listening to the professor","sha1")

digest("I do not learn a lot from this class when I am absent and playing on my Iphone", "sha512")
