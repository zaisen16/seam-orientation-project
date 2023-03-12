
people.0 <- read.csv("~/Coding stuff/people-0.csv")
people.1 <- read.csv("~/Coding stuff/people-1.csv")
people.2 <- read.csv("~/Coding stuff/people-2.csv")
people.3 <- read.csv("~/Coding stuff/people-3.csv")
people.4 <- read.csv("~/Coding stuff/people-4.csv")
people.5 <- read.csv("~/Coding stuff/people-5.csv")
people.6 <- read.csv("~/Coding stuff/people-6.csv")
people.7 <- read.csv("~/Coding stuff/people-7.csv")
people.8 <- read.csv("~/Coding stuff/people-8.csv")
people.9 <- read.csv("~/Coding stuff/people-9.csv")
people.a <- read.csv("~/Coding stuff/people-a.csv")
people.b <- read.csv("~/Coding stuff/people-b.csv")
people.c <- read.csv("~/Coding stuff/people-c.csv")
people.d <- read.csv("~/Coding stuff/people-d.csv")
people.e <- read.csv("~/Coding stuff/people-e.csv")
people.f <- read.csv("~/Coding stuff/people-f.csv")


Names <- rbind(people.0, people.1, people.2, people.3, people.4, people.5, 
               people.6, people.7, people.8, people.9, people.a, people.b,
               people.c, people.d, people.e, people.f) %>% 
  filter(pro_played_last > 2015) %>% 
  select(key_mlbam, name_first, name_last, pro_played_last) %>% na.omit()
  Names$Full <- paste(Names$name_first, Names$name_last, sep = " ") 

 X <- subset(Names, Full == "Marcus Stroman")
 