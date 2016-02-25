# Carré de sable pour du code utile


read.csv("./data/raw/apd.2012.2013.eng.985cc934.a02c.4fdb.b8b6.02011d62ce8d.csv", fileEncoding="Latin1", header=T)

mypath <- "./data/raw"
multmerge <- function(mypath){
        filenames=list.files(path=mypath, full.names=TRUE)
        datalist = lapply(filenames, function(x){read.csv(file=x,header=T, fileEncoding="Latin1")})
        Reduce(function(x,y) {merge(x,y)}, datalist)
}

apd <- multmerge(mypath)

####################

Sect <- apd %>%
        select(No, Sector.ID, Sector.name, Sector.percent) %>%
        unique(by = c("No", "Sector.ID"))
View(Sect)

test <- Sect %>%
        group_by(No) %>%
        summarise(controle = sum(Sector.percent))

z <- test[controle !=1]


index <- test[controle != 1, No]

View(apd[index])

###################

df <- data.frame(
        x = sample(10, 100, rep = TRUE),
        y = sample(10, 100, rep = TRUE)
)
nrow(df)
nrow(distinct(df))
distinct(df, x)
distinct(df, y)


####

df <- apd %>%
        filter(No == "A021460001") %>%
        group_by(No, FY, Org.ID) %>%
        mutate(Org.disb = sum(Amount.spent))
View




x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)


new_counter <- function() {
        i <- 0
        function() {
                i <<- i + 1
                i
        }
}

i <- 0
new_counter2 <- function() {
        i <<- i + 1
        i
}


bc <- function(lambda) {
        if (lambda == 0) {
                function(x) log(x)
        } else {
                function(x) (x ^ lambda - 1) / lambda
        }
}


require(graphics)

x <- 1:10
y <- rnorm(10)
par(mfrow = c(2,1))
plot(x, y, main = "approx(.) and approxfun(.)")
points(approx(x, y), col = 2, pch = "*")
points(approx(x, y, method = "constant"), col = 4, pch = "*")

f <- approxfun(x, y)
curve(f(x), 0, 11, col = "green2")
points(x, y)
is.function(fc <- approxfun(x, y, method = "const")) # TRUE
curve(fc(x), 0, 10, col = "darkblue", add = TRUE)
## different extrapolation on left and right side :
plot(approxfun(x, y, rule = 2:1), 0, 11,
     col = "tomato", add = TRUE, lty = 3, lwd = 2)

## Show treatment of 'ties' :

x <- c(2,2:4,4,4,5,5,7,7,7)
y <- c(1:6, 5:4, 3:1)
approx(x, y, xout = x)$y # warning
(ay <- approx(x, y, xout = x, ties = "ordered")$y)
stopifnot(ay == c(2,2,3,6,6,6,4,4,1,1,1))
approx(x, y, xout = x, ties = min)$y
approx(x, y, xout = x, ties = max)$y

bc <- function(lambda) {
        if (lambda == 0) {
                function(x) log(x)
        } else {
                function(x) (x ^ lambda - 1) / lambda
        }
}

moment <- function(x) {
        f <- function(y) mean(
                (y - mean(y))^x
        )
}
x <- 1:100
n <- 2
mean((x - mean(x))^n)


μn := E[(X − E[X])n]


m1 <- moment(1)
m2 <- moment(2)

x <- runif(100)
stopifnot(all.equal(m1(x), 0))
stopifnot(all.equal(m2(x), var(x) * 99 / 100))

pick <- function(i) {
        f <- function (x) {
                x[i]
        }
}

lapply(mtcars, pick(5))
lapply(mtcars, function(x) x[[5]])

compute_mean <- list(
        base = function(x) mean(x),
        sum = function(x) sum(x) / length(x),
        manual = function(x) {
                total <- 0
                n <- length(x)
                for (i in seq_along(x)) {
                        total <- total + x[i] / n
                }
                total
        }
)

x <- runif(1e5)
system.time(compute_mean$base(x))
system.time(compute_mean[[2]](x))
system.time(compute_mean[["manual"]](x))

call_fun <- function(f, ...) f(...)

funs <- list(
        sum = function(x) sum(x),
        mean = function(x) mean(x),
        Q1 = function(x) quantile(x, 0.25),
        median = function(x) median(x),
        Q3 = function(x) quantile(x, 0.75),
        max = function(x) max(x)
)

x <- 1:10

lapply(funs, function(f) f(x))

####

aj <- function(df, col, elem1, elem2) {
        a <- df %>% filter(elem1)
        b <- df %>% filter(elem2)
        ret <- anti_join(a, b)
        
        
}


DT[!"a",...]
DT[J(x = unique(DT)[x!="a"][,x])]
DT = data.table(x=rep(c("a","b","c"),each=5), y=c(2000:2004), v=1:9, key="x")
Y <- data.table(x = c("b", "c", "c"), y = c("2000", "2000", "2001"), key = "x")
DT[Y]
DT[x == "b" & y == 2000]
Josh <- DT[J(x = unique(DT)[x!="a"][,x])]
Andrie <-  DT[-DT["a", which=TRUE]]

DT[-DT["a", which = TRUE], x2 := "a"] # Se baser sur ceci.
DT[, x2 := NA]
DT[DT["b"]]
[-DT["a", which = TRUE], x2 := "a"] 


########## Détermination des années de départ et de fin
require(data.table)
require(dplyr)

id <- c(rep("A", 3), rep("B", 2), rep("C", 3), rep("D", 4), rep("E", 5))
y <- c(2006:2008, 2007:2008, 2007:2009, 2007:2010, 2006:2010)
budget <- round(runif(17, 50, 100))
dt <- data.table(id = id, y = as.character(y), budget = budget)
setkey(dt, y)
years <- data.table(y = as.character(2006:2010))
setkey(years, y)

v <- as.character(2006:2010)

test <- for (i in years[[1]]) {
        dt %>% group_by(id) %>%
        anti_join(test[1])
}

test <- data.table(y = as.character(2006))
setkey(test, y)

dt %>% group_by(id) %>%
        anti_join(test[1])

##### Nouvel essai avec df plutôt que dt

project <- c(rep("A", 3), rep("B", 2), rep("C", 3), rep("D", 4), rep("E", 5))
y <- c(2006:2008, 2007:2008, 2007:2009, 2007:2010, 2006:2010)
budget <- round(runif(17, 50, 100))
df <- data.frame(project = project, y = y, budget = budget)
years <- data.frame(y = as.character(2006:2010))

# Fonctionne
df %>% filter(y == 2007) %>% anti_join(filter(df, y == 2006), by = "project") 

# On généralise
i = 2007
dfStart <- df %>% filter(y == i) %>% anti_join(filter(df, y == i-1), by = "project") %>% select(project)
cbind(dfStart, i)

# En boucle -- ne fonctionne pas
dfStart <- data.frame(project = NA)
dfproj <- NA
dfStart <- for(i in c(2007:2010)) {
        df %>% filter(y == i) %>% 
                anti_join(filter(df, y == i-1), by = "project") %>%
                select(project)
        return(df)
}

dfStart <- data.frame(project = NA)
dfproj <- NA

# Comme fonction
id <- "project"
var_annee = "y"
debut = 2006
fin = 2007

trouverDebut <- function(df, id = "project", var_annee = "y", debut, fin) {
        dfStart <- df %>% filter(get(var_annee) == debut + 1) %>% 
                anti_join(filter(df, get(var_annee) == debut), by = get(id)) %>% 
                select(get(id))
        return(dfStart)
        #cbind(dfStart, debut)
}        

trouverDebut(df = df, var_annee = y, debut = 2007, fin = 2008)

####
# Test pour la fonction précédente
id <- "project"


########## Changement d'approche: min, max ÇA MARCHE!
df %>% group_by(project) %>% summarise(start = min(y), end = max(y))







dfSummary <- df %>% group_by(project) %>%
        select(one_of(c("project"))) %>%
        unique(by = project)


# Marche pas.

test <- for (i in years[[1]]) {
        df %>% group_by(project) %>%
                anti_join(years[i, 1])
}

test <- data.table(y = as.character(2006))
setkey(test, y)

dt %>% group_by(id) %>%
        anti_join(test[1])


dfSummary <- dfSummary %>% mutate(df)
                


###########
library('devtools')
install_github('ProjectTemplate', username = 'johnmyleswhite')

###########
test <- "Canadian Non-Profit Making"
z <- str_split(test, " ")
str(z)

# Façon peu élégante de convertir la liste en df
temp <- org[1:10,]
names(temp)
names(temp)[5] <- "Type"
z <- str_split(temp$Type, " ")
View(transpose(as.data.frame(z))) # et après on extrait

# Autre manière: en utilisant lapply ou plyr

ddply(temp, .(Type), .(id), mutate, location = str_split(pattern = " "))

test_orig <- 
        
        temp %>% str_split(Org.type.location.profit.not.for.profit, " ")



extrait_origine <- function(z){
        str_split(z)[[1]][1]
}
lapply(X = z, function )

org2 <- org[1:20, ]
org2 %>% 
        separate(org2, Org.type.location.profit.not.for.profit, c("Origine", "Type", "Rien"), sep = " ", extra = "drop", fill = "warn")

############

p <- ggplot(mtcars, aes(factor(cyl), mpg))

p + geom_boxplot()
qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot")

p + geom_boxplot() + geom_jitter()
p + geom_boxplot() + coord_flip()
qplot(factor(cyl), mpg, data = mtcars, geom = "boxplot") +
        coord_flip()

############### WAFFLE CHART


library(ggplot2)

# Here's some data I had lying around
tb <- structure(list(region = c("Africa", "Asia", "Latin America", 
                                "Other", "US-born"), ncases = c(36L, 34L, 56L, 2L, 44L)), .Names = c("region", 
                                                                                                     "ncases"), row.names = c(NA, -5L), class = "data.frame")


# A bar chart of counts
ggplot(tb, aes(x = region, weight = ncases, fill = region)) +
        geom_bar()

# Bar chart of percentages
ggplot(tb, aes(x = region, weight = ncases/sum(ncases), fill = region)) +
        geom_bar() +
        scale_y_continuous(formatter = 'percent')

# Pie chart equivalents.  Forgive me, Hadley, for I must sin.
ggplot(tb, aes(x = factor(1), weight = ncases, fill = region)) +
        geom_bar(width = 1) +
        coord_polar(theta = "y") +
        labs(x = "", y = "")

ggplot(tb, aes(x = factor(1), weight = ncases/sum(ncases), fill = region)) +
        geom_bar() +
        scale_y_continuous(formatter = 'percent') +
        coord_polar(theta = "y") +
        labs(x = "", y = "")


# Waffles
# How many rows do you want the y axis?
ndeep <- 5

# I need to convert my data into a data.frame with a unique specified x
# and y axis for each case
# Note - it's actually important to specify y first for a
# horizontally-accumulating waffle
tb4waffles <- expand.grid(y = 1:ndeep,
                          x = seq_len(ceiling(sum(tb$ncases) / ndeep)))

# Expand the counts into a full vector of region labels - i.e., de-aggregate
regionvec <- rep(tb$region, tb$ncases)

# Depending on the value of ndeep, there might be more spots on the x-y grid
# than there are cases - so fill those with NA
tb4waffles$region <- c(regionvec, rep(NA, nrow(tb4waffles) - length(regionvec)))

# Plot it
ggplot(tb4waffles, aes(x = x, y = y, fill = region)) + 
        geom_tile(color = "white") + # The color of the lines between tiles
        scale_fill_manual("Region of Birth",
                          values = RColorBrewer::brewer.pal(5, "Dark2"))
##### Utilisation de sapply

df <- read.csv("./data/liste.fichiers.csv")
df %>% paste0(chemin, nom.fichier)

tst <- data.frame(a = c("un", "deux", "trois"), b = c(1:3))



#######

a1 <- data.frame(x = c(1:3), y = (22:24))
a1 %>% mutate %>% paste(x, y, sep = "/")
