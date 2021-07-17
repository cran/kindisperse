## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(kindisperse)

## ----appdata------------------------------------------------------------------
fullsibs <- simulate_kindist_composite(nsims = 100, ovisigma = 25, kinship = "FS")
reset_appdata()
mount_appdata(fullsibs, "fullsibs")
display_appdata()
fullsibs2 <- retrieve_appdata("fullsibs")
reset_appdata()

## ----graphical_simulation, fig.width = 10, fig.height = 10--------------------

## run graphical simulation
graphdata <- simgraph_data(nsims = 1000, posigma = 25, dims = 250)
simgraph_graph(graphdata, nsim = 5, kinship = "1C")

## ----simgraphpin, fig.width = 10, fig.height = 10-----------------------------

graphdata <- simgraph_data(nsims = 1000, posigma = 25, dims = 250)
simgraph_graph(graphdata, nsims = 1000, pinwheel = T, kinship = "1C")


## ----simgraphhist, fig.width = 10, fig.height = 10----------------------------

graphdata <- simgraph_data(nsims = 1000, posigma = 25, dims = 250)
simgraph_graph(graphdata, nsims = 1000, histogram = T, kinship = "1C")


## ----simplesim----------------------------------------------------------------
simulate_kindist_simple(nsims = 5, sigma = 100, method = "Gaussian", kinship = "PO", lifestage = "immature")

## ----compsim------------------------------------------------------------------
simulate_kindist_composite(nsims = 5, initsigma = 50, breedsigma = 30, gravsigma = 50, ovisigma = 10, method = "Laplace", kinship = "H1C", lifestage = "ovipositional")

## ----dmodel-------------------------------------------------------------------
dmodel <- dispersal_model(juvenile = 50, breeding = 40, gestation = 30, .FS = "juvenile", .HS = "breeding", .sampling_stage = "gestation")
dmodel

## ----customsim----------------------------------------------------------------
simulate_kindist_custom(nsims = 5, model = dmodel, kinship = "PO")


## ----sampledims---------------------------------------------------------------

compsim <- simulate_kindist_composite(nsims = 100000, kinship = "H2C")

sample_kindist(compsim, upper = 1000, lower = 200, spacing = 50, n = 25)


## ----files--------------------------------------------------------------------
kinobject <- simulate_kindist_simple(nsims = 25, kinship = "FS", lifestage = "immature")
#kinpair_to_csv(kinobject, "FS_kin.csv") # saves file
#csv_to_kinpair("FS_kin.csv") # reloads it


## ----to kinpairdata-----------------------------------------------------------
kinvect <- c(25, 23, 43, 26, 14, 38)

vector_to_kinpair(kinvect, kinship = "H1C", lifestage = "immature")


## ----axials-------------------------------------------------------------------
paroff <- simulate_kindist_simple(nsims = 1000, sigma = 75, kinship = "PO")
axials(paroff)

## ----axials2------------------------------------------------------------------
fullsibs <- simulate_kindist_composite(nsims = 10000, ovisigma = 25, kinship = "FS")
axials(fullsibs, composite = 2)

## ----axials subtract----------------------------------------------------------
axials_subtract(24, 19)

## ----axial setup--------------------------------------------------------------

# set up initial sigma values

init = 50
brd = 25
grv = 75
ovs = 10

# calculate theoretical PO value
po_sigma <- sqrt(init^2 + brd^2 + grv^2 + ovs^2)
po_sigma


## ----axial standard-----------------------------------------------------------

# set up sims

fullsibs <- simulate_kindist_composite(nsims = 75, initsigma = init, breedsigma = brd, gravsigma = grv, ovisigma = ovs, kinship = "FS")

fullcous <- simulate_kindist_composite(nsims = 75, initsigma = init, breedsigma = brd, gravsigma = grv, ovisigma = ovs, kinship = "1C")

# calculate PO axial sigma C.I. 

axpermute_standard(fullcous, fullsibs)


## -----------------------------------------------------------------------------
# Set up new distributions
halfsibs <- simulate_kindist_composite(nsims = 75, initsigma = init, breedsigma = brd, gravsigma = grv, ovisigma = ovs, kinship = "HS")

halfcous <- simulate_kindist_composite(nsims = 75, initsigma = init, breedsigma = brd, gravsigma = grv, ovisigma = ovs, kinship = "H1C")

# combine cousin distributions & recompose as object. Chaning kinship
# to standard value for unknown as I will be combining the distributions. 
fc <- dplyr::mutate(kinpair_to_tibble(fullcous), kinship = "UN")
hc <- dplyr::mutate(kinpair_to_tibble(halfcous), kinship = "UN")
cc <- tibble::add_row(fc, hc) 
cousins <- df_to_kinpair(cc)
cousins


## ----fancy estimate-----------------------------------------------------------

# amix allows supply of additional (mixed) kin category H1C to acat 1C;
# bcomp allows supply of distribution to composite with bvect (this is done to match 
# the cousin mixture in phase)
axpermute_standard(avect = cousins, acat = "1C", amix = TRUE, amixcat = "H1C", bvect = fullsibs, bcomp = TRUE, bcompvect = halfsibs)


## ----dispersal model----------------------------------------------------------

antechinus_model <- dispersal_model(juvenile = 100, breeding = 50, gestation = 25, pouch = 25, .FS = "juvenile", .HS = "breeding", .sampling_stage = "juvenile")
antechinus_model


## ----initial_sim--------------------------------------------------------------
library(magrittr)
ant_po <- simulate_kindist_custom(nsims = 10000, model = antechinus_model, kinship = "PO")
ant_po


## ----ant basic po estimate----------------------------------------------------

ant_fs <- simulate_kindist_custom(nsims = 10000, model = antechinus_model, kinship = "FS")
ant_1c <- simulate_kindist_custom(nsims = 10000, model = antechinus_model, kinship = "1C")

axials_standard(ant_1c, ant_fs) # larger dispersal category goes first. 


## ----switch sampling----------------------------------------------------------
antechinus_model <- dispersal_model(juvenile = 100, breeding = 50, gestation = 25, pouch = 25, .FS = "juvenile", .HS = "breeding", 
                                    .sampling_stage = "pouch", .breeding_stage = "breeding", .visible_stage = "juvenile")
antechinus_model

## ----antechinus 1c------------------------------------------------------------
ant_1c_juv <- simulate_kindist_custom(nsims = 100000, model = antechinus_model, kinship = "1C", cycle = -1, method = "vgamma")
ant_1c_juv

## ----antechinus fs------------------------------------------------------------
ant_fs_juv <- simulate_kindist_custom(nsims = 100000, model = antechinus_model, kinship = "FS", cycle = -1, method = "vgamma")
ant_fs_juv

## ----ant po est---------------------------------------------------------------

axpermute_standard(ant_1c_juv, ant_fs_juv, nsamp = 100, override = TRUE)

## ----ant sample1--------------------------------------------------------------
ant_1c_juv %>% sample_kindist(dims = 100, n = 1000) %>% axpermute_standard(ant_fs_juv, nsamp = 100, override = TRUE)


## ----ant sample2--------------------------------------------------------------
ant_1c_juv %>% sample_kindist(dims = 1000, n = 1000) %>% axpermute_standard(ant_fs_juv, nsamp = 100, override = TRUE)


## ----ant sample3--------------------------------------------------------------
ant_1c_juv %>% sample_kindist(dims = 2000, n = 1000) %>% axpermute_standard(ant_fs_juv, nsamp = 100, override = TRUE)


