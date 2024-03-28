#_____________________________________________________
# define parameters ----------------------------------
mean      <- 51.5660557757944
povline   <- 57.791666666666664
A         <- 0.795981535745657
B         <- -1.4445933880119242
C         <- 0.14728191995919815
e         <- -0.498670067692931
m         <- -1.0970760862948583
n         <- 0.851623285340541
r         <- 1.3477796260474386
s1        <- -0.22612667749534146
s2        <- 1.002393060455814
headcount <- 0.76005810499191284
pov_gap   <- 0.27617606019159308


test_that("old_gd_compute_poverty_stats_lq works as expected", {

  benchmark <- list(
    headcount = 0.76005810499191284,
    pg        = 0.27617606019159308,
    p2        = 0.12832887439632906,
    eh        = -0.87181309219603054,
    epg       = -1.7520781651553494,
    ep        = -2.3041920454886071,
    gh        = -0.093916119652440649,
    gpg       = 0.70353220826204077,
    gp        = 1.5363306438390838,
    watts     = 0.39088363448720104,
    dl        = 1.1207307654300092,
    ddl       = 1.691340795153677
  )

  out <- old_gd_compute_poverty_stats_lq(
    mean    = mean,
    povline = povline,
    A       = A,
    B       = B,
    C       = C,
    e       = e,
    m       = m,
    n       = n,
    r       = r,
    s1      = s1,
    s2      = s2
  )

  # Expectations----
  expect_equal(length(out),
               length(benchmark))
  expect_equal(names(out),
               c("headcount", "pg", "p2", "eh", "epg",
                 "ep", "gh", "gpg", "gp", "watts", "dl", "ddl"))
  expect_equal(round(out$headcount, 7),
               round(benchmark$headcount, 7))
  expect_equal(out$pg,
               benchmark$pg)
  expect_equal(out$p2,
               benchmark$p2)
  expect_equal(round(out$eh, 6),
               round(benchmark$eh, 6)) # Due to headcount difference
  expect_equal(round(out$epg, 7),
               round(benchmark$epg, 7)) # Due to headcount difference
  expect_equal(out$ep,
               benchmark$ep)
  expect_equal(out$gh,
               benchmark$gh, tolerance = 1.1e-07)
  expect_equal(out$gpg,
               benchmark$gpg)
  expect_equal(out$gp,
               benchmark$gp)
  expect_equal(out$watts,
               benchmark$watts)
  expect_equal(round(out$dl, 7),
               round(benchmark$dl, 7))
  expect_equal(round(out$ddl, 6),
               round(benchmark$ddl, 6))
  expect_equal(out,
               benchmark,
               tolerance = 0.000001)
})

test_that("old_gd_compute_poverty_stats_lq works as gd_compute_poverty_stats_lq", {

  benchmark <- gd_compute_poverty_stats_lq(
    mean    = mean,
    povline = povline,
    A       = A,
    B       = B,
    C       = C,
    e       = e,
    m       = m,
    n       = n,
    r       = r,
    s1      = s1,
    s2      = s2)

  out <- old_gd_compute_poverty_stats_lq(
    mean    = mean,
    povline = povline,
    A       = A,
    B       = B,
    C       = C,
    e       = e,
    m       = m,
    n       = n,
    r       = r,
    s1      = s1,
    s2      = s2
  )

  expect_equal(length(out), length(benchmark))
  expect_equal(names(out),
               c("headcount", "pg", "p2", "eh", "epg",
                 "ep", "gh", "gpg", "gp", "watts", "dl", "ddl"))
  expect_equal(round(out$headcount, 7),
               round(benchmark$headcount, 7))
  expect_equal(out$pg,
               benchmark$pg)
  expect_equal(out$p2,
               benchmark$p2)
  expect_equal(round(out$eh, 6),
               round(benchmark$eh, 6)) # Due to headcount difference
  expect_equal(round(out$epg, 7),
               round(benchmark$epg, 7)) # Due to headcount difference
  expect_equal(out$ep,
               benchmark$ep)
  expect_equal(out$gh,
               benchmark$gh,
               tolerance = 1.1e-07)
  expect_equal(out$gpg,
               benchmark$gpg)
  expect_equal(out$gp,
               benchmark$gp)
  expect_equal(out$watts,
               benchmark$watts)
  expect_equal(round(out$dl, 7),
               round(benchmark$dl, 7))
  expect_equal(round(out$ddl, 6),
               round(benchmark$ddl, 6))
  expect_equal(out,
               benchmark,
               tolerance = 0.000001)
})
