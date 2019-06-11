context("downloads.R")

test_that(".albouy.2015", {
    skip_on_cran()
    expect_warning(data <- .albouy.2015(), "attributes are not identical across measure variables; they will be dropped")
    expect_equal(nrow(data$numeric),  3147)
    expect_equal(length(unique(data$numeric$variable)),  5)
    expect_equal(nrow(data$character), 10561)
    expect_equal(length(unique(data$character$variable)),  17)
})

test_that(".ameztegui.2016", {
    skip_on_cran()
    data <- .ameztegui.2016()
    expect_equal(nrow(data$numeric),  430)
    expect_equal(length(unique(data$numeric$variable)),  9)
    expect_equal(nrow(data$character), 192)
    expect_equal(length(unique(data$character$variable)),  4)
})

test_that(".arnold.2016", {
    skip_on_cran()
    data <- .arnold.2016()
    expect_equal(nrow(data$numeric), 4350)
    expect_equal(length(unique(data$numeric$variable)),  15)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".artacho.2015", {
    skip_on_cran()
    data <- .artacho.2015()
    expect_equal(nrow(data$numeric),  1332)
    expect_equal(length(unique(data$numeric$variable)),  7)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".aubret.2012 family", {
    skip_on_cran()
    data <- .aubret.2012a()
    expect_equal(nrow(data$numeric),  2278)
    expect_equal(length(unique(data$numeric$variable)),  4)
    expect_equal(nrow(data$character), 592)
    expect_equal(length(unique(data$character$variable)),  1)

    data <- .aubret.2012b()
    expect_equal(nrow(data$numeric),  920)
    expect_equal(length(unique(data$numeric$variable)),  13)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)

    data <- .aubret.2012c()
    expect_equal(nrow(data$numeric),  966)
    expect_equal(length(unique(data$numeric$variable)),  2)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".augspurger.2016 family", {
    skip_on_cran()
    data <- .augspurger.2016a()
    expect_equal(nrow(data$numeric),  720)
    expect_equal(length(unique(data$numeric$variable)),  4)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)

    data <- .augspurger.2016b()
    expect_equal(nrow(data$numeric),  72)
    expect_equal(length(unique(data$numeric$variable)),  6)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".bellobedoy.2015 family", {
    skip_on_cran()
    data <- .bellobedoy.2015a()
    expect_equal(nrow(data$numeric),  808)
    expect_equal(length(unique(data$numeric$variable)),  4)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)

    expect_warning(data <- .bellobedoy.2015b(), "attributes are not identical across measure variables; they will be dropped")
    expect_equal(nrow(data$numeric),  1194)
    expect_equal(length(unique(data$numeric$variable)),  5)
    expect_equal(nrow(data$character), 478)
    expect_equal(length(unique(data$character$variable)),  2)

})

test_that(".benesh.2017", {
    skip_on_cran()
    expect_warning(data <- .benesh.2017(), "attributes are not identical across measure variables; they will be dropped")
    expect_equal(nrow(data$numeric),  39941)
    expect_equal(length(unique(data$numeric$variable)),  8)
    expect_equal(nrow(data$character), 26990)
    expect_equal(length(unique(data$character$variable)),  6)
})

test_that(".bengtsson.2016", {
    skip_on_cran()
    data <- .bengtsson.2016()
    expect_equal(nrow(data$numeric),  3679)
    expect_equal(length(unique(data$numeric$variable)),  27)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that('.bennett.2016', {
    data <- .bennett.2016()
    # Updating numeric and character values, now that shoot_mass variableis being captured as numeric
    expect_equal(nrow(data$numeric), 4124 )
    expect_equal(length(unique(data$numeric$variable)), 8 )
    expect_equal(nrow(data$character), NULL )
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that(".broeckhoven.2016", {
    skip_on_cran()
    data <- .broeckhoven.2016()
    expect_equal(nrow(data$numeric),  2520)
    expect_equal(length(unique(data$numeric$variable)),  21)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".brown.2015", {
    skip_on_cran()
    data <- .brown.2015()
    expect_equal(nrow(data$numeric),  4920)
    expect_equal(length(unique(data$numeric$variable)),  6)
    expect_equal(nrow(data$character), 5740)
    expect_equal(length(unique(data$character$variable)),  7)
})

test_that(".buzzard.2015", {
    skip_on_cran()
    data <- .buzzard.2015()
    expect_equal(nrow(data$numeric),  1038)
    expect_equal(length(unique(data$numeric$variable)),  6)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".cariveau.2016", {
    skip_on_cran()
    data <- .cariveau.2016()
    expect_equal(nrow(data$numeric),  700)
    expect_equal(length(unique(data$numeric$variable)),  7)
    expect_equal(nrow(data$character), 121)
    expect_equal(length(unique(data$character$variable)),  3)
})

test_that(".carmona.2014", {
    skip_on_cran()
    data <- .carmona.2014()
    expect_equal(nrow(data$numeric),  7414)
    expect_equal(length(unique(data$numeric$variable)),  3)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".case.2016", {
    skip_on_cran()
    expect_warning(data <- .case.2016(), "attributes are not identical across measure variables; they will be dropped")
    expect_equal(nrow(data$numeric),  1454)
    expect_equal(length(unique(data$numeric$variable)),  13)
    expect_equal(nrow(data$character), 351)
    expect_equal(length(unique(data$character$variable)),  3)
})

test_that(".castillo.2016", {
    skip_on_cran()
    data <- .castillo.2016()
    expect_equal(nrow(data$numeric),  1027)
    expect_equal(length(unique(data$numeric$variable)),  5)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".cavenderbares.2015 family", {
    skip_on_cran()
    data <- .cavenderbares.2015a()
    expect_equal(nrow(data$numeric),  3032)
    expect_equal(length(unique(data$numeric$variable)),  4)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)

    data <- .cavenderbares.2015b()
    expect_equal(nrow(data$numeric),  188)
    expect_equal(length(unique(data$numeric$variable)),  2)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)

    data <- .cavenderbares.2015c()
    expect_equal(nrow(data$numeric),  189397)
    expect_equal(length(unique(data$numeric$variable)),  40)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that('.clemente.2014', {
    data <- .clemente.2014()
    expect_equal(nrow(data$numeric), 248 )
    expect_equal(length(unique(data$numeric$variable)), 2 )
    expect_equal(nrow(data$character), 248 )
    expect_equal(length(unique(data$character$variable)), 2 )
})

test_that('.collar.2016', {
    data <- .collar.2016()
    expect_equal(nrow(data$numeric), 1181 )
    expect_equal(length(unique(data$numeric$variable)), 13 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that(".comeault.2013", {
    skip_on_cran()
    data <- .comeault.2013()
    expect_equal(nrow(data$numeric),  4147)
    expect_equal(length(unique(data$numeric$variable)),  13)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that('.delariva.2015', {
    data <- .delariva.2015()
    expect_equal(nrow(data$numeric), 900)
    expect_equal(length(unique(data$numeric$variable)), 12)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0)
})

test_that(".deraison.2014", {
    skip_on_cran()
    data <- .deraison.2014()
    expect_equal(nrow(data$numeric),  154)
    expect_equal(length(unique(data$numeric$variable)),  7)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".dmitriew.2014", {
    skip_on_cran()
    data <- .dmitriew.2014()
    expect_equal(nrow(data$numeric),  5331)
    expect_equal(length(unique(data$numeric$variable)),  3)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".edwards.2015 family", {
    skip_on_cran()
    expect_warning(data <- .edwards.2015a(), "attributes are not identical across measure variables; they will be dropped")
    expect_equal(nrow(data$numeric),  2762)
    expect_equal(length(unique(data$numeric$variable)),  35)
    expect_equal(nrow(data$character), 384)
    expect_equal(length(unique(data$character$variable)),  1)

    data <- .edwards.2015b()
    expect_equal(nrow(data$numeric),  134)
    expect_equal(length(unique(data$numeric$variable)),  1)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".engemann.2016", {
    skip_on_cran()
    expect_warning(data <- .engemann.2016(), "attributes are not identical across measure variables; they will be dropped")
    expect_equal(nrow(data$numeric),  NULL)
    expect_equal(length(unique(data$numeric$variable)),  0)
    expect_equal(nrow(data$character), 134826)
    expect_equal(length(unique(data$character$variable)),  2)
})

test_that(".enriquezurzelai.2015", {
    skip_on_cran()
    data <- .enriquezurzelai.2015()
    expect_equal(nrow(data$numeric),  657)
    expect_equal(length(unique(data$numeric$variable)),  3)
    expect_equal(nrow(data$character), 235)
    expect_equal(length(unique(data$character$variable)),  1)
})

test_that(".falster.2015", {
    skip_on_cran()
    expect_warning(data <- .falster.2015(), "attributes are not identical across measure variables; they will be dropped")
    expect_equal(nrow(data$numeric),  252916)
    expect_equal(length(unique(data$numeric$variable)),  47)
    expect_equal(nrow(data$character), 77659)
    expect_equal(length(unique(data$character$variable)),  5)
})

test_that(".fargevieille.2017", {
    skip_on_cran()
    data <- .fargevieille.2017()
    expect_equal(nrow(data$numeric),  16065)
    expect_equal(length(unique(data$numeric$variable)),  10)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".friedman.2014", {
    skip_on_cran()
    data <- .friedman.2014()
    expect_equal(nrow(data$numeric),  8791)
    expect_equal(length(unique(data$numeric$variable)),  11)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".fitzgerald.2017", {
    skip_on_cran()
    data <- .fitzgerald.2017()
    expect_equal(nrow(data$numeric),  5275)
    expect_equal(length(unique(data$numeric$variable)),  40)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".goncalves.2018", {
    skip_on_cran()
    data <- .goncalves.2018()
    expect_equal(nrow(data$numeric),  82866)
    expect_equal(length(unique(data$numeric$variable)),  4)
    expect_equal(nrow(data$character), 77333)
    expect_equal(length(unique(data$character$variable)), 3)
})

test_that(".gossner.2015", {
    skip_on_cran()
    expect_warning(data <- .gossner.2015(), "attributes are not identical across measure variables; they will be dropped")
    expect_equal(nrow(data$numeric),  2460)
    expect_equal(length(unique(data$numeric$variable)),  2)
    expect_equal(nrow(data$character), 7778)
    expect_equal(length(unique(data$character$variable)),  9)
})

test_that(".grootemaat.2015", {
    skip_on_cran()
    data <- .grootemaat.2015()
    expect_equal(nrow(data$numeric),  22150)
    expect_equal(length(unique(data$numeric$variable)),  23)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that(".grutters.2017", {
    skip_on_cran()
    data <- .grutters.2017()
    expect_equal(nrow(data$numeric),  451)
    expect_equal(length(unique(data$numeric$variable)),  11)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that('.hadfield.2013', {
    data <- .hadfield.2013()
    expect_equal(nrow(data$numeric), 5227 )
    expect_equal(length(unique(data$numeric$variable)), 3 )
    expect_equal(nrow(data$character), 1593 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.hebert.2016', {
    data <- .hebert.2016()
    expect_equal(nrow(data$numeric), 7966 )
    expect_equal(length(unique(data$numeric$variable)), 51 )
    expect_equal(nrow(data$character), 3098 )
    expect_equal(length(unique(data$character$variable)), 10 )
})

test_that('.hintze.2013', {
    data <- .hintze.2013()
    expect_equal(nrow(data$numeric), 41754 )
    expect_equal(length(unique(data$numeric$variable)), 20 )
    expect_equal(nrow(data$character), 12370 )
    expect_equal(length(unique(data$character$variable)), 5 )
})

test_that('.husak.2016', {
    data <- .husak.2016()
    expect_equal(nrow(data$numeric), 1875 )
    expect_equal(length(unique(data$numeric$variable)), 21 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.ingram.2016', {
    data <- .ingram.2016()
    expect_equal(nrow(data$numeric), 508 )
    expect_equal(length(unique(data$numeric$variable)), 2 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.jennings.2015', {
    data <- .jennings.2015()
    expect_equal(nrow(data$numeric), 27675 )
    expect_equal(length(unique(data$numeric$variable)), 5 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.jennings.2016 family', {
    data <- .jennings.2016a()
    expect_equal(nrow(data$numeric), 196 )
    expect_equal(length(unique(data$numeric$variable)), 5 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )

    data <- .jennings.2016b()
    expect_equal(nrow(data$numeric), 554 )
    expect_equal(length(unique(data$numeric$variable)), 8 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
    
    data <- .jennings.2016c()
    expect_equal(nrow(data$numeric), 396 )
    expect_equal(length(unique(data$numeric$variable)), 10 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.jones.2009', {
    data <- .jones.2009()
    expect_equal(nrow(data$numeric), 104306 )
    expect_equal(length(unique(data$numeric$variable)), 49 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.kamath.2016', {
    data <- .kamath.2016()
    expect_equal(nrow(data$numeric), 152 )
    expect_equal(length(unique(data$numeric$variable)), 4 )
    # Updating expected character data, to accomodate capture of sex (which is no longer captured as metadata)
    expect_equal(nrow(data$character), 38)
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.kamilar.2015', {
    data <- .kamilar.2015()
    expect_equal(nrow(data$numeric), 1533 )
    expect_equal(length(unique(data$numeric$variable)), 13 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.kefi.2016', {
    data <- .kefi.2016()
    # Updating expected numeric and character data, to reflect capture of 5 different variables as numeric
    expect_equal(nrow(data$numeric), 152)
    expect_equal(length(unique(data$numeric$variable)), 4)
    expect_equal(nrow(data$character), 38)
    expect_equal(length(unique(data$character$variable)), 1)
})

test_that('.kelt.2015', {
    data <- .kelt.2015()
    expect_equal(nrow(data$numeric), 558 )
    expect_equal(length(unique(data$numeric$variable)), 2 )
    expect_equal(nrow(data$character), 279 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.kissling.2014', {
    data <- .kissling.2014()
    expect_equal(nrow(data$numeric), 42693 )
    expect_equal(length(unique(data$numeric$variable)), 21 )
    expect_equal(nrow(data$character), 2033 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.klomp.2016', {
    data <- .klomp.2016()
    expect_equal(nrow(data$numeric), 151 )
    expect_equal(length(unique(data$numeric$variable)), 12 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.knief.2012', {
    data <- .knief.2012()
    expect_equal(nrow(data$numeric), 15568 )
    expect_equal(length(unique(data$numeric$variable)), 7 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.kolbe.2011', {
    data <- .kolbe.2011()
    expect_equal(nrow(data$numeric), 315 )
    expect_equal(length(unique(data$numeric$variable)), 15 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.kraft.2015 family', {
    data <- .kraft.2015a()
    expect_equal(nrow(data$numeric), 2324 )
    expect_equal(length(unique(data$numeric$variable)), 11 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )

    data <- .kraft.2015b()
    expect_equal(nrow(data$numeric), 1980 )
    expect_equal(length(unique(data$numeric$variable)), 15 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.kuo.2014', {
    data <- .kuo.2014()
    expect_equal(nrow(data$numeric), 280 )
    expect_equal(length(unique(data$numeric$variable)), 10 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.lagisz.2013', {
    data <- .lagisz.2013()
    expect_equal(nrow(data$numeric), 1111 )
    expect_equal(length(unique(data$numeric$variable)), 23 )
    expect_equal(nrow(data$character), 98 )
    expect_equal(length(unique(data$character$variable)), 2 )
})

test_that('.lawson.2015', {
    data <- .lawson.2015()
    expect_equal(nrow(data$numeric), 244 )
    expect_equal(length(unique(data$numeric$variable)), 3 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.lessard.2016', {
    data <- .lessard.2016()
    expect_equal(nrow(data$numeric), 1395 )
    expect_equal(length(unique(data$numeric$variable)), 5 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.limpens.2014 family', {
    data <- .limpens.2014a()
    expect_equal(nrow(data$numeric), 715 )
    expect_equal(length(unique(data$numeric$variable)), 11 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )

    data <- .limpens.2014b()
    expect_equal(nrow(data$numeric), 182)
    expect_equal(length(unique(data$numeric$variable)), 13)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0)
})

test_that('.lislevand.2006', {
    data <- .lislevand.2006()
    expect_equal(nrow(data$numeric), 284 )
    expect_equal(length(unique(data$numeric$variable)), 4 )
    expect_equal(nrow(data$character), 71 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.olli.2015', {
    data <- .olli.2015()
    expect_equal(nrow(data$numeric), 619 )
    expect_equal(length(unique(data$numeric$variable)), 1 )
    expect_equal(nrow(data$character), 6190 )
    expect_equal(length(unique(data$character$variable)), 10 )
})

test_that('.olson.2018', {
    data <- .olson.2018()
    expect_equal(nrow(data$numeric), 19572 )
    expect_equal(length(unique(data$numeric$variable)), 2 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.ord.2015', {
    data <- .ord.2015()
    expect_equal(nrow(data$numeric), NULL)
    expect_equal(length(unique(data$numeric$variable)), 0 )
    expect_equal(nrow(data$character), 96 )
    expect_equal(length(unique(data$character$variable)), 2 )
})

test_that('.lu.2016 family', {
    data <- .lu.2016a()
    expect_equal(nrow(data$numeric), 882 )
    expect_equal(length(unique(data$numeric$variable)), 3 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )

    data <- .lu.2016b()
    expect_equal(nrow(data$numeric), 3895 )
    expect_equal(length(unique(data$numeric$variable)), 3 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )

    data <- .lu.2016c()
    expect_equal(nrow(data$numeric), 61915 )
    expect_equal(length(unique(data$numeric$variable)), 6 )
    expect_equal(nrow(data$character), 10322 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.lupold.2013', {
    data <- .lupold.2013()
    expect_equal(nrow(data$numeric), 1048 )
    expect_equal(length(unique(data$numeric$variable)), 8 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.madin.2016', {
    data <- .madin.2016()
    expect_equal(nrow(data$numeric), 9390 )
    expect_equal(length(unique(data$numeric$variable)), 53 )
    expect_equal(nrow(data$character), 15190 )
    expect_equal(length(unique(data$character$variable)), 42 )
})

test_that('.martin.2014', {
    data <- .martin.2014()
    expect_equal(nrow(data$numeric), 480 )
    expect_equal(length(unique(data$numeric$variable)), 3 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.martin.2016', {
    data <- .martin.2016()
    expect_equal(nrow(data$numeric), 13248 )
    expect_equal(length(unique(data$numeric$variable)), 35 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.marx.2016', {
    data <- .marx.2016()
    expect_equal(nrow(data$numeric), 1129 )
    expect_equal(length(unique(data$numeric$variable)), 5 )
    expect_equal(nrow(data$character), 415 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.mccullough.2015', {
    data <- .mccullough.2015()
    expect_equal(nrow(data$numeric), 4842 )
    expect_equal(length(unique(data$numeric$variable)), 2 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.maire.2016', {
    data <- .maire.2016()
    expect_equal(nrow(data$numeric), 132515 )
    expect_equal(length(unique(data$numeric$variable)), 59 )
    expect_equal(nrow(data$character), 16576 )
    expect_equal(length(unique(data$character$variable)), 7 )
})

test_that('.mesquita.2016', {
    data <- .mesquita.2016()
    expect_equal(nrow(data$numeric), 4565 )
    expect_equal(length(unique(data$numeric$variable)), 8 )
    expect_equal(nrow(data$character), 1480 )
    expect_equal(length(unique(data$character$variable)), 2 )
})

test_that('.molinari.2014', {
    data <- .molinari.2014()
    expect_equal(nrow(data$numeric), 65)
    expect_equal(length(unique(data$numeric$variable)), 1)
    expect_equal(nrow(data$character), 325)
    expect_equal(length(unique(data$character$variable)), 5)
})

test_that('.myhrvold.2015', {
    data <- .myhrvold.2015()
    expect_equal(nrow(data$numeric), 113105 )
    expect_equal(length(unique(data$numeric$variable)), 29 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.munoz.2014', {
    data <- .munoz.2014()
    expect_equal(nrow(data$numeric), 1106 )
    expect_equal(length(unique(data$numeric$variable)), 1 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.nandy.2013', {
    data <- .nandy.2013()
    expect_equal(nrow(data$numeric), 81 )
    expect_equal(length(unique(data$numeric$variable)), 1 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.neuheimer.2016', {
    data <- .neuheimer.2016()
    expect_equal(nrow(data$numeric), 8916 )
    expect_equal(length(unique(data$numeric$variable)), 2 )
    expect_equal(nrow(data$character), )
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.olli.2015', {
    data <- .olli.2015()
    expect_equal(nrow(data$numeric), 619 )
    expect_equal(length(unique(data$numeric$variable)), 1 )
    expect_equal(nrow(data$character), 6190 )
    expect_equal(length(unique(data$character$variable)), 10 )
})

test_that('.paquette.2015', {
    expect_warning(data <- .paquette.2015(), "NAs introduced by coercion")
    expect_equal(nrow(data$numeric), 297 )
    expect_equal(length(unique(data$numeric$variable)), 5 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.pearse.2014', {
    data <- .pearse.2014()
    expect_equal(nrow(data$numeric), 7642 )
    expect_equal(length(unique(data$numeric$variable)), 3 )
    expect_equal(nrow(data$character), 2481 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.peel.2016', {
    data <- .peel.2016()
    expect_equal(nrow(data$numeric), 29671 )
    expect_equal(length(unique(data$numeric$variable)), 45 )
    expect_equal(nrow(data$character), 9851 )
    expect_equal(length(unique(data$character$variable)), 7 )
})

test_that('.perez.2014', {
    data <- .perez.2014()
    expect_equal(nrow(data$numeric), NULL)
    expect_equal(length(unique(data$numeric$variable)), 0 )
    expect_equal(nrow(data$character), 72 )
    expect_equal(length(unique(data$character$variable)), 4 )
})

test_that('.petry.2016', {
    data <- .petry.2016()
    expect_equal(nrow(data$numeric), 891 )
    expect_equal(length(unique(data$numeric$variable)), 16 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.philipson.2014', {
    data <- .philipson.2014()
    expect_equal(nrow(data$numeric), 43896 )
    expect_equal(length(unique(data$numeric$variable)), 6 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.philipson.2016', {
    data <- .philipson.2016()
    expect_equal(nrow(data$numeric), 24052 )
    expect_equal(length(unique(data$numeric$variable)), 4 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.pfautsch.2016', {
    expect_warning(data <- .pfautsch.2016(), "NAs introduced by coercion")
    expect_equal(nrow(data$numeric), 489362 )
    expect_equal(length(unique(data$numeric$variable)), 20 )
    expect_equal(nrow(data$character), 75348 )
    expect_equal(length(unique(data$character$variable)), 3 )
})

test_that('.pigot.2015', {
    data <- .pigot.2015()
    expect_equal(nrow(data$numeric), 1048)
    expect_equal(length(unique(data$numeric$variable)), 2)
    expect_equal(nrow(data$character), 9899)
    expect_equal(length(unique(data$character$variable)), 19)
})

test_that('.price.2014', {
    data <- .price.2014()
    expect_equal(nrow(data$numeric), 4193)
    expect_equal(length(unique(data$numeric$variable)), 11 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.plourde.2015', {
    data <- .plourde.2015()
    expect_equal(nrow(data$numeric), 24873 )
    expect_equal(length(unique(data$numeric$variable)), 27 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.rahman.2013', {
    data <- .rahman.2013()
    expect_equal(nrow(data$numeric), 935 )
    expect_equal(length(unique(data$numeric$variable)), 9 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

# Updating .anderson.2015 to .reese.2016 and moving to alphabetical location in file
test_that(".reese.2016", {
    skip_on_cran()
    data <- .reese.2016()
    expect_equal(nrow(data$numeric),  8387)
    expect_equal(length(unique(data$numeric$variable)),  39)
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)),  0)
})

test_that('.rojas.2013', {
    data <- .rojas.2013()
    expect_equal(nrow(data$numeric), 488 )
    expect_equal(length(unique(data$numeric$variable)), 8 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.rutschmann.2016', {
    data <- .rutschmann.2016()
    expect_equal(nrow(data$numeric), 2095 )
    expect_equal(length(unique(data$numeric$variable)), 2 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.sherratt.2013', {
    data <- .sherratt.2013()
    expect_equal(nrow(data$numeric), 2328 )
    expect_equal(length(unique(data$numeric$variable)), 3 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.sherratt.2018', {
    data <- .sherratt.2018()
    expect_equal(nrow(data$numeric), 231 )
    expect_equal(length(unique(data$numeric$variable)), 5 )
    expect_equal(nrow(data$character), 17 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.shibata.2016 family', {
    data <- .shibata.2016a()
    expect_equal(nrow(data$numeric), 1152 )
    expect_equal(length(unique(data$numeric$variable)), 12 )
    expect_equal(nrow(data$character), 96 )
    expect_equal(length(unique(data$character$variable)), 1 )
    
    data <- .shibata.2016b()
    expect_equal(nrow(data$numeric), 264)
    expect_equal(length(unique(data$numeric$variable)), 11)
    expect_equal(nrow(data$character), 72)
    expect_equal(length(unique(data$character$variable)), 3)
})

test_that('.simmons.2014', {
    data <- .simmons.2014()
    expect_equal(nrow(data$numeric), 480 )
    expect_equal(length(unique(data$numeric$variable)), 6 )
    expect_equal(nrow(data$character), 163)
    expect_equal(length(unique(data$character$variable)), 3 )
})

test_that('.simpson.2015', {
    data <- .simpson.2015()
    expect_equal(nrow(data$numeric), 1125 )
    expect_equal(length(unique(data$numeric$variable)), 7 )
    expect_equal(nrow(data$character), 633 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.stephens.2017', {
    data <- .stephens.2017()
    expect_equal(nrow(data$numeric), 6560 )
    expect_equal(length(unique(data$numeric$variable)), 4 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.tian.2016', {
    data <- .tian.2016()
    expect_equal(nrow(data$numeric), 99 )
    expect_equal(length(unique(data$numeric$variable)), 1 )
    expect_equal(nrow(data$character), 1098 )
    expect_equal(length(unique(data$character$variable)), 11 )
})

test_that('.vanier.2013', {
    data <- .vanier.2013()
    expect_equal(nrow(data$numeric), 7404 )
    expect_equal(length(unique(data$numeric$variable)), 2 )
    expect_equal(nrow(data$character), 7404 )
    expect_equal(length(unique(data$character$variable)), 2 )
})

test_that('.visser.2016', {
    data <- .visser.2016()
    expect_equal(nrow(data$numeric), 911798 )
    expect_equal(length(unique(data$numeric$variable)), 2 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.wilman.2014 family', {
    expect_warning(data <- .wilman.2014a(), "attributes are not identical across measure variables; they will be dropped")
    expect_equal(nrow(data$numeric), 229839 )
    expect_equal(length(unique(data$numeric$variable)), 23 )
    expect_equal(nrow(data$character), 129935 )
    expect_equal(length(unique(data$character$variable)), 13 )

    expect_warning(data <- .wilman.2014b(), "attributes are not identical across measure variables; they will be dropped")
    expect_equal(nrow(data$numeric), 81000 )
    expect_equal(length(unique(data$numeric$variable)), 15 )
    expect_equal(nrow(data$character), 48627 )
    expect_equal(length(unique(data$character$variable)), 9 )
})

test_that('.winchell.2016', {
    data <- .winchell.2016()
    expect_equal(nrow(data$numeric), 5380 )
    expect_equal(length(unique(data$numeric$variable)), 17 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test_that('.yin.2015', {
    data <- .yin.2015()
    expect_equal(nrow(data$numeric), 731 )
    expect_equal(length(unique(data$numeric$variable)), 9 )
    expect_equal(nrow(data$character), 82 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.zagar.2017', {
    data <- .zagar.2017()
    expect_equal(nrow(data$numeric), 1236 )
    expect_equal(length(unique(data$numeric$variable)), 12 )
    expect_equal(nrow(data$character), 103 )
    expect_equal(length(unique(data$character$variable)), 1 )
})

test_that('.zhang.2014', {
    expect_warning(data <- .zhang.2014(), "NAs introduced by coercion")
    expect_equal(nrow(data$numeric), 8852 )
    expect_equal(length(unique(data$numeric$variable)), 4 )
    expect_equal(nrow(data$character), NULL)
    expect_equal(length(unique(data$character$variable)), 0 )
})

test <- function(x){
    val <- x()
    cat("test_that('",deparse(substitute(x)),"', {\n", sep="")
    cat("    skip_on_cran()")
    cat("    data <- ",deparse(substitute(x)), "()\n", sep="")
    cat("    expect_equal(nrow(data$numeric),", nrow(val$numeric), ")\n")
    cat("    expect_equal(length(unique(data$numeric$variable)),", length(unique(val$numeric$variable)), ")\n")
    cat("    expect_equal(nrow(data$character),", nrow(val$character), ")\n")
    cat("    expect_equal(length(unique(data$character$variable)),", length(unique(val$character$variable)), ")\n")
    cat("})\n")
}
