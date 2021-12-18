# A function that takes user inputs for correction methods for
# expresso (affy). Default values can be missing, in which case the
# first element will be chosen as the default.

expressoWidget <- function(BGMethods, normMethods, PMMethods, expMethods,
                           BGDefault, normDefault, PMDefault, expDefault){
    methodList <- list()
    END <- FALSE

    if(any(missing(BGMethods), missing(normMethods),
           missing(PMMethods), missing(expMethods))){
        stop("At least one of the method arguments is missing")
    }
    if(any(c(length(BGMethods), length(normMethods),
             length(PMMethods), length(expMethods)) == 0)){
        stop("At least one of the method argument is of length 1")
    }

    if(missing(BGDefault)){
        BGM <- tcltk::tclVar(BGMethods[1])
    }else{
        BGM <- tcltk::tclVar(BGDefault)
    }
    if(missing(normDefault)){
        NMM <- tcltk::tclVar(normMethods[1])
    }else{
        NMM <- tcltk::tclVar(normDefault)
    }
    if(missing(PMDefault)){
        PMM <- tcltk::tclVar(PMMethods[1])
    }else{
        PMM <- tcltk::tclVar(PMDefault)
    }
    if(missing(expDefault)){
        EXM <- tcltk::tclVar(expMethods[1])
    }else{
        EXM <- tcltk::tclVar(expDefault)
    }


    quit <- function(){
        tcltk::tkdestroy(base)
    }
    end <- function(){
        END <<- TRUE
        methodList[["BG"]] <<- tcltk::tclvalue(BGM)
        methodList[["NORM"]] <<- tcltk::tclvalue(NMM)
        methodList[["PM"]] <<- tcltk::tclvalue(PMM)
        methodList[["EXP"]] <<- tcltk::tclvalue(EXM)
        quit()
    }

    base <- tcltk::tktoplevel()
    ## post -- hook
    on.exit(tcltk::tkdestroy(base))

    tcltk::tktitle(base) <- "Expresso methods selection"
    ## Description text
    tcltk::tkpack(tcltk::tklabel(base, text = "Welcome to Expresso methods selection"),
           expand = FALSE, fill = "x", padx = 5, pady = 5)
    tcltk::tkpack(tcltk::tklabel(base, text = paste("You need to choose correction",
                         "methods or go with the defaults")),
           expand = FALSE, fill = "x", padx = 5)

    ## Selections for correction methods
    methodFrame <- tcltk::tkframe(base)
    ## Background selection
    BGLabel <- tcltk::tklabel(methodFrame, text = "Background correction")
    BGDropdown <- tcltk::tkframe(methodFrame)
    widgetTools::dropdownList(BGDropdown, BGMethods, BGM, 20,
                                      tcltk::tclvalue(BGM), TRUE)
    tcltk::tkgrid(BGLabel, BGDropdown)
    tcltk::tkgrid.configure(BGLabel, sticky = "e")
    tcltk::tkgrid.configure(BGDropdown, sticky = "w")

    ## Normlization
    NMLabel <- tcltk::tklabel(methodFrame, text = "Normalization")
    NMDropdown <- tcltk::tkframe(methodFrame)
    widgetTools::dropdownList(NMDropdown,normMethods, NMM, 20,
                                      tcltk::tclvalue(NMM), TRUE)
    tcltk::tkgrid(NMLabel, NMDropdown)
    tcltk::tkgrid.configure(NMLabel, sticky = "e")
    tcltk::tkgrid.configure(NMDropdown, sticky = "w")

    ## PM correction
    PMLabel <- tcltk::tklabel(methodFrame, text = "PM correction")
    PMDropdown <- tcltk::tkframe(methodFrame)
    widgetTools::dropdownList(PMDropdown, PMMethods, PMM, 20,
                                      tcltk::tclvalue(PMM), TRUE)
    tcltk::tkgrid(PMLabel, PMDropdown)
    tcltk::tkgrid.configure(PMLabel, sticky = "e")
    tcltk::tkgrid.configure(PMDropdown, sticky = "w")

    ## PM correction
    EXLabel <- tcltk::tklabel(methodFrame, text = "Expression")
    EXDropdown <- tcltk::tkframe(methodFrame)
    widgetTools::dropdownList(EXDropdown, expMethods, EXM, 20,
                                      tcltk::tclvalue(EXM), TRUE)
    tcltk::tkgrid(EXLabel, EXDropdown)
    tcltk::tkgrid.configure(EXLabel, sticky = "e")
    tcltk::tkgrid.configure(EXDropdown, sticky = "w")

    tcltk::tkpack(methodFrame, expand = TRUE, fill = "both", padx = 5,
           pady = 10)

    butFrame <- tcltk::tkframe(base)
    quitBut <- tcltk::tkbutton(butFrame, text = "Quit", width = 7, command = quit)
    endBut <- tcltk::tkbutton(butFrame, text = "Select", width = 7, command = end)
    tcltk::tkgrid(quitBut, endBut, padx = 5)
    tcltk::tkpack(butFrame, expand = FALSE, fill = "x", pady = 5)

    tcltk::tkwait.window(base)

    if(END){
        return(methodList)
    }else{
        return(NULL)
    }
}
