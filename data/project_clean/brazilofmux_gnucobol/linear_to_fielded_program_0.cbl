identification division.
function-id. linear_to_gregorian.
environment division.
configuration section.
repository.
    function isleapyear
    function floor-divmod
    function gregorian_to_linear
    function all intrinsic.
data division.
working-storage section.
    ltg-d0                pic s9(8) comp-5.
    divmod-400.
    05  ltg-n400          pic s9(8) comp-5.
    05  ltg-d1            pic s9(8) comp-5.
    divmod-100.
    05  ltg-n100          pic s9(8) comp-5.
    05  ltg-d2            pic s9(8) comp-5.
    divmod-4.
    05  ltg-n4            pic s9(8) comp-5.
    05  ltg-d3            pic s9(8) comp-5.
    divmod-1.
    05  ltg-n1            pic s9(8) comp-5.
    05  ltg-d4            pic s9(8) comp-5.
    ltg-jan01         pic s9(8) comp-5.
    ltg-mar01         pic s9(8) comp-5.
    ltg-correction    pic 9     comp-5.
    ltg-prior-days    pic s9(8) comp-5.
    ltg-temp          pic s9(8) comp-5.
    ltg-1st           pic s9(8) comp-5.
    ltg-cache-year    pic s9(5) comp-5  value -27257.
    ltg-cache-jan01   pic s9(8) comp-5.
    ltg-cache-mar01   pic s9(8) comp-5.
    c146097      pic s9(8) comp-5 value 146097.
    c36524       pic s9(8) comp-5 value 36524.
    c1461        pic s9(8) comp-5 value 1461.
    c365         pic s9(8) comp-5 value 365.
    c7           pic s9(8) comp-5 value 7.
    divmod.
    05  fdm-div pic s9(8) comp-5.
    05  fdm-mod pic s9(8) comp-5.
linkage section.
    ltg-linear        pic s9(8) comp-5.
    ltg-fielded.
    05  ltg-year          pic s9(5) comp-5.
    05  ltg-month         pic 99    comp-5.
    05  ltg-day-of-month  pic 99    comp-5.
    05  ltg-day-of-year   pic 999   comp-5.
    05  ltg-day-of-week   pic 9     comp-5.
procedure division using ltg-linear returning ltg-fielded.
    -main.
    subtract 1 from ltg-linear giving ltg-d0.
    move floor-divmod(ltg-d0, c146097) to divmod-400.
    move floor-divmod(ltg-d1, c36524) to divmod-100.
    move floor-divmod(ltg-d2, c1461) to divmod-4.
    move floor-divmod(ltg-d3, c365) to divmod-1.
    compute ltg-year = 400 * ltg-n400 + 100 * ltg-n   
                     + 4 * ltg-n4 + ltg-n1.
    if (ltg-n100 is not equal 4) and (ltg-n1 is not equal 4)
        add 1 to ltg-year
    end-if.
    move 1 to ltg-day-of-month.
    if ltg-year is equal to ltg-cache-year
        move ltg-cache-jan01 to ltg-jan  
        move ltg-cache-mar01 to ltg-mar  
    else
        move 1 to ltg-month
        move gregorian_to_linear(ltg-year, ltg-month, ltg-day-of-month) to ltg-jan  
        move 3 to ltg-month
        move gregorian_to_linear(ltg-year, ltg-month, ltg-day-of-month) to ltg-mar  
        move ltg-year  to ltg-cache-year
        move ltg-jan01 to ltg-cache-jan  
        move ltg-mar01 to ltg-cache-mar  
    end-if.
    if (ltg-linear  < ltg-mar01)
        move 0 to ltg-correction
    else
        if isleapyear(ltg-year) = 'Y'
            move 1 to ltg-correction
        else
            move 2 to ltg-correction
        end-if
    end-if.
    subtract ltg-jan01 from ltg-linear giving ltg-prior-days.
    add 1 to ltg-prior-days giving ltg-day-of-year.
    compute ltg-temp =
        (12 * (ltg-prior-days + ltg-correction) + 373) / 367.
    move ltg-temp to ltg-month.
    move gregorian_to_linear(ltg-year, ltg-month, ltg-day-of-month) to ltg-1st.
    compute ltg-day-of-month = ltg-linear - ltg-1st + 1.
    move floor-divmod(ltg-linear, c7) to divmod.
    move fdm-mod to ltg-day-of-week.
    goback.
end function linear_to_gregorian.
