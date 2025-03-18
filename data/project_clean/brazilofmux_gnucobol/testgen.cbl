identification division.
program-id. testgen.
environment division.
data division.
working-storage section.
    csvhandle       usage pointer.
    filename        pic x(40).
    buffer          pic x(8000).
    bool            usage signed-int.
    88  success             value 0.
    88  failure             value -1.
procedure division.
   -open.
    string 'generated.csv' x'00' delimited by size into filename.
    call 'csvgen_createfile' using
        by reference csvhandle
        by reference filename
        by value  
        returning bool.
    if success
        perform 004-genfile
        call 'csvgen_closefile' using
            by value csvhandle
            returning bool
        end-call
        if failure
            display 'Cannot close test.csv.'
        end-if
    else
        display 'Cannot create test.csv.'
    end-if.
    goback.
   -genfield1.
    string 'Foo' x'00' delimited by size into buffer.
    call 'csvgen_putfield' using
        by value     csvhandle
        by reference buffer
        returning bool
    end-call
    if failure
        display 'Cannot begin row.'
    end-if.
   -genfield2.
    string 'Bar,Baz' x'00' delimited by size into buffer.
    call 'csvgen_putfield' using
        by value     csvhandle
        by reference buffer
        returning bool
    end-call
    if failure
        display 'Cannot begin row.'
    end-if.
   -genline.
    call 'csvgen_beginrow' using
        by value     csvhandle
        returning bool
    end-call
    if failure
        display 'Cannot begin row.'
    end-if
    perform 002-genfield 
    perform 002-genfield 
    call 'csvgen_endrow' using
        by value     csvhandle
        returning bool
    end-call
    if failure
        display 'Cannot end row.'
    end-if.
   -genfile.
    perform 003-genline.
end program testgen.
