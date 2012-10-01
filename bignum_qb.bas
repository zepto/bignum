' Finds all the digits of 2 ^ 100.
' Copyright (C) 2010  Josiah Gordon <josiahg@gmail.com>
'
' This program is free software: you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation, either version 3 of the License, or
' (at your option) any later version.
'
' This program is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.
'
' You should have received a copy of the GNU General Public License
' along with this program.  If not, see <http://www.gnu.org/licenses/>.

' To compile with freebasic, use the command fbc bignum.bas -lang qb.

' Function declarations
declare function add$ (a as string, b as string)
declare function multi$ (a as string, b as string)
declare function pow$ (x as long, n as long)
declare function fib$ (n as long)
declare function fact$ (n as long)

' Test all the functions by un-commenting the correct line.

' This is why the program was written.
print pow$(2, 100)
    
'print add$("12323423234234234234234234234440234", "1233")
'print add$("145224", "144301")
'print multi$("999", "990")
'print pow$(22, 44)
'print fib$(20000)
'print fact$(200)
'print add$("99", "99")

function add$(a as string, b as string)
    ' add$(a$, b$) -> Takes two strings as arguments.  Returns a string
    ' containing all the digits of the sum of its two arguments.

    dim c as string
    dim lf as string
    dim cm as integer
    dim m as integer
    dim am as integer
    dim i as integer

    ' Swap the two arguments so the longer one is in the top number when added.
    if len(a) < len(b) then
        c = a
        a = b
        b = c
    endif

    ' The carry digit is initialized to zero.
    cm = 0

    ' The return string is initially empty.
    lf = ""

    ' Loop through the larger of the two arguments.
    for i = 0 to len(a) - 1
        ' Make sure there are some digits left in the bottom number.
        if b <> "" then
            ' The temporary number m is set to the sum of the last two digits
            ' in the two argumnts and the carry number.
            m = (val(right$(a, 1)) + val(right$(b, 1))) + cm
            
            ' Remove the last digit from each number.
            a = left$(a, len(a) - 1)
            b = left$(b, len(b) - 1)
        else
            ' There are no digits in the bottom number so add the cary number
            ' to the next top digit if one exists otherwise prepend the rest
            ' of the top digits to the return string and exit the loop.
            if cm > 0 then
                m = val(right$(a, 1)) + cm
                a = left$(a, len(a) - 1)
            else
                lf = ltrim$(rtrim$(a))+ltrim$(rtrim$(lf))
                exit for
            endif
        endif
        ' Reset the carry number after it has been used.
        cm = 0
        if m >= 10 then
            ' Take the tens position digit and store it.
            cm = int(m / 10)
            ' Store the remainder to be prepended to the return string.
            am = m mod 10
        else
            ' The sum was less then ten so just prepend it to the return 
            ' string.
            am = m
        endif
        ' Place the next digit at the start of the return string.
        lf = ltrim$(rtrim$(str$(am)))+ltrim$(rtrim$(lf))
    next

    ' If there was a carry number left place it at the start of the return 
    ' string.
    if cm > 0 then
        lf = ltrim$(rtrim$(str$(cm)))+ltrim$(rtrim$(lf))
    endif

    ' Return the sum.
    add$ = ltrim$(rtrim$(lf))
end function

function multi$(a as string, b as string)
    ' multi$(a$, b$) -> Takes to strings as arguments and returns all the 
    ' digits of their product.

    dim c as string
    dim ef as string
    dim ec as string
    dim ib as string
    dim ia as string
    dim lfb as string
    dim m as integer
    dim am as integer
    dim cm as integer
    dim i as integer
    dim i2 as integer

    ' Swap the arguments so the longer is on the bottom in the equation.
    if len(a) < len(b) then
        c = a
        a = b
        b = c
    end if

    ' String spaces from the beginings and ends of the arguments.
    a = ltrim$(rtrim$(a))
    b = ltrim$(rtrim$(b))

    ' Set the return string to empty.
    ef = ""

    ' Outer loop loops through the top number.
    for i = 0 to len(b) - 1
        ' Grab the next digit (from the right) in the top number.
        ib = mid$(b, (len(b) - i), 1)
        ' Reset the temporary string to empty.
        lfb = ""
        ' Reset the carry number to zero.
        cm = 0
        ' The inner loop loops through the bottom number.
        for i2 = len(a) to 1 step -1
            ' Grab the next digit (from the right) in the bottom number.
            ia = mid$(a, i2, 1)
            ' Multiply the next two digits and add the carry number to them.
            m = (val(ia) * val(ib)) + cm
            ' Reset the carry number after it is used.
            cm = 0
            if m >= 10 then
                ' The product was larger than 9 so store the tens place digit
                ' to be added to the next product.
                cm = int(m / 10)
                ' Take the ones place digit to be placed at the start of the 
                ' temporary string.
                am = m mod 10
            else
                ' The product was less than 10 so place it at the start of the
                ' temporary string.
                am = m
            endif
            ' Build the temporary string.
            lfb = ltrim$(rtrim$(str$(am))) + ltrim$(rtrim$(lfb))
        next i2
        
        ' If there is a remaining carry digit place it at the start of the
        ' temporary string before adding it to the previous product.
        if cm > 0 then
            lfb = ltrim$(rtrim$(str$(cm))) + ltrim$(rtrim$(lfb))
        endif
        ' Check if this is the first time through the loop.
        if ef <> "" then
            ' Move the last i digits from the sum to ec to be tacked on the 
            ' end after the next sum.
            ec = ltrim$(rtrim$(mid$(ef, (len(ef) - i) + 1, i)))
            ef = ltrim$(rtrim$(mid$(ef, 1, (len(ef) - i))))
            ' Add the current temporary string to the sum of the last, and
            ' append the droped digits on the end.
            ef = add$(lfb, ef) + ec
        else
            ' First time through the loop just store the temporary string
            ' in the return string.
            ef = lfb
        endif
    next i
    
    ' Return all the digits of the product of the two arguments.
    multi$ = ltrim$(rtrim$(ef))
end function

function pow$(x as long, n as long)
    ' pow$(x, n) -> Given the two integers x and n, return a string
    ' containing all the digits of x raised to the nth power.
    
    dim sx as string
    dim ix as string
    dim i as long

    ' Keep a trimmed string of x.
    sx = ltrim$(rtrim$(str$(x)))
    ' Keep the initial value of x.
    ix = sx 
    for i = 1 to n - 1
        ' Multiply the last product by the initial value of x.
        sx = multi$(sx, ix)
    next

    ' Return all the digits of x ^ n.
    pow$ = sx
end function

function fib$(n as long)
    ' fib$(n) -> Given an integer n, return a string containing all the digits
    ' of the nth fibonacci number.

    dim a as string
    dim b as string
    dim c as string
    dim i as long

    ' Initialize a and b to the first two fibonacci numbers.
    a = "0"
    b = "1"
    ' c is a temporary variable used to store the previous value of a.
    c = a
    ' Loop n - 1 times.
    for i = 1 to (n - 1)
        ' a equals the last number in the sequence.
        a = b
        ' b equals the next.
        b = add$(c, b)
        ' c holds a temporaraly so a can hold the last.
        c = a
    next
    
    ' Return all the digits of the nth fibonacci number.
    fib$ = b
end function

function fact$(n as long)
    ' fact$(n) -> Given an integer n return a string containing all the digits
    ' of the factorial of n.
    
    dim t as string
    dim i as long

    ' Create a trimmed string of n to be used in the multiplication.
    t = ltrim$(rtrim$(str$(n)))
    ' Loop n -1 times
    for i = 1 to n - 1
        ' Multiply the previous factorial by i.
        t = multi$(t, ltrim$(rtrim$(str$(i))))
    next

    ' Return a string containing all the digis of the factorial of n.
    fact$ = t
end function
