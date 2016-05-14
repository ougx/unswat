SUBROUTINE caps(file_name)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the input and output names given in file.cio
!!    and converts all capital letters to lowercase letters.

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    file_name   |NA        |dummy argument, file name character string
!!    ii          |none      |counter in loop
!!    j           |none      |counter in loop
!!    low_case    |NA        |string containing all the lowercase letters
!!    temp_name   |NA        |string with all capitals switched to lowercase
!!    up_case     |NA        |string containing all the uppercase letters
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Len, Index, AdjustL

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


INTRINSIC LEN, INDEX, adjustl


CHARACTER (LEN=*), INTENT(INOUT)         :: file_name

CHARACTER (LEN=LEN(file_name)) ::  temp_name
CHARACTER (LEN=26) :: low_case = "abcdefghijklmnopqrstuvwxyz",  &
    up_case = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
INTEGER :: ii, j

temp_name = ""
j = 0

temp_name = file_name

DO ii = 1, LEN(file_name)
  j = INDEX (up_case,file_name(ii:ii))
  IF (j /= 0) temp_name(ii:ii) = low_case(j:j)
END DO

temp_name = adjustl (temp_name)     !moves leading blanks to right end

file_name = temp_name

RETURN
END SUBROUTINE caps
