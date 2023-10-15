!     Last change:  JBM  10 Dec 96   10:04 pm
PROGRAM Fink_Truss

!Module with external subroutines (no functions)
USE fink
implicit none

!  EF 1005-12m             12-11-96             Mills, Jason B.

! This program uses subroutines that will output to the screen and a file
! the six angles and six member lengths in a two dimensional array given any
! height and width for input. The file to be output to is called fink.out.
! The output will be in two formatted columns of numbers in a user friendly
! format with units of angles in degrees to the nearest degree and lengths
! in meters to the nearest cm. The P1-P5 represent the values of the forces 
! placed upon the joints of the truss. To make things faster for me to write,
! certain names of unknowns will be shortened to the following variables:

! Angle wvu = A1     Length of segment tu = tu     Force on TU = FTU
! Angle vwu = A2     Length of segment uv = uv     Force on UV = FUV
! Angle uwt = A3     Length of segment vw = vw     Force on VW = FVW
! Angle twx = A4     Length of segment wx = wx     Force on WX = FWX
! Angle utw = A5     Length of segment uw = uw     Force on UW = FUW
! Angle wtx = A6     Length of segment tw = tw     Force on TW = FTW

! The two dimensional array containing the angles and truss segments to be
! output from the first portion of this program is called values.

REAL::width,height,p1,p2,p3,p4,a1,a2,a4,UV
REAL::FUV,FVW,FTW,FUW,FTU,FWX
REAL,DIMENSION(6,2):: values=0.0

OPEN(UNIT=23,FILE='fink.out') !Output file

! Three subroutines that will execute the calculation of the lengths of the
! truss segments and the angles specified in the project description...
call input1(width,height)
call calc1(width,height,values,UV,a1,a2,a4)
call output1(values)

! Three subroutines that will execute the calculation of the forces on the
! joints specified in the project description...
call input2(p1,p2,p3,p4)
call calc2(width,UV,a1,a2,a4,p1,p2,p3,p4,FUV,FVW,FTW,FUW,FTU,FWX)
call output2(FUV,FVW,FTW,FUW,FTU,FWX)

stop
END PROGRAM Fink_Truss
