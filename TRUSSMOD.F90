!     Last change:  JBM  10 Dec 96   10:04 pm
module fink
implicit none
contains


! This subroutine will input the values for width and height.
subroutine input1(width,height)
implicit none
REAL, INTENT(out):: width,height
WRITE(*,*)'You are using the first portion of the truss program...'
WRITE(*,*)'-------------------------------------------------------'
WRITE(*,*)'Please enter the desired width of the truss:  '
READ(*,*) width
WRITE(*,*)'Please enter the desired height of the truss:  '
READ(*,*) height
return
end subroutine input1


! This subroutine will input the values for the vertical forces P1-P5.
subroutine input2(p1,p2,p3,p4)
implicit none
REAL,INTENT(OUT)::p1,p2,p3,p4
WRITE(*,*)'You are now dealing with the second part of the truss program...'
WRITE(*,*)'----------------------------------------------------------------'
WRITE(*,*)'Please enter the value for the force P1 in KiloNewtons:  '
READ(*,*)p1
WRITE(*,*)'Please enter the value for the force P2 in KiloNewtons:  '
READ(*,*)p2
WRITE(*,*)'Please enter the value for the force P3 in KiloNewtons:  '
READ(*,*)p3
WRITE(*,*)'Please enter the value for the force P4 in KiloNewtons:  '
READ(*,*)p4
return
end subroutine input2


! This subroutine deals with the calculation of lengths and angles based on
! the values entered by the user for width and height.  Once these values are
! calculated, each angle and length will be assigned a position in a two
! dimensional array.
subroutine calc1(width,height,values,UV,a1,a2,a4)
implicit none
REAL,INTENT(IN)::width,height
REAL,INTENT(OUT)::UV,a1,a2,a4
REAL::VW,TW,UW,TU,WX,a3,a5,a6
REAL,DIMENSION(:,:),intent(IN out)::values
REAL::tv,pi
!calculate angles and segments based on width and height...
pi= ACOS(-1.0)
wx= (1./3.)*width
vw= (1./3.)*width
tv= SQRT(((width/2.)**2.)+(height**2.))
a1= (atan(height/(0.5*width)))*(360./(2.*pi))
a2= (pi-a1-(pi/2.))*(360./(2.*pi))
uv= SIN(a2)*vw
tu= tv-uv
uw= SQRT((vw**2.)-(uv**2.))
tw= SQRT(((width/6.)**2.)+(height**2.))
a3= (ATAN(tu/uw))*(360./(2.*pi))
a4= (pi-(a2+a3))*(360./(2.*pi))
a5= (pi-(pi/2.)-a3)*(360./(2.*pi))
a6= (pi-(a4*2.))*(360./(2.*pi))
!assign angles and segments to the array...
values(1,2)=tu
values(2,2)=uv
values(3,2)=vw
values(4,2)=wx
values(5,2)=uw
values(6,2)=tw
values(1,1)=a1
values(2,1)=a2
values(3,1)=a3
values(4,1)=a4
values(5,1)=a5
values(6,1)=a6
return
end subroutine calc1


! Subroutine Calc2 will calculate the forces acting on the specified segments
! of the truss.
subroutine calc2(width,UV,a1,a2,a4,p1,p2,p3,p4,FUV,FVW,FTW,FUW,FTU,FWX)
implicit none
REAL,INTENT(IN)::width,UV,a1,a2,a4,p1,p2,p3,p4
REAL,INTENT(OUT)::FUV,FVW,FTW,FUW,FTU,FWX
REAL::Rv

Rv=((p1*width)+p2*(width-(UV*COS(a1)))+(p3*width)/2+(p4*UV*COS(a1)))/width
FUV= (p1-Rv)/SIN(a1)
FVW= -FUV*COS(a1)
FTU= (FUV*COS(a1)-FUW*COS(a2))/COS(a1)
FUW= ((FTU-FUV)*SIN(a1)-p2)/SIN(a2)
FTW= (-FUW*SIN(a2))/SIN(a4)
FWX= FVW+(FUW*SIN(a2))-(FTW*COS(a4))
return
end subroutine calc2


! This subroutine will output the lengths and angles in a two dimensional
! array.  It will also output everything on the screen to a file called
! 'fink.out'.
subroutine output1(values)
implicit none
REAL,DIMENSION(:,:),INTENT(IN):: values
INTEGER::r
WRITE(*,*)'Here are the values for the angles and the truss segments:  '
! This do loop will write each element in the two dimensional array both to
! the screen and to the file 'fink.out'...
do r=1,6
   WRITE(*,'(tr5,f6.0,a,tr5,f6.2,a)')values(r,1),'deg',values(r,2),'meters'
   WRITE(23,'(tr5,f6.0,tr5,f6.2)')values(r,1),values(r,2)
end do
return
end subroutine output1


! Subroutine output2 will output the six forces calculated in engineering
! notation.  It will also output to the file 'fink.out'.
subroutine output2(FUV,FVW,FTW,FUW,FTU,FWX)
implicit none
REAL,INTENT(IN)::FUV,FVW,FTW,FUW,FTU,FWX
! Output to screen...
WRITE(*,*)'Here are the forces on the segments:  '
WRITE(*,'(a,e10.3,a)')'FUV= ',FUV,'KN'
WRITE(*,'(a,e10.3,a)')'FVW= ',FVW,'KN'
WRITE(*,'(a,e10.3,a)')'FTW= ',FTW,'KN'
WRITE(*,'(a,e10.3,a)')'FUW= ',FUW,'KN'
WRITE(*,'(a,e10.3,a)')'FTU= ',FTU,'KN'
WRITE(*,'(a,e10.3,a)')'FWX= ',FWX,'KN'
! Output to file 'fink.out'...
WRITE(23,*)'Here are the forces on the segments:  '
WRITE(23,'(e10.3)')FUV
WRITE(23,'(e10.3)')FVW
WRITE(23,'(e10.3)')FTW
WRITE(23,'(e10.3)')FUW
WRITE(23,'(e10.3)')FTU
WRITE(23,'(e10.3)')FWX
return
end subroutine output2

end module fink
