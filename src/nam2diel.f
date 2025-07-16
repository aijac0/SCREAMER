       Subroutine decodematerial (name,diel,flag)
c
c
c      This routine reads an ASCII field and gives back a real number.  This
c      number is used by the sw_model to determine the material type parameters.
c
c      written by Ken Struve
c      Jan. 12, 1995.
c
c
c Define passed variables
c
      character*(*) name
      real diel
      integer flag
c
c
c
       flag = 0
       if ((name.eq.'H2O').or.(name.eq.'h2o')) then
          diel = 1.0
       else if ((name.eq.'H20').or.(name.eq.'h20')) then
          diel = 1.0
       else if ((name.eq.'OIL').or.(name.eq.'oil')) then
          diel = 2.0
       else if ((name.eq.'SF6').or.(name.eq.'sf6')) then
          diel = 3.0
       else if ((name.eq.'AIR').or.(name.eq.'air')) then
          diel = 4.0
       else if ((name.eq.'HE').or.(name.eq.'He').or.(name.eq.'he')) then
          diel = 5.0
       else if ((name.eq.'H2').or.(name.eq.'h2')) then
          diel = 6.0
       else
          diel = 0.0
          flag = 1
       end if
c
       return
       end

