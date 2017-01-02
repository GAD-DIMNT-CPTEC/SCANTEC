   program RGrib
      use read_grib

      implicit none

      type(grib) :: grb
      real, allocatable :: fld(:,:)

      grb%file="GPOSCPT20130101122013010118P.fct.TQ0299L064.grb"


      call OpenGrib(grb)

      allocate(fld(grb%gds(2),grb%gds(3)))
   
      call ReadGrib(grb,'UVEL',1000,fld)
      print*,minval(fld),maxval(fld)

      call ReadGrib(grb,'PSLC',0000,fld)
      print*,minval(fld),maxval(fld)

      call CloseGrib(grb)

   end program

