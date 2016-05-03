   program RGrib
     !ftn -c -I./ -I../lib/mpeu  -Mpreprocess -Mnorecursive -mp  -O3  m_string.f90
     !ftn -c -I./ -I../lib/mpeu  -Mpreprocess -Mnorecursive -mp  -O3  m_ReadGrib.f90
     !ftn -o teste.x teste.f90 -I../core/ -I../lib/mpeu/ -I../lib/w3lib ../core/m_ReadGrib.o ../core/m_string.o -L../lib/mpeu/ -L../lib/w3lib -lw3 -lmpeu
      use read_grib

      implicit none
      character(len=5), dimension(28) :: pds5
      integer, dimension(28) :: pds7
      integer :: i,j,iv,y

      type(grib) :: grb
      real, allocatable :: fld(:,:)


    pds5( 1:10) = (/'UMES','UMES','UMES','TEMP','TEMP','TEMP','TEMP','PSNM','AGPL','ZGEO'/)
    pds7( 1:10) = (/  925 ,  850 ,  500 ,  925 ,  850 ,  500 ,  250 ,  000 ,  000 ,  850 /)

    pds5(11:20) = (/'ZGEO','ZGEO','UVEL','UVEL','UVEL','VVEL','VVEL','VVEL','PREC','PRCV'/)
    pds7(11:20) = (/  500 ,  250 ,  850 ,  500 ,  250 ,  850 ,  500 ,  250 ,  000 ,  000 /)

    pds5(21:28) = (/'TEMS','Q02M','TP2M','USSL','UZRS','UZDS','U10M','V10M'/)
    pds7(21:28) = (/  000 ,  002 ,  002 ,  000 ,  000 ,  000 ,  010 ,  010 /)


       !grb%file='/scratchin/grupos/assim_dados/home/joao.gerd/NEW_MODELO_GLOBAL/pos/dataout/KUO/SSIB/21/LRun/2014/2014110200/GPOSERA20141102002014110200C.inz.TQ0062L028.grb'
       grb%file='/stornext/online6/assim_dados/eduardo.khamis/SCAMTEC/core/WRF_ams_09KM_2015010100+2015010100.grb2'


      call OpenGrib(grb)
      do i=1,20
       write(*,'(A,I3.1,AI8)')"grb%gds(",i,") = ",grb%gds( i)
      enddo

!      allocate(fld(grb%gds(2),grb%gds(3)))

!      do iv=1,28
!         call ReadGrib(grb,trim(adjustl(pds5(iv))),pds7(iv),fld)
!         print*,trim(adjustl(pds5(iv))),minval(fld),maxval(fld)
!      enddo

      call CloseGrib(grb)

   end program

