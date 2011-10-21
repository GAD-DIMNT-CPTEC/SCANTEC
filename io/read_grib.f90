PROGRAM read_grib	
	IMPLICIT NONE	
	integer :: gbret,jret,jpds(200),jgds(200),gridDesc(200),kpds(200)
	INTEGER :: iv, lugb, iret
	integer, dimension(44) :: pds5, pds7
	real :: lubi,kf,k
	integer :: saldas,c,r,j
	LOGICAL, DIMENSION(10512) :: lb
	
	REAL, DIMENSION(10368,44) :: f
	
	CHARACTER(47) :: name
	
   DO IV=1, 44
	lugb = iv
        lubi = 0 
        j = 0 
	saldas=10512
	pds5 = (/ 132,081,135,192,033,194,034,039,035,007,002,188,011,226,052,051,054,083,187,182,183,184,128,199,130,131,061,063,064,178,179,122,121,057,193,195,071,207,211,114,209,212,214,191/) !parameter
  	pds7 = (/ 000,000,000,000,100,000,100,100,100,100,000,000,100,000,100,100,000,000,000,000,000,000,002,000,010,010,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000,000/) !htlev2
        
        jpds = -1
        jpds(5) = pds5(iv)
        jpds(7) = pds7(iv)
	
	name="GPOSNMC20040101002004010106P.fct.TQ0062L028.grb"
	
        call baopenr(lugb,name,iret) 
	call getgb(lugb,lubi,saldas,j,jpds,jgds,kf,k,kpds,gridDesc,lb,f(:,iv),gbret)
	call baclose(lugb,jret)
   ENDDO
	
	print*,f(:,43)
	print*,'Numero do ERRO',gbret
	
END PROGRAM read_grib
