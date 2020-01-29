#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Public domain 2013 Wesley Ebisuzaki 
 *
 *
 * call grb_rd(file,invfile,inv_in,grep_option,data,nmax,n,inv_out,ierr)
 *
 * a fortran callable routine to read a grib2 file using grep and wgrib2
 *
 * INPUT:
 *   character*(*) file          name of grib2 file
 *   character*(*) invfile       name of grib2 inventory file, produced by wgrib2 file > file.inv
 *                               the invfile is used as an index file
 *   character*(*) inv_in        search string to find record
 *   character*(*) grep_option   option used by grep ('-E', '-F', or '')
 *   int nmax                    maximum size data(nmax)
 *
 * OUTPUT:
 *   real data(nmax)             grid point data array
 *   int n                       number of grid points used
 *   character*(*)               inventory of grib message read
 *   int ierr                    error codes, 0 = no error
 *
 * comments:
 *   the data is in WE:SN order
 *   routines uses popen() which is part of POSIX.1-2001
 *
 * customization:
 *    INT4 must refer to a 4-byte integer type
 *    the fortan calling name/sequence is compiler dependent
 *
 * file names must not terminate in a space character
 *
 * example
 *
 * real :: d(10000)
 * nmax = 10000
 *
 * ! a.grb2 is grib2 file
 * ! made inv file by $ wgrib2 a.grb2 >a.inv
 *
 * call grb_rd('a.grb2','a.inv' ":UGRD:100 mb:", "", data, nmax, ndata, inv_out,ierr)
 *
 */

#define STRING_SIZE 300
#define INT4	int

static void trim_fort(char *cstring, int clen, char *fstring, int flen);


void grb_rd_(char *file, char *file_inv, char *search, char *grep_option, float *data, int *nmax, int *n, 
    char *inv_out, int *ierr, int filelen, int file_invlen, int searchlen, int grep_optionlen, int inv_outlen) {

    char grb_file[STRING_SIZE];
    char inv_file[STRING_SIZE];
    char search_str[STRING_SIZE];
    char grep_opn[STRING_SIZE];
    char wgrib2_cmd[STRING_SIZE];
    char invout[STRING_SIZE];
    FILE *pfile;
    int i, j;
    INT4 len1, len2;

    *n = *ierr = 0;

    trim_fort(grb_file, STRING_SIZE, file, filelen);
    trim_fort(inv_file, STRING_SIZE, file_inv, file_invlen);
    trim_fort(search_str, STRING_SIZE, search, searchlen);
    trim_fort(grep_opn, STRING_SIZE, grep_option, grep_optionlen);

    i = snprintf(wgrib2_cmd, STRING_SIZE, "grep %s \"%s\" \"%s\" | /opt/grads/2.0.a9/bin/wgrib2 \"%s\" -i -s -nl -bin -\n", grep_opn, search_str, inv_file, grb_file);

    if (i <= 0) { *ierr=1; return; }
    if (i >= STRING_SIZE) { *ierr=2; return; }

    pfile = popen(wgrib2_cmd,"r");
    if (pfile == NULL) { *ierr = 3; return; }
    /*printf("wgrib2_cmd=%s\n",wgrib2_cmd);*/

    fgets(invout, STRING_SIZE, pfile);

    invout[STRING_SIZE-1] = 0;
    j = strlen(invout);
    memcpy(inv_out, invout, j);
    for (i = j; i < inv_outlen; i++) inv_out[i] = ' ';

    i = fread(&len1, 4, 1, pfile);
    if (i != 1) { *ierr = 4; pclose(pfile); return; }
    j = len1/4;
    if (j > *nmax) { 
	fprintf(stderr,"grb_rd, too many grid points to read %d allocated %d\n", j, *nmax);
	*ierr = 5;
	pclose(pfile);
	return;
    }
    i = fread(data, 4, j, pfile);
    if (i != j) {
	fprintf(stderr,"grb_rd, read %d points wanted %d\n", i, j);
	*ierr = 6;
	pclose(pfile);
	return;
    }
    i = fread(&len2, 4, 1, pfile);
    if (i != 1 || len1 != len2) { *ierr = 7; pclose(pfile); return; } 

    *n = len1/4;   
    pclose(pfile);
    return;
}


/* convert a fortran string to a C string with trim of trailing blanks */

static void trim_fort(char *cstring, int clen, char *fstring, int flen) {
    int len, i;

    len = clen-1 < flen ? clen-1 : flen;
    memcpy(cstring, fstring, len);
    cstring[len] = 0;

    i = strlen(cstring);
    while (i && cstring[i-1] == ' ') {
        cstring[--i] = 0;
    }

    return;
}


