/*
 * Tcl interface to tamet
 * PdeV 4/01
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifdef USE_DMALLOC
#include <dmalloc.h>
#endif

#include <tcl.h>
#include "date.h"

// Globals
Tcl_Interp   *Interp;

// Fortran externals.
/* regarding trailing underscores: (from g77 manual)
 * ...appends two underscores to
 * names with underscores and one underscore to external names with
 * no underscores.
 */
extern "C" int tamet2_zero_variables__();
extern "C" int tamet2_zero_daily_variables__();
extern "C" int tamet2_setup_links__();
extern "C" int tamet2_get_other_variables__();
extern "C" int tamet2_main__();

int zero_variables(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST objv[]);
int daily_main(ClientData, Tcl_Interp *, int, Tcl_Obj *CONST objv[]);

/* This is the main entry point for the whole library.
 * Create the Tcl commands to provide the interface
 */

extern "C" int Tamet_Init(Tcl_Interp *interp) {

#ifdef USE_TCL_STUBS
    if (Tcl_InitStubs(interp, "8.0", 0) == NULL) {
        return TCL_ERROR;
    }
#endif

  /*
   * Call Tcl_CreateCommand for application-specific commands, if
   * they weren't already created by the init procedures called above.
   */
  Tcl_CreateObjCommand(interp, "zero_variables", zero_variables, NULL, NULL);
  Tcl_CreateObjCommand(interp, "daily_main", daily_main, NULL, NULL);

  /* link the fortran common block locations to tcl variables */
  Interp = interp;
  tamet2_setup_links__();

  Tcl_PkgProvide(interp, "tamet", "0.0");
  
  return TCL_OK;
}

extern "C" int Tamet_SafeInit(Tcl_Interp *interp) {
#ifdef USE_TCL_STUBS
    if (Tcl_InitStubs(interp, "8.0", 0) == NULL) {
        return TCL_ERROR;
    }
#endif
  Tcl_PkgProvide(interp, "tamet", "0.0");
  return(TCL_OK);
}


// Link creators. These routines link a tcl variable to a fortran common
// block location. Note that array offsets start at 1 (fortran usage).
// 'Interp' is the global tcl interpreter we're working with - set 2
// levels higher in the call stack.
 
// Make a symbol linked to some double variable
 extern "C" int makelink_dbl__(int *length,
			       double *addr, char *name, 
			       long namelen) {
   char nbuf[80], vbuf[80];

   strncpy(nbuf, name, namelen);     // Fortran stings might not be 
   nbuf[namelen] = '\0';             // null terminated, but usually are.
   
   if (*length == 1) {
     *addr = 0.0;
     Tcl_LinkVar(Interp, nbuf, (char*)addr, TCL_LINK_DOUBLE);
  } else {
    for (int i = 0; i < *length; i++) {
      *addr = 0.0;
      sprintf(vbuf, "%s(%d)", nbuf, i+1);  
      Tcl_LinkVar(Interp, vbuf, (char*)(addr+i), TCL_LINK_DOUBLE);
    }
  }	
   return(0);
}

extern "C" int makelink_int__(int *length,
			      int *addr, char *name, long namelen) {
  char nbuf[80], vbuf[80];
  
  strncpy(nbuf, name, namelen);     // Fortran stings might not be 
  nbuf[namelen] = '\0';             // null terminated, but usually are.
  
  if (*length == 1) {
    *addr = 0;
    Tcl_LinkVar(Interp,  nbuf, (char*)addr, TCL_LINK_INT);
  } else {
    for (int i = 0; i < *length; i++) {
      *addr = 0;
      sprintf(vbuf, "%s(%d)", nbuf, i+1);  
      Tcl_LinkVar(Interp, vbuf, (char*)(addr+i), TCL_LINK_INT);
    }
  }	
  return(0);
}

extern "C" int makelink_bool__(int *length, 
			       int *addr, char *name, long namelen) {
  char nbuf[80], vbuf[80];
  
  strncpy(nbuf, name, namelen);     // Fortran stings might not be 
  nbuf[namelen] = '\0';             // null terminated, but usually are.
  
  if (*length == 1) {
    *addr = 0;
    Tcl_LinkVar(Interp, nbuf, (char*)addr, TCL_LINK_BOOLEAN);
  } else {
    for (int i = 0; i <= *length; i++) {
      *addr = 0;
	  sprintf(vbuf, "%s(%d)", nbuf, i+1);  
	  Tcl_LinkVar(Interp, vbuf, (char*)(addr+i), TCL_LINK_BOOLEAN);
	}
  }	
  return(0);}

extern "C" int makelink_string__(int *length, 
                 char *addr, char *name, long addrlen, 
                 long namelen) {
  char nbuf[80];

  strncpy(nbuf, name, namelen);     // Fortran stings might not be 
  nbuf[namelen] = '\0';             // null terminated, but usually are.
  
  Tcl_LinkVar(Interp, nbuf, addr, TCL_LINK_STRING);
  return(0);
}

// Instead of writing to a summary file, we just append
// to interp->result. 
extern "C" int write_string__ (char *string, long stringlen) {
  char *buf = ckalloc(stringlen+1);
  strncpy (buf, string, stringlen);
  buf[stringlen] = '\0';
  Tcl_AppendResult(Interp, buf, "\n", NULL);
  ckfree(buf);
  return 1;
}


int zero_variables(ClientData, Tcl_Interp *interp, 
			      int objc, Tcl_Obj *CONST objv[]){
  Interp = interp;
  tamet2_zero_variables__();
  return TCL_OK;
}

int daily_main(ClientData, Tcl_Interp *interp, 
	       int objc, Tcl_Obj *CONST objv[]){
  Interp = interp;
  tamet2_zero_daily_variables__();
  tamet2_get_other_variables__();
  tamet2_main__();
  return TCL_OK;
}

// Replacements for apsim libary code 
extern "C" int day_of_year_to_date__ (int *day, int *year, int *idate) {
  Date d(*year, 1, *day);
  d.decode(&idate[2], &idate[1], &idate[0]);
  //printf("%d %d -> %d %d %d:", *year, *day, idate[2], idate[1], idate[0]);
  return 0;
}

extern "C" int date_to_jday__ (int *day, int *month, int *year) {
  Date d(*year, *month, *day);
  return d.intval();
}

extern "C" int jday_to_date__ (int *day, int *month, int *year, int *jday) {
  Date d(*jday);
  d.decode(year, month, day);
  return 0;
}

extern "C" int end_month__ (int *day, int *year) {
  Date d(*year, 1, *day);
  //FIXME
  return 0;
}

extern "C" int reals_are_equal__ (float *r1, float *r2) {
  if (fabs(*r1 - *r2) < 1.0E-6) {return 1;}
  return 0;
}

extern "C" float bound_ (float *value, float *lower, float *upper) {
  if (*lower >= *upper) {
    fprintf(stderr,"Aieee lower(%f) > upper (%f)\n",*lower, *upper);
    exit(1);
  }
  if (*value < *lower) return *lower;
  if (*value > *upper) return *upper;
  return *value;
}
