// Ugliness mostly due to inability to include tk.h into apsim framework..

#include <windows.h>

#include <tcl.h>
#include <tk.h>

extern "C" void TclWinInit(HINSTANCE);
extern "C" void TkWinXCleanup(HINSTANCE);
extern "C" void TkWinXInit(HINSTANCE);

#pragma package(smart_init)

extern int apsimGetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
extern int apsimSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
extern int apsimRegisterGetSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
extern int apsimSendEventProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);

Tcl_Interp * StartTcl (ClientData cd,  const char *exeFileName)
   {
   char buffer[MAX_PATH +1];
   Tcl_DString argString;
   Tcl_Interp *interp;

   TclWinInit(GetModuleHandle(NULL));
   TkWinXInit(GetModuleHandle(NULL));

   interp = Tcl_CreateInterp();
   Tcl_Preserve((ClientData) interp);

   Tcl_InitMemory(interp);

   char *p= buffer; const char * q=exeFileName;
   while (*q != '\0') {
       if (*q == '\\') {
         *p = '/';
       } else {
         *p = *q;
       }
       p++, q++;
   }
   *p = '\0';
   Tcl_FindExecutable(buffer); // This is ignored anyway - they use GetModule()!!

   Tcl_VarEval(interp, "set tcl_library [file join [file dirname ", buffer, "] tcl[info tclversion]]", NULL);
   Tcl_VarEval(interp, "set tk_library [file join [file dirname ", buffer, "] tk[info tclversion]]", NULL);

   Tcl_SetVar(interp, "argv", "", TCL_GLOBAL_ONLY);
   Tcl_SetVar(interp, "argc", "0", TCL_GLOBAL_ONLY);

   Tcl_ExternalToUtfDString(NULL, buffer, -1, &argString);
   Tcl_SetVar(interp, "argv0", Tcl_DStringValue(&argString), TCL_GLOBAL_ONLY);
   Tcl_SetVar(interp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);
   Tcl_DStringFree(&argString);

   if (Tcl_Init(interp) != TCL_OK) {MessageBox(0, interp->result, "Error in Tcl Start", MB_ICONSTOP); return NULL;}
   if (Tk_Init(interp) != TCL_OK) {MessageBox(0, interp->result, "Error in Tk Start", MB_ICONSTOP); return NULL;}

   Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);
   Tcl_CreateObjCommand(interp, "apsimGet", apsimGetProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSet", apsimSetProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimRegisterGetSet", apsimRegisterGetSetProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSendEvent", apsimSendEventProc, cd, NULL);
   return interp;
   }

void StopTcl(Tcl_Interp *interp)
   {
   //MessageBox(0, "TCl Stop", "TCl Stop", MB_ICONSTOP);
   if (0)  //!Tcl_InterpDeleted(interp))
       {
       Tk_Window t = Tk_MainWindow(interp);
       if (t != NULL) { Tk_DestroyWindow(t); }

       Tcl_Eval(interp, "exit");
       if (!Tcl_InterpDeleted(interp))
           {
           Tcl_DeleteInterp(interp);
           }
       Tcl_Release((ClientData) interp);
       }
   //Tcl_Finalize();
   //TkWinXCleanup(hinst);
   }

