#include <windows.h>
// ------------------------------------------------------------------
//  Short description:
//    executes a program and optionally waits until it has finished.
//    Returns true if program was successfully executed.  NOTE:  This
//    routine is NOT THREAD SAFE.  Wrap inside a Synchronize method call
//    if being called from a thread.
//    If the terminateFlagToCheck is specified, then the routine will
//    check the doTerminate flag periodically and if true, the process
//    will terminate the child process and exit the routine.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool Exec(const char* Command_line,
          unsigned int Show_flag,
          bool Wait_for_finish,
          bool* doTerminateFlag = NULL);

