#include <general\general.h>
// ------------------------------------------------------------------
//  Short description:
//    executes a program and optionally waits until it has finished.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
bool GENERAL_EXPORT Exec(const char* Command_line,
                         unsigned int Show_flag,
                         bool Wait_for_finish);

