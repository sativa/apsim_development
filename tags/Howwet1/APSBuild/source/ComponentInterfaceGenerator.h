//---------------------------------------------------------------------------
#ifndef ComponentInterfaceGeneratorH
#define ComponentInterfaceGeneratorH

#include <string>
// ------------------------------------------------------------------
//  Short description:
//     generate a component interface from the APSIM interface filename
//     passed in.  The generated output files will go in the current
//     working directory.

//  Notes:

//  Changes:
//    NIH 13/12/2000

// ------------------------------------------------------------------
void GenerateComponentInterface(const std::string& interfaceFile);
#endif
