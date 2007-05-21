//---------------------------------------------------------------------------

#ifndef SoilPickerH
#define SoilPickerH
namespace Forms {
   class TForm;
   };
class Soils;
namespace Controls {
   class TWinControl;
   };
//---------------------------------------------------------------------------
// Shows a modal dialog box and allows the user to pick a soil. The APSIM
// parameter files for that soil are then returned.  Returns true if a soil
// was picked.
//---------------------------------------------------------------------------
extern "C" DWORD _stdcall _export PickSoil(char* dbFileName, const char* title, char* w2FileName, char* n2FileName);

//---------------------------------------------------------------------------
// Return a point to an instance of a characterisation form.
//---------------------------------------------------------------------------
TForm* _export createCharacterisationForm(TWinControl* parent,
                                          Soils& soils,
                                          bool readOnly);

extern "C" bool _stdcall _export getSoil(const char* dbFileName, const char* title, char* w2FileName, char* n2FileName);

#endif
