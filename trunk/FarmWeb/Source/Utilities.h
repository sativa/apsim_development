//---------------------------------------------------------------------------

#ifndef UtilitiesH
#define UtilitiesH

#include <vector>
#include <string>
#include "IWCompListbox.hpp"
#include "IWCompEdit.hpp"
#include "IWTmsCal.hpp"
#include "IWAppForm.hpp"
#include "Data.h"
class TWebSession;
//---------------------------------------------------------------------------
// populate an edit box by setting the text property
//---------------------------------------------------------------------------
void populateEdit(TIWEdit* edit,
                  Data* data,
                  const string& userName,
                  const string& paddockName,
                  const string& dataName);
//---------------------------------------------------------------------------
// populate a combo box by setting the text property and initialising
// the lookup items.
//---------------------------------------------------------------------------
void populateCombo(TIWComboBox* combo,
                   Data* data,
                   const string& userName,
                   const string& paddockName,
                   const string& dataName);
//---------------------------------------------------------------------------
// populate a date picker control from the database for the specified
// data name.
//---------------------------------------------------------------------------
void populateDatePicker(TTIWDatePicker* datePicker,
                        Data* data,
                        const string& userName,
                        const string& paddockName,
                        const string& dataName);
//---------------------------------------------------------------------------
// save an edit box value
//---------------------------------------------------------------------------
void saveEdit(TIWEdit* edit,
              Data* data,
              const string& userName,
              const string& paddockName,
              const string& dataName);
//---------------------------------------------------------------------------
// save a combo box value
//---------------------------------------------------------------------------
void saveCombo(TIWComboBox* combo,
               Data* data,
               const string& userName,
               const string& paddockName,
               const string& dataName);
//---------------------------------------------------------------------------
// save a DatePicker value
//---------------------------------------------------------------------------
void saveDatePicker(TTIWDatePicker* datePicker,
                    Data* data,
                    const string& userName,
                    const string& paddockName,
                    const string& dataName);

//---------------------------------------------------------------------------
// Email all report files to specified addresses.Return true if the
// soil water parameters were bounded.
//---------------------------------------------------------------------------
bool generateReport(std::string toEmailAddress,
                    TWebSession* webSession,
                    Data* data,
                    const string& userName,
                    const string& paddockName,
                    const string& reportName,
                    const Data::Properties& properties,
                    bool generateTempFiles,
                    const string& descriptiveNameForReport);

//---------------------------------------------------------------------------
// Draw a menu header at the specified position
//---------------------------------------------------------------------------
void drawMenuHeader(TIWAppForm* form, int lineNumber, const std::string& text);

//---------------------------------------------------------------------------
// Draw a menu item at the specified position
//---------------------------------------------------------------------------
void drawMenuItem(TIWAppForm* form, int lineNumber, const string& text,
                  const std::string& gif, TNotifyEvent event);

//---------------------------------------------------------------------------
// populate the station number combo
//---------------------------------------------------------------------------
void populateStationNumberCombo(Data* data,
                                const std::string& userName,
                                const std::string& paddockName,
                                const std::string& region,
                                TIWComboBox* WeatherStationCombo);

//---------------------------------------------------------------------------
// populate the soil type combo
//---------------------------------------------------------------------------
void populateSoilTypeCombo(Data* data,
                           const std::string& userName,
                           const std::string& paddockName,
                           const std::string& region,
                           TIWComboBox* SoilTypeCombo);


#endif
