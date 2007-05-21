#include <fstream>
#include <stdexcept>
#include <process.h>
#include <string>

#include <boost/date_time/gregorian/gregorian.hpp>

#include <general/string_functions.h>
#include <general/path.h>
#include <general/http.h>

#include <ApsimShared/ApsimDataFile.h>
#include <ComponentInterface/Component.h>

#include <StringVariant.h>
#include <InputComponent.h>
#include "SiloInputComponent.h"

using namespace std;

// ------------------------------------------------------------------
// createComponent
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new SiloInputComponent;
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
SiloInputComponent::SiloInputComponent(void)
   {
   unsigned int pid = getpid();
   fileName = Path::getTempFolder().Get_path() + string("/temp") + itoa(pid) + string(".met");
   stationNumber = 0;
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
SiloInputComponent::~SiloInputComponent(void)
   {
   data.close();
   unlink(fileName.c_str());
   }
// ------------------------------------------------------------------
// INIT 2 - temporary
// ------------------------------------------------------------------
void SiloInputComponent::doInit2(void)
   {
   string msg = "SILOINPUT station number: " + itoa(stationNumber);
   writeString(msg.c_str());
   }
// ------------------------------------------------------------------
// Open the input file associtated with this module.
// ------------------------------------------------------------------
void SiloInputComponent::openInputFile(void)
   {
   readParameter ("parameters", "station_number", stationNumber, 0, 100000);

   string requestString =
      string("http://192.168.0.60/cgi-bin/silo/getQdb.cgi?format=APSIM&station=") +
      itoa(stationNumber) +
      string("&ddStart=1&mmStart=1&yyyyStart=1800&ddFinish=31&mmFinish=12&yyyyFinish=2100");

   tHTTP http;

   if (http.Get(fileName, requestString) == false)
     throw std::runtime_error("HTTP error: " + http.responseText());

   data.open(fileName);
   }
