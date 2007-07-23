#include <fstream>
#include <stdexcept>
#include <string>
#include <sys/stat.h>

#ifdef __WIN32__
 #include <process.h>
// #include <dos.h>
// #define sleep _sleep
#else
 #include <unistd.h>
#endif

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
      string("http://192.168.0.60/cgi-bin/getData.tcl?format=apsim&station=") +
      itoa(stationNumber) +
      string("&ddStart=1&mmStart=1&yyyyStart=1800&ddFinish=31&mmFinish=12&yyyyFinish=2100");

   tHTTP http;

   bool ok = http.Get(fileName, requestString);

   if (!ok) {throw std::runtime_error(http.ErrorMessage());}

   struct stat statbuf;
   if (stat(fileName.c_str(), &statbuf) < 0) 
       throw std::runtime_error("Temporary met file " + fileName + " is missing");

   if (statbuf.st_size == 0) 
       throw std::runtime_error("No data for station " + itoa(stationNumber) + 
                                 " appeared in " + fileName);

   data.open(fileName);
   }
