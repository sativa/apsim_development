#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "SiloInputComponent.h"
#include <ApsimShared\ApsimComponentData.h>
#include <general\path.h>
#include <process.h>
#include <IdBaseComponent.hpp>
#include <IdComponent.hpp>
#include <IdHTTP.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>

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
   fileName = Path::getTempFolder().Get_path() + "\\temp" + itoa(pid) + ".met";
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
   string msg = "SILOINPUT station number: " + stationNumber;
   writeString(msg.c_str());

/*   string title;
   ApsimDataFile::iterator i = find(data.constantsBegin(), data.constantsEnd(),
                                    "title");
   if (i != data.constantsEnd() && i->values.size() > 0)
      title = i->values[0];
   title = "SILOINPUT station name:   " + title;
   writeString(title.c_str());
*/   }
// ------------------------------------------------------------------
// Open the input file associtated with this module.
// ------------------------------------------------------------------
void SiloInputComponent::openInputFile(void)
   {
   stationNumber = componentData->getProperty("parameters", "station_number");
   if (stationNumber == "")
      throw "Cannot find a SILO station number parameter for module: " + string(name);

   string requestString =
      "http://192.168.0.60/cgi-bin/silo/getQdb.cgi?format=APSIM&station=xxxx"
      "&ddStart=1&mmStart=1&yyyyStart=1800&ddFinish=31&mmFinish=12&yyyyFinish=2100";
   replaceAll(requestString, "xxxx", stationNumber);

   TIdHTTP* http = new TIdHTTP(NULL);
   String contents = http->Get(requestString.c_str());
   ofstream out(fileName.c_str(), ios::binary);
   out << contents.c_str();
   out.close();
   data.open(fileName);
   delete http;
   }

