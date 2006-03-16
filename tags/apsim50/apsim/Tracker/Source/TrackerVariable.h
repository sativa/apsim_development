//---------------------------------------------------------------------------
#ifndef TrackerVariableH
#define TrackerVariableH
#include <string>
#include <general\stringtokenizer.h>
namespace protocol
   {
   class Component;
   struct QueryValueData;
   };
//---------------------------------------------------------------------------
// Keeps track of a single Tracker variable.
//---------------------------------------------------------------------------
class TrackerVariable
   {
   public:
      TrackerVariable(protocol::Component* parent, const std::string& fullName);
      std::string getName(void) const {return name;}

      void doRegistrations(void);
      void respondToEvent(unsigned fromID, unsigned eventID);
      void respondToGet(unsigned int& fromID,
                        protocol::QueryValueData& queryData);

   private:
      std::string name;
      protocol::Component* parent;
      enum Stat {sumStat, averageStat, minimumStat, maximumStat,
                 countStat, dateStat, valueStat};
      Stat stat;

      std::string variableName;
      std::string eventName;
      std::string eventNameComponent;

      std::string startPeriod;
      std::string startPeriodComponent;
      std::string endPeriod;
      std::string endPeriodComponent;

      unsigned last;
      std::string sampleDate;

      unsigned variableID;
      unsigned startPeriodID;
      unsigned endPeriodID;
      unsigned nameID;
      unsigned eventID;
      unsigned todayID;


      bool inWindow;
      bool isArray;
      int count;
      vector<vector<float> > values;

      void parse(const std::string& fullName);
      void parseStat(StringTokenizer& tokenizer);
      void parsePeriod(StringTokenizer& tokenizer);
      void parseLast(StringTokenizer& tokenizer);
      void parseAs(StringTokenizer& tokenizer);
      void parseEventName(StringTokenizer& tokenizer);
      void doSample(void);
      void onStartPeriod(void);
      void onEndPeriod(void);
      void getCurrentValues(vector<float>& values);

   };
#endif
