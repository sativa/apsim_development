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
      enum Stat {sumStat, averageStat, minimumStat, maximumStat, countStat};
      Stat stat;

      std::string variable;
      std::string startPeriod;
      std::string endPeriod;
      std::string since;
      std::string on;
      std::string onComponent;
      unsigned last;
      unsigned sinceComponentID;

      unsigned variableID;
      unsigned startPeriodID;
      unsigned endPeriodID;
      unsigned sinceID;
      unsigned nameID;
      unsigned onID;

      bool inWindow;
      bool isArray;
      int count;
      vector<vector<float> > values;

      void parse(const std::string& fullName);
      void parseStat(StringTokenizer& tokenizer);
      void parseBetween(StringTokenizer& tokenizer);
      void parseLast(StringTokenizer& tokenizer);
      void parseSince(StringTokenizer& tokenizer);
      void parseAs(StringTokenizer& tokenizer);
      void parseOn(StringTokenizer& tokenizer);
      void doSample(void);
      void onStartPeriod(void);
      void onEndPeriod(void);
      void getCurrentValues(vector<float>& values);

   };
#endif
