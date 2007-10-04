#ifndef PoolH
#define PoolH
#include <string>
class ScienceAPI;
class Delta;
class Pool
   {
   public:
      Pool(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName);
      float DM;
      float N;
      float P;
      void Add (Delta& Dlt);
      void Remove (Delta& Dlt);
   private:
      
   };

#endif