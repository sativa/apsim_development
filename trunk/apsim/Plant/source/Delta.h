#ifndef DeltaH
#define DeltaH
#include <string>
class ScienceAPI;
class Pool;
class Delta
   {
   public:
      Delta(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName);
      float DM;
      float N;
      float P;
      void Move (Pool& From, Pool& To);
   private:
      
   };

#endif