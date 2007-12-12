#ifndef ReproductivePartH
#define ReproductivePartH

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <iomanip>

#include <boost/function.hpp>
#include <boost/bind.hpp>

#include <ComponentInterface/Type.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/datatypes.h>
#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>
#include <ComponentInterface/ScienceAPI.h>


#include "SimplePart.h"

class ReproductivePart : public SimplePart
   {
   public:
   ReproductivePart(ScienceAPI& api, plantInterface *p, const string &name);


   protected:


   private:

};

#endif
