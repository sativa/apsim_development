#include "NonHerbageConverter.h"


#pragma package(smart_init)
using namespace std;


#define singleArrayTypeDDML \
   "<type  array=\"T\" kind=\"single\"/>"
#define singleTypeDDML \
   "<type  kind=\"single\"/>"
#define stringTypeDDML \
   "<type  kind=\"string\"/>"

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
NonHerbageConverter::NonHerbageConverter(protocol::Component *s) : ConverterBase(s)
   {
////      system = s;
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
NonHerbageConverter::~NonHerbageConverter(void)
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void NonHerbageConverter::doInit1(const FString& sdml)
   {
   day_lengthID = system->addRegistration(RegistrationType::get, "day_length", singleTypeDDML);
   tramplingID = system->addRegistration(RegistrationType::get, "trampling", singleTypeDDML);
   ureaID = system->addRegistration(RegistrationType::get, "urea", singleArrayTypeDDML);
   dltUreaID = system->addRegistration(RegistrationType::set, "dlt_urea", singleArrayTypeDDML);
   labilePID = system->addRegistration(RegistrationType::get, "labile_p", singleArrayTypeDDML);
   dltLabilePID = system->addRegistration(RegistrationType::set, "dlt_labile_p", singleArrayTypeDDML);

   dayLengthID = system->addRegistration(RegistrationType::respondToGet, "dayLength", singleTypeDDML);
   addExcretaID = system->addRegistration(RegistrationType::respondToEvent, "add_excreta", protocol::DDML(protocol::add_excretaType()).c_str());

   stockBuyID = system->addRegistration(RegistrationType::respondToEvent, "buystock", stringTypeDDML);
//   buyID = system->addRegistration(RegistrationType::event, "buy", buystockTypeDDML);

   stockMoveID = system->addRegistration(RegistrationType::respondToEvent, "movestock", stringTypeDDML);
   moveID = system->addRegistration(RegistrationType::event, "move", DDML(protocol::movestockType()).c_str());

   stockSellID = system->addRegistration(RegistrationType::respondToEvent, "sellstock", stringTypeDDML);
   sellID = system->addRegistration(RegistrationType::event, "sell", protocol::DDML(protocol::sellstockType()).c_str());

   addManureID = system->addRegistration(RegistrationType::event, "add_surfaceom", "", "", "");

   std::vector<protocol::intakeType> dummy;
   intakeGetID = system->addRegistration(RegistrationType::get, "intake", protocol::DDML(dummy).c_str());
   intakeSendID = system->addRegistration(RegistrationType::respondToGet, "intakestock", singleTypeDDML);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void NonHerbageConverter::doInit2(void)
   {
      readParameters (); // Read constants
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void NonHerbageConverter::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
{
   if (eventID == addExcretaID)
   {
      variant.unpack(excreted);
      ostringstream msg1;
      if (c.debug == "on")
      {
         msg1 << endl << "Excretion:-" << endl;

         msg1 << "   faeces om " <<  " (weight) =  " << excreted.faeces_om.weight << " (kg/ha)" << endl;
         msg1 << "   faeces om " <<  " (n) =       " << excreted.faeces_om.n      << " (kg/ha)" << endl;
         msg1 << "   faeces om " <<  " (p) =       " << excreted.faeces_om.p      << " (kg/ha)" << endl;
         msg1 << "   faeces om " <<  " (s) =       " << excreted.faeces_om.s      << " (kg/ha)" << endl;
         msg1 << "   faeces om " <<  " (ash_alk) = " << excreted.faeces_om.ash_alk << " (mol/ha)" << endl;

         msg1 << "   faeces inorg " <<  " (n) = " << excreted.faeces_inorg.n << " (kg/ha)" << endl;
         msg1 << "   faeces inorg " <<  " (p) = " << excreted.faeces_inorg.p << " (kg/ha)" << endl;
         msg1 << "   faeces inorg " <<  " (s) = " << excreted.faeces_inorg.s << " (kg/ha)" << endl;

         msg1 << "   urine " <<  " (volume) =  " << excreted.urine.volume  << " (m3/ha)" << endl;
         msg1 << "   urine " <<  " (urea) =    " << excreted.urine.urea    << " (kg/ha)" << endl;
         msg1 << "   urine " <<  " (pox) =     " << excreted.urine.pox     << " (kg/ha)" << endl;
         msg1 << "   urine " <<  " (so4) =     " << excreted.urine.so4     << " (kg/ha)" << endl;
         msg1 << "   urine " <<  " (ash_alk) = " << excreted.urine.ash_alk << " (mol/ha)" << endl;

         msg1 << ends;

         system->writeString (msg1.str().c_str());
      }

      const string omName = "manure";
      const string omType = "manure";
      sendAddSurfaceOMEvent (omName, omType, excreted.faeces_om);
      addUrine (excreted.urine);
    }

   else if (eventID == stockBuyID)
   {
      stockBuy(variant);
   }
   else if (eventID == stockSellID)
   {
      stockSell(variant);
   }
   else if (eventID == stockMoveID)
   {
      stockMove(variant);
   }
   else
   {   // Don't respond to any other events.
   }
}
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void NonHerbageConverter::stockBuy (protocol::Variant &v/*(INPUT) message variant*/)
{
    std::string  valuestr;
    int      value4;
    double   value;
    protocol::buystockType buystock;

    protocol::ApsimVariant incomingApsimVariant(this);
    incomingApsimVariant.aliasTo(v.getMessageData());

    if (incomingApsimVariant.get("number", protocol::DTint4, false, value4) == true)
    {
         buystock.number = value4;

         ostringstream msg;
         msg << "Buy stock :-" << endl
             << "   number = " << setw(10) << value4 << " (-)" << ends;
         system->writeString (msg.str().c_str());
    }
    else
         buystock.number = 0;

    if (incomingApsimVariant.get("genotype", protocol::DTstring, false, valuestr) == true)
    {
         buystock.genotype = valuestr;

         ostringstream msg;
         msg << "   genotype = " << valuestr << " (-)" << ends;
         system->writeString (msg.str().c_str());
    }
    else
         buystock.genotype = "";

    if (incomingApsimVariant.get("sex", protocol::DTstring, false, valuestr) == true)
    {
         buystock.sex = valuestr;

         ostringstream msg;
         msg << "   sex = " << valuestr << " (-)" << ends;
         system->writeString (msg.str().c_str());
    }
    else
         buystock.sex = "";

    if (incomingApsimVariant.get("age", protocol::DTdouble, false, value) == true)
    {
         buystock.age = value;

         ostringstream msg;
         msg << "   age = "  << value << " (months)" << ends;
         system->writeString (msg.str().c_str());
    }
    else
         buystock.age = 0.0;

    if (incomingApsimVariant.get("weight", protocol::DTdouble, false, value) == true)
    {
         buystock.weight = value;

         ostringstream msg;
         msg << "   weight = "  << value << " (kg)" << ends;
         system->writeString (msg.str().c_str());

    }
    else
         buystock.weight = 0.0;

    if (incomingApsimVariant.get("fleece_wt", protocol::DTdouble, false, value) == true)
    {
         buystock.fleece_wt = value;

         ostringstream msg;
         msg << "   fleece_wt = "  << value << " (kg)" << ends;
         system->writeString (msg.str().c_str());
    }
    else
         buystock.fleece_wt = 0.0;

    if (incomingApsimVariant.get("cond_score", protocol::DTdouble, false, value) == true)
    {
         buystock.cond_score = value;

         ostringstream msg;
         msg << "   cond_score = "  << value << " ()" << ends;
         system->writeString (msg.str().c_str());
    }
    else
         buystock.cond_score = 0.0;

    if (incomingApsimVariant.get("mated_to", protocol::DTstring, false, valuestr) == true)
         buystock.mated_to = valuestr;
    else
         buystock.mated_to = "";

    if (incomingApsimVariant.get("pregnant", protocol::DTint4, false, value4) == true)
         buystock.pregnant = value4;
    else
         buystock.pregnant = 0;

    if (incomingApsimVariant.get("lactating", protocol::DTint4, false, value4) == true)
         buystock.lactating = value4;
    else
         buystock.lactating = 0;

    if (incomingApsimVariant.get("no_young", protocol::DTint4, false, value4) == true)
         buystock.no_young = value4;
    else
         buystock.no_young = 0;

    if (incomingApsimVariant.get("young_wt", protocol::DTdouble, false, value) == true)
         buystock.young_wt = value;
    else
         buystock.young_wt = 0.0;

    if (incomingApsimVariant.get("young_fleece_wt", protocol::DTdouble, false, value) == true)
         buystock.young_fleece_wt = value;
    else
         buystock.young_fleece_wt = 0.0;

    system->publish (buyID, buystock);
}

void NonHerbageConverter::stockSell (protocol::Variant &v/*(INPUT) message variant*/)
{
    int      value4;
    protocol::sellstockType sellstock;

    ostringstream msg;
    msg << "Sell stock :-" << endl;

    protocol::ApsimVariant incomingApsimVariant(this);
    incomingApsimVariant.aliasTo(v.getMessageData());

    if (incomingApsimVariant.get("number", protocol::DTint4, false, value4) == true)
    {
         sellstock.number = value4;

         msg << "   number = " << setw(10) << value4 << " (-)" << endl;
    }
    else
    {
         sellstock.number = 0;
    }

    if (incomingApsimVariant.get("group", protocol::DTint4, false, value4) == true)
    {
         sellstock.group = value4;

         msg << " Group = " << setw(10) << value4 << " (-)" << endl;
    }
    else
    {
         sellstock.group = 0;
    }
    msg << ends;
    system->writeString (msg.str().c_str());

    system->publish (sellID, sellstock);
}

void NonHerbageConverter::stockMove (protocol::Variant &v/*(INPUT) message variant*/)
{
    std::string  valuestr;
    int      value4;
    protocol::movestockType movestock;

    ostringstream msg;
    msg << "Name stock :-" << endl;

    protocol::ApsimVariant incomingApsimVariant(this);
    incomingApsimVariant.aliasTo(v.getMessageData());

    if (incomingApsimVariant.get("group", protocol::DTint4, false, value4) == true)
    {
         movestock.group = value4;

         msg << "   Group = " << setw(10) << value4 << " (-)" << endl;
    }
    else
    {
         movestock.group = 0;
    }

    if (incomingApsimVariant.get("paddock", protocol::DTstring, false, valuestr) == true)
    {
         movestock.paddock = valuestr;
         msg << " paddock = " << setw(10) << valuestr << " (-)" << endl;
    }
    else
    {
         movestock.paddock = "";
    }
    msg << ends;
    system->writeString (msg.str().c_str());

    system->publish (moveID, movestock);
}

void NonHerbageConverter::sendAddSurfaceOMEvent (const string& omName, const string& omType, protocol::faeces_omType faecesOM)
{
    protocol::ApsimVariant outgoingApsimVariant(this);
    outgoingApsimVariant.store("name", protocol::DTstring, false, FString(omName.c_str()));
    outgoingApsimVariant.store("type", protocol::DTstring, false, FString(omType.c_str()));

    outgoingApsimVariant.store("mass", protocol::DTdouble, false, faecesOM.weight * c.fractionFaecesAdded);
    outgoingApsimVariant.store("n", protocol::DTdouble, false, faecesOM.n * c.fractionFaecesAdded);
    outgoingApsimVariant.store("p", protocol::DTdouble, false, faecesOM.p * c.fractionFaecesAdded);
    outgoingApsimVariant.store("s", protocol::DTdouble, false, faecesOM.s * c.fractionFaecesAdded);
    outgoingApsimVariant.store("ash_alk", protocol::DTdouble, false, faecesOM.ash_alk * c.fractionFaecesAdded);

    system->publish (addManureID, outgoingApsimVariant);
    return;
}

void NonHerbageConverter::addUrine (protocol::urineType urine)
{
    std::vector<float> values;               // Scratch area
    float urea[max_layer];                     // soil Urea change (kg/ha)
    float labileP[max_layer];                     // soil Urea change (kg/ha)
    int   layer;                                  // soil layer no.
    int   num_layers;                             // number of layers

    system->getVariable (ureaID, values, 0.0, 1000.0, true);
    num_layers = values.size();

    for (layer = 0; layer != num_layers; layer++) {urea[layer] = 0.0;}
    urea[0] = urine.urea * c.fractionUrineAdded;
    protocol::vector<float> ureaValues(urea, urea+num_layers);
    system->setVariable (dltUreaID, ureaValues);

    values.clear();
    if (system->getVariable (labilePID, values, 0.0, 1000.0, true))
    {
       num_layers = values.size();

       for (layer = 0; layer != num_layers; layer++) {labileP[layer] = 0.0;}
       labileP[0] = urine.pox * c.fractionUrineAdded;
       protocol::vector<float> labilePValues(labileP, labileP+num_layers);
       system->setVariable (dltLabilePID, labilePValues);
    }

    return;
}

// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void NonHerbageConverter::respondToGet(unsigned int& fromID,
                                             protocol::QueryValueData& queryData)
{
   // Daylength
   if (queryData.ID == dayLengthID) daylengthRelay(queryData);
   else if (queryData.ID == intakeSendID) intakeRelay(queryData);

   else
   {   // don't respond to any other gets.
   }
}

void NonHerbageConverter::daylengthRelay (protocol::QueryValueData& queryData)
{
      protocol::Variant* variant;
      bool ok = system->getVariable(day_lengthID, variant, true);
      if (ok)
      {
         float dayLength;
         bool ok = variant->unpack(dayLength);  // what happens if this is not ok?
         system->sendVariable(queryData, dayLength);
      }
      else
      {   // didn't get the day_length ID ok. Do nothing about it.
      }
}


void NonHerbageConverter::intakeRelay (protocol::QueryValueData& queryData)
{
      protocol::Variant* variant;
      bool ok = system->getVariable(intakeGetID, variant, true);
      if (ok)
      {
         protocol::intakeType intake[1];
         bool ok = variant->unpack(intake);  // what happens if this is not ok?
         float weight = static_cast<float>(intake[0].weight);
         system->sendVariable(queryData, weight);
      }
      else
      {   // didn't get the intake ID ok. Do nothing about it.
      }
}


void NonHerbageConverter::readParameters ( void )
{

//+  Constant Values
    const char*  section_name = "parameters" ;

//- Implementation Section ----------------------------------

    system->writeString (" - non-herbage converter reading parameters");

    c.debug = system->readParameter (section_name, "debug");
    system->readParameter (section_name, "fraction_faeces_added", c.fractionFaecesAdded, 0.0, 1.0);
    system->readParameter (section_name, "fraction_urine_added", c.fractionUrineAdded, 0.0, 1.0);
    string stockModuleName = system->readParameter (section_name, "stock_module");

      ostringstream msg;
      msg << "Stock_module = " << stockModuleName << endl;
      msg << "Fraction_faeces_added = " << c.fractionFaecesAdded << endl;
      msg << "Fraction_urine_added = " << c.fractionUrineAdded << endl;
      msg << "Debug = " << c.debug << ends;
      system->writeString (msg.str().c_str());

   string buy;
   if (stockModuleName == "")
      buy = "buy";
   else
      buy = stockModuleName + ".buy";
   buyID = system->addRegistration(RegistrationType::event, buy.c_str(), DDML(protocol::buystockType()).c_str());

}