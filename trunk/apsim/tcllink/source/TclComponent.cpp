#include <string.h>

#include <map>
#include <general/string_functions.h>
#include <general/stristr.h>

#include <ApsimShared/FStringExt.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ComponentInterface/Messages.h>
#include <Protocol/transport.h>

#include <tcl.h>
#include <tclInt.h>
#include "TclComponent.h"

using namespace std;
using namespace protocol;

extern void StartTcl (const char *);
extern Tcl_Interp *NewInterp (Tcl_Interp *, ClientData, const char *);
extern void StopTcl(Tcl_Interp *);

static int initialisationState = 0;
static Tcl_Interp *TopLevelInterp = NULL;

#define nullDDML       "<type/>"
#define intString      "<type kind=\"integer4\" array=\"F\"/>"
#define intStringArray "<type kind=\"integer4\" array=\"T\"/>"
#define fltString      "<type kind=\"single\" array=\"F\"/>"
#define fltStringArray "<type kind=\"single\" array=\"T\"/>"
#define dblString      "<type kind=\"double\" array=\"F\"/>"
#define dblStringArray "<type kind=\"double\" array=\"T\"/>"
#define strString      "<type kind=\"string\" array=\"F\"/>"
#define strStringArray "<type kind=\"string\" array=\"T\"/>"

static const char* messageNames[45] =
     {"ActivateComponent", "AddComponent", "Checkpoint", "Commence",
      "Complete", "DeactivateComponent", "DeleteComponent", "Deregister",
      "Event", "GetValue", "Init1", "Init2",
      "NotifyAboutToDelete", "NotifyRegistrationChange", "NotifySetValueSuccess",
      "NotifyTermination", "PauseSimulation", "PublishEvent", "QueryInfo",
      "QuerySetValue", "QueryValue", "Register", "ReinstateCheckpoint",
      "ReplySetValueSuccess", "ReplyValue",
      "RequestComponentID", "RequestSetValue", "ResumeSimulation",
      "ReturnComponentID", "ReturnInfo", "ReturnValue",
      "TerminateSimulation", "<unused>", "<unused>", "<unused>", "<unused>", "<unused>", "<unused>", "<unused>",
      "ApsimGetQuery", "ApsimSetQuery", "ApsimChangeOrder"};

int apsimGetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimRegisterGetSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSendEventProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimCatchMessages(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);

// ------------------------------------------------------------------
//  Short description:
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void __stdcall getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" _export void __stdcall getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
// ------------------------------------------------------------------
// Create an instance of the TclLink module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new TclComponent;
   }
// ------------------------------------------------------------------
// Initialises the Tcl component.
// ------------------------------------------------------------------
TclComponent::TclComponent()
   {
   Interp = NULL;
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
TclComponent::~TclComponent(void)
   {
   if (!terminationRule.empty()) { Tcl_Eval(Interp, terminationRule.c_str()); }

   if ( Interp != TopLevelInterp ) 
      Tcl_DeleteInterp(Interp);
   else 
      StopTcl(TopLevelInterp);
   }
// ------------------------------------------------------------------
// Stage 1 initialisation. We can initialise the TCL world now that we know where we are..
// ------------------------------------------------------------------
void TclComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);

   if (initialisationState == 0) {
     StartTcl(Component::componentData->getExecutableFileName().c_str());
     initialisationState = 1;
   }
   //MessageBox(0,  Component::componentData->getExecutableFileName().c_str(), "Init", MB_ICONSTOP);
   }

// ------------------------------------------------------------------
// Initialise the Tcl component.
// ------------------------------------------------------------------
void TclComponent::doInit2(void)
   {
   protocol::Component::doInit2();

   if (initialisationState == 1) {
      // write copyright notice(s).
      writeString("Copyright (C) 1991-1994 The Regents of the University of California.");
      writeString("Copyright (C) 1996-1997 Sun Microsystems, Inc.");
      writeString("Copyright (C) 2001      ActiveState.");
      TopLevelInterp = NewInterp(NULL, this, "toplevel");
   } 
   initialisationState++;

   string isMaster = this->readParameter ("parameters", "master");
   if (isMaster == string("true") || isMaster == string("yes")) {
      Interp = TopLevelInterp;
      writeString("Top Level Interpreter");
   } else {
      // Create a slave interpreter for this instance.
      Interp = NewInterp(TopLevelInterp, this, this->getName());  
      writeString((string("Interpreter name: '")+ this->getName() + "'").c_str());
   }
   
   string initRule;
   std::vector<string> ruleNames;
   componentData->getRuleNames(ruleNames);
   rules.clear();

   for (unsigned int i = 0; i != ruleNames.size(); i++)
      {
         string condition, rule;
         componentData->getRule(ruleNames[i], condition, rule);
         unsigned id = protocol::Component::addRegistration(RegistrationType::respondToEvent, condition.c_str(), "");
         rules.insert(UInt2StringMap::value_type(id, rule));

         string msg = "--->Section: " + condition;
         writeString(msg.c_str());
         writeString(rule.c_str());
         if (condition == string("init")) {initRule.append(rule);}
         if (condition == string("exit")) {terminationRule.append(rule);}
      }
   writeString("--->End");

   // Do the init rule if specified..
   if (!initRule.empty())
      {
      int result = Tcl_Eval(Interp, initRule.c_str());
      if (result != TCL_OK)
          {
          throw std::runtime_error(string(Tcl_GetStringResult(Interp)));
          }
      }
      //char buf[80]; sprintf(buf, "this=%x", this);
      //MessageBox(0,  buf, "Init", MB_ICONSTOP);
   }
// ------------------------------------------------------------------
// Look for messages.
// ------------------------------------------------------------------
void TclComponent::respondToEvent(unsigned int& /*fromID*/, unsigned int& eventID, protocol::Variant &variant)
   {
   UInt2StringMap::iterator ip, ip1, ip2;

   ip1 = rules.lower_bound(eventID);
   ip2 = rules.upper_bound(eventID);

   for (ip = ip1; ip != ip2; ip++)
     {
     string rule = ip->second;
     if (!rule.empty())
        {
        // Set the global variable "incomingApsimVariant" to the variant's binary data
        const MessageData message = variant.getMessageData();
        Tcl_ObjSetVar2(Interp, Tcl_NewStringObj("incomingApsimVariant",-1), NULL, 
                       Tcl_NewByteArrayObj( message.start(), message.totalBytes()), TCL_GLOBAL_ONLY);

        int result = Tcl_Eval(Interp, rule.c_str());
        if (result != TCL_OK)
           throw std::runtime_error(string(Tcl_GetStringResult(Interp)));
        }
     }  
   }
// ------------------------------------------------------------------
// Return a variable to caller.
// ------------------------------------------------------------------
void TclComponent::respondToGet(unsigned int& /*fromID*/, protocol::QueryValueData& queryData)
   {
   const char *variable = getRegistrationName(queryData.ID);
   if (variable != NULL) 
      {
      if (strchr(variable, '(') == NULL) 
         {
         // A scalar variable
         const char *result = Tcl_GetVar(Interp, variable, TCL_GLOBAL_ONLY);
         if (result != NULL)
            sendVariable(queryData, FString(result));
         }
      else 
         {
         // An array variable
         std::vector<string> nv;
         Split_string (variable, "()", nv);
         if (nv.size() != 2) {throw std::runtime_error("can't grok array variable called " + string(variable));}

         const char *result = Tcl_GetVar2(Interp, nv[0].c_str(), nv[1].c_str(), TCL_GLOBAL_ONLY);
         if (result != NULL)
            sendVariable(queryData, FString(result));
         }
      }
   }
// ------------------------------------------------------------------
// Something is asking whether we know about a variable. If we do, register it.
// ------------------------------------------------------------------
void TclComponent::onApsimGetQuery(protocol::ApsimGetQueryData& apsimGetQueryData)
   {
   if (Interp != NULL) 
      {
      std::vector<string> vs;
      Split_string (asString(apsimGetQueryData.name), "()", vs);
      const char *varName = vs[0].c_str();
      Var *varPtr = TclVarTraceExists(Interp, varName);
      if ((varPtr != NULL) && !TclIsVarUndefined(varPtr)) 
         {
            // It exists, so register it as rw..
            protocol::Component::addRegistration(RegistrationType::respondToGetSet, asString(apsimGetQueryData.name).c_str(), strString);
            // XX should register type info properly here XX
            
         }
      }
   }   
// ------------------------------------------------------------------
// Set the value of a variable for the specified
// variable name.  If this module owns the variable and does
// change it's value, return true.
// ------------------------------------------------------------------
bool TclComponent::respondToSet(unsigned int& /*fromID*/, protocol::QuerySetValueData& setValueData)
   {
   string newValue;

   setValueData.variant.unpack(newValue);

   string name = asString(getRegistrationName(setValueData.ID));

   const char *result = Tcl_SetVar(Interp, name.c_str(), newValue.c_str(), TCL_GLOBAL_ONLY);
   if (result != NULL)
      return true;
   return false;
   }

// (Called from TCL interpreter). Find component() and ask protoman for a variable
int apsimGetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc == 2)
      {
      TclComponent *component = (TclComponent *) cd;
      string apsimName(Tcl_GetStringFromObj(objv[1], NULL));
      return (component->apsimGet(interp, apsimName));
      }
   Tcl_SetResult(interp,"Wrong num args: apsimGet <variableName>", NULL);
   return TCL_ERROR;
   }

// Get an apsim variable into interp->result. avoid shimmering between strings:floats etc..
int TclComponent::apsimGet(Tcl_Interp *interp, const string &varname)
   {
   string ModuleName, VariableName;

   unsigned posPeriod = varname.find('.');
   if (posPeriod != string::npos)
      {
      ModuleName = varname.substr(0, posPeriod);
      VariableName = varname.substr(posPeriod+1);
      }
   else
      {
      ModuleName = string("");
      VariableName = varname;
      }

   unsigned variableID = protocol::Component::addRegistration(
                             RegistrationType::get,
                             VariableName.c_str(),
                             strString,  /* This is misleading? we need to decide what type type later */
                             "",
                             ModuleName.c_str());

   protocol::Variant *variant;
   if (protocol::Component::getVariable(variableID, variant, true))
      {
      switch (variant->getType().getCode()) {
      	case DTstring:                        /* strings*/
      	   {
            variant->setTypeConverter( NULL ); /* undo the typeconverter created above */
            if (variant->getType().isArray())
      	       {
                std::vector<string> scratch;
                variant->unpack(scratch);
                Tcl_Obj *result = Tcl_GetObjResult(interp);
                Tcl_SetListObj(result, 0, NULL);
                for (std::vector<string>::iterator p = scratch.begin(); p != scratch.end(); p++)
                   {
                   Tcl_ListObjAppendElement(interp, result, Tcl_NewStringObj((char *)(*p).c_str(), -1));
                   }
                return TCL_OK;
                }
             else
                {
                string scratch;
                variant->unpack(scratch);
                Tcl_Obj *result = Tcl_GetObjResult(interp);
                Tcl_SetStringObj(result, scratch.c_str(), -1);
                return TCL_OK;
                }
             /* notreached */
            }

      	case DTboolean:                       /* 4 byte integer */
      	case DTint4:                          /* 4 byte integer */
      	   {
            variant->setTypeConverter( NULL ); /* undo the typeconverter created above */
            if (variant->getType().isArray())
      	       {
                std::vector<int> scratch;
                variant->unpack(scratch);
                Tcl_Obj *result = Tcl_GetObjResult(interp);
                Tcl_SetListObj(result, 0, NULL);
                for (std::vector<int>::iterator p = scratch.begin(); p != scratch.end(); p++)
                   {
                   Tcl_ListObjAppendElement(interp, result, Tcl_NewIntObj((int)*p));
                   }
                return TCL_OK;
                }
             else
                {
                int scratch = 0;
                variant->unpack(scratch);
                Tcl_Obj *result = Tcl_GetObjResult(interp);
                Tcl_SetIntObj(result, scratch);
                return TCL_OK;
                }
             /* notreached */
            }
         case DTsingle:                       /* floats */
      	   {
            variant->setTypeConverter( NULL ); /* undo the typeconverter created above */
            if (variant->getType().isArray())
      	       {
                std::vector<float> scratch;
                variant->unpack(scratch);
                Tcl_Obj *result = Tcl_GetObjResult(interp);
                Tcl_SetListObj(result, 0, NULL);
                for (std::vector<float>::iterator p = scratch.begin(); p != scratch.end(); p++)
                   {
                   Tcl_ListObjAppendElement(interp, result, Tcl_NewDoubleObj((double)*p));
                   }
                return TCL_OK;
                }
             else
                {
                float scratch = 0.0;
                variant->unpack(scratch);
                Tcl_Obj *result = Tcl_GetObjResult(interp);
                Tcl_SetDoubleObj(result, (double) scratch);
                return TCL_OK;
                }
             /* notreached */
             }
         case DTdouble:                       /* floats */
      	   {
            variant->setTypeConverter( NULL ); /* undo the typeconverter created above */
            if (variant->getType().isArray())
      	       {
                std::vector<double> scratch;
                variant->unpack(scratch);
                Tcl_Obj *result = Tcl_GetObjResult(interp);
                Tcl_SetListObj(result, 0, NULL);
                for (std::vector<double>::iterator p = scratch.begin(); p != scratch.end(); p++)
                   {
                   Tcl_ListObjAppendElement(interp, result, Tcl_NewDoubleObj(*p));
                   }
                return TCL_OK;
                }
             else
                {
                double scratch = 0.0;
                variant->unpack(scratch);
                Tcl_Obj *result = Tcl_GetObjResult(interp);
                Tcl_SetDoubleObj(result, scratch);
                return TCL_OK;
                }
             /* notreached */
             }

        default:
             {
             Tcl_Obj *result = Tcl_GetObjResult(interp);
             char buf[80];
             sprintf(buf, "Undefined GET type %d (from %s)",
                     variant->getType().getCode(), varname.c_str());
             Tcl_SetStringObj(result, buf, -1);
             return TCL_ERROR;
             }
      	}
      }
      Tcl_Obj *result = Tcl_GetObjResult(interp);
      Tcl_SetStringObj(result, "Unknown variable ", -1);
      Tcl_AppendToObj(result, varname.c_str(), -1);
      return TCL_ERROR;
   }

// (Called from TCL interpreter.) Find component() and set apsim value
int apsimSetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc == 3)
      {
      TclComponent *component = (TclComponent *) cd;
      string apsimName = Tcl_GetStringFromObj(objv[1], NULL);
      if (component->apsimSet(interp, apsimName, objv[2]))
         {
         return TCL_OK;
         }
      Tcl_AppendResult(interp, "Can't set the apsim variable ", apsimName.c_str(), NULL);
      return TCL_ERROR;
      }
   Tcl_SetResult(interp,"Wrong num args: apsimSet <variableName> <value>", NULL);
   return TCL_ERROR;
   }

// Set an apsim variable
bool TclComponent::apsimSet(Tcl_Interp *interp, const string &varname, Tcl_Obj *value)
   {
   string ModuleName, VariableName;

   unsigned posPeriod = varname.find('.');
   if (posPeriod != string::npos)
      {
      ModuleName = varname.substr(0, posPeriod);
      VariableName = varname.substr(posPeriod+1);
      }
   else
      {
      ModuleName = string("");
      VariableName = varname;
      }

   Tcl_ObjType *listType = Tcl_GetObjType("list");
   Tcl_ObjType *intType = Tcl_GetObjType("int");
   Tcl_ObjType *dblType = Tcl_GetObjType("double");
   Tcl_ObjType *stringType = Tcl_GetObjType("string");

   Tcl_ObjType *outGoingType = value->typePtr;
   if (outGoingType == NULL) { outGoingType = stringType; } // Default type is strings

   //MessageBox(NULL, "Type 1", outGoingType->name, MB_ICONSTOP);
   // Is it an array?
   // NB. we can't really tell if it's a string that should be turned into a list  FIXME  XX
   bool isArray = false;
   if (outGoingType==listType)
      {
      isArray = true;
      Tcl_Obj *firstListElement;
      if (Tcl_ListObjIndex(interp, value, 0, &firstListElement) != TCL_OK)
          {
          Tcl_SetStringObj(Tcl_GetObjResult(interp), "Can't extract value from list ", -1);
          return false;
          }
      if (firstListElement==NULL)
          {
          Tcl_SetStringObj(Tcl_GetObjResult(interp), "FLE is NULL??? ", -1);
          return false;
          }
      outGoingType = firstListElement->typePtr;
      if (outGoingType == NULL) { outGoingType = stringType; }
      }

   //MessageBox(NULL, "Type 2", outGoingType->name, MB_ICONSTOP);
   if (outGoingType==intType)
      {
      unsigned variableID = protocol::Component::addRegistration(RegistrationType::set,
                          VariableName.c_str(), isArray?intStringArray:intString, "", ModuleName.c_str());
      if (isArray)
          {
          std::vector<int> outValue;
          Tcl_Obj *listElement;
          int      listLength;
          Tcl_ListObjLength(interp, value, &listLength);
          for (int idx = 0; idx < listLength; idx++)
              {
              int scratch;
              Tcl_ListObjIndex(interp, value, idx, &listElement);
              Tcl_GetIntFromObj(interp, listElement, &scratch);
              outValue.push_back(scratch);
              }
          return (setVariable(variableID, outValue));
          }
      else
          {
          int outValue;
          Tcl_GetIntFromObj(interp, value, &outValue);
          return (setVariable(variableID, outValue));
          }
      /* notreached*/
      }
   else if (outGoingType==dblType)
      {
      unsigned variableID = protocol::Component::addRegistration(RegistrationType::set,
                          VariableName.c_str(), isArray?fltStringArray:fltString, "", ModuleName.c_str());
      if (isArray)
          {
          std::vector<float> outValue;
          Tcl_Obj *listElement;
          int      listLength;
          Tcl_ListObjLength(interp, value, &listLength);
          for (int idx = 0; idx < listLength; idx++)
              {
              double scratch;
              Tcl_ListObjIndex(interp, value, idx, &listElement);
              Tcl_GetDoubleFromObj(interp, listElement, &scratch);
              outValue.push_back((float)scratch);
              }
          return (setVariable(variableID, outValue));
          }
      else
          {
          double outValue;
          Tcl_GetDoubleFromObj(interp, value, &outValue);
          return (setVariable(variableID, (float)outValue));
          }
      /* notreached*/
      }
   else if (outGoingType==stringType)
      {
      unsigned variableID = protocol::Component::addRegistration(RegistrationType::set,
                          VariableName.c_str(), isArray?strStringArray:strString, "", ModuleName.c_str());
      if (isArray)
          {
          std::vector<string> outValue;
          Tcl_Obj *listElement;
          int      listLength;
          Tcl_ListObjLength(interp, value, &listLength);
          for (int idx = 0; idx < listLength; idx++)
              {
              Tcl_ListObjIndex(interp, value, idx, &listElement);
              string scratch = string(Tcl_GetStringFromObj(listElement, NULL));
              outValue.push_back(scratch);
              }
          return (setVariable(variableID, outValue));
          }
      else
          {
          string outValue = string(Tcl_GetStringFromObj(value, NULL));
          return (setVariable(variableID, outValue));
          }
      /* notreached*/
      }

   // Failure
   Tcl_AppendStringsToObj(Tcl_GetObjResult(interp), "Unknown SET type '",
                          outGoingType->name, "' (to ", varname.c_str(), ")",
                          -1);
   return false;
   }

void TclComponent::addRegistration(const string &name)
   {
   protocol::Component::addRegistration(RegistrationType::set, name.c_str(), strString);
   protocol::Component::addRegistration(RegistrationType::get, name.c_str(), strString);
   }

// Called from TCL script. Tell protoman of something we own
int apsimRegisterGetSetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc == 2)
      {
      TclComponent *component = (TclComponent *) cd;
      string name(Tcl_GetStringFromObj(objv[1], NULL));
      component->addRegistration(name);
      return TCL_OK;
      }
   Tcl_SetResult(interp, "Couldn't register variable", NULL);
   return TCL_ERROR;
   }

// Called from TCL script. Send a message to the system, eg:
// apsimSendMessage wheat sow {plants 100} {depth 20}
int apsimSendMessageProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
      TclComponent *component = (TclComponent *) cd;

      if (objc < 3)
         {
         Tcl_SetResult(interp,"Wrong num args: apsimSendEvent <moduleName> <message> {<name> <value>} ...]", NULL);
         return TCL_ERROR;
         }

      // 1. destination
      char *moduleName = Tcl_GetStringFromObj(objv[1], NULL);
      char *actionName = Tcl_GetStringFromObj(objv[2], NULL);

      // 2. build variant
      protocol::ApsimVariant outgoingApsimVariant(component);
      outgoingApsimVariant.store(FString("sender"), protocol::DTstring, false, FString(component->getName()));
      outgoingApsimVariant.store(FString("sender_id"), protocol::DTint4, false, (int)component->getId());

      for (int i = 3; i < objc; i++)
         {
         int check = 0;
         if (Tcl_ListObjLength(interp, objv[i], &check) != TCL_OK) {
            Tcl_SetResult(interp,"arg not a list??", NULL);
            return TCL_ERROR;
         }
         
         if (check != 2) {
            Tcl_SetResult(interp,"arg format error: apsimSendEvent <moduleName> <message> {<name> <value>} ...", NULL);
            return TCL_ERROR;
         }

         Tcl_Obj *firstListElement, *secondListElement;
         Tcl_ListObjIndex(interp, objv[i], 0, &firstListElement);
         Tcl_ListObjIndex(interp, objv[i], 1, &secondListElement);
         
         if (firstListElement==NULL || secondListElement==NULL)
            {
            Tcl_SetStringObj(Tcl_GetObjResult(interp), "LE is NULL??? ", -1);
            return TCL_ERROR;
            }

         FString variableName = FString(Tcl_GetStringFromObj(firstListElement, NULL));

         FString variableValue = FString(Tcl_GetStringFromObj(secondListElement, NULL));

         bool isArray = false;    // XX wrong.. Should test secondListElement and be sure..

         outgoingApsimVariant.store(variableName, protocol::DTstring, isArray, variableValue);
         }

      // Send it
      component->sendMessage(moduleName, actionName, outgoingApsimVariant);
      return TCL_OK;
   }

void TclComponent::sendMessage(const char *moduleName, const char *actionName,
                               protocol::ApsimVariant &outgoingApsimVariant)
   {
   unsigned actionID = protocol::Component::addRegistration(RegistrationType::event,
                                                            actionName,
                                                            "<type\>",
                                                            "",
                                                            moduleName);
   publish(actionID, outgoingApsimVariant);
   }

int apsimWriteToSummaryFileProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   if (objc != 2)
         {
         Tcl_SetResult(interp,"Wrong num args: apsimWriteToSummaryFile <message>", NULL);
         return TCL_ERROR;
         }
   char *message = Tcl_GetStringFromObj(objv[1], NULL);
   component->writeString(FString(message));
   return TCL_OK;
}

int apsimRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   if (objc != 3)
         {
         Tcl_SetResult(interp, "Wrong num args: apsimRegisterEvent <eventName> <script>", NULL);
         return TCL_ERROR;
         }
   string eventName = string(Tcl_GetStringFromObj(objv[1], NULL));
   string script = string(Tcl_GetStringFromObj(objv[2], NULL));
   
   unsigned int id = component->registerEvent(eventName, script);
   Tcl_SetObjResult(interp, Tcl_NewIntObj((int) id));
   return TCL_OK;
}

int apsimUnRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   if (objc != 2)
         {
         Tcl_SetResult(interp, "Wrong num args: apsimUnRegisterEvent <id>", NULL);
         return TCL_ERROR;
         }
   unsigned int id = Tcl_GetIntFromObj(interp, objv[1], NULL);
   
   component->unRegisterEvent(id);
   return TCL_OK;
}

int apsimCatchMessages(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
   TclComponent *component = (TclComponent *) cd;
   if (objc != 2)
         {
         Tcl_SetResult(interp, "Wrong num args: apsimCatchMessages <command>", NULL);
         return TCL_ERROR;
         }
   string command = string(Tcl_GetStringFromObj(objv[1], NULL));
   
   component->catchMessages(command);
   return TCL_OK;
}

unsigned int TclComponent::registerEvent(string &eventName, string &script)
   {
   unsigned id = protocol::Component::addRegistration(RegistrationType::respondToEvent, eventName.c_str(), "");
   rules.insert(UInt2StringMap::value_type(id, script));

   if (eventName == string("exit")) {terminationRule.append(script);}
   return id;
   }

void TclComponent::unRegisterEvent(unsigned int id) 
   {
   throw("TclComponent::unRegisterEvent not implemented");
   }

void TclComponent::catchMessages(string &command)
   {
   messageCallbackCommand = command;
   if (command == "")
      setMessageHook(NULL);
   else
      setMessageHook(this);
   }

void TclComponent::callback(const std::string& toName, const protocol::Message* message)
   {
   if (messageCallbackCommand != "") 
      {
      int ncmd = 1;
      Tcl_Obj **cmd = (Tcl_Obj**)malloc(20*sizeof(Tcl_Obj*));
      cmd[0] = Tcl_NewStringObj(messageCallbackCommand.c_str(), -1);

      if (toName != "")
         {
         cmd[ncmd] = Tcl_NewListObj(0, NULL);
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("toName",-1));
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj(toName.c_str(),-1));
         ncmd++;
         } 

      if (message != NULL) 
         {
         cmd[ncmd] = Tcl_NewListObj(0, NULL);
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("from",-1));
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewIntObj(message->from));
         ncmd++;

         cmd[ncmd] = Tcl_NewListObj(0, NULL);
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("to",-1));
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewIntObj(message->to));
         ncmd++;

         cmd[ncmd] = Tcl_NewListObj(0, NULL);
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("id",-1));
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewIntObj(message->messageID));
         ncmd++;

         if (message->messageType >= 45) {throw "messageType oob in TclComponent::callback";}
         if (message->messageType > 0) 
            {
            cmd[ncmd] = Tcl_NewListObj(0, NULL);
            Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("type",-1));
            Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj(messageNames[message->messageType-1],-1));
            ncmd++;
            }
    
         switch(message->messageType)
            {
            case protocol::Event:
               {
               protocol::MessageData messageData(message->dataPtr, message->nDataBytes);
               protocol::EventData eventData;
               messageData >> eventData;
               cmd[ncmd] = Tcl_NewListObj(0, NULL);
               Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("name",-1));
               Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj(getRegistrationName(eventData.ID),-1));
               ncmd++;
               break;
               }
            case protocol::GetValue:
               {
               protocol::MessageData messageData(message->dataPtr, message->nDataBytes);
               protocol::GetValueData getValueData;
               messageData >> getValueData;
               cmd[ncmd] = Tcl_NewListObj(0, NULL);
               Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("name",-1));
               Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj(getRegistrationName(getValueData.ID),-1));
               ncmd++;
               break;
               }
            }

         cmd[ncmd] = Tcl_NewListObj(0, NULL);
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewStringObj("data",-1));
         Tcl_ListObjAppendElement(Interp, cmd[ncmd], Tcl_NewByteArrayObj(message->dataPtr,message->nDataBytes));
         ncmd++;
         }

      int res = Tcl_EvalObjv(Interp, ncmd, cmd, TCL_EVAL_GLOBAL);

      if (res != TCL_OK) 
         fprintf(stdout, "messageHook error, result=%s\n", Tcl_GetStringResult(Interp));

      //for (int obj = 0; obj < ncmd; obj++)
      //  Tcl_DecrRefCount(cmd[obj]);

      free(cmd);
      }
   }