//---------------------------------------------------------------------------
#include <general/pch.h>
#include <vcl.h>

#pragma hdrstop

#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/FStringExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <general/string_functions.h>
#include <general/stristr.h>
#include <tcl.h>
#include "TclComponent.h"

#pragma package(smart_init)

using namespace std;
using namespace protocol;

extern Tcl_Interp *StartTcl (ClientData, const char *);
extern void StopTcl(Tcl_Interp *);

// The main Tcl interpreter for this dll. Set during CreateComponent()
static Tcl_Interp *Interp = NULL;
static int InterpRefCnt = 0;

#define intString      "<type kind=\"integer4\" array=\"F\"/>"
#define intStringArray "<type kind=\"integer4\" array=\"T\"/>"
#define fltString      "<type kind=\"single\" array=\"F\"/>"
#define fltStringArray "<type kind=\"single\" array=\"T\"/>"
#define strString      "<type kind=\"string\" array=\"F\"/>"
#define strStringArray "<type kind=\"string\" array=\"T\"/>"

int apsimGetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimRegisterGetSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSendEventProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);

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
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
TclComponent::~TclComponent(void)
   {
   InterpRefCnt--;
   if (InterpRefCnt == 0)
      {
      StopTcl(Interp);
      }
   }
// ------------------------------------------------------------------
// Stage 1 initialisation. We can initialise the TCL world now that wee know where we are..
// ------------------------------------------------------------------
void TclComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);

   if (InterpRefCnt == 0)
      {
      if ((Interp = StartTcl(this, protocol::Component::componentData->getExecutableFileName().c_str())) == NULL)
          {
          error("TCL initialisation failed", true);
          }
      }
   InterpRefCnt++;
   //MessageBox(0,  Tcl_GetStringResult(Interp), "TCl Init Done", MB_ICONSTOP);
   //MessageBox(0,  Component::componentData->getExecutableFileName().c_str(), "Init", MB_ICONSTOP);
   }
// ------------------------------------------------------------------
// Initialise the Tcl component.
// ------------------------------------------------------------------
void TclComponent::doInit2(void)
   {
   protocol::Component::doInit2();

   // write copyright notice(s).
   writeString("Copyright (C) 1991-1994 The Regents of the University of California.");
   writeString("Copyright (C) 1996-1997 Sun Microsystems, Inc.");
   writeString("Copyright (C) 2001      ActiveState.");

   string initRule;
   std::vector<string> ruleNames;
   componentData->getRuleNames(ruleNames);
   rules.clear();

   for (unsigned int i = 0; i != ruleNames.size(); i++)
      {
         string condition, rule;
         componentData->getRule(ruleNames[i], condition, rule);
         unsigned id = protocol::Component::addRegistration(protocol::respondToEventReg, condition.c_str(), "");
         rules.insert(UInt2StringMap::value_type(id, rule));

         string msg = "Section: " + condition;
         writeString(msg.c_str());
         writeString(rule.c_str());
         if (condition == string("init")) {initRule = rule;}
      }
   writeString("End");

   // Do the init rule if specified..
   if (!initRule.empty())
      {
      int result = Tcl_Eval(Interp, initRule.c_str());
      if (result != TCL_OK)
          {
          error(Tcl_GetStringResult(Interp), true);
          }
      }
      //char buf[80]; sprintf(buf, "this=%x", this);
      //MessageBox(0,  buf, "Init", MB_ICONSTOP);
   }
// ------------------------------------------------------------------
// Look for messages.
// ------------------------------------------------------------------
void TclComponent::respondToEvent(unsigned int& /*fromID*/, unsigned int& eventID, protocol::Variant& /*variant*/)
   {
   string rule = rules[eventID];
   if (!rule.empty())
       {
       int result = Tcl_Eval(Interp, rule.c_str());
       if (result != TCL_OK)
           {
           error(Tcl_GetStringResult(Interp), true);
           }
       }
   }
// ------------------------------------------------------------------
// Return a variable to caller.
// ------------------------------------------------------------------
void TclComponent::respondToGet(unsigned int& /*fromID*/, protocol::QueryValueData& queryData)
   {
   string variable = variables[queryData.ID];
   if (!variable.empty()) {
      const char *result = Tcl_GetVar(Interp, variable.c_str(), TCL_GLOBAL_ONLY);
      if (result != NULL)
         {
         sendVariable(queryData, result);
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

   string name = variables[setValueData.ID];

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
                             protocol::getVariableReg,
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
      unsigned variableID = protocol::Component::addRegistration(protocol::setVariableReg,
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
      unsigned variableID = protocol::Component::addRegistration(protocol::setVariableReg,
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
      unsigned variableID = protocol::Component::addRegistration(protocol::setVariableReg,
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
   unsigned id = protocol::Component::addRegistration(protocol::respondToGetSetReg,
                        	      name.c_str(),
                           	   strString);
   variables.insert(UInt2StringMap::value_type(id, name));
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
// apsimSendEvent wheat sow plants=100 depth=20 
int apsimSendEventProc(ClientData /*cd*/, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
      if (objc < 3) 
         {
         Tcl_SetResult(interp,"Wrong num args: apsimSendEvent <moduleName> <message> [<name value> ...]", NULL);
         return TCL_ERROR;
         }
      string moduleName = Tcl_GetStringFromObj(objv[1], NULL);
      string message = Tcl_GetStringFromObj(objv[2], NULL);
      
      return TCL_OK;   
   }