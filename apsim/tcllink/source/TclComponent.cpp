//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TclComponent.h"
#include <ComponentInterface\MessageDataExt.h>
#include <ApsimShared\FStringExt.h>
#include <ApsimShared\ApsimComponentData.h>

#include <general\string_functions.h>
#include <general\stristr.h>
#include <tcl.h>
#pragma package(smart_init)

using namespace std;
using namespace protocol;

extern Tcl_Interp *StartTcl (ClientData, const char *);
extern void StopTcl(Tcl_Interp *);

// The main Tcl interpreter for this dll. Set during CreateComponent()
static Tcl_Interp *Interp = NULL;
static int InterpRefCnt = 0;

//static const char* stringArrayType = "<type kind=\"string\" array=\"T\">";
static const char* stringType = "<type kind=\"string\" array=\"F\">";
int apsimGetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimRegisterGetSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSendEventProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);

// ------------------------------------------------------------------
// Create an instance of the TclLink module
// ------------------------------------------------------------------
Component* createComponent(void)
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
   Component::doInit1(sdml);

   if (InterpRefCnt == 0)
      {
      if ((Interp = StartTcl(this, Component::componentData->getExecutableFileName().c_str())) == NULL)
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
   Component::doInit2();

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
         unsigned id = Component::addRegistration(protocol::respondToEventReg, condition.c_str(), "");
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
bool TclComponent::respondToSet(unsigned int& /*fromID*/, QuerySetValueData& setValueData)
   {
   string newValue;
   setValueData.variant.unpack(newValue);
   string name = variables[setValueData.ID];

   const char *result = Tcl_SetVar(Interp, name.c_str(), newValue.c_str(), TCL_GLOBAL_ONLY);
   if (result != NULL)
      return true;
   return false;
   }

// Get an apsim variable
bool TclComponent::apsimGet(const string &variable, string &result)
   {
   unsigned posPeriod = variable.find('.');
   if (posPeriod != string::npos)
      {
      string ModuleName = variable.substr(0, posPeriod);
      string VariableName = variable.substr(posPeriod+1);

      unsigned variableID = Component::addRegistration(protocol::getVariableReg,
                                VariableName.c_str(),
                                stringType,
                                "",
                                ModuleName.c_str());

      protocol::Variant* variant;
      if (getVariable(variableID, variant, true))
         {
         return (variant->unpack(result));
         }
      }
   return false;
   }

// Set an apsim variable
bool TclComponent::apsimSet(const string &variable, const string &value)
   {
   unsigned posPeriod = variable.find('.');
   if (posPeriod != string::npos)
      {
      string ModuleName = variable.substr(0, posPeriod);
      string VariableName = variable.substr(posPeriod+1);

      unsigned variableID = Component::addRegistration(protocol::setVariableReg,
                                VariableName.c_str(),
                                stringType,
                                "",
                                ModuleName.c_str());
      return (setVariable(variableID, value));
      }
   return false;
   }
void TclComponent::addRegistration(const string &name) 
   {
   unsigned id = Component::addRegistration(protocol::respondToGetSetReg,
                        	      name.c_str(),
                           	   stringType);
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

// Called from TCL script. Find component() and ask protoman
int apsimGetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc == 2)
      {
      TclComponent *component = (TclComponent *) cd;
      string apsimName(Tcl_GetStringFromObj(objv[1], NULL));
      string result;
      if (component->apsimGet(apsimName, result))
         {
         Tcl_SetResult(interp, const_cast<char *> (result.c_str()), TCL_VOLATILE);
         return TCL_OK;
         }
      Tcl_SetResult(interp,"No such variable in apsim", NULL);
      return TCL_ERROR;
      }
   Tcl_SetResult(interp,"Wrong num args: apsimGet <variableName>", NULL);
   return TCL_ERROR;
   }

// Called from TCL script. Find component() and tell protoman
int apsimSetProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
   {
   if (objc == 3)
      {
      TclComponent *component = (TclComponent *) cd;
      string apsimName = Tcl_GetStringFromObj(objv[1], NULL);
      string value = Tcl_GetStringFromObj(objv[2], NULL);
      if (component->apsimSet(apsimName, value))
         {
         Tcl_SetResult(interp, const_cast<char *> (value.c_str()),NULL);
         return TCL_OK;
         }
      Tcl_SetResult(interp,"Can't set variable in apsim", NULL);
      return TCL_ERROR;
      }
   Tcl_SetResult(interp,"Wrong num args: apsimSet <variableName> <value>", NULL);
   return TCL_ERROR;
   }

// Called from TCL script. Send a message to the system, eg:
// apsimSendEvent wheat sow plants 100 depth 20 
int apsimSendEventProc(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
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