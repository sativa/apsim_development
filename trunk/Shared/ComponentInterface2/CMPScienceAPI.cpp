#include "CMPData.h"
#include "CMPComponentInterface.h"
#include "CMPScienceAPI.h"
#include "Variant.h"

using namespace std;
CMPScienceAPI::CMPScienceAPI(CMPComponentInterface& componentinterface)
   : componentInterface(componentinterface){}

string CMPScienceAPI::name()
   {return componentInterface.getName();}
string CMPScienceAPI::FQName()
   {return componentInterface.getFQName();}
void CMPScienceAPI::write(const string& msg)
   {
   componentInterface.write(msg);
   }

void CMPScienceAPI::query(const string& pattern, vector<QueryMatch>& matches)
   {
   componentInterface.query(pattern, matches);
   }

void CMPScienceAPI::setSearchOrder(const std::vector<string> &list) {componentInterface.setSearchOrder(list);};
void CMPScienceAPI::getSearchOrder(std::vector<string> &list) {componentInterface.getSearchOrder(list);};

bool CMPScienceAPI::readRaw(const string& parName, vector<string> &values)
   {return componentInterface.readRaw(parName, values);}

// null
void CMPScienceAPI::subscribe(const std::string& name, boost::function0<void> handler)
   {componentInterface.subscribe(name, new CMPMethod0 (handler));}
void CMPScienceAPI::publish(const std::string& name)
   {
   Null dummy;
   componentInterface.publish(name, new CMPType<Null>(dummy));
   }

// bool
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, bool& data)
   {
   IPackableData* temp = new CMPBuiltIn<bool >(data);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, bool& data)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltIn<bool >(data));
   }

void CMPScienceAPI::set(const std::string& name, const std::string& units, bool& data)
   {componentInterface.set(name, units, new CMPBuiltIn<bool >(data));}
void CMPScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, bool writable, bool& variable)
   {componentInterface.expose(name, units, description, writable, new CMPBuiltIn<bool >(variable));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, bool&> fn)
   {componentInterface.expose(name, units, description, false, new CMPMethod1<boost::function1<void, bool&>, bool >(fn, true));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                   boost::function1<void, bool&> getter,
                                   boost::function1<void, bool&> setter)
   {
   componentInterface.expose(name, units, description, true,
                             new CMPMethod1<boost::function1<void, bool&>, bool > (getter, setter));
   }
void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, bool&> handler)
   {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, bool&>, bool > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, bool& data)
   {componentInterface.publish(name, new CMPBuiltIn<bool >(data));}

// int
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, int& data)
   {
   IPackableData* temp = new CMPBuiltIn<int >(data);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, int& data, int lower, int upper)
   {
   IPackableData* temp = new CMPBuiltInBounded<int, int >(name, data, lower, upper);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, int& data)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltIn<int >(data));
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, int& data, int lower, int upper)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltInBounded<int, int >(name, data, lower, upper));
   }

void CMPScienceAPI::set(const std::string& name, const std::string& units, int& data)
   {componentInterface.set(name, units, new CMPBuiltIn<int >(data));}
void CMPScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, bool writable, int& variable)
   {componentInterface.expose(name, units, description, writable, new CMPBuiltIn<int >(variable));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, int&> fn)
   {componentInterface.expose(name, units, description, false, new CMPMethod1<boost::function1<void, int&>, int >(fn, true));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                   boost::function1<void, int&> getter,
                                   boost::function1<void, int&> setter)
   {
   componentInterface.expose(name, units, description, true,
                             new CMPMethod1<boost::function1<void, int&>, int > (getter, setter));
   }
void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, int&> handler)
   {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, int&>, int > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, int& data)
   {componentInterface.publish(name, new CMPBuiltIn<int >(data));}

// float
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, float& data)
   {
   IPackableData* temp = new CMPBuiltIn<float >(data);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, float& data, float lower, float upper)
   {
   IPackableData* temp = new CMPBuiltInBounded<float, float >(name, data, lower, upper);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, float& data)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltIn<float >(data));
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, float& data, float lower, float upper)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltInBounded<float, float >(name, data, lower, upper));
   }

void CMPScienceAPI::set(const std::string& name, const std::string& units, float& data)
   {componentInterface.set(name, units, new CMPBuiltIn<float >(data));}
void CMPScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, bool writable, float& variable)
   {componentInterface.expose(name, units, description, writable, new CMPBuiltIn<float >(variable));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, float&> fn)
   {componentInterface.expose(name, units, description, false, new CMPMethod1<boost::function1<void, float&>, float >(fn, true));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                   boost::function1<void, float&> getter,
                                   boost::function1<void, float&> setter)
   {
   componentInterface.expose(name, units, description, true,
                             new CMPMethod1<boost::function1<void, float&>, float > (getter, setter));
   }
void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, float&> handler)
   {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, float&>, float > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, float& data)
   {componentInterface.publish(name, new CMPBuiltIn<float >(data));}

// double
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, double& data)
   {
   IPackableData* temp = new CMPBuiltIn<double >(data);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, double& data, double lower, double upper)
   {
   IPackableData* temp = new CMPBuiltInBounded<double, double >(name, data, lower, upper);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, double& data)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltIn<double >(data));
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, double& data, double lower, double upper)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltInBounded<double, double >(name, data, lower, upper));
   }

void CMPScienceAPI::set(const std::string& name, const std::string& units, double& data)
   {componentInterface.set(name, units, new CMPBuiltIn<double >(data));}
void CMPScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, bool writable, double& variable)
   {componentInterface.expose(name, units, description, writable, new CMPBuiltIn<double >(variable));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, double&> fn)
   {componentInterface.expose(name, units, description, false, new CMPMethod1<boost::function1<void, double&>, double >(fn, true));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                   boost::function1<void, double&> getter,
                                   boost::function1<void, double&> setter)
   {
   componentInterface.expose(name, units, description, true,
                             new CMPMethod1<boost::function1<void, double&>, double > (getter, setter));
   }
void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, double&> handler)
   {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, double&>, double > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, double& data)
   {componentInterface.publish(name, new CMPBuiltIn<double >(data));}

// std::string
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::string& data)
   {
   IPackableData* temp = new CMPBuiltIn<std::string >(data);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, std::string& data)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltIn<std::string >(data));
   }

void CMPScienceAPI::set(const std::string& name, const std::string& units, std::string& data)
   {componentInterface.set(name, units, new CMPBuiltIn<std::string >(data));}
void CMPScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::string& variable)
   {componentInterface.expose(name, units, description, writable, new CMPBuiltIn<std::string >(variable));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::string&> fn)
   {componentInterface.expose(name, units, description, false, new CMPMethod1<boost::function1<void, std::string&>, std::string >(fn, true));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                   boost::function1<void, std::string&> getter,
                                   boost::function1<void, std::string&> setter)
   {
   componentInterface.expose(name, units, description, true,
                             new CMPMethod1<boost::function1<void, std::string&>, std::string > (getter, setter));
   }
void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, std::string&> handler)
   {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, std::string&>, std::string > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, std::string& data)
   {componentInterface.publish(name, new CMPBuiltIn<std::string >(data));}

// std::vector<bool>
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<bool>& data)
   {
   IPackableData* temp = new CMPBuiltIn<std::vector<bool> >(data);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, std::vector<bool>& data)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltIn<std::vector<bool> >(data));
   }

void CMPScienceAPI::set(const std::string& name, const std::string& units, std::vector<bool>& data)
   {componentInterface.set(name, units, new CMPBuiltIn<std::vector<bool> >(data));}
void CMPScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::vector<bool>& variable)
   {componentInterface.expose(name, units, description, writable, new CMPBuiltIn<std::vector<bool> >(variable));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::vector<bool>&> fn)
   {componentInterface.expose(name, units, description, false, new CMPMethod1<boost::function1<void, std::vector<bool>&>, std::vector<bool> >(fn, true));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                   boost::function1<void, std::vector<bool>&> getter,
                                   boost::function1<void, std::vector<bool>&> setter)
   {
   componentInterface.expose(name, units, description, true,
                             new CMPMethod1<boost::function1<void, std::vector<bool>&>, std::vector<bool> > (getter, setter));
   }
void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, std::vector<bool>&> handler)
   {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, std::vector<bool>&>, std::vector<bool> > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, std::vector<bool>& data)
   {componentInterface.publish(name, new CMPBuiltIn<std::vector<bool> >(data));}

// std::vector<int>
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<int>& data)
   {
   IPackableData* temp = new CMPBuiltIn<std::vector<int> >(data);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<int>& data, int lower, int upper)
   {
   IPackableData* temp = new CMPBuiltInBounded<std::vector<int>, int >(name, data, lower, upper);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, std::vector<int>& data)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltIn<std::vector<int> >(data));
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, std::vector<int>& data, int lower, int upper)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltInBounded<std::vector<int>, int >(name, data, lower, upper));
   }

void CMPScienceAPI::set(const std::string& name, const std::string& units, std::vector<int>& data)
   {componentInterface.set(name, units, new CMPBuiltIn<std::vector<int> >(data));}
void CMPScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::vector<int>& variable)
   {componentInterface.expose(name, units, description, writable, new CMPBuiltIn<std::vector<int> >(variable));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::vector<int>&> fn)
   {componentInterface.expose(name, units, description, false, new CMPMethod1<boost::function1<void, std::vector<int>&>, std::vector<int> >(fn, true));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                   boost::function1<void, std::vector<int>&> getter,
                                   boost::function1<void, std::vector<int>&> setter)
   {
   componentInterface.expose(name, units, description, true,
                             new CMPMethod1<boost::function1<void, std::vector<int>&>, std::vector<int> > (getter, setter));
   }
void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, std::vector<int>&> handler)
   {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, std::vector<int>&>, std::vector<int> > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, std::vector<int>& data)
   {componentInterface.publish(name, new CMPBuiltIn<std::vector<int> >(data));}

// std::vector<float>
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<float>& data)
   {
   IPackableData* temp = new CMPBuiltIn<std::vector<float> >(data);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<float>& data, float lower, float upper)
   {
   IPackableData* temp = new CMPBuiltInBounded<std::vector<float>, float >(name, data, lower, upper);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, std::vector<float>& data)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltIn<std::vector<float> >(data));
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, std::vector<float>& data, float lower, float upper)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltInBounded<std::vector<float>, float >(name, data, lower, upper));
   }

void CMPScienceAPI::set(const std::string& name, const std::string& units, std::vector<float>& data)
   {componentInterface.set(name, units, new CMPBuiltIn<std::vector<float> >(data));}
void CMPScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::vector<float>& variable)
   {componentInterface.expose(name, units, description, writable, new CMPBuiltIn<std::vector<float> >(variable));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::vector<float>&> fn)
   {componentInterface.expose(name, units, description, false, new CMPMethod1<boost::function1<void, std::vector<float>&>, std::vector<float> >(fn, true));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                   boost::function1<void, std::vector<float>&> getter,
                                   boost::function1<void, std::vector<float>&> setter)
   {
   componentInterface.expose(name, units, description, true,
                             new CMPMethod1<boost::function1<void, std::vector<float>&>, std::vector<float> > (getter, setter));
   }
void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, std::vector<float>&> handler)
   {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, std::vector<float>&>, std::vector<float> > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, std::vector<float>& data)
   {componentInterface.publish(name, new CMPBuiltIn<std::vector<float> >(data));}

// std::vector<double>
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<double>& data)
   {
   IPackableData* temp = new CMPBuiltIn<std::vector<double> >(data);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<double>& data, double lower, double upper)
   {
   IPackableData* temp = new CMPBuiltInBounded<std::vector<double>, double >(name, data, lower, upper);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, std::vector<double>& data)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltIn<std::vector<double> >(data));
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, std::vector<double>& data, double lower, double upper)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltInBounded<std::vector<double>, double >(name, data, lower, upper));
   }

void CMPScienceAPI::set(const std::string& name, const std::string& units, std::vector<double>& data)
   {componentInterface.set(name, units, new CMPBuiltIn<std::vector<double> >(data));}
void CMPScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::vector<double>& variable)
   {componentInterface.expose(name, units, description, writable, new CMPBuiltIn<std::vector<double> >(variable));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::vector<double>&> fn)
   {componentInterface.expose(name, units, description, false, new CMPMethod1<boost::function1<void, std::vector<double>&>, std::vector<double> >(fn, true));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                   boost::function1<void, std::vector<double>&> getter,
                                   boost::function1<void, std::vector<double>&> setter)
   {
   componentInterface.expose(name, units, description, true,
                             new CMPMethod1<boost::function1<void, std::vector<double>&>, std::vector<double> > (getter, setter));
   }
void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, std::vector<double>&> handler)
   {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, std::vector<double>&>, std::vector<double> > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, std::vector<double>& data)
   {componentInterface.publish(name, new CMPBuiltIn<std::vector<double> >(data));}

// std::vector<std::string>
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<std::string>& data)
   {
   IPackableData* temp = new CMPBuiltIn<std::vector<std::string> >(data);
   bool ok = componentInterface.read(name, temp, optional);
   delete temp;
   return ok;
   }
bool CMPScienceAPI::get(const std::string& name, const std::string& units, bool optional, std::vector<std::string>& data)
   {
   return componentInterface.get(name, units, optional, new CMPBuiltIn<std::vector<std::string> >(data));
   }

void CMPScienceAPI::set(const std::string& name, const std::string& units, std::vector<std::string>& data)
   {componentInterface.set(name, units, new CMPBuiltIn<std::vector<std::string> >(data));}
void CMPScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, bool writable, std::vector<std::string>& variable)
   {componentInterface.expose(name, units, description, writable, new CMPBuiltIn<std::vector<std::string> >(variable));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, std::vector<std::string>&> fn)
   {componentInterface.expose(name, units, description, false, new CMPMethod1<boost::function1<void, std::vector<std::string>&>, std::vector<std::string> >(fn, true));}
void CMPScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description,
                                   boost::function1<void, std::vector<std::string>&> getter,
                                   boost::function1<void, std::vector<std::string>&> setter)
   {
   componentInterface.expose(name, units, description, true,
                             new CMPMethod1<boost::function1<void, std::vector<std::string>&>, std::vector<std::string> > (getter, setter));
   }
void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, std::vector<std::string>&> handler)
   {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, std::vector<std::string>&>, std::vector<std::string> > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, std::vector<std::string>& data)
   {componentInterface.publish(name, new CMPBuiltIn<std::vector<std::string> >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, CompleteType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, CompleteType&>, CompleteType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, CompleteType& data)
     {componentInterface.publish(name, new CMPType< CompleteType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, ErrorType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, ErrorType&>, ErrorType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, ErrorType& data)
     {componentInterface.publish(name, new CMPType< ErrorType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, EventType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, EventType&>, EventType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, EventType& data)
     {componentInterface.publish(name, new CMPType< EventType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, GetValueType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, GetValueType&>, GetValueType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, GetValueType& data)
     {componentInterface.publish(name, new CMPType< GetValueType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, Init1Type&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, Init1Type&>, Init1Type > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, Init1Type& data)
     {componentInterface.publish(name, new CMPType< Init1Type >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, NotifySetValueSuccessType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, NotifySetValueSuccessType&>, NotifySetValueSuccessType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, NotifySetValueSuccessType& data)
     {componentInterface.publish(name, new CMPType< NotifySetValueSuccessType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PublishEventType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PublishEventType&>, PublishEventType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PublishEventType& data)
     {componentInterface.publish(name, new CMPType< PublishEventType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, QueryInfoType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, QueryInfoType&>, QueryInfoType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, QueryInfoType& data)
     {componentInterface.publish(name, new CMPType< QueryInfoType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, RegisterType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, RegisterType&>, RegisterType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, RegisterType& data)
     {componentInterface.publish(name, new CMPType< RegisterType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, ReplyValueType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, ReplyValueType&>, ReplyValueType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, ReplyValueType& data)
     {componentInterface.publish(name, new CMPType< ReplyValueType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, RequestSetValueType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, RequestSetValueType&>, RequestSetValueType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, RequestSetValueType& data)
     {componentInterface.publish(name, new CMPType< RequestSetValueType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, ReturnInfoType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, ReturnInfoType&>, ReturnInfoType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, ReturnInfoType& data)
     {componentInterface.publish(name, new CMPType< ReturnInfoType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, ReturnValueType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, ReturnValueType&>, ReturnValueType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, ReturnValueType& data)
     {componentInterface.publish(name, new CMPType< ReturnValueType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, QueryValueType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, QueryValueType&>, QueryValueType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, QueryValueType& data)
     {componentInterface.publish(name, new CMPType< QueryValueType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, QuerySetValueType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, QuerySetValueType&>, QuerySetValueType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, QuerySetValueType& data)
     {componentInterface.publish(name, new CMPType< QuerySetValueType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, LayeredType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, LayeredType&>, LayeredType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, LayeredType& data)
     {componentInterface.publish(name, new CMPType< LayeredType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, TimeType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, TimeType&>, TimeType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, TimeType& data)
     {componentInterface.publish(name, new CMPType< TimeType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, NewMetType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, NewMetType&>, NewMetType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, NewMetType& data)
     {componentInterface.publish(name, new CMPType< NewMetType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SoilWaterProfileLayerType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SoilWaterProfileLayerType&>, SoilWaterProfileLayerType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SoilWaterProfileLayerType& data)
     {componentInterface.publish(name, new CMPType< SoilWaterProfileLayerType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SoilWaterLayerType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SoilWaterLayerType&>, SoilWaterLayerType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SoilWaterLayerType& data)
     {componentInterface.publish(name, new CMPType< SoilWaterLayerType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, LateralFlowLayerType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, LateralFlowLayerType&>, LateralFlowLayerType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, LateralFlowLayerType& data)
     {componentInterface.publish(name, new CMPType< LateralFlowLayerType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SoilWaterBalanceType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SoilWaterBalanceType&>, SoilWaterBalanceType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SoilWaterBalanceType& data)
     {componentInterface.publish(name, new CMPType< SoilWaterBalanceType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, NewSoluteType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, NewSoluteType&>, NewSoluteType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, NewSoluteType& data)
     {componentInterface.publish(name, new CMPType< NewSoluteType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, layerType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, layerType&>, layerType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, layerType& data)
     {componentInterface.publish(name, new CMPType< layerType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SoluteProfileType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SoluteProfileType&>, SoluteProfileType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SoluteProfileType& data)
     {componentInterface.publish(name, new CMPType< SoluteProfileType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, IrrigatedType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, IrrigatedType&>, IrrigatedType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, IrrigatedType& data)
     {componentInterface.publish(name, new CMPType< IrrigatedType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, InterceptionType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, InterceptionType&>, InterceptionType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, InterceptionType& data)
     {componentInterface.publish(name, new CMPType< InterceptionType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, LightProfileType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, LightProfileType&>, LightProfileType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, LightProfileType& data)
     {componentInterface.publish(name, new CMPType< LightProfileType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, CanopyType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, CanopyType&>, CanopyType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, CanopyType& data)
     {componentInterface.publish(name, new CMPType< CanopyType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, CanopyWaterBalanceType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, CanopyWaterBalanceType&>, CanopyWaterBalanceType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, CanopyWaterBalanceType& data)
     {componentInterface.publish(name, new CMPType< CanopyWaterBalanceType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, OrganicMatterFractionType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, OrganicMatterFractionType&>, OrganicMatterFractionType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, OrganicMatterFractionType& data)
     {componentInterface.publish(name, new CMPType< OrganicMatterFractionType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, ResidueType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, ResidueType&>, ResidueType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, ResidueType& data)
     {componentInterface.publish(name, new CMPType< ResidueType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, soluteType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, soluteType&>, soluteType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, soluteType& data)
     {componentInterface.publish(name, new CMPType< soluteType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SurfaceWaterType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SurfaceWaterType&>, SurfaceWaterType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SurfaceWaterType& data)
     {componentInterface.publish(name, new CMPType< SurfaceWaterType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SurfaceWaterBalanceType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SurfaceWaterBalanceType&>, SurfaceWaterBalanceType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SurfaceWaterBalanceType& data)
     {componentInterface.publish(name, new CMPType< SurfaceWaterBalanceType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, FertiliserConstituentsType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, FertiliserConstituentsType&>, FertiliserConstituentsType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, FertiliserConstituentsType& data)
     {componentInterface.publish(name, new CMPType< FertiliserConstituentsType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, FPoolType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, FPoolType&>, FPoolType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, FPoolType& data)
     {componentInterface.publish(name, new CMPType< FPoolType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, FPoolProfileLayerType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, FPoolProfileLayerType&>, FPoolProfileLayerType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, FPoolProfileLayerType& data)
     {componentInterface.publish(name, new CMPType< FPoolProfileLayerType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, StandingFractionType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, StandingFractionType&>, StandingFractionType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, StandingFractionType& data)
     {componentInterface.publish(name, new CMPType< StandingFractionType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, LyingFractionType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, LyingFractionType&>, LyingFractionType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, LyingFractionType& data)
     {componentInterface.publish(name, new CMPType< LyingFractionType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SurfaceOrganicMatterType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SurfaceOrganicMatterType&>, SurfaceOrganicMatterType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SurfaceOrganicMatterType& data)
     {componentInterface.publish(name, new CMPType< SurfaceOrganicMatterType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SurfaceOrganicMatterDecompType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SurfaceOrganicMatterDecompType&>, SurfaceOrganicMatterDecompType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SurfaceOrganicMatterDecompType& data)
     {componentInterface.publish(name, new CMPType< SurfaceOrganicMatterDecompType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, NBalanceType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, NBalanceType&>, NBalanceType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, NBalanceType& data)
     {componentInterface.publish(name, new CMPType< NBalanceType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, CBalanceType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, CBalanceType&>, CBalanceType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, CBalanceType& data)
     {componentInterface.publish(name, new CMPType< CBalanceType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, IncorpFomType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, IncorpFomType&>, IncorpFomType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, IncorpFomType& data)
     {componentInterface.publish(name, new CMPType< IncorpFomType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SoilOrganicMatterType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SoilOrganicMatterType&>, SoilOrganicMatterType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SoilOrganicMatterType& data)
     {componentInterface.publish(name, new CMPType< SoilOrganicMatterType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, CropChoppedType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, CropChoppedType&>, CropChoppedType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, CropChoppedType& data)
     {componentInterface.publish(name, new CMPType< CropChoppedType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, NewProfileType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, NewProfileType&>, NewProfileType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, NewProfileType& data)
     {componentInterface.publish(name, new CMPType< NewProfileType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, NewCanopyType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, NewCanopyType&>, NewCanopyType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, NewCanopyType& data)
     {componentInterface.publish(name, new CMPType< NewCanopyType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, NewCropType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, NewCropType&>, NewCropType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, NewCropType& data)
     {componentInterface.publish(name, new CMPType< NewCropType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, NewZoneType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, NewZoneType&>, NewZoneType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, NewZoneType& data)
     {componentInterface.publish(name, new CMPType< NewZoneType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SoilLayersType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SoilLayersType&>, SoilLayersType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SoilLayersType& data)
     {componentInterface.publish(name, new CMPType< SoilLayersType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, rlv_layerType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, rlv_layerType&>, rlv_layerType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, rlv_layerType& data)
     {componentInterface.publish(name, new CMPType< rlv_layerType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, demandsType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, demandsType&>, demandsType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, demandsType& data)
     {componentInterface.publish(name, new CMPType< demandsType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastureWaterDemandType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastureWaterDemandType&>, PastureWaterDemandType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastureWaterDemandType& data)
     {componentInterface.publish(name, new CMPType< PastureWaterDemandType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, suppliesType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, suppliesType&>, suppliesType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, suppliesType& data)
     {componentInterface.publish(name, new CMPType< suppliesType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastureWaterSupplyType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastureWaterSupplyType&>, PastureWaterSupplyType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastureWaterSupplyType& data)
     {componentInterface.publish(name, new CMPType< PastureWaterSupplyType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, water_uptakeType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, water_uptakeType&>, water_uptakeType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, water_uptakeType& data)
     {componentInterface.publish(name, new CMPType< water_uptakeType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastureWaterUptakeType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastureWaterUptakeType&>, PastureWaterUptakeType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastureWaterUptakeType& data)
     {componentInterface.publish(name, new CMPType< PastureWaterUptakeType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, water_infoType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, water_infoType&>, water_infoType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, water_infoType& data)
     {componentInterface.publish(name, new CMPType< water_infoType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, WaterInfoType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, WaterInfoType&>, WaterInfoType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, WaterInfoType& data)
     {componentInterface.publish(name, new CMPType< WaterInfoType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, fomType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, fomType&>, fomType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, fomType& data)
     {componentInterface.publish(name, new CMPType< fomType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, FomAddedType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, FomAddedType&>, FomAddedType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, FomAddedType& data)
     {componentInterface.publish(name, new CMPType< FomAddedType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastureNutrientUptakeType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastureNutrientUptakeType&>, PastureNutrientUptakeType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastureNutrientUptakeType& data)
     {componentInterface.publish(name, new CMPType< PastureNutrientUptakeType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastureSowType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastureSowType&>, PastureSowType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastureSowType& data)
     {componentInterface.publish(name, new CMPType< PastureSowType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastureKillType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastureKillType&>, PastureKillType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastureKillType& data)
     {componentInterface.publish(name, new CMPType< PastureKillType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastureCultivateType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastureCultivateType&>, PastureCultivateType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastureCultivateType& data)
     {componentInterface.publish(name, new CMPType< PastureCultivateType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastureCutType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastureCutType&>, PastureCutType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastureCutType& data)
     {componentInterface.publish(name, new CMPType< PastureCutType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastureOnCutType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastureOnCutType&>, PastureOnCutType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastureOnCutType& data)
     {componentInterface.publish(name, new CMPType< PastureOnCutType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastureWeatherType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastureWeatherType&>, PastureWeatherType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastureWeatherType& data)
     {componentInterface.publish(name, new CMPType< PastureWeatherType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, FaecesType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, FaecesType&>, FaecesType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, FaecesType& data)
     {componentInterface.publish(name, new CMPType< FaecesType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, FaecesInorgType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, FaecesInorgType&>, FaecesInorgType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, FaecesInorgType& data)
     {componentInterface.publish(name, new CMPType< FaecesInorgType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, IntakeType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, IntakeType&>, IntakeType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, IntakeType& data)
     {componentInterface.publish(name, new CMPType< IntakeType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, PastIntakeType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, PastIntakeType&>, PastIntakeType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, PastIntakeType& data)
     {componentInterface.publish(name, new CMPType< PastIntakeType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SuppIntakeType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SuppIntakeType&>, SuppIntakeType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SuppIntakeType& data)
     {componentInterface.publish(name, new CMPType< SuppIntakeType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, faeces_omType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, faeces_omType&>, faeces_omType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, faeces_omType& data)
     {componentInterface.publish(name, new CMPType< faeces_omType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, faeces_inorgType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, faeces_inorgType&>, faeces_inorgType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, faeces_inorgType& data)
     {componentInterface.publish(name, new CMPType< faeces_inorgType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, urineType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, urineType&>, urineType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, urineType& data)
     {componentInterface.publish(name, new CMPType< urineType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, AddExcretaType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, AddExcretaType&>, AddExcretaType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, AddExcretaType& data)
     {componentInterface.publish(name, new CMPType< AddExcretaType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, RemoveHerbageType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, RemoveHerbageType&>, RemoveHerbageType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, RemoveHerbageType& data)
     {componentInterface.publish(name, new CMPType< RemoveHerbageType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SuppEatenType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SuppEatenType&>, SuppEatenType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SuppEatenType& data)
     {componentInterface.publish(name, new CMPType< SuppEatenType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, herbageType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, herbageType&>, herbageType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, herbageType& data)
     {componentInterface.publish(name, new CMPType< herbageType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, seedType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, seedType&>, seedType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, seedType& data)
     {componentInterface.publish(name, new CMPType< seedType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, Plant2StockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, Plant2StockType&>, Plant2StockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, Plant2StockType& data)
     {componentInterface.publish(name, new CMPType< Plant2StockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, BuyStockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, BuyStockType&>, BuyStockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, BuyStockType& data)
     {componentInterface.publish(name, new CMPType< BuyStockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SellStockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SellStockType&>, SellStockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SellStockType& data)
     {componentInterface.publish(name, new CMPType< SellStockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, CastrateStockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, CastrateStockType&>, CastrateStockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, CastrateStockType& data)
     {componentInterface.publish(name, new CMPType< CastrateStockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, DryOffStockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, DryOffStockType&>, DryOffStockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, DryOffStockType& data)
     {componentInterface.publish(name, new CMPType< DryOffStockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, JoinStockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, JoinStockType&>, JoinStockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, JoinStockType& data)
     {componentInterface.publish(name, new CMPType< JoinStockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, MoveStockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, MoveStockType&>, MoveStockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, MoveStockType& data)
     {componentInterface.publish(name, new CMPType< MoveStockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, ShearStockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, ShearStockType&>, ShearStockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, ShearStockType& data)
     {componentInterface.publish(name, new CMPType< ShearStockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SplitStockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SplitStockType&>, SplitStockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SplitStockType& data)
     {componentInterface.publish(name, new CMPType< SplitStockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, TagStockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, TagStockType&>, TagStockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, TagStockType& data)
     {componentInterface.publish(name, new CMPType< TagStockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, WeanStockType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, WeanStockType&>, WeanStockType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, WeanStockType& data)
     {componentInterface.publish(name, new CMPType< WeanStockType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, dmType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, dmType&>, dmType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, dmType& data)
     {componentInterface.publish(name, new CMPType< dmType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, RemoveCropDmType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, RemoveCropDmType&>, RemoveCropDmType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, RemoveCropDmType& data)
     {componentInterface.publish(name, new CMPType< RemoveCropDmType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, RemoveResidueDmType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, RemoveResidueDmType&>, RemoveResidueDmType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, RemoveResidueDmType& data)
     {componentInterface.publish(name, new CMPType< RemoveResidueDmType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SupplementBuyType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SupplementBuyType&>, SupplementBuyType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SupplementBuyType& data)
     {componentInterface.publish(name, new CMPType< SupplementBuyType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SupplementFeedType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SupplementFeedType&>, SupplementFeedType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SupplementFeedType& data)
     {componentInterface.publish(name, new CMPType< SupplementFeedType >(data));}

void CMPScienceAPI::subscribe(const std::string& name, boost::function1<void, SupplementMixType&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, SupplementMixType&>, SupplementMixType > (handler, false));}
void CMPScienceAPI::publish(const std::string& name, SupplementMixType& data)
     {componentInterface.publish(name, new CMPType< SupplementMixType >(data));}

void CMPScienceAPI::subscribe(const string& name, boost::function1<void, Variant&> handler)
     {componentInterface.subscribe(name, new CMPMethod1<boost::function1<void, Variant&>, Variant > (handler, false));}
void CMPScienceAPI::publish(const string& name, Variant& data)
     {componentInterface.publish(name, new CMPType< Variant >(data));}
