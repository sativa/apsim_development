#pragma hdrstop

#include "CMPScienceAPI.h"
#include "CMPComponentInterface.h"
#include "CMPData.h"

using namespace std;
CMPScienceAPI::CMPScienceAPI(CMPComponentInterface& componentinterface)
   : componentInterface(componentinterface){}

std::string CMPScienceAPI::name()
   {return componentInterface.getName();}
std::string CMPScienceAPI::parent()
   {return componentInterface.getParentName();}
void CMPScienceAPI::write(const std::string& msg)
   {
   componentInterface.write(msg);
   }

void CMPScienceAPI::query(const std::string& pattern, std::vector<QueryMatch>& matches)
   {
   componentInterface.query(pattern, matches);
   }

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
   return componentInterface.read(name, CMPBuiltIn<bool >(data), optional);
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
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, int& data, int lower, int upper)
   {
   return componentInterface.read(name, CMPBuiltInBounded<int, int >(name, data, lower, upper), optional);
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
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, float& data, float lower, float upper)
   {
   return componentInterface.read(name, CMPBuiltInBounded<float, float >(name, data, lower, upper), optional);
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
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, double& data, double lower, double upper)
   {
   return componentInterface.read(name, CMPBuiltInBounded<double, double >(name, data, lower, upper), optional);
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
   return componentInterface.read(name, CMPBuiltIn<std::string >(data), optional);
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
   return componentInterface.read(name, CMPBuiltIn<std::vector<bool> >(data), optional);
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
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<int>& data, int lower, int upper)
   {
   return componentInterface.read(name, CMPBuiltInBounded<std::vector<int>, int >(name, data, lower, upper), optional);
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
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<float>& data, float lower, float upper)
   {
   return componentInterface.read(name, CMPBuiltInBounded<std::vector<float>, float >(name, data, lower, upper), optional);
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
bool CMPScienceAPI::read(const std::string& name, const std::string& units, bool optional, std::vector<double>& data, double lower, double upper)
   {
   return componentInterface.read(name, CMPBuiltInBounded<std::vector<double>, double >(name, data, lower, upper), optional);
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
   return componentInterface.read(name, CMPBuiltIn<std::vector<std::string> >(data), optional);
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


