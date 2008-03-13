#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PhotosynthesisModel.h"
#include "RUEModel.h"
using namespace std;

// Return one of the PhotosynthesisModel objects we know about.
PhotosynthesisModel* constructPhotosynthesisModel (ScienceAPI& scienceAPI, plantInterface& p)
  {
  PhotosynthesisModel *object;

  string type;
  if(!scienceAPI.readOptional("photosynthesismodel", type))
    type = "rue";
  //scienceAPI.readOptional("photosynthesismodel", type);

  if (type == "rue")
     {
    object = new RUEModel(scienceAPI, p);
     }
  else if (type == "wang")
   {
//    object = new WangModel(scienceAPI, p);
   }
  else
    throw std::invalid_argument("Unknown Photosynthesis Model '" + type + "'");

  return (object);
  }