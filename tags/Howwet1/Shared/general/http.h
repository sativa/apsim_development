//---------------------------------------------------------------------------
#ifndef GENERAL_HTTP_H
#define GENERAL_HTTP_H

//---------------------------------------------------------------------------
// This tiny http class encapsulates libXMLs nanoHTTP interface.
//---------------------------------------------------------------------------
class tHTTP 
   {
   private:
      void *myContext;
      char *myContentType;

   public:
      tHTTP(void);
      ~tHTTP();

      std::string Get(const std::string& url);                        // get a URL, returning string
      bool Get(const std::string& filename, const std::string& url);   // get a URL, contents into file, true on success
      
      int responseCode(void);
      std::string responseText(void);
      std::string contentType(void);
   };

#endif
