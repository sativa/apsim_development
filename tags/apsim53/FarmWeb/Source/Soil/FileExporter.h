//---------------------------------------------------------------------------

#ifndef FileExporterH
#define FileExporterH
class Soils;
//---------------------------------------------------------------------------
// This class will generate a set of file(s) given a template file and
// a charactData object.
//---------------------------------------------------------------------------
class FileExporter
   {
   public:
      FileExporter(void) { }

      void doExport(Soils& soils, unsigned refno,
                    const std::string& templateFile,
                    std::vector<std::string>& filesGenerated);

   private:
   };
#endif
